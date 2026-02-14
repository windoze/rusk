# VM GC: Handle-Based Immix (Stop-the-World, Single-Threaded)

Date: 2026-02-14

This proposal describes how to replace the current bytecode VM heap representation
(`Rc<RefCell<...>>`) with a **stable handle** representation and implement a
specialized/simplified **Immix-style** GC suitable for a **single-threaded, STW**
runtime.

The key constraints:

- **Moving/compaction-friendly references**: VM object references must not be raw pointers.
- **No `unsafe` in the VM**: `crates/rusk-vm` keeps `#![forbid(unsafe_code)]`.
- All `unsafe` needed for low-level heap management is **contained** inside a new GC crate.

---

## Motivation

Today, the bytecode VM (`crates/rusk-vm`) represents heap objects as:

- `RefValue { obj: Rc<RefCell<HeapValue>> }`

This has several downsides:

- Prevents **moving / compaction**, because the VM’s “reference” is literally an owning pointer.
- Adds per-object overhead (refcount + `RefCell`) and runtime borrow checks.
- Makes GC experimentation hard: memory management is outsourced to Rust’s allocator + refcounting.

We want a real GC heap with:

- stable **handles** (`(index, generation)`) that survive movement
- an Immix-style allocator/collector for reasonable performance and fragmentation behavior

---

## Goals

1) Replace VM object references with **slot/handle style** references:
   - New VM ref type: `GcRef { index: u32, generation: u32 }` (name bikesheddable).

2) Introduce a dedicated GC crate for the VM, with a safe public API:
   - e.g. `crates/rusk-gc` (name bikesheddable).

3) Implement an Immix-inspired heap:
   - STW, single-threaded, no read/write barriers
   - line/block metadata + hole-filling allocation
   - optional selective defragmentation via evacuation (moving)

4) Keep **all `unsafe` contained** within the GC crate:
   - `rusk-vm` remains `#![forbid(unsafe_code)]`.

---

## Non-Goals (For This Proposal)

- Concurrent / incremental GC
- Generational GC (and its write barrier / remembered sets)
- Exposing raw pointers to heap objects across any API boundary
- Fully compacting “everything” every cycle (Immix defrag should be selective/bounded)
- Reworking all “payload” allocations (e.g. array buffers) to live inside the GC heap
  - this can be follow-up work; the first milestone focuses on stable handles + movable headers

---

## Design Overview

### New Crate Boundary

Create a new workspace crate:

- `crates/rusk-gc/` — the only place `unsafe` is allowed.

`crates/rusk-vm` depends on `rusk-gc` and uses only safe APIs.

### Handle / Slot Model

Use a stable handle indirection:

- `GcRef = (slot_index, generation)`
- `slots: Vec<Slot>` where `Slot` stores:
  - generation (for stale handle detection)
  - pointer to the current object location (updated on move/evacuation)
  - optional bookkeeping (mark bits, pinned flag, etc.)

This provides:

- **stable references** for VM values
- ability to **move objects** by updating `slots[handle.index].ptr`
- stale handle detection via generation mismatch

### Object Header Includes Slot Index

Each heap object has a header that includes at least:

- `slot_index: u32`

This supports the desired property:

- when scanning objects by address (sweep/defrag), the collector can recover the owning slot
  directly from the header, without needing a separate reverse map (ptr → slot).

Important clarification:

- The header containing `slot_index` does **not** eliminate the need for a slot table.
  We still need `handle → current address` mapping for VM access and relocation.

### VM Representation Changes

In `crates/rusk-vm`:

- Replace `RefValue { obj: Rc<RefCell<HeapValue>> }` with:
  - `RefValue { readonly: bool, handle: GcRef }`
- Store a heap in the VM:
  - `heap: rusk_gc::ImmixHeap<HeapValue>` (exact type TBD)

Then all object access becomes:

- `heap.get(handle)` / `heap.get_mut(handle)` instead of `r.obj.borrow()` / `borrow_mut()`.

Because the VM executes with `&mut Vm`, we can structure borrows so that:

- object borrows cannot outlive the VM borrow
- allocations/GC cannot happen while `&HeapValue` / `&mut HeapValue` borrows are live

This is a major reason a safe API is feasible without `RefCell`.

---

## Immix Model (Simplified)

### Terminology

- **Block**: fixed-size region (e.g. 32KiB) managed by the GC
- **Line**: fixed-size subdivision (e.g. 128B) within a block

The collector maintains metadata per block and per line:

- line mark table (“this line contains at least one live object”)
- block state (available holes, allocation cursor, fragmentation stats, etc.)

### Collection Phases (STW)

1) **Mark**
   - Start from roots (VM state, frames, handlers, continuation states).
   - For each reachable handle:
     - check slot/generation
     - mark object
     - mark lines spanned by the object
     - trace the object payload to discover more handles

2) **Sweep / Hole Discovery**
   - Iterate blocks and identify free vs marked lines.
   - For unreachable objects:
     - run their destructors (see “Drop Semantics”)
     - free their slots (advance generation, push to free list)
   - Recompute per-block allocation data structures for hole-filling.

3) **Optional Defragmentation (Evacuation)**
   - Choose candidate blocks with high fragmentation.
   - For each live object in those blocks:
     - allocate new space elsewhere
     - **move** the object (not memcpy; see below)
     - update the slot table pointer using `slot_index` from the header
   - Reclaim evacuated blocks (ideally whole blocks become free).

Key benefit of handle-based references:

- object fields contain handles, not raw pointers, so evacuation does **not** require updating
  pointers inside objects; only slot table pointers change.

### Drop Semantics (Important)

VM heap objects contain owned Rust data (`String`, `Vec`, etc.). Therefore:

- unreachable objects must have their destructors run during sweep
- moved objects must not be dropped twice

In practice, this implies the GC crate will store payloads in raw memory using something like:

- `ManuallyDrop<T>` / `MaybeUninit<T>`

and it will perform explicit `drop_in_place` when reclaiming.

This is one of the main reasons the implementation requires `unsafe`, and why it must be confined
to `rusk-gc`.

---

## Root Set (VM-Specific)

At minimum, tracing must include:

- `Vm.frames[*].regs[*]` (register file values)
- any values stored in the VM state machine:
  - e.g. suspended execution state if it holds `Value`s (not just `AbiValue`)
- continuation state captured by language-level continuations:
  - `Value::Continuation(...)` currently contains frames/handlers with `Value`s

Even if internal continuations remain refcounted (e.g. `Rc<...>`), they still embed `Value`s.
Those embedded values must be traced so their referenced heap objects remain live.

Note: the external `ContinuationHandle` used by the step API is not, by itself, a GC root unless it
indirectly keeps `Value`s alive. (Today it is mostly a VM “suspended” token, not a heap object.)

---

## Proposed `rusk-gc` Public API Shape (Sketch)

This is intentionally minimal and safe:

- `pub struct GcRef { index: u32, generation: u32 }`
- `pub trait Trace { fn trace(&self, tracer: &mut dyn Tracer); }`
- `pub trait Tracer { fn mark(&mut self, handle: GcRef); }`

- `pub trait GcHeap<T> {`
  - `fn alloc(&mut self, value: T) -> GcRef;`
  - `fn get(&self, handle: GcRef) -> Option<&T>;`
  - `fn get_mut(&mut self, handle: GcRef) -> Option<&mut T>;`
  - `fn collect(&mut self, roots: &dyn Trace);`
  - `fn live_objects(&self) -> usize;`
  - `}`

Then `rusk-vm` implements `Trace` for its `Value` and `HeapValue` types, and calls
`vm.heap.collect(&vm_roots)` at safe points.

(This is similar to the existing `crates/rusk-interpreter/src/gc.rs` traits, but VM-oriented and
implemented in a separate crate.)

---

## Migration Plan (Implementation Steps)

This is a staged plan to reduce risk.

### Phase 1 — Refactor VM to Handles (No Immix Yet)

1) Add `crates/rusk-gc` with:
   - handle type (`GcRef`)
   - trace traits
   - a simple baseline heap implementation (e.g. mark-sweep over a slot table)

2) Update `crates/rusk-vm`:
   - replace `Rc<RefCell<HeapValue>>` with `GcRef`
   - route all heap accesses through `vm.heap.get/get_mut`
   - add a conservative root tracer for the VM state/frames/continuations

3) Add tests that lock in the critical invariants:
   - handles remain valid across GC cycles
   - stale handles are rejected after object collection + slot reuse
   - readonly view behavior remains unchanged

Rationale: this phase isolates “VM semantics changes” from “Immix complexity”.

### Phase 2 — Immix Allocator + Mark/Sweep (Non-Moving)

4) Implement Immix space management in `rusk-gc`:
   - blocks + lines + metadata
   - hole-filling allocation
   - mark phase uses the `Trace` API; sweep discovers holes and drops dead objects

5) Swap `rusk-vm` to use the Immix heap implementation.

### Phase 3 — Selective Defragmentation (Moving)

6) Implement evacuation of selected blocks:
   - uses header `slot_index` to update slot pointers while scanning blocks
   - bounded copying (cap max bytes or max blocks per cycle)

7) Add tests for “moving correctness”:
   - allocate graph, trigger defrag, verify all reachable objects still readable/mutable
   - verify no use-after-free via stale handles

---

## Risks / Concerns

- `rusk-vm` currently forbids `unsafe` and will keep doing so; the `rusk-gc` API must be designed so
  the VM never needs to “peek behind the curtain”.
- `HeapValue` contains owned Rust allocations; correct drop + move semantics are mandatory to avoid
  leaks or double-free.
- Immix defrag should be selective; evacuating everything defeats the point and increases pause
  time.
- Long-term performance may be limited if large buffers remain allocated outside the GC heap. This
  is acceptable initially, but should be tracked.

---

## Open Questions

1) Should we share types with the MIR interpreter GC (`crates/rusk-interpreter/src/gc.rs`), or keep
   the VM heap fully separate for now?

2) Do we want the VM heap to ever expose “pinned” objects (non-movable) for future FFI needs?

3) What block/line sizes should we start with (tunable constants), and how do we want to measure
   fragmentation in practice for selecting defrag candidates?

