# Allow Multiple Outstanding Continuations at the Host Interface

Status: Implemented (2026-02-20)

This proposal extends the VM/host boundary so the host can hold **multiple** Rusk continuations
concurrently (as opaque handles) and resume them later, while ensuring the captured Rusk state
remains alive until resumed or explicitly dropped.

---

## Problem

Rusk already supports **first-class, one-shot** delimited continuations in-language:

- `perform` captures a continuation token `k` (type `cont(<param>) -> <ret>`) when a resumptive
  handler clause requests it.
- A handler may store `k` and return without resuming, then resume `k` later.

At the **host interface** (bytecode FFI / step API), we currently have two constraints that block a
host-owned continuation storage pattern:

1) **Continuation tokens cannot cross the ABI boundary**
   - `AbiValue` intentionally excludes continuation tokens (except as the dedicated `k` field in
     `StepResult::Request` for externalized effects).

2) **Only one suspended external request per VM**
   - `StepResult::Request { .., k: ContinuationHandle }` suspends the VM, and the host must call
     `vm_resume(vm, k, value)` or `vm_drop_continuation(vm, k)` before `vm_step` can be called
     again.

The expected embedding scenario (from the motivating snippet) is:

- a continuation `k` is captured inside the VM,
- the host stores it in host-owned state (`cont = Some(k)` on the host side),
- the host-held `cont` keeps the captured Rusk computation **alive** on the Rusk side until resumed.

Today this is not possible because there is no stable, ABI-safe way to pass continuation tokens to
the host *and* keep them rooted once Rusk drops its last reference.

---

## Goals

- Allow the VM to pass a continuation token to the host as an **opaque, ABI-safe handle**.
- Allow the host to store **multiple** such handles concurrently (per VM).
- Ensure host-held handles keep the continuation’s captured Rusk state **alive** (GC-rooted) until:
  - resumed (one-shot; consumes), or
  - explicitly dropped by the host.
- Keep existing invariants:
  - continuations remain **one-shot**
  - synchronous host imports remain **non-reentrant** (no `vm_step`/`vm_resume` from within a host
    call)

---

## Non-goals

- Multi-shot continuations.
- Cross-VM continuation migration (a handle is only meaningful within the VM it came from).
- Serializing continuations into `.rbc` (handles are runtime-only).
- Multiple simultaneous *pending* `StepResult::Request` suspensions inside a single VM (requires a
  scheduler/fibers and is a separate proposal).

---

## Motivation (Concrete Example)

We want to make patterns like this feasible when the storage is on the host side:

```rusk
interface Foo { fn bar(); }

// Host surface (conceptual):
//   host::store_cont(k: cont () -> unit) -> unit
//   host::take_cont() -> Option<cont () -> unit>

fn main() {
    match @Foo.bar() {
        @Foo.bar() -> k => {
            // store k on the host, not in Rusk locals/heap
            host::store_cont(k);
        }
        _ => ()
    }

    match host::take_cont() {
        Some(k) => k(), // resume later
        None => ()
    }
}
```

For this to be correct, once `host::store_cont(k)` returns, the continuation must remain valid
even if Rusk drops all references to `k`.

---

## Proposed Design

### 1) Make continuation handles ABI-safe values

Extend the VM boundary ABI with a new type/value:

- `AbiType::Continuation`
- `AbiValue::Continuation(ContinuationHandle)`

Where `ContinuationHandle` is a small stable identifier, e.g.:

```text
ContinuationHandle { index: u32, generation: u32 }
```

This matches our existing boundary design principles:

- no raw pointers
- stable, fixed-width integers
- explicit invalidation via generation

### 2) Add a VM-managed “pinned continuation” table (GC-rooted)

Introduce a per-VM table that stores continuation tokens that have been exported to the host:

- Conceptually: `Vec<Slot { generation: u32, token: Option<ContinuationToken> }>`
- A `ContinuationHandle` points at a slot.

When exporting a continuation token to the host, the VM:

1. Allocates (or reuses) a free slot.
2. Stores a clone of the internal continuation token in the slot.
3. Returns `AbiValue::Continuation(handle)` to the host.

**GC rooting requirement**

The pinned continuation table must be part of the VM root set so that:

- captured frames/values referenced by pinned continuations keep heap objects alive.

Dropping a pinned continuation handle removes it from the table, allowing the captured state to be
collected normally.

### 3) Conversion rules at the boundary

#### VM → host (argument/result conversion)

When a `Value::Continuation(token)` crosses the boundary (as a host import arg/result, or as an
externalized effect arg/result), it converts to:

- `AbiValue::Continuation(handle)` where `handle` pins `token` in the VM table.

This makes it possible for host code to store continuations without holding any VM-internal data
structures.

#### host → VM

When the VM receives `AbiValue::Continuation(handle)`, it converts to:

- `Value::Continuation(token)` by looking up `handle` in the pinned table.

If the handle is invalid (wrong generation, empty slot, or already-consumed one-shot state), the VM
traps or returns a structured `InvalidContinuation` error (implementation choice, but it should be
consistent with current invalid-resume behavior).

### 4) Host-visible operations (drop/unpin)

Even with ABI support, the host needs a way to release pinned continuations to avoid leaks.

Add (at minimum) a VM API:

- `vm_drop_pinned_continuation(vm, handle) -> Result<(), VmError>`

Semantics:

- Removes the pinned entry (if still valid).
- After dropping, the handle becomes invalid.
- Dropping without resuming is explicitly allowed (matches MIR “abandoning a continuation”).

Optionally add:

- `vm_is_valid_pinned_continuation(vm, handle) -> bool` (debugging/FFI ergonomics)

### 4b) Host-visible tail resume (non-interlaced)

While pinned continuation handles can always be passed back into Rusk code and resumed in-language
(`k(value)`), it is also useful for embeddings to be able to resume a pinned continuation
**directly from the host**.

This proposal implements a *tail-resume* operation at the host boundary:

- `vm_resume_pinned_continuation_tail(vm, handle, value) -> Result<(), VmError>`

Semantics:

- Consumes the one-shot continuation state referenced by `handle`.
- Tail-resumes by **splicing** the captured continuation segment *on top of* the current VM stack.
  This matches the intended embedding model: the host "handler frame" is not representable in the
  VM, so a tail-resume from the host behaves like "replace the (logical) host handler frame with
  the continuation segment", which degrades to "push the continuation segment on top of the
  current VM stack".
- The continuation’s final return value is **discarded** (there is no "second half" host handler
  frame to receive it). If the VM stack is empty, the resumed continuation becomes the new entry
  computation and its return value becomes the program result as usual.
- The operation is **schedule-only**: it does not step the VM or produce a `StepResult`. The host
  must call `vm_step` afterwards to drive execution and observe the next VM boundary (`yield` /
  `request` / `trap` / `done`).

This is intentionally *tail-only*: a non-tail host resume would require the host to keep state for
the “second half” of a host effect handler, which is out of scope for the current embedding model.

### 5) One-shot semantics remain unchanged

Pinned continuations are still **one-shot**:

- If the continuation is resumed (in-language `resume`, or any future host-driven resume API), the
  underlying state is consumed and further resume attempts must fail.

This is naturally enforced if the pinned table stores the same internal one-shot token that
in-language `resume` already consumes.

---

## Type Surface Changes (Compiler / MIR / Bytecode)

To typecheck host APIs that accept/return continuations, the host signature type model must be able
to express `cont`.

### Compiler/MIR host types

Extend `HostType` with a continuation form:

```rust
HostType::Cont { param: Box<HostType>, ret: Box<HostType> }
```

Map it to the existing internal type representation:

```text
Ty::Cont { param: <ty>, ret: <ty> }
```

### Bytecode ABI types

Extend `rusk_bytecode::AbiType` with:

- `AbiType::Continuation`

This ABI type is **opaque** and does not encode `param/ret` (those remain a compile-time surface
for typechecking and tooling). Any `cont(P)->R` in host signatures lowers to
`AbiType::Continuation`.

### `.rbc` format version bump

Because ABI type tags are serialized in `.rbc`, adding a new `AbiType` requires a minor format
bump (e.g. `0.9 → 0.10`).

---

## Interaction with Existing Externalized Effects

This proposal intentionally does **not** change the “single outstanding external request per VM”
rule:

- A `StepResult::Request` still suspends the VM until `vm_resume` / `vm_drop_continuation`.

What changes is that *within a running VM* the program can capture continuation tokens and export
them to the host as pinned handles, without suspending the VM.

This enables the host to hold multiple outstanding continuations while the VM continues running
(i.e. multiple outstanding *handles*, not multiple pending `vm_step` suspensions).

---

## Safety / Correctness Notes

- **Liveness**: Host-held continuation handles keep captured Rusk state alive because the VM pins
  the corresponding token in a GC-rooted table.
- **Invalid handles**: Generation checks prevent use-after-free when a slot is reused.
- **Leaks**: If the host never drops a pinned continuation, the captured stack remains live. This
  is expected (host-owned resource). The API must make dropping explicit and easy.
- **Reentrancy**: No change is required to the “no VM re-entry during host import call” rule. Host
  imports may receive/store handles, but must not attempt to drive the VM from inside a host call.

---

## Alternatives Considered

1) **Require programs to store continuations in-language only**
   - Works today, but blocks embeddings where the host must own the continuation lifecycle (e.g.
     integration with a native event loop that stores wakeups externally).

2) **Use multiple VMs instead of multiple continuations**
   - Avoids cross-boundary continuation values, but forces state partitioning and complicates
     shared-memory language-level coordination.

3) **Allow multiple pending `StepResult::Request` per VM**
   - Requires fibers/scheduler inside the VM or a split-stack model. This is likely feasible but
     significantly larger in scope than “continuations as host-storable values”.

---

## Implementation Sketch (VM)

1. Add a pinned continuation table to `Vm`.
2. Include it in GC root tracing.
3. Extend ABI conversion:
   - `Value::Continuation` ↔ `AbiValue::Continuation(handle)` via the pinned table.
4. Add `vm_drop_pinned_continuation`.
5. Bump `.rbc` ABI type encoding version and update encoder/decoder/verifier accordingly.

---

## Open Questions

- Should `StepResult::Request.k` be unified with the pinned-table handle mechanism (so all
  host-visible continuations are “the same kind of handle”)?
- Should `ContinuationHandle` include a VM instance identifier to detect cross-VM misuse in debug
  builds?
- Do we want a non-tail host resume API (interlaced host/VM control flow)?

  Currently we intentionally only provide **tail resumption** at the host boundary via:

  - `vm_resume_pinned_continuation_tail(vm, handle, value)`

  Rationale:

  - In-language `resume` / `resume_tail` are defined in terms of a *handler frame* and a captured
    *continuation segment*.
  - A host "handler frame" is not representable in the VM stack, so a host-driven resume cannot
    sensibly "return to the host handler after resuming" without introducing a more complex
    interlaced host/VM execution model.
  - Tail-resume semantics avoid this: the host consumes the pinned continuation state and splices
    the captured continuation segment onto the VM stack without any host-side "second half" to
    return into.
