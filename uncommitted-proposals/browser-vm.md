# Browser Bytecode VM (WASM + WASM GC)

Date: 2026-02-15  
Status: Draft

This proposal describes a plan to run the **Rusk bytecode VM** in the browser as a WebAssembly
module, while using **WebAssembly GC** (host-provided GC) for VM heap objects instead of Rusk’s
built-in/custom GC (`crates/rusk-gc`).

It also specifies a **minimal JavaScript ↔ VM interface** for:

- **Host functions** (bytecode `CallTarget::Host`): synchronous calls into JavaScript.
- **Externalized effects** (bytecode `perform` → `StepResult::Request`): suspension points handled
  by JavaScript with explicit resumption/cancellation.

The design goal is to keep the browser embedding surface small, stable, and portable while still
allowing rich platform integration to live in JavaScript (DOM, timers, fetch, etc.).

---

## Motivation

Rusk is designed to be embedded. The browser is an important embedding target:

- **Easy distribution**: a `.wasm` + `.rbc` can be loaded via standard web tooling.
- **Host integration**: DOM, networking, storage, and UI are “native” in JavaScript.
- **Concurrency model**: the JS event loop maps naturally onto Rusk’s **step-driven** VM and
  **externalized effects** model.

Today’s bytecode VM (`crates/rusk-vm`) uses a custom GC heap (`crates/rusk-gc`). This is a good fit
for native execution, but suboptimal for browsers:

- increases WASM bundle size (allocator + GC + metadata),
- duplicates memory management (custom VM GC + browser GC),
- complicates integration with host-managed JS values.

WebAssembly GC enables the VM to allocate and reference GC-managed objects directly, letting the
engine reclaim VM heap objects without shipping an in-VM collector.

---

## Goals

### Runtime / VM goals

- Run a `rusk_bytecode::ExecutableModule` in a browser-hosted WASM instance.
- Use **WASM GC** for VM heap objects (structs/enums/arrays/tuples) instead of `rusk-gc`.
- Preserve the **bytecode semantics** as specified in `BYTECODE_SPEC.md`:
  - deterministic execution given deterministic host calls/effect handling,
  - non-reentrant host calls,
  - externalized effects suspend and resume via continuation handles.

### Embedding / interop goals

- Define a **minimal** JavaScript ↔ VM interface that:
  - installs host function implementations,
  - drives execution with `step`,
  - handles `Request` results (“externalized effects”) in JavaScript,
  - resumes or cancels execution via `resume` / `drop_continuation`.
- Keep the ABI boundary restricted to `AbiValue` primitives:
  `unit | bool | int(i64) | float(f64) | string | bytes`.

### Build / tooling goals

- Produce a browser-consumable WASM artifact (`wasm32-unknown-unknown`).
- Provide a thin JS wrapper (ESM) that implements ergonomic APIs on top of the minimal exports.

---

## Non-goals (for this proposal)

- Exposing VM heap objects (`struct/enum/array/tuple`) directly to JavaScript as mutable shared
  objects. (We keep v0 boundary limited to ABI primitives.)
- “Yield across host calls”: host imports remain synchronous and must not suspend the VM.
- Multi-threaded VM execution or shared-memory concurrency inside a single VM instance.
- JIT or ahead-of-time compilation of bytecode to native code.
- Committing to a single binding technology forever (we may start with wasm-bindgen and later move
  to component-model tooling, as long as the JS surface remains stable).

---

## Background: current bytecode VM boundary

The current VM boundary (see `BYTECODE_SPEC.md` and `docs/embedding-vm.md`) already splits host
interaction into two mechanisms:

1) **Host imports**: synchronous calls (`CallTarget::Host(HostImportId)`) with ABI-safe args/ret.
2) **Externalized effects**: unhandled `perform` becomes `StepResult::Request { effect_id, args, k }`
   and the host later calls `resume(k, value)` or `drop_continuation(k)`.

This proposal preserves that boundary, but relocates the “heap object memory management” from
`crates/rusk-gc` into the WebAssembly engine’s GC.

---

## Design overview

### 1) New WASM-facing crate

Introduce a new crate dedicated to browser WASM output (name bikesheddable):

- `crates/rusk-vm-wasm/`

Responsibilities:

- expose a small WASM export surface for:
  - VM creation from `.rbc` bytes (or from a decoded module),
  - `step`, `resume`, `drop_continuation`,
  - host import installation,
  - minimal inspection helpers (host import list, external effect list).
- contain browser-specific glue (feature flags, `wasm-bindgen` or component bindings).

Non-responsibilities:

- it should not contain “DOM stdlib” itself; that belongs in JavaScript host code built on top of
  externalized effects.

### 2) Make `rusk-vm` heap pluggable (GC backend abstraction)

Today `crates/rusk-vm` directly embeds `rusk_gc::ImmixHeap<HeapValue>`.

To enable a WASM-GC heap, refactor the VM so heap management is behind a small internal trait:

```rust
trait VmHeap {
    type Ref: Copy + Eq; // VM heap reference type
    fn alloc(&mut self, v: HeapValue<Self::Ref>) -> Self::Ref;
    fn get(&self, r: Self::Ref) -> Option<&HeapValue<Self::Ref>>;
    fn get_mut(&mut self, r: Self::Ref) -> Option<&mut HeapValue<Self::Ref>>;
    fn maybe_collect(&mut self, roots: &dyn Trace<Self::Ref>);
}
```

Key follow-up refactors required:

- Make `Value::Ref` store `H::Ref` instead of `rusk_gc::GcRef`.
- Make `HeapValue` parameterized over the reference type (so fields can contain references):
  `HeapValue<R> { fields: Vec<Value<R>> }` etc.
- Keep the tracing interface, but make it parametric as well:
  `Trace<R>` / `Tracer<R>`.

We retain the existing native backend as one implementation:

- `ImmixHeapBackend` backed by `rusk-gc` (native / CLI builds).

We add a new browser backend:

- `WasmGcHeapBackend` (details below).

### 3) WASM GC heap representation strategy

The VM heap stores objects with identity:

- structs, enums, arrays, tuples

Constraints when using WASM GC:

- WASM GC objects are managed by the engine; we do **not** run Rust destructors for unreachable
  objects.
- Therefore, **heap objects must not own Rust-managed allocations** that would require `Drop` to
  reclaim memory (e.g. `String`, `Vec`, `HashMap`) unless those allocations are themselves GC-traced
  and reclaimed by the engine.

This pushes us toward representing heap objects and their payloads as GC-managed values:

- GC `struct` and GC `array` types inside WASM, and/or
- JS host objects referenced via `externref` (also traced by the engine).

#### Recommended approach (phased)

We implement the browser heap in two stages to reduce toolchain risk:

**Stage A — Host-GC heap via `externref` (bootstrap, fastest to ship)**

- Represent each Rusk heap object as an opaque JS object (or a small JS class instance).
- Store references to those objects inside WASM as `externref`.
- The JS engine GC then naturally collects unreachable heap objects.

Pros:

- works with mature toolchains (`wasm-bindgen`) today,
- easy to introspect/debug in DevTools,
- avoids custom GC and avoids destructor issues (JS owns the memory).

Cons:

- field loads/stores may require JS interop calls unless we design the JS object layout carefully,
- performance overhead from crossing WASM ↔ JS boundary on object access (mitigation below).

**Stage B — True WASM GC objects (performance path, fewer boundary crossings)**

- Represent heap objects as WASM GC `struct`/`array` values, manipulated entirely within WASM.
- JS only sees ABI primitives at the boundary; it does not need to understand the heap.

Pros:

- VM hot path stays within WASM (no per-field JS calls),
- engine can optimize GC objects better than cross-boundary `externref` patterns.

Cons:

- depends on toolchain maturity for authoring WASM-GC types from Rust (or requires a dedicated
  lowering step that rewrites/links in a WASM-GC heap implementation).

This proposal is compatible with shipping Stage A first, then upgrading to Stage B without
changing the JavaScript API.

---

## Minimal JavaScript ↔ VM interface

The goal is a **small, explicit, step-driven** interface which mirrors the Rust embedding API but
maps cleanly to JavaScript.

### 1) Value mapping: `AbiValue` in JavaScript

At the JS boundary, represent ABI values as a restricted JS union:

- `unit` → `undefined`
- `bool` → `boolean`
- `int(i64)` → `bigint`
- `float(f64)` → `number`
- `string` → `string`
- `bytes` → `Uint8Array`

Rules:

- Host code must validate inputs and outputs against the signature (`AbiType`) declared in the
  bytecode module.
- The VM must trap on type mismatch to preserve `BYTECODE_SPEC.md` semantics.

This keeps the boundary “native-feeling” in JS without introducing a custom serialization format.

### 2) Module introspection helpers (optional but recommended)

Expose read-only queries so JS can wire up imports/effects by name:

- `list_host_imports() -> Array<{ id: number, name: string, params: AbiType[], ret: AbiType }>`
- `list_external_effects() -> Array<{ id: number, interface: string, method: string, params: AbiType[], ret: AbiType }>`

These can be implemented either:

- in WASM and returned as JS objects (easy via bindings), or
- as a compact binary blob for a thinner ABI (later optimization).

### 3) Host functions (imports)

JS installs host functions by `HostImportId` (or by name → id lookup using the helper above):

- `register_host_import(id: number, fn: (args: AbiValue[]) => AbiValue): void`

VM-side requirements (carried over from the spec):

- Host calls are synchronous and non-yielding.
- No VM re-entry during host calls.

Implementation strategy inside WASM:

- The VM calls a single imported dispatcher:
  - `host_call(id, args) -> ret`
- JS dispatcher looks up the registered function and invokes it.

This avoids having to generate one WASM import per host function.

### 4) Externalized effects (“effects as JS-owned side effects”)

The VM executes until it hits a boundary:

- `Done { value }`
- `Trap { message }`
- `Request { effect_id, args, k }`
- `Yield { remaining_fuel }` (optional, for cooperative scheduling)

#### Effect handling contract

- A `Request` means: “the VM is suspended; JavaScript must decide what to do next.”
- JavaScript must respond with exactly one of:
  - `resume(k, value)` to provide a return value for the suspended effect, or
  - `drop_continuation(k)` to cancel and force a deterministic trap.
- A continuation handle `k` is **one-shot**:
  - resuming twice is an error,
  - resuming after dropping is an error,
  - handles should be validated with `(index, generation)` like the native VM.
- Only one outstanding request is allowed per VM instance (matches the current VM model).

#### Why effects are “externalized”

Externalized effects are the mechanism for integrating with:

- timers (`setTimeout`, `requestAnimationFrame`)
- network (`fetch`)
- DOM and events
- storage APIs

…without requiring async/stack switching inside WASM. The VM remains deterministic and the host
owns scheduling and side effects.

### 5) Step / resume API surface (JS-facing)

Minimal, stable operations:

- `new_vm(rbc_bytes: Uint8Array, opts?: { fuel?: number }): Vm`
- `vm.step(fuel?: number): StepResult`
- `vm.resume(k: { index: number, generation: number }, value: AbiValue): void`
- `vm.drop_continuation(k: { index: number, generation: number }): void`

Recommended additional helpers:

- `vm.is_done(): boolean`
- `vm.is_trapped(): boolean`
- `vm.take_metrics(): VmMetrics` (optional; useful for profiling in browser)

### 6) `StepResult` representation in JavaScript

For clarity and debuggability, start with object shapes like:

- `{"tag":"done","value":AbiValue}`
- `{"tag":"trap","message":string}`
- `{"tag":"request","effectId":number,"args":AbiValue[],"k":{"index":number,"generation":number}}`
- `{"tag":"yield","remainingFuel":number}`

If this becomes a performance bottleneck, we can later switch to:

- numeric tags + out-parameters, or
- a compact “result record” stored in WASM memory and read by JS.

The high-level JS wrapper can keep returning the object shapes even if the low-level ABI becomes
more compact.

---

## Recommended host-side driver loop (JavaScript)

The browser host typically runs a VM until it:

- finishes,
- traps, or
- requests an externalized effect.

Pseudocode:

```js
async function runVm(vm, { fuelPerTick = 50_000, effects }) {
  while (true) {
    const r = vm.step(fuelPerTick);
    switch (r.tag) {
      case "done":
        return r.value;

      case "trap":
        throw new Error(r.message);

      case "yield":
        // Keep UI responsive; optionally schedule next chunk.
        await new Promise(requestAnimationFrame);
        break;

      case "request": {
        const handler = effects.get(r.effectId);
        if (!handler) {
          vm.drop_continuation(r.k);
          throw new Error(`unhandled external effect id ${r.effectId}`);
        }

        try {
          const v = await handler(...r.args);
          vm.resume(r.k, v);
        } catch (e) {
          // Decide policy: cancel (trap), or resume with an error encoding.
          vm.drop_continuation(r.k);
          throw e;
        }
        break;
      }
    }
  }
}
```

Notes:

- This keeps the VM fully cooperative with the browser (no long blocking runs).
- For concurrency, create many VM instances and interleave stepping (“VM swarm” model).

---

## Host functions vs externalized effects: guidance

To keep the boundary portable and simple:

- Use **host functions** (`CallTarget::Host`) for:
  - synchronous, “intrinsic-like” operations,
  - cheap computations,
  - deterministic environment queries (if needed).
- Use **externalized effects** for:
  - any operation that may block, wait, or logically suspend,
  - UI integration that should yield back to the browser,
  - host-owned scheduling and side-effect boundaries.

This follows the “no yield across FFI” rule from the bytecode VM model.

---

## WASM export/import surface (low-level)

This section lists the conceptual low-level boundary between the WASM module and JavaScript.
Exact names depend on binding tech, but the *shape* should remain stable.

### WASM exports

- `vm_new_from_rbc(bytes: Uint8Array) -> VmHandle`
- `vm_step(vm: VmHandle, fuel: u64) -> StepResultHandle`
- `vm_resume(vm: VmHandle, k_index: u32, k_generation: u32, value: AbiValue) -> Result`
- `vm_drop_continuation(vm: VmHandle, k_index: u32, k_generation: u32) -> Result`
- `vm_register_host_import(vm: VmHandle, id: u32, fn: HostFnHandle) -> Result`
- `module_list_host_imports(vm: VmHandle) -> HostImportListHandle` (optional)
- `module_list_external_effects(vm: VmHandle) -> EffectListHandle` (optional)

### WASM imports (provided by JavaScript)

- `host_call(id: u32, args: AbiValue[]) -> AbiValue`

If Stage A heap uses `externref` objects with JS-owned storage, we may also import a small set of
helpers for manipulating those objects (field load/store, array ops). In Stage B (true WASM GC
heap), these helpers disappear and object access stays inside WASM.

---

## Implementation plan (phases)

This plan is intentionally staged to keep the “user-visible interface” stable while de-risking GC
and toolchain work.

### Phase 0 — Decide binding/toolchain + smoke test

- Pick an initial binding approach:
  - wasm-bindgen (fastest path), or
  - component-model tooling (if already viable for the target browsers).
- Add a tiny browser example that:
  - loads the WASM module,
  - loads a `.rbc`,
  - calls `step()` until `done`.

Exit criteria:

- A minimal web page can execute a trivial `.rbc` program end-to-end.

### Phase 1 — Make the VM heap-backend generic

- Refactor `crates/rusk-vm` to not hardcode `rusk-gc`.
- Introduce a heap backend trait and keep the existing native backend (`rusk-gc`) as the default.

Exit criteria:

- All existing tests still pass on native builds.
- No behavioral changes in bytecode semantics (beyond internal refactors).

### Phase 2 — Stage A browser heap backend (`externref` host GC)

- Implement `WasmGcHeapBackend` that stores heap objects as JS-owned structures and references them
  from WASM via `externref`.
- Ensure heap object payloads do not rely on Rust `Drop` (JS owns the payload).
- Implement the necessary VM operations over this backend:
  - allocation,
  - field/element load/store,
  - pattern matching reads,
  - identity/equality rules as required by bytecode semantics.

Exit criteria:

- `rusk-vm-wasm` can run representative bytecode programs in a browser.
- No custom VM GC code is shipped in the browser build.

### Phase 3 — JavaScript interop: host calls + externalized effects

- Implement `host_call` dispatcher and install host functions by module host import list.
- Expose `StepResult::Request` to JS and implement `resume` / `drop_continuation`.
- Provide a small reference JS “driver” that supports async effect handlers.

Exit criteria:

- A demo program can:
  - call host imports,
  - perform an externalized effect handled by JS and resume,
  - complete deterministically.

### Phase 4 — Stage B: true WASM GC heap (performance)

When toolchain support is ready:

- Move heap objects from JS-owned `externref` to WASM GC `struct`/`array` types.
- Keep the JS-facing API unchanged.

Exit criteria:

- VM object access does not require JS calls on the hot path.
- Performance is competitive with the native VM for compute-heavy bytecode (modulo browser limits).

---

## Risks and mitigations

- **Browser support for WASM GC**: may vary by engine/version.
  - Mitigation: feature-detect at runtime; provide a fallback build (native-only) or a degraded
    mode (e.g. run in a Worker, or ship a non-GC build only for environments that support it).
- **Toolchain maturity** (authoring WASM-GC types from Rust).
  - Mitigation: Stage A (`externref`) first; Stage B later.
- **Boundary overhead** (WASM ↔ JS calls).
  - Mitigation: keep host calls coarse-grained; prefer externalized effects for async boundaries;
    design JS APIs to batch work where possible.
- **Determinism hazards** in host code (randomness, time, floating-point differences).
  - Mitigation: document that determinism is host-controlled; provide deterministic host sets for
    test mode.
- **Debuggability** of trapped states in browser.
  - Mitigation: keep trap messages stable and expose a minimal “stack trace” API later if needed.

---

## Open questions

1) What is the target browser baseline for WASM GC (Chrome/Firefox/Safari versions)?
2) Do we want the browser build to support `Yield { remaining_fuel }` from day one to guarantee UI
   responsiveness, or do we require hosts to use Workers instead?
3) Should we standardize a small set of “browser host functions” (console logging, time, random),
   or require all platform integration to go through externalized effects?
4) Do we want a standardized JS packaging story (`@rusk/vm` ESM package) in this repo, or keep it
   as an external companion project?

