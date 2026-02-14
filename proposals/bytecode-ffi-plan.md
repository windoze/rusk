# Bytecode FFI + Host-Driven `step()` API — Implementation Plan

This document is a **detailed execution plan** for implementing the design in
`proposals/bytecode-ffi.md`.

Scope: **plan only** (no code changes besides this plan file).

---

## Progress (implementation tracking)

- [x] Milestone A — Bytecode module skeleton + VM API surface
- [x] Milestone B — MIR → bytecode lowering (minimal subset)
- [x] Milestone C — `CALL_HOST` + non-reentrancy enforcement
- [ ] Milestone D — Externalized effects (`Request`/`resume`/`drop`)
- [ ] Milestone E — Expand bytecode to pass fixture suite
- [ ] Milestone F — Generic specialization
- [ ] Milestone G — Stable serialization + verification

Last updated: 2026-02-14

## 0) Goals, Constraints, and Deliverables

### Primary deliverables

1) A **portable bytecode layer** under MIR:
   - register-based, linear instruction stream
   - compact IDs instead of stringly lookups on hot paths
   - constant pool
   - deterministic semantics aligned with MIR

2) A **host-driven VM boundary**:
   - `vm_step(vm, fuel) -> StepResult` (runs until done/trap/request/yield)
   - `vm_resume(vm, k, value)` resumes a suspended continuation
   - `vm_drop_continuation(vm, k)` cancels a suspended computation

3) Two host interaction mechanisms:
   - `CALL_HOST` for **synchronous, non-yielding** host calls
   - **externalized effects**: unhandled `perform` yields `StepResult::Request { effect_id, args, k }`

### Hard constraints from the proposal

- **No yield across FFI**: host calls cannot participate in effects/continuations.
- **Non-reentrant host calls**: host imports must not call back into the VM.
- **Single outstanding request per VM**: the VM cannot be stepped while suspended; it must be
  resumed or cancelled first.
- **ABI-safe value boundary** (v0): only builtin primitives cross the boundary:
  `unit`, `bool`, `int(i64)`, `float(f64)`, `string(utf-8)`, `bytes`.
- The VM/bytecode spec must be **explicit** and implementable in C/JVM/CLR/JS (avoid Rust-only
  encoding/assumptions).

### Non-goals (for v0 unless explicitly listed as future work)

- Allowing VM heap objects (`struct/enum/array/tuple`) to cross the VM/host boundary.
- Host imports yielding/suspending or resuming continuations.
- Multi-fiber scheduling inside a single VM instance.
- JIT/AOT compilation.

---

## 1) Architecture: New Components and Their Boundaries

### 1.1 Crates / modules to add (proposed)

> Naming is a bikeshed; plan assumes these placeholders.

- `crates/rusk-bytecode/`
  - Defines bytecode module data structures and (eventually) stable serialization.
  - Owns ID types: `BcFunctionId`, `HostImportId` (or re-use existing), `TypeId`, `EffectId`,
    `MethodId`, `ConstId`, etc.
  - Implements MIR → bytecode lowering (or hosts the lowering entrypoint).

- `crates/rusk-vm/`
  - Bytecode VM runtime.
  - Owns `Vm`, `StepResult`, `ContinuationHandle`, `AbiValue`, `VmError`, host import registration,
    external effect request/resume.
  - May reuse existing GC code from `crates/rusk-interpreter/src/gc` (either by factoring GC into a
    shared crate, or temporarily duplicating until refactor is worth it).
  - v0 recommendation: **`std`-only** for the VM runtime to prioritize simplicity (aligns with the
    proposal’s non-goal of preserving `no_std` constraints).

> Alternative: combine into one crate (`rusk-bytecode`) with a `vm` module. If we do this, still
> keep the API split clean: “module + serialization” vs “runtime execution”.

### 1.2 Public API surface (Rust)

We want a minimal surface that translates cleanly to other languages:

```rust
pub enum StepResult {
    Done { value: AbiValue },
    Trap { message: String },
    Request { effect_id: EffectId, args: Vec<AbiValue>, k: ContinuationHandle },
    Yield { remaining_fuel: u64 }, // optional but recommended in v0
}

pub fn vm_step(vm: &mut Vm, fuel: Option<u64>) -> StepResult;
pub fn vm_resume(vm: &mut Vm, k: ContinuationHandle, value: AbiValue) -> Result<(), VmError>;
pub fn vm_drop_continuation(vm: &mut Vm, k: ContinuationHandle) -> Result<(), VmError>;
```

Notes / decisions to lock early:

- **Fuel / Yield**: implement in v0 (recommended). Without it, swarms are possible but fairness is
  host-only and a single VM can hog CPU.
- **Cancellation semantics**: `vm_drop_continuation` should transition the VM into a terminal state
  (recommend: `Trap { message: "cancelled" }` or a structured `VmError::Cancelled` mapped to
  `Trap`). This avoids “continuation dropped but VM keeps running” ambiguity.
- **Terminal results**: `Done` returns an `AbiValue` (not full VM `Value`) so it is always safe for
  embedding targets. If we need richer results later, add handles (future work).

### 1.3 Host import ABI (Rust)

Host imports must not get `&mut Vm` (compile-time enforcement of non-reentrancy).

Proposed host function trait:

```rust
pub trait HostFn: 'static {
    fn call(&mut self, args: &[AbiValue]) -> Result<AbiValue, HostError>;
}
```

Allow `&mut self` so host fns can keep state (counters, caches) without globals; still non-reentrant
because there is no VM handle.

The VM enforces at runtime:

- cannot call `vm_step/vm_resume` while executing a `HostFn` (“in-host-call” critical section),
  returning a trap if violated (defense-in-depth).

---

## 2) Bytecode Module Design (ExecutableModule)

### 2.1 IDs and tables (“IDs everywhere”)

Bytecode should avoid runtime string lookups:

- `FunctionId` already exists in MIR; bytecode keeps `u32` function IDs.
- Extend ID usage to:
  - nominal types (`TypeId`)
  - methods (`MethodId`) and method dispatch metadata
  - effects (`EffectId`) (including instantiated interface type args)
  - fields (prefer indices / `FieldIndex` rather than IDs where layout is known)

**Diagnostic strings** remain in tables for debugging, but hot paths use IDs only.

### 2.2 Constant pool

Constant pool needs stable encoding:

- integers (`i64`)
- floats (`f64`)
- strings (`utf-8`)
- bytes (`[u8]`)
- possibly `TypeRepLit` base forms (if we treat some typereps as constants)

### 2.3 Instruction set and encoding strategy

Target: **register machine** with a linear instruction stream and relative jumps.

Key requirements:

- Fixed-width integer types in the encoding (`u8` opcode, `u32` indices, `i64` immediates, etc.)
- Explicit endianness (little-endian)
- Versioned module header (major/minor) for format evolution

#### Immediate plan: two representations

To keep implementation tractable, build in two layers:

1) **In-memory bytecode IR (Rust structs/enums)** used by the compiler pipeline and tests.
2) **Stable serialized form** as a separate step once semantics are validated.

This allows correctness-first implementation without prematurely committing to binary layout details.

### 2.4 Externalized effect declarations in the module

We need module-level metadata that answers:

- Which effect operations are **externalized**?
- What is the ABI signature for their args/result?

Proposed:

```text
external_effects: Vec<ExternalEffectDecl>

ExternalEffectDecl {
  effect_key: (InterfaceId, MethodId, [TypeRepId...]) // instantiated interface args
  effect_id: EffectId
  arg_abi: Vec<AbiType>
  ret_abi: AbiType
}
```

Notes:

- This makes “unhandled perform” → `Request` purely a bytecode/runtime policy and does not require
  changes to MIR semantics.
- Instantiated interface args may be empty for non-generic interfaces. For v0 we can restrict:
  - either “no generic interface args for externalized effects”, or
  - allow them but require exact matching with runtime `TypeRepId`s.

---

## 3) MIR → Bytecode Lowering Plan (Compiler Side)

### 3.1 Lowering entrypoint(s)

Add a compiler API that is explicit about the backend:

- `compile_file_to_bytecode_with_options(...) -> Result<ExecutableModule, CompileError>`
  - internally: parse/typecheck → MIR → optimize (existing) → lower to bytecode

Keep existing MIR compilation API intact for now so we can:

- compare MIR interpreter vs bytecode VM results (regression safety)
- stage adoption

### 3.2 String interning → IDs

During lowering:

- Build intern tables for:
  - nominal type names (struct/enum/interface)
  - method names
  - effect interface names + effect method names
  - handler IDs (if needed for debug)
- Replace MIR instructions that are still stringly with ID-based forms where possible:
  - `MakeStruct { type_name: String, fields: Vec<(String, Operand)> }`
  - `MakeEnum { enum_name: String, variant: String, ... }`
  - `GetField/SetField` (should already be optimized into `StructGet/StructSet` on hot paths)
  - `VCall { method: String, ... }`
  - `Perform { effect: EffectSpec { interface: String, method: String, ... } }`
  - `PushHandler { handler_id: String, ... }`

### 3.3 CFG → linear instruction stream

The key compiler work is eliminating block argument passing and high-level terminators.

Plan per function:

1) **Register assignment**
   - Map MIR `Local(usize)` directly to bytecode register indices (`u32`).
   - Reserve extra temps for:
     - parallel move resolution on branches
     - switch lowering temporaries

2) **Block layout**
   - Choose a deterministic block ordering (e.g. MIR order) initially.
   - Emit block label table: `BlockId -> pc_offset`.

3) **Lower instructions**
   - Each MIR instruction becomes 0..n bytecode instructions.
   - Avoid embedding `Vec` payloads where possible by using “count + inline sequence” encodings.

4) **Lower terminators**
   - `Br` / `CondBr`:
     - Insert register moves that assign target block params.
     - Use a **parallel-move** algorithm to handle cycles without clobbering.
     - Emit relative jump(s).
   - `Switch`:
     - Start with a generic decision chain:
       - evaluate scrutinee once
       - compare against patterns in order
       - jump to match target
     - Later optimize to jump tables for integer cases.
   - `Return` / `Trap` straightforward.

5) **Pattern lowering**
   - MIR `Pattern` is used by `Switch` and effect handler argument matching.
   - v0 strategy:
     - lower patterns into explicit runtime checks and destructuring operations
     - avoid carrying `Pattern` values into bytecode runtime

### 3.4 Validation during lowering

Lowering should validate:

- **Host import ABI safety** for any host import reachable via `CALL_HOST`:
  - only `AbiType` subset is permitted
  - reject `Any`, `TypeRep`, `Array`, `Tuple`, etc.
- **Externalized effects ABI safety**:
  - effect arg/result types must be ABI-safe
- Optional: add a bytecode verifier that checks internal invariants:
  - valid register indices
  - jump targets in range
  - constant pool indices in range
  - function IDs in range

### 3.5 How we declare “externalized effects” (source of truth)

We need an explicit, opt-in way to decide which effects become host requests when unhandled.

Proposed v0 mechanism (no language syntax changes):

- Extend `rusk_compiler::CompileOptions` with an `external_effects` list, e.g.:
  - `(interface_name, method_name, abi_sig)` for non-generic interfaces
  - later: include instantiated interface type args when needed
- During compilation/lowering:
  - validate the named interface+method exists in the compiled module
  - validate the interface method signature is ABI-safe and matches the declared ABI signature
  - assign an `EffectId` and mark it `externalized` in the bytecode module metadata

Rationale:

- Keeps externalization a *host/environment policy* without changing Rusk language semantics.
- Keeps bytecode modules self-describing for embedding targets (no need for stringly external config
  at runtime).

---

## 4) Bytecode VM Runtime Plan (Execution Side)

### 4.1 VM state machine

Explicit VM states:

- `Running`
- `Suspended { k }`
- `Done`
- `Trapped`

State transitions:

- `Running --step--> Done | Trapped | Suspended | Running(Yield)`
- `Suspended --resume(k,v)--> Running`
- `Suspended --drop(k)--> Trapped` (recommended)
- Any other operation in the wrong state should return `VmError` (and in the embedding ABI, map to
  `StepResult::Trap { ... }`).

### 4.2 `vm_step` execution loop + fuel

`vm_step(vm, fuel)` behavior:

1) If `vm.state != Running`, return a trap:
   - `Suspended`: `"vm is suspended; call resume/drop first"`
   - `Done`: `"vm already completed"` (or idempotent `Done` — decide and document)
   - `Trapped`: `"vm already trapped"` (or idempotent `Trap`)

2) While running:
   - Fetch+decode next bytecode instruction.
   - Execute it atomically (no partial instruction results).
   - Decrement fuel counter if provided.
   - Stop when:
     - return/trap happens
     - an externalized effect is requested
     - fuel hits 0 (`Yield`)

Fuel decisions:

- Cost model: **1 fuel per bytecode instruction** (including `CALL_HOST` / `PERFORM`).
- `Yield { remaining_fuel }` returns `0` (or the remaining after the last executed instruction if
  we stop *before* executing one). Pick one and keep it consistent.
- `vm_resume` does not itself consume fuel; fuel only applies to `vm_step`.

### 4.3 Value model: VM `Value` vs boundary `AbiValue`

We need two value layers:

- **VM-internal values**: full language runtime (heap refs, functions, continuations, typereps, …).
- **ABI values**: only the stable primitive subset.

Proposed `AbiValue`:

```rust
pub enum AbiValue {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
}
```

Conversions:

- `Value -> AbiValue` is only defined for primitive values; otherwise trap:
  - `"FFI ABI violation: value kind <...> is not ABI-safe"`
- `AbiValue -> Value` always succeeds (copies into VM storage).

This is required in three places:

1) `CALL_HOST` arguments/results
2) `Request { args }` for externalized effects
3) `vm_resume(..., value)` injection value

### 4.4 `CALL_HOST` implementation + enforcement

Bytecode `CALL_HOST`:

- Looks up the `HostImportId` in the module’s host import table.
- Validates:
  - arity matches declared signature
  - argument ABI types match declared signature (exact match, no coercions)
- Calls the registered `HostFn` synchronously.
- On `HostFn` error:
  - convert to `Trap { message }` (or include richer error type later).

“No yield across FFI” enforcement:

- Host import cannot trigger `StepResult::Request`.
- Host import cannot call back into the VM.

Implementation plan for enforcing non-reentrancy (portable concept):

- VM has a boolean “in-host-call” flag.
- `vm_step` sets it before calling `HostFn` and clears it after.
- `vm_step`, `vm_resume`, and any public “enter VM” method checks this flag and traps if reentered.

Even if safe Rust makes reentry difficult, other language embeddings (C/JS) can violate this, so
we still enforce at runtime.

### 4.5 Externalized effects → `StepResult::Request`

Runtime effect handling order on `PERFORM effect(args...)`:

1) Evaluate argument expressions → VM values.
2) Search handler stack:
   - On match: behave like MIR today (capture delimited continuation, unwind to handler owner,
     jump to handler clause target with `(binds..., k)`).
3) If **no handler matches**:
   - Compute runtime effect key: `(InterfaceId, MethodId, instantiated interface TypeRepIds...)`.
   - If the key is **externalized**:
     - Validate args are ABI-safe (should be guaranteed by compiler validation; still assert).
     - Capture a one-shot continuation token `k` with delimitation at VM root
       (i.e. capture the whole current call stack).
     - Transition to `Suspended { k }`.
     - Return `StepResult::Request { effect_id, args: Vec<AbiValue>, k }`.
   - Else:
     - Trap `"unhandled effect"` (preserve current MIR behavior).

### 4.6 Continuation handles and one-shot semantics

Continuation handle representation must be C/JVM/JS-friendly:

- Use `(index: u32, generation: u32)` to avoid use-after-free:
  - index into a VM-owned continuation table
  - generation increments when a slot is reused

Captured continuation state must include:

- captured stack frames (register arrays + PC + function id)
- captured handler stack
- the destination register for `perform` (if any) to inject the resume value

`vm_resume(vm, k, value)`:

- requires VM is `Suspended` and the provided `k` matches the outstanding one
- consumes the continuation (one-shot)
- injects `value` as the result of the suspended `perform`
- returns to `Running`

`vm_drop_continuation(vm, k)`:

- same validity requirements as resume
- consumes the continuation
- transitions VM to a terminal cancelled state (recommend: `Trapped` with message `cancelled`)

### 4.7 Generic specialization (“host shadowing”)

Support host-provided specializations:

- A specialization maps:
  - key: `(GenericFunctionId, [TypeRepId...])`
  - value: `HostImportId`

Runtime behavior at generic call sites:

1) Evaluate type args → internal `TypeRepId`s.
2) Look up specialization:
   - if found: `CALL_HOST` the specialized import (passing **only value args**)
   - else: call normal bytecode function

v0 restrictions:

- Specialized host imports must have ABI-safe param/return types (same as normal `CALL_HOST`).
- Matching is exact (no wildcards).

### 4.8 Module loading / linking API

We need an explicit “linking” step between a loaded bytecode module and the embedding host:

- `Vm::new(module)`
- `Vm::register_host_import(id, host_fn)`
- `Vm::register_generic_specialization(fn_id, type_args, host_import_id)`

Validation at load/link time:

- All required host imports must be registered before first `vm_step` (or `vm_step` traps with a
  missing host import list).
- Specializations must reference valid function IDs and host import IDs.

---

## 5) Serialization and Versioning Plan (Portable Bytecode)

We implement serialization in two stages to avoid blocking runtime correctness on format design.

### Stage 1: Internal serialization for tests (fast iteration)

- Use an internal serializer (e.g. `bitcode` behind a feature flag) to allow snapshot tests and
  quick roundtrips.
- Not considered stable ABI.

### Stage 2: Stable on-disk format

Define a spec + implementation for:

- module header:
  - magic bytes
  - version (major/minor)
  - endianness marker (or fixed little-endian)
- tables (strings, constants, types, functions, effects, host imports)
- function bodies:
  - instruction stream
  - register count

Tests must enforce:

- deterministic encoding (same module → same bytes)
- version mismatch produces a clear error
- invalid/truncated input is rejected safely (no panics)

---

## 6) Tooling + CLI Integration (Rust Reference Host)

The Rust reference host should be able to run:

- MIR interpreter backend (existing)
- bytecode VM backend (new)

Proposed CLI shape (minimal):

- `rusk <file.rusk>` defaults to MIR initially
- add `--backend=mir|bytecode`
- add `--emit=mir|bytecode` to write `.mir` / `.rbc` (bytecode blob) files

This is not required for correctness, but it materially improves developer feedback loops.

---

## 6.1 Docs / Specs to Update

The bytecode layer and step boundary need to be specified somewhere besides a proposal.

Planned doc changes:

- Add `BYTECODE_SPEC.md` (new):
  - module format overview (tables, IDs, constants, function bodies)
  - opcode semantics (including fuel accounting)
  - `step()` / `resume()` / `drop()` boundary semantics
  - ABI value set definition (`AbiValue` / `AbiType`)
  - host import contract (“no yield across FFI”, “no reentry”)
  - externalized effects contract (`Request` semantics)
  - versioning rules
- Update `MIR_SPEC.md` (minimal):
  - explicitly state that MIR semantics remain unchanged (unhandled effect traps),
    and that externalization is a *bytecode/runtime* policy.
- Update `RUSK_SPEC.md` (as needed):
  - only if user-visible syntax/semantics changes are introduced (ideally avoid for v0).

---

## 7) Milestones (Order of Implementation)

Each milestone includes explicit tests so we always have a green, check-pointed branch.

### Milestone A — Bytecode module skeleton + VM API surface

Deliverables:

- Create `rusk-bytecode` + `rusk-vm` crates (or equivalent modules).
- Define:
  - `ExecutableModule` (in-memory IR)
  - `AbiValue` / `AbiType`
  - `Vm`, `StepResult`, `ContinuationHandle`
  - host import registration API (no VM handle to host fns)
- Implement `vm_step` scaffolding that can:
  - return `Done` for an empty/no-op program
  - return `Trap` for missing host imports / invalid state transitions

Tests:

- Unit tests in `crates/rusk-vm`:
  - `step_done_for_trivial_program`
  - `step_traps_if_called_when_done` (or idempotent done; whichever we choose)
  - `resume_fails_when_not_suspended`
  - `drop_fails_when_not_suspended`

### Milestone B — MIR → bytecode lowering for a minimal instruction subset

Target subset (enough to run tiny programs):

- constants: unit/bool/int/float/string
- register moves/copies
- arithmetic/logical opcodes already present in MIR (`IntAdd`, `IntLt`, `BoolNot`, etc.)
- `Return`, `Trap`
- `CallId` to MIR function calls (bytecode internal call)
- `CALL_HOST` for ABI-safe host imports only

Tests:

- Unit tests in `crates/rusk-bytecode`:
  - `lower_const_and_return`
  - `lower_simple_branch_with_block_args_parallel_move`
  - `reject_non_abi_safe_host_import_signature`
- Integration test in `tests/bytecode_smoke.rs`:
  - compile a tiny source to bytecode and run to completion.

### Milestone C — Implement `CALL_HOST` and non-reentrancy enforcement

Deliverables:

- host import dispatch by ID
- arity/type checking against `AbiType` signatures
- runtime reentrancy guard (“in-host-call”)

Tests:

- Unit tests in `crates/rusk-vm`:
  - `call_host_int_add_ok`
  - `call_host_type_mismatch_traps`
  - `call_host_arity_mismatch_traps`
  - `call_host_non_reentrant_guard_traps` (use an `unsafe` test-only reentry attempt if needed)

New fixtures (bytecode backend):

- `fixtures/200_bytecode_call_host_add_int_ok.rusk` (`// expect: ok int 3`)
- `fixtures/201_bytecode_call_host_concat_string_ok.rusk` (`// expect: ok string "ab"`)
- `fixtures/202_bytecode_call_host_bool_not_ok.rusk` (`// expect: ok bool true/false`)
- `fixtures/203_bytecode_call_host_float_mul_ok.rusk` (`// expect: ok bool true` via `host_mul(...) == <float>`)
- `fixtures/204_bytecode_call_host_unit_ok.rusk` (`// expect: ok unit`)
- `fixtures/205_bytecode_call_host_bytes_echo_ok.rusk` (`// expect: ok bool true` via `host_echo(b\"hi\") == b\"hi\"`)

Harness setup for these fixtures:

- `tests/bytecode_fixtures.rs` compiles with a `CompileOptions` that registers a dedicated
  `test` host module declaration containing the required ABI-safe host functions.
- The bytecode fixture runner installs Rust implementations for these host imports by ID (not by
  string) to mirror the intended embedding model.

### Milestone D — Externalized effects (`Request`/`resume`/`drop`)

Deliverables:

- module metadata for externalized effects + ABI signatures
- runtime `PERFORM` behavior:
  - handled effects: current MIR semantics
  - unhandled externalized: `Request`
  - unhandled non-externalized: trap
- `ContinuationHandle` capture/resume/drop
- VM state machine enforcement (single outstanding request)

Tests (Rust integration, step-level):

- `tests/bytecode_step_api.rs`:
  - `unhandled_externalized_effect_returns_request`
  - `resume_continues_after_request`
  - `step_while_suspended_traps`
  - `resume_wrong_handle_fails`
  - `resume_twice_fails`
  - `drop_cancels_vm`

New fixtures (bytecode backend, driven by a built-in test host driver):

- `fixtures/210_bytecode_external_effect_add_int_ok.rusk`
  - interface `TestFfi { fn add(a:int,b:int)->int }`
  - program calls `@TestFfi.add(1,2)` and returns result
  - harness externalizes `TestFfi.add` and resumes with `a+b`
- `fixtures/211_bytecode_external_effect_echo_string_ok.rusk`
  - `@TestFfi.echo("hi")` returns `"hi"`
- `fixtures/212_bytecode_external_effect_two_requests_ok.rusk`
  - calls external effect twice; host driver returns deterministic values; program sums them
- `fixtures/213_bytecode_external_effect_echo_bytes_ok.rusk`
  - `@TestFfi.echo_bytes(b"hi")` resumes with `b"hi"`; program returns `@... == b"hi"` (`bool true`)

Harness setup for these fixtures:

- `tests/bytecode_fixtures.rs` compiles with compile options that:
  - register the `test` host module (for the `200_*` CALL_HOST fixtures)
  - mark selected `TestFfi.*` effect operations as externalized (for the `210_*` fixtures)
- The bytecode fixture runner includes a deterministic host driver that:
  - handles `TestFfi.add(a,b)` by resuming with `a+b`
  - handles `TestFfi.echo(s)` by resuming with `s`
  - handles any other external request by trapping (so we don’t accidentally “pass” unexpected requests)

Negative fixtures:

- `fixtures/214_bytecode_unhandled_effect_not_externalized_runtime_error.rusk`
  - uses a non-externalized interface (e.g. `interface NoExternal { fn boom() -> unit }`)
  - ensures bytecode backend still traps when no handler matches and the effect is not externalized

### Milestone E — Expand bytecode coverage to pass the existing fixture suite

Goal:

- `tests/bytecode_fixtures.rs` runs **all existing fixtures** through the bytecode backend and
  matches MIR interpreter behavior (ok + runtime_error) for the supported surface.

Work items (likely iterative):

- Implement bytecode lowering + VM ops for:
  - arrays: `MakeArray`, `IndexGet/Set`, `Len`
  - tuples: `MakeTuple`, `TupleGet/Set`
  - structs/enums:
    - allocation
    - field access by index (prefer using existing `StructGet/Set` in MIR)
    - enum variant checks and field extraction
  - `Switch` lowering (pattern support used in fixtures)
  - `VCall` method dispatch via ID-based tables
  - `PushHandler/PopHandler` + handler matching (for in-VM effects)
  - `Resume` instruction (in-VM, not host-driven) — required for effect handlers in source

Tests:

- `tests/bytecode_fixtures.rs`:
  - discover `fixtures/` (reuse existing harness logic)
  - for each fixture:
    - compile to MIR
    - lower to bytecode
    - run bytecode VM to completion (host driver handles *only* the externalized test interfaces;
      otherwise preserve normal semantics)
    - compare to expected result / expected runtime_error substring

Note:

- This milestone intentionally treats existing fixtures as the semantic oracle. We do *not* change
  fixture expectations unless the language spec itself changes.

### Milestone F — Generic specialization + tests

Deliverables:

- runtime specialization table
- module/linker API to register specializations
- dispatch on generic call sites

Tests:

- Unit test verifies dispatch happens:
  - register a specialization for `T=int` and have the host import increment a counter
  - run a program that calls the generic function with `int`
  - assert counter incremented (and result is still correct)
- Unit test verifies fallback:
  - call the same generic function with `string`
  - assert specialization not invoked

### Milestone G — Stable serialization + verification

Deliverables:

- stable on-disk format encoder/decoder
- verifier for decoded modules
- `.rbc` file support in CLI

Tests:

- roundtrip: encode → decode → encode (bytes must match)
- fuzz-ish tests with truncated inputs (ensure graceful errors, no panics)
- version mismatch test

---

## 8) Complete Test Suite Inventory (What We Will Add)

This section is intentionally redundant: it is a checklist to ensure coverage is not forgotten.

### 8.1 New unit test modules (by crate)

`crates/rusk-bytecode`

- Lowering:
  - intern tables contain no duplicates
  - function lowering preserves arity and local/register mapping
  - branch arg passing uses correct parallel-move semantics
  - switch lowering correctness for:
    - int cases
    - bool cases
    - enum variant cases (when implemented)
- Validation:
  - rejects non-ABI-safe host import signatures
  - rejects externalized effects with non-ABI-safe arg/result types

`crates/rusk-vm`

- `vm_step` state machine and fuel:
  - yield when fuel exhausted
  - deterministic continuation across yields
- Host imports:
  - signature mismatch traps
  - host error → trap mapping
  - non-reentrancy guard
- Externalized effects:
  - request contains correct effect id and ABI args
  - single outstanding request enforced
  - resume injects correct value
  - drop cancels and consumes continuation
- Continuations:
  - generation/index handle prevents use-after-free

### 8.2 New integration tests (workspace `tests/`)

- `tests/bytecode_smoke.rs` (quick sanity)
- `tests/bytecode_step_api.rs` (step-level request/resume/drop/yield)
- `tests/bytecode_fixtures.rs` (fixture corpus; semantic parity with MIR interpreter)
- (optional) `tests/bytecode_vs_mir_quickcheck.rs` (sampled corpus for faster CI loops)

### 8.3 New fixtures (Rusk source programs)

File naming follows existing numeric style; these are *additions* (no changes to current fixtures):

- Host calls (`CALL_HOST`)
  - `fixtures/200_bytecode_call_host_add_int_ok.rusk`
  - `fixtures/201_bytecode_call_host_concat_string_ok.rusk`
  - `fixtures/202_bytecode_call_host_bool_not_ok.rusk`
  - `fixtures/203_bytecode_call_host_float_mul_ok.rusk` (returns `bool` via float comparison)
  - `fixtures/204_bytecode_call_host_unit_ok.rusk`
  - `fixtures/205_bytecode_call_host_bytes_echo_ok.rusk` (returns `bool` via bytes equality)

- Externalized effects (host-driven)
  - `fixtures/210_bytecode_external_effect_add_int_ok.rusk`
  - `fixtures/211_bytecode_external_effect_echo_string_ok.rusk`
  - `fixtures/212_bytecode_external_effect_two_requests_ok.rusk`
  - `fixtures/213_bytecode_external_effect_echo_bytes_ok.rusk`
  - `fixtures/214_bytecode_unhandled_effect_not_externalized_runtime_error.rusk`

Notes:

- The fixture expectation language today is `ok|compile_error|runtime_error`. For step-level behavior
  like “must produce a Request”, we’ll use Rust integration tests instead of inventing a new
  fixture directive.

---

## 9) Risks / Unknowns (Callouts Before Coding)

1) **Pattern lowering complexity**: MIR’s `Pattern` is used in `Switch` and effect handler clauses.
   This may be the largest piece of lowering work.
2) **Core intrinsics portability**: the compiler currently relies on `core::intrinsics::*`.
   We must decide which become:
   - bytecode opcodes,
   - runtime builtins implemented inside the VM,
   - or (less ideal) external host imports (ABI-restricted).
3) **TypeRep identity stability**: specialization and externalized effect matching require stable
   `TypeRepId` construction rules.
4) **Error message stability**: fixtures match substrings; we should keep trap/error text reasonably
   stable while evolving internals.

---

## 10) Definition of Done (for the overall feature)

We consider this project complete when:

- `tests/bytecode_fixtures.rs` passes on the entire `fixtures/` corpus (or an explicitly documented
  subset, if we choose to stage unsupported features).
- `tests/bytecode_step_api.rs` covers request/resume/drop/yield semantics thoroughly.
- The bytecode module has:
  - ID-based tables (no hot-path string lookups)
  - a documented serialization format (even if only v0)
- The VM boundary enforces:
  - non-reentrant host calls
  - non-yielding `CALL_HOST`
  - single outstanding request per VM
