# Cranelift Backend: JIT + AOT Execution for Rusk Bytecode
Date: 2026-03-05  
Status: Draft proposal

## Summary

This proposal adds a **Cranelift-based native backend** for Rusk with two execution modes:

- **JIT** (just-in-time): compile bytecode functions to native code in-process and execute them via
  the existing `rusk_vm::vm_step` host-driven model.
- **AOT** (ahead-of-time): compile bytecode modules to native **object files** (and optionally
  linkable shared libraries / executables) for faster startup and distribution.

Key principles:

- The **bytecode + VM boundary stays the semantic source of truth**: traps, determinism, host
  imports, externalized effects, continuation semantics, and fuel/yield behavior must match the
  reference VM.
- JIT/AOT are **optional engines** behind a feature/flag; the interpreter remains the reference
  implementation and a fallback.
- We start with a **baseline compiler** (correctness-first) and iteratively optimize hot opcodes.

---

## Background & Current State

Today the execution pipeline is:

1. `rusk-compiler` compiles `.rusk` to bytecode (`ExecutableModule`) and can serialize it as `.rbc`.
2. `rusk-vm` executes bytecode with a **host-driven stepping API**:
   - `vm_step(vm, fuel) -> StepResult::{Done, Trap, Request, Yield}`
   - externalized effects suspend the VM with a continuation handle (`StepResult::Request`), then
     the host resumes with `vm_resume(...)`.

This model is intentionally designed to be:

- deterministic,
- embeddable, and
- portable to other runtimes/languages (bytecode + step boundary).

However, the reference interpreter still pays classic VM costs:

- per-instruction dispatch,
- boxed/indirect value representations,
- frequent bounds checks and dynamic checks,
- dynamic dispatch overhead for common primitives.

The repo has already captured that long-term performance likely needs an execution-engine upgrade
(`completed-proposals/012-optimization-round-2.md`).

---

## Motivation

We want two complementary capabilities:

1. **JIT** for developer workflows and dynamic workloads
   - fast “edit → run” cycles with less runtime overhead than interpretation
   - adaptive compilation (compile only what gets executed)

2. **AOT** for distribution and stable scripts
   - predictable startup time (no JIT warmup)
   - easier packaging as a native artifact
   - potential for platform-specific optimizations and linking

Cranelift is a good fit because it is:

- designed to be embedded in Rust programs,
- supports both JIT and object-file emission through a shared API surface,
- fast to compile relative to heavyweight toolchains,
- multi-arch (x86_64, aarch64, …) with a single code generator stack.

---

## Goals

### Execution goals

- Provide a new engine that can execute existing `.rbc` modules:
  - `Interpreter` (existing)
  - `CraneliftJit` (new)
  - `CraneliftAot` (new, offline compile artifact)
- Preserve the semantics of:
  - traps (including message quality),
  - `fuel`-based yielding,
  - externalized effects and continuation handles,
  - host-import behavior (non-reentrancy, ABI checks),
  - determinism (modulo host nondeterminism).

### Engineering goals

- Keep implementation incremental:
  - land the scaffolding first (feature-gated),
  - grow opcode coverage over time,
  - keep an interpreter fallback to avoid blocking progress.
- Introduce a testing strategy that makes semantic regressions obvious:
  - differential execution (JIT vs interpreter) for the same bytecode module,
  - fuzzing hooks later (out of scope here, but planned).

---

## Non-goals (for this proposal)

- A fully optimizing compiler on day 1 (we start baseline / correctness-first).
- Cross-version “stable AOT ABI” guarantees:
  - **AOT artifacts are toolchain-locked** initially (must match the runtime build/version).
- Compiling *source* directly to native code without bytecode:
  - we keep bytecode as the canonical portable format in v1 of this backend.
- GC parallelism or moving-collector redesign.
- Native sandboxing (JIT/AOT executes machine code; sandboxing is a separate project).

---

## Proposed Design

### 1) New crate: `crates/rusk-cranelift/`

Add a new workspace member crate that depends on:

- `rusk-bytecode`
- `rusk-vm` (runtime semantics + helpers)
- Cranelift crates:
  - `cranelift-codegen`
  - `cranelift-frontend`
  - `cranelift-module`
  - `cranelift-jit` (JIT mode)
  - `cranelift-object` (AOT mode)

Primary responsibilities:

- translate bytecode `Function` bodies into Cranelift IR (CLIF),
- compile to machine code (JIT) or object code (AOT),
- provide a dispatcher that plugs into `rusk-vm`’s step model.

### 2) Engine abstraction (VM-facing)

Introduce an internal engine abstraction so the VM can be driven by either interpreter or native
code with the same external contract:

```rust
pub enum EngineKind {
    Interpreter,
    CraneliftJit,
    CraneliftAot, // when a precompiled artifact is loaded
}

pub struct VmConfig {
    pub engine: EngineKind,
    // Future: tiering policy, compilation cache size, debug flags, …
}
```

This is intentionally **not** a stable public API yet; it is a way to keep `rusk` CLI and tests
simple while the backend evolves.

### 3) Keep the host-driven `vm_step` boundary

The cranelift engine must integrate at the same layer as the interpreter:

- The host still calls `vm_step(&mut vm, fuel)` repeatedly.
- External effects still surface as `StepResult::Request { effect_id, args, k }`.
- Resumption still uses `vm_resume(vm, k, value)`.

This preserves:

- embedding ergonomics,
- existing host/effect designs, and
- a single semantic boundary for all runtimes (including future non-Rust VMs).

### 4) “Stackless” native execution to support continuations

Rusk continuations (one-shot delimited continuations) fundamentally require a capture/resume
model that is *not* compatible with a naive “compile to normal native call stack” approach.

Therefore, the native backend must keep the **Rusk call stack explicit** (as it is today in
`rusk-vm`), rather than mapping it to the machine call stack.

Concretely:

- The VM’s call stack remains `vm.frames: Vec<Frame>`.
- Compiled code executes the current top frame and **returns to a dispatcher** whenever it needs
  to:
  - enter another frame (call),
  - return from a frame,
  - trap,
  - suspend due to an externalized effect,
  - yield due to fuel exhaustion.

This ensures:

- continuation capture remains a “copy/own frame segment” operation,
- there is no requirement to copy/unwind native stack frames,
- GC roots remain in VM-managed structures (see GC section).

### 5) A JIT-friendly internal value representation (recommended prerequisite)

For JIT/AOT to be correct and maintainable, VM values must be **plain data** that native code can:

- load/store with well-defined layout,
- copy/move without invoking Rust destructors,
- compare tags and extract payloads cheaply.

The current interpreter uses Rust enums and `Option<Value>` in registers, and uses `Rc` for
continuations. This is ergonomically great for a reference interpreter, but it makes native code
generation much harder because “copy” and “drop” are not bitwise operations.

This proposal recommends an internal VM refactor:

- Replace `Value` with a `#[repr(C)]` POD representation (`VmValue`) that is `Copy`.
- Replace `Option<Value>` registers with a `VmValue::Uninit` tag (removes `Option` overhead and
  makes register file trivially addressable).
- Represent continuations as **VM-managed handles** (index + generation) rather than `Rc` values,
  matching the existing pinned continuation handle pattern used at the ABI boundary.

This is the single biggest enabling move for JIT/AOT, and it is also likely to benefit the
interpreter.

If we do *not* do this refactor, then either:

- the native backend would be forced to call back into Rust helpers for almost every operation,
  or
- it would need to embed Rust layout assumptions and destructor behavior into generated code,
  which is brittle and error-prone.

---

## JIT Mode Design

### Compilation granularity

Compile at the **bytecode-function** granularity:

- Each `FunctionId` gets compiled to one native function `jit_fn_{id}`.
- Compilation is **lazy by default**:
  - compile entry function eagerly,
  - compile others on first call (or optionally precompile all with a flag).

### JIT entrypoint shape

Each compiled function runs the currently active frame until it reaches a boundary (call/return/
trap/request/yield), then returns to the dispatcher.

We avoid exposing Rust enums across the JIT boundary by returning a small `#[repr(C)]` exit record:

```rust
#[repr(C)]
pub struct JitExit {
    pub reason: u32, // e.g. Call, Return, Trap, Request, Yield, Continue
    pub aux0: u64,
    pub aux1: u64,
    pub remaining_fuel: u64,
}
```

The dispatcher (in Rust) interprets this and produces a `StepResult` (or loops again if the exit
was an internal call/return transition).

### Runtime helpers (imports into generated code)

Generated code will need to call into the runtime for operations that are either:

- complex / correctness sensitive, or
- require heap allocation / GC / host interaction.

These are implemented as `extern "C"` helpers in `rusk-vm` (or a `rusk-runtime` submodule) so
Cranelift can call them with a stable calling convention.

Examples (sketch):

- `rusk_rt_trap(vm, msg_ptr, msg_len) -> JitExit`
- `rusk_rt_call_host(vm, host_import_id) -> JitExit`
- `rusk_rt_perform(vm, effect_id, ...) -> JitExit`
- `rusk_rt_alloc_*` helpers (strings, arrays, structs, …)
- `rusk_rt_gc_poll(vm)` (optional explicit safepoint)

The initial backend can be conservative and route more operations through helpers. Over time we
inline the hot-path operations directly in CLIF.

### Tiering and caching policy (initial)

- One compilation tier: “baseline”.
- Cache compiled code for the lifetime of the VM/module instance.
- No deoptimization in v1.

Future: add a 2-tier model (baseline → optimized) based on counters/metrics.

---

## AOT Mode Design

### Output formats

Minimum viable AOT output:

- a native **object file** (`.o`) plus a small metadata manifest describing:
  - target triple / CPU features used,
  - a hash of the input `.rbc`,
  - a runtime ABI/hash stamp (see below),
  - exported symbol names for each `FunctionId`.

Optional later outputs:

- shared library (`.so` / `.dylib` / `.dll`) that can be `dlopen`’d and bound at runtime,
- fully linked executable that embeds the runtime + module bytes.

### Toolchain-locking and ABI stamp

In v1, AOT artifacts are intentionally not “portable across tool versions”.

We enforce this with a stamp check:

- `RUSK_RUNTIME_ABI_HASH` is computed at build time (e.g., from a version string plus critical
  layout hashes / feature flags).
- The AOT manifest embeds that hash.
- The loader refuses to run an AOT artifact if the hash does not match.

This keeps us free to refactor the VM/runtime without needing compatibility promises during early
development.

### Link strategy

AOT-compiled code will call into the same runtime helpers as JIT.

Two reasonable linking models:

1) **Static runtime link** (recommended for v1)
   - AOT output is an object file that expects the runtime helpers to be linked in by the final
     link step (the `rusk` tool can drive this, or external build systems can).

2) **Dynamic runtime link**
   - AOT output is a shared library that imports runtime helpers from a host runtime library.

We start with (1) because it is simplest and avoids cross-platform dynamic loader details early.

---

## Semantics & Correctness Considerations

### Fuel / `Yield`

`vm_step(vm, Some(n))` must execute at most `n` bytecode instructions.

Native execution must decrement fuel in the same places the interpreter does (per executed
instruction, with the same edge cases like implicit returns).

Design choice: compiled code returns `remaining_fuel` so the dispatcher can produce:

- `StepResult::Yield { remaining_fuel: 0 }` when fuel is exhausted, and
- otherwise continue stepping until it hits a semantic boundary.

### Traps

Trap messages should remain as close as possible to the interpreter’s messages for now to keep the
test suite stable and debugging consistent.

This encourages routing trap generation through shared runtime helpers initially.

### Host imports

Host import semantics are correctness-critical and involve:

- ABI arity/type checking,
- non-reentrancy rules,
- conversions between VM values and `AbiValue`.

In v1, host calls should remain implemented in `rusk-vm` and be invoked via a runtime helper from
native code.

Longer-term, once a JIT-friendly `VmValue` exists, we can:

- perform cheaper inlined ABI checks, and
- lower more host-call marshaling code into generated code (optional).

### Externalized effects and continuations

`perform`/`resume` is the “hard part” semantically.

Strategy:

- Keep effect-handler lookup, continuation capture, and resumption logic in `rusk-vm` runtime code
  initially.
- Native code calls `rusk_rt_perform(...)` / `rusk_rt_resume(...)` helpers and returns a `JitExit`
  that maps to `StepResult::Request` / “continue”.

Once this is stable, we can incrementally inline common fast paths:

- handler-cache lookups,
- effect hash computations,
- common clause matching patterns.

### GC integration

Rusk uses a tracing GC with stable handles, and the VM’s root set is derived from:

- VM frames (registers),
- pinned continuation storage.

To keep GC precise without stack maps in v1:

- Treat runtime helper calls as GC safepoints.
- Ensure compiled code maintains the invariant:
  - any live `VmValue` that contains a GC handle is stored in a VM register slot (or explicitly
    passed to the helper) before reaching a safepoint.

With the “stackless execution” design, this is tractable: Rusk values primarily live in the VM’s
explicit register file, not in native temporaries.

---

## CLI / User Experience (proposed)

Add flags/subcommands to the `rusk` binary (exact shape TBD):

- `rusk run --engine=vm file.rbc` (explicit interpreter)
- `rusk run --engine=jit file.rbc`
- `rusk build --aot file.rusk -o out.o` (compile source, then AOT-compile the resulting module)
- `rusk aot file.rbc -o out.o` (AOT-compile an existing bytecode module)

The default engine can remain the interpreter until:

- opcode coverage is complete, and
- the differential test suite provides confidence.

---

## Testing Strategy

### 1) Differential testing (engine equivalence)

For a given `.rbc` module and input argv:

- run via interpreter engine,
- run via JIT engine,
- assert `StepResult` traces match (or at least final outcome matches).

Because external effects involve host interaction, tests can:

- use deterministic host imports, and
- avoid externalized effects in engine-equivalence tests initially.

### 2) Opcode-level golden tests

Add focused integration tests that compile small programs to bytecode and assert:

- arithmetic ops trap exactly when expected,
- control-flow works (jump/jumpif/switch),
- register uninitialized reads trap,
- host import ABI mismatch traps.

### 3) Benchmarks (non-blocking for correctness)

Use existing `benchmarks/compare.py` to track improvements, but do not gate correctness on
benchmark results.

---

## Rollout Plan (phased)

### Phase 0 — Scaffolding

- Add `crates/rusk-cranelift`.
- Add a minimal dispatcher that can execute a trivial “return unit” module via JIT.
- Add CLI flag plumbing (engine selection) behind a feature.

### Phase 1 — Minimal opcode coverage + interpreter fallback

- Implement codegen for basic control flow and a subset of ops.
- Any unsupported opcode exits to the interpreter for that frame (slow path).
  - This avoids “all-or-nothing” pressure and enables incremental landing.

### Phase 2 — Value/continuation POD refactor (if not done earlier)

- Introduce `VmValue` as a `Copy` POD.
- Remove `Rc`-based internal continuations in favor of VM-managed handles.
- Update interpreter to use the same representation.

This unlocks:

- reliable codegen for `Copy`/`Move` semantics,
- efficient register file access in generated code,
- easier GC correctness.

### Phase 3 — Full opcode coverage (JIT parity)

- Implement lowering for all bytecode instructions.
- Remove (or heavily reduce) interpreter fallback usage.

### Phase 4 — AOT object emission

- Add `cranelift-object` backend to emit `.o` + manifest.
- Add ABI stamp checking.
- (Optional) Provide a simple linker driver in the `rusk` CLI.

### Phase 5 — Optimization passes (optional)

- Inline hot arithmetic/branch ops.
- Add basic block layout improvements.
- Add inline caches for dynamic dispatch (`VCall`/`SCall`) if beneficial.

---

## Alternatives Considered

### 1) LLVM backend

Pros:
- best-in-class optimization.

Cons:
- heavy dependency footprint and integration complexity,
- slower compile times for JIT workflows,
- more work to embed cleanly.

### 2) “Compile MIR to C” as an AOT strategy

Pros:
- conceptually simple.

Cons:
- C becomes the “real backend”, but we still need a C toolchain and deal with undefined behavior,
- hard to preserve exact trap semantics and dynamic checks,
- doesn’t help JIT or embedding use cases.

### 3) Handwritten machine-code JIT

Pros:
- potentially fastest for a narrow target.

Cons:
- high maintenance,
- hard to support multi-arch and relocations,
- reinventing Cranelift’s core value.

---

## Open Questions

1) Should the native backend target **bytecode** long-term, or should we also add a MIR→CLIF
   backend once the pipeline stabilizes?
2) What is the right stable internal representation for VM values (`VmValue`) given:
   - string/bytes “view” semantics,
   - continuation handles,
   - readonly reference views,
   - potential future features like unboxed option/value specialization?
3) How do we want to expose debug info and stack traces for JIT/AOT code?
4) What is the minimal useful AOT artifact format for v1:
   - object only, or object + automatic link step?
5) Should `fuel` remain an instruction-count mechanism, or do we eventually want a more flexible
   “budget” system that can account for host calls and allocations?

