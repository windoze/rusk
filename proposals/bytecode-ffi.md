# Bytecode FFI + Host-Driven `step()` API (VM/Host Boundary)

## Summary

This proposal introduces a **portable bytecode execution layer** under MIR together with a
**host-driven `step()` API**. The goal is to make Rusk code:

- embeddable as a small VM in generated outputs (C/JVM/CLR/JavaScript targets), and
- interoperable with native/platform functionality via a **simple FFI model**.

Key decisions in this proposal:

- **`CALL_HOST` is synchronous and non-yielding**. It behaves like an IR intrinsic call into a
  native function and returns a direct result.
- **No “yield across FFI”**: host calls do not participate in effects/continuation protocols.
- External/native functionality that may block or suspend is modeled as **externalized effects**
  that cause the VM to return a **meaningful step result** to the host:
  `StepResult::Request { effect, args, k }`.
- **Single outstanding request per VM**: a VM can be blocked on at most one external effect at a
  time. Concurrency is achieved by a host-managed **VM swarm** (many VM instances), not by
  multi-fiber scheduling inside one VM.
- The VM/FFI boundary uses a **small stable value set** (initially: builtin primitive values
  only). Complex values remain VM-internal unless/until we add ABI-stable handles.

This design cleanly separates concerns:

- The VM owns language semantics (locals, heap objects, handlers, continuations).
- The host owns platform integration (poll/epoll/io_uring, threads, timers, networking, channels,
  OS APIs, etc.).

### Status / alignment notes (post “Optimization Round 2”)

This proposal was written before “Optimization Round 2” landed. A few points are now already
partially true in today’s MIR/interpreter:

- **Function/host calls are already ID-based** on hot paths:
  - MIR has `FunctionId`, `HostImportId`, and `CallTarget::{Mir,Host}` plus `Instruction::CallId`.
  - The compiler generally resolves textual calls into `CallId` during compilation.
- MIR still carries **strings** for:
  - nominal type names (`struct` / `enum` / `interface`),
  - field names (for `GetField`/`SetField`),
  - method names (for `VCall`), and
  - effect identity (`EffectSpec { interface, method, ... }`).
- MIR has grown more “VM-shaped” with dedicated opcodes for hot primitives
  (`IntAdd`, `IntLt`, `BoolNot`, etc.), which a future bytecode layer should reuse rather than
  re-route through `CALL_HOST` for basic arithmetic.
- The **“no yield across FFI”** rule in this proposal is intentionally *stricter* than today’s
  in-process Rust embedding API: the current interpreter allows host code to resume continuations
  (re-enter) for embedding/tests. A portable bytecode/FFI boundary should likely forbid this to
  keep the VM/host boundary simple and target-agnostic.

---

## Motivation

Rusk MIR is a high-level CFG IR designed for interpretation and lowering. It is not a good
cross-target portability layer because it contains:

- stringly-typed names for types/fields/methods/effects (even though functions/host imports can be
  ID-resolved via `CallId`),
- composite instruction payloads (`Vec`s inside instructions),
- patterns and other high-level constructs that are awkward to encode as a compact “instruction
  stream”.

Separately, to translate to C/JVM/CLR/JS we need an execution format and runtime boundary where:

- the VM can be embedded in the output and driven by the host,
- the host can use native APIs (e.g. `poll`/`epoll`/`io_uring`) without adopting Rusk’s
  continuation protocol,
- the VM can pause at meaningful boundaries and resume deterministically.

This proposal defines that boundary.

---

## Goals

### Primary goals

- **Portable execution format**: a bytecode layer under MIR suitable for embedding and
  translation.
- **Host-driven execution**: a `step()` API that runs until a boundary and returns a meaningful
  reason for stopping (done/trap/request).
- **Simple interop**: `CALL_HOST` as a direct synchronous native call.
- **No yield across FFI**: foreign code never needs to “know” about effects/continuations.
- **Small stable ABI surface**: restrict values crossing the boundary to a small set initially.
- **Deterministic resumption**: `resume(k, value)` continues exactly where the VM stopped.
- **Portability-first VM boundary**: the bytecode format + `step()` API must be fully specified
  and implementable in non-Rust runtimes (C/Java/C#/JavaScript), without relying on Rust-specific
  data structures or undefined behavior.

### Secondary goals

- Make it straightforward to build “VM swarms” (many VMs) with host-managed scheduling.
- Support “event-loop style” integration where the host uses `poll/epoll/io_uring` and resumes VMs
  when events occur.

---

## Non-goals

- Defining a language-level `async/await` feature (this API can support it, but doesn’t require it).
- Multi-fiber scheduling inside a single VM instance.
- Allowing arbitrary VM heap objects (`struct/enum/array`) to cross the FFI boundary.
- Allowing effects/continuations to cross host call frames (“yield across FFI”).
- JIT/AOT native compilation (bytecode + small VM is the near-term target).
- Preserving `no_std` constraints for the existing MIR crate and the current Rust MIR interpreter.
  Once the bytecode VM becomes the primary approach for WASM/embedding, these crates can prioritize
  simplicity and developer ergonomics over `no_std` compatibility.

---

## Glossary

- **MIR**: existing mid-level IR (`MIR_SPEC.md`).
- **Bytecode module**: a compact, ID-based “ExecutableModule” representation derived from MIR.
- **Host import**: a native function callable via `CALL_HOST`.
- **Externalized effect**: an effect operation that, if unhandled in the VM, becomes a host request
  instead of trapping.
- **Continuation token (`k`)**: a one-shot suspended computation captured at `perform` and resumed
  by `resume`.
- **VM swarm**: multiple VM instances managed by the host to achieve concurrency without shared
  memory.

---

## Design Overview

### 1) Bytecode under MIR

Introduce a bytecode layer (“ExecutableModule”, name TBD) as the lowering target of MIR.
Characteristics:

- **IDs everywhere**: no string lookups on the hot path.
  - MIR already provides `FunctionId` / `HostImportId` and `CallTarget` for direct call dispatch.
  - A bytecode layer should extend this with IDs for the remaining stringly domains:
    `TypeRepId`/type handles, `FieldId` (or struct-layout indices), `MethodId`, `EffectId`, etc.
- **Constant pool** for embedded literals (`int`, `float`, `string`, `bytes`, etc).
- **Linear instruction stream** with relative jumps.
- **Register machine**: operations reference local registers (slots) directly.

This is a representational change; operational semantics remain aligned with MIR.

### Portability constraints (bytecode + VM)

Because this VM may be re-implemented in other languages (C/Java/C#/JavaScript), the bytecode
format and `step()` boundary must avoid “Rust-isms” and be explicit about details that are usually
implicit in an in-process Rust interpreter:

- **Fixed-width numeric types** at the boundary (`i64` for `int`, `f64` for `float`, etc.); avoid
  `usize` in serialized formats.
- **Explicit encoding rules** for module serialization:
  - define endianness (recommend: little-endian) and integer widths,
  - define string encoding (UTF-8) and length prefixing,
  - define instruction/opcode encoding and versioning.
- **Stable IDs and handles** (`u32` indices are a good default) instead of pointers or host
  object references.
- **No host-language reentrancy requirements**: hosts should not need to support “VM calls back
  into host which calls back into VM” patterns to correctly embed Rusk.

### 2) Two host interaction mechanisms

#### A) `CALL_HOST`: synchronous “intrinsic-like” interop

`CALL_HOST` calls a native function and immediately returns a result. It is used for:

- pure/cheap computations (math, hashing, cheap syscalls that do not block),
- “environment queries” (time, random, configuration).

Constraints:

- **Non-yielding**: host code must not attempt to suspend the VM via effects.
- **Non-reentrant**: host functions must not call back into the VM (at least initially).

#### B) Externalized effects: host-driven suspension points

For any operation that may block or conceptually “wait” (FD readiness, timers, etc.), bytecode
executes `PERFORM effect(args...)`. If no VM handler matches and the effect is marked
externalized, the VM returns `StepResult::Request` to the host with:

- the `effect_id`,
- the evaluated arguments (restricted to ABI-safe primitives),
- a captured one-shot continuation token `k`.

The host later resumes execution with `resume(k, value)` and calls `step()` again.

This gives the host complete control over platform integration (e.g. `poll/epoll/io_uring`).

---

## Step API

### API surface (conceptual)

The bytecode executor exposes (at minimum):

- `vm_step(vm, fuel) -> StepResult`
- `vm_resume(vm, k, value) -> Result<(), VmError>`
- `vm_drop_continuation(vm, k) -> Result<(), VmError>` (optional but recommended)

Where `fuel` is an optional instruction budget to guarantee responsiveness and fairness when the
host schedules multiple VMs. `fuel` may be:

- `None` / “unlimited” (run until done/trap/request), or
- a concrete instruction count.

### `StepResult`

`vm_step` runs the VM until one of these boundaries occurs:

- `Done { value }`:
  - the program (or entry function) returned successfully.
- `Trap { message }`:
  - a runtime trap occurred (including “unhandled effect” for non-externalized effects).
- `Request { effect_id, args: [AbiValue], k: ContinuationHandle }`:
  - the VM performed an externalized effect and suspended.
- `Yield { remaining_fuel }` (optional):
  - execution stopped because `fuel` was exhausted.

### Single outstanding request per VM

Each VM instance can be in one of these states:

- **Running**: can be stepped.
- **Suspended**: has yielded a single `Request` with continuation `k`.
- **Done/Trapped**: terminal.

When a VM returns `Request`, it is now **Suspended**. The host must call exactly one of:

- `vm_resume(vm, k, value)` to continue, or
- `vm_drop_continuation(vm, k)` to abandon (cancels the suspended computation),

before calling `vm_step(vm, ...)` again.

This constraint is deliberate: it keeps both VM implementation and host scheduling trivial.

---

## Semantics: Externalized `perform`

MIR defines `perform` as:

1) evaluate operands,
2) search handler stack,
3) on first match, capture a one-shot continuation `k`, unwind to the handler’s owning frame, and
   jump to handler block with `(bindings..., k)`,
4) trap if no handler matches.

This proposal extends the “no handler matches” case:

- If `(interface, method, instantiated interface args)` identifies an effect that is declared
  **externalized** in the bytecode module:
  - capture a one-shot continuation token `k` representing “the rest of computation after the
    perform”. In current MIR semantics, continuation capture is delimited by the selected
    handler’s owning frame; for an *unhandled externalized* effect (no selected handler),
    delimitation defaults to the VM root boundary (i.e. the whole current VM call stack),
  - transition VM into **Suspended** state,
  - return `StepResult::Request { effect_id, args, k }` to the host.
- Otherwise:
  - trap `"unhandled effect"`.

The host can later call:

- `vm_resume(vm, k, value)`:
  - consumes the one-shot `k`,
  - resumes the captured computation, injecting `value` as the result of the suspended `perform`.

Dropping `k` without resuming is explicitly supported (mirrors MIR’s “abandon continuation”).

---

## `CALL_HOST` semantics and the “no yield across FFI” rule

`CALL_HOST` is intended to be the easiest possible interop when no effects are involved (e.g. math).

### Contract

While executing a host import:

- The VM is in an **in-host-call** critical section.
- The host function **must not**:
  - call back into the VM (`vm_step`, `vm_resume`, or any “evaluate bytecode” entrypoint),
  - attempt to trigger effects/continuations inside the VM (directly or indirectly).
- The host function must return synchronously with a value (or error/trap mapping, see below).

Note: this is stricter than the current in-process Rust interpreter embedding API, which may allow
host functions to resume continuations for testing/embedding. For the bytecode/FFI boundary, we
prefer a hard “no re-entry” rule to keep the boundary portable across C/JVM/CLR/JS.

### Enforcement

This proposal recommends enforcing the contract at runtime:

- If the host function attempts to re-enter the VM, the VM traps or returns a VM error.

This keeps the semantics predictable and avoids “half-captured” stacks across foreign frames.

---

## External specialization of generic functions (host “shadowing”)

Rusk uses **reified type arguments** (via runtime `TypeRep`) for generics. This keeps semantics
simple, but it means generic calls often carry extra runtime overhead:

- constructing/passing type arguments,
- dynamic dispatch that cannot assume a concrete type,
- inability to exploit target-native operations (SIMD, fused multiply-add, etc.).

To address this, we allow the host to provide **externally specialized implementations** for
specific instantiations of a generic Rusk function, and have the VM dispatch to them when the
type arguments match.

Example intent:

- Rusk defines `fn f<T>(v: T) -> T { ... }`
- Host provides a specialized implementation for `T = int`: `fn f<int>(v: int) -> int`
- A call to `f` where `T` is `int` uses the host implementation.

This is primarily an optimization mechanism (semantics must remain equivalent).

### Contract and constraints

Specialized host implementations are still host imports, so they inherit the `CALL_HOST` rules:

- non-yielding (no effects/continuations across FFI),
- non-reentrant (must not call back into the VM),
- synchronous result.

Additionally, for v0 this proposal restricts specialization to **ABI-safe monomorphic shapes**:

- Specialized functions must take and return only the ABI-safe primitive set (`unit/bool/int/float/bytes/string`).
- Specializations match **exact** type arguments (no wildcards/partial matches in v0).

### Dispatch model (bytecode-level)

At the bytecode level, generic calls include both:

- type arguments (VM-internal `TypeRep` values / IDs), and
- value arguments (registers).

On a generic call, the VM performs:

1) Evaluate/obtain the type argument `TypeRep` IDs (VM-internal).
2) Look up an optional specialization:
   - key: `(GenericFunctionId, [TypeRepId...])`
   - value: `HostImportId` for the specialized implementation.
3) If a match exists:
   - execute `CALL_HOST` to that specialized import, passing **only the value arguments**
     (no `TypeRep` args, since the specialization is monomorphic).
4) Otherwise:
   - call the generic bytecode function normally (fallback).

This dispatch can be implemented efficiently as:

- a per-function small vector of specializations (linear scan; good when there are few), or
- a hash map keyed by `(fn_id, type_rep_tuple)` (better if many specializations exist).

### Link-time registration

Specializations are provided by the embedding host (or by generated target code) as part of
linking/loading the module:

- `register_host_import(name/id, fn_ptr_or_handle)`
- `register_generic_specialization(generic_fn_id, concrete_type_args, host_import_id)`

In C targets, the specialized host functions can be normal `extern` symbols that are linked in,
then installed into the specialization table during VM initialization.

### Relationship to existing work

This mechanism overlaps with the ideas in `proposals/internal-specialization.md`:

- compiler-side name mangling (compile-time specialization), and
- runtime dispatch tables.

For the bytecode layer, the preferred approach is **ID-based specialization tables** (no string
name mangling required). Compiler-side shortcuts (emitting a direct specialized call when type
args are statically known) can be added later as an optimization, without changing semantics.

---

## ABI Value Set (FFI boundary types)

To keep the boundary stable and portable across C/JVM/CLR/JS, values exchanged via:

- `CALL_HOST` arguments/results, and
- external effect `Request` arguments / `resume` values,

are restricted to a small, versioned set of **ABI-safe** values.

### Initial set (v0)

Start with builtin primitive values only:

- `unit`
- `bool`
- `int` (signed 64-bit)
- `float` (IEEE-754 binary64)
- `bytes` (opaque byte vector)
- `string` (UTF-8)

Explicitly not allowed across the boundary in v0:

- VM heap references (`struct/enum/array/tuple` references)
- function references
- continuation tokens (except as the dedicated `k` in `Request`)
- `typerep` (can be added later if needed)

### Ownership and lifetime (recommended)

To avoid pinning/GC complexity, adopt a simple rule:

- Values returned to the host in `StepResult::Request` are valid until the next VM entry
  (`vm_step`/`vm_resume`) and must be copied by the host if it needs to persist them.
- Values passed from host to VM are copied into VM-owned storage (or into a VM-managed arena).

Translators can tighten this later (zero-copy buffers) once a stable handle story exists.

### Validation

The compiler/lowerer should reject (or the loader should trap on) any bytecode where:

- a `CALL_HOST` signature includes non-ABI-safe types, or
- an externalized effect uses non-ABI-safe argument/result types.

---

## Host-driven I/O integration (poll/epoll/io_uring)

This design supports native event loops without VM participation.

### Pattern

1) VM performs an externalized effect that represents a wait/operation:
   - e.g. `Io.read(fd, n) -> bytes`
2) VM returns `Request { effect_id, args=(fd, n), k }`.
3) Host registers interest and later completes the operation using native APIs:
   - `poll`/`epoll` readiness + `read()`,
   - or `io_uring` submit/poll completion.
4) Host resumes the VM with `resume(k, bytes)` and continues stepping.

### Important property

No native function ever needs to “yield” or know about continuation tokens. The only entity that
captures and resumes continuations is the VM boundary API.

---

## VM swarm scheduling model

Instead of a single VM with internal fibers, the host manages a swarm:

- Each VM is single-threaded and isolated.
- Each VM can have at most one outstanding external request.
- The host multiplexes many VMs onto the platform event loop:
  - step VMs until they suspend or finish,
  - poll the OS,
  - resume ready VMs.

This model avoids shared-memory concurrency issues (e.g. data races) inside the VM runtime.

---

## Channels and inter-VM communication

Channels are considered an **environmental facility**, not a core language feature in this stage.

Implications:

- The host may provide channels via:
  - host imports (`CALL_HOST`), for non-blocking operations, or
  - externalized effects (`perform Env.recv(chan)`), for blocking/waiting operations.
- The VM does not need built-in concurrency primitives to support message passing between VMs.

This keeps the language core small while allowing rich embedding/runtime environments.

---

## Translator implications (C/JVM/CLR/JS)

The bytecode + step ABI is designed to be implementable as:

- an embedded interpreter runtime in each target (small “VM shim”), and/or
- a translation target where bytecode is compiled into target source/bytecode that implements the
  same `step()` boundary.

### C target (example shape)

- Generated output includes:
  - the bytecode blob (`const uint8_t program[] = ...`),
  - the VM runtime (`rusk_vm.c/.h`),
  - exported `vm_step/vm_resume` functions.
- Host imports become normal C symbols (easy to link):
  - `extern int64_t host_math_add(int64_t a, int64_t b);`
  - `CALL_HOST` maps to direct calls with ABI-safe primitives.

### JVM/CLR/JS targets

- `CALL_HOST` maps to method calls / delegates / JS functions with primitive args/results.
- Externalized effects map to a host “driver” that handles requests using the platform runtime.

---

## Error handling

This proposal intentionally keeps error semantics simple:

- VM traps remain VM traps (`Trap { message }`).
- Host imports may either:
  - return an ABI-safe “error value” (e.g. `Result`-like encoding in user space), or
  - signal a trap (implementation-defined; translator/runtime may provide a helper).

Future work can standardize a richer error ABI.

---

## Implementation plan (staged)

1) Define the bytecode module format (“ExecutableModule”) and internal IDs.
2) Lower MIR → bytecode:
   - intern strings into IDs,
   - move literals into constant pool,
   - lower CFG edges to jumps/moves.
3) Implement a bytecode interpreter with:
   - `CALL_HOST`,
   - externalized `perform` → `StepResult::Request`,
   - `resume`.
4) Add a minimal C embedding target:
   - runtime + linking story for host imports + request/resume loop.

---

## Future work

- Expand ABI-safe value set:
  - typed handles for VM heap objects,
  - `typerep` across boundary (if generics require it),
  - shared buffers/zero-copy IO.
- Add “batch request” or multi-fiber support (if needed), while keeping a compatible host boundary.
- Provide standard library/environment conventions for common effects (IO, timers, channels).
- Add verifier and stable on-disk serialization with versioning guarantees.

---

## Open questions

1) Do we want `StepResult::Yield` (fuel-based preemption) in v0, or keep `step()` as “run until
   done/trap/request” only?

  We can add fuel-based preemption later if needed, but starting without it keeps the initial API simpler and more focused on the core host interaction patterns.

2) Should `CALL_HOST` use:
   - typed signatures (recommended for simplest C interop), or
   - a single generic “Value slice” ABI (simpler VM runtime, but worse host ergonomics)?

  Starting with typed signatures is likely better for host ergonomics and translator simplicity, especially in C where variadic or slice-based APIs can be awkward. We can consider a more generic ABI later if needed.

3) How do we version the ABI across translators and runtimes (module version vs runtime version)?

  A module versioning scheme with explicit compatibility guarantees is likely best for long-term stability, but we can start with a single version and evolve it as needed.
