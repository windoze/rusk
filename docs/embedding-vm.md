# Embedding the Rusk Bytecode VM (`rusk-vm`)

This document describes the **bytecode VM embedding API** exposed by the `rusk-vm` crate:

- how to compile `.rusk` into a bytecode `rusk_bytecode::ExecutableModule`,
- how to **embed** and **drive** a VM instance from a host application,
- how to install **host functions** (imports),
- how to declare and handle **externalized effects** (effects that “bubble” to the host), and
- how to set up **compiler-visible prototypes** so compilation/typechecking can succeed.

The bytecode VM is designed as a small, step-driven runtime suitable for embedding into CLIs,
editors, servers, or other runtimes.

---

## Table of Contents

1. [Crates and layers](#crates-and-layers)
2. [ABI boundary: `AbiType` / `AbiValue`](#abi-boundary-abitype--abivalue)
3. [Compile-time prototypes (“what exists”)](#compile-time-prototypes-what-exists)
   - [Host function prototypes](#host-function-prototypes)
   - [External effect declarations](#external-effect-declarations)
4. [Producing a bytecode module](#producing-a-bytecode-module)
   - [Compile from source](#compile-from-source)
   - [Load/store `.rbc`](#loadstore-rbc)
5. [Embedding the VM](#embedding-the-vm)
   - [Create a VM](#create-a-vm)
   - [Install host imports](#install-host-imports)
   - [Drive execution with `vm_step`](#drive-execution-with-vm_step)
6. [Externalized effects: `StepResult::Request`](#externalized-effects-stepresultrequest)
   - [Host-side effect dispatch](#host-side-effect-dispatch)
   - [Resuming vs cancelling](#resuming-vs-cancelling)
   - [Limitations](#limitations)
7. [Pinned Continuations: host-storable continuation handles](#pinned-continuations-host-storable-continuation-handles)
   - [Host functions with continuation signatures](#host-functions-with-continuation-signatures)
   - [How continuation values cross the ABI boundary](#how-continuation-values-cross-the-abi-boundary)
   - [Host-driven tail resumption](#host-driven-tail-resumption)
   - [Dropping pinned continuations](#dropping-pinned-continuations)
8. [Generic specialization (advanced)](#generic-specialization-advanced)
9. [Troubleshooting](#troubleshooting)

---

## Crates and layers

At a high level the pipeline is:

`Rusk source (.rusk)` → `MIR` → `Bytecode module (.rbc)` → `VM execution`

In this repo, that corresponds to:

- `rusk-compiler` (`rusk_compiler` in Rust code): parses/typechecks Rusk and lowers to MIR, and can
  also produce bytecode by invoking the lowerer.
- `rusk-bytecode` (`rusk_bytecode`): the bytecode module data model (`ExecutableModule`), verifier,
  and `.rbc` encode/decode.
- `rusk-vm` (`rusk_vm`): a small bytecode interpreter with a **step API** and a strict VM/host ABI.
- `rusk-host` (`rusk_host`): optional helpers for defining host-module prototypes + installing
  matching runtime implementations (example: `std::print`, `std::println`).

This document focuses on embedding `rusk-vm`.

---

## ABI boundary: `AbiType` / `AbiValue`

`rusk-vm` intentionally exposes a small ABI surface for crossing the VM/host boundary.

On the host side you deal with:

- `rusk_vm::AbiValue`: runtime values that can cross the boundary.
- `rusk_bytecode::AbiType`: the type-level view of those values, stored in bytecode signatures.

### ABI-safe primitives (v0)

The current bytecode VM ABI supports:

- `unit`
- `bool`
- `int` (signed 64-bit)
- `float` (IEEE-754 `f64`)
- `string` (UTF-8)
- `bytes` (`Vec<u8>`)
- `continuation` (opaque continuation handles; see [Pinned Continuations](#pinned-continuations-host-storable-continuation-handles))

In Rust:

```rust
use rusk_vm::AbiValue;

let v = AbiValue::Int(123);
assert_eq!(v.ty(), rusk_bytecode::AbiType::Int);
```

### What this means for embeddings

- **Host imports** (host functions called via `call`) can only take/return ABI-safe primitives.
- **Externalized effects** (handled via `StepResult::Request`) can only take/return ABI-safe
  primitives.
- If you need richer payloads, the current pattern is to encode them into `bytes` (or `string`).

This is a deliberate constraint for v0: it keeps embeddings simple and makes `.rbc` modules
portable.

---

## Compile-time prototypes (“what exists”)

Rusk is designed to be embedded: platform integration is provided by host-defined surfaces.

There are **two distinct “registration” phases**:

1. **Compiler-visible prototypes** (names + signatures): required so *compilation* can resolve and
   typecheck calls.
2. **Runtime implementations**: required so *execution* can succeed.

If you skip (1), compilation fails. If you skip (2), the VM traps at runtime when a missing import
is called.

### Host function prototypes

Host function prototypes are grouped into **host modules** and passed to compilation via
`rusk_compiler::CompileOptions`.

Example: declare a `std` module with `print` and `println`:

```rust
use rusk_compiler::{
    CompileOptions, HostFnSig, HostFunctionDecl, HostModuleDecl, HostType, HostVisibility,
};

let mut options = CompileOptions::default();

options.register_host_module(
    "std",
    HostModuleDecl {
        visibility: HostVisibility::Public,
        functions: vec![
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "println".to_string(),
                sig: HostFnSig { params: vec![HostType::String], ret: HostType::Unit },
            },
        ],
    },
)?;
```

In a Rusk program, those are called like normal functions:

```rusk
fn main() -> unit {
    std::println("hello from rusk");
}
```

#### ABI safety for bytecode

For programs you intend to run on `rusk-vm`, **host prototypes must be ABI-safe**:

- Allowed: `unit`, `bool`, `int`, `float`, `string`, `bytes`
- Not currently ABI-safe for bytecode v0: `any`, `typerep`, arrays, tuples, and other composites

If a program calls a host import that isn’t ABI-safe, `compile_*_to_bytecode_with_options(...)`
fails with a message like:

> `host import '<name>' is not ABI-safe for bytecode v0`

#### Recommended: use `rusk-host` to keep prototypes and implementations aligned

This repository provides `rusk-host` helpers to avoid drift between “what the compiler thinks
exists” and “what the runtime actually provides”.

For example, `rusk_host::std_io` contains:

- `std_io::register_host_module(&mut CompileOptions)` (compiler prototypes)
- `std_io::install_vm(&ExecutableModule, &mut Vm)` (runtime implementations for `rusk-vm`)

---

### External effect declarations

Effects are declared in Rusk as `interface`s and *performed* using `@Interface.method(...)`.

Normally, if an effect is performed and **no in-language handler** is found, execution traps with
an “unhandled effect” runtime error.

To let the **host** handle a subset of effects, you can declare them as **externalized effects**
when compiling to bytecode. Then, an unhandled `perform` becomes a `StepResult::Request` instead of
a trap.

#### 1) Declare the interface in Rusk source

```rusk
interface TestFfi {
    fn add(a: int, b: int) -> int;
}

fn main() -> int {
    @TestFfi.add(1, 2)
}
```

#### 2) Register the externalized effect when compiling to bytecode

```rust
use rusk_compiler::{CompileOptions, HostFnSig, HostType};

let mut options = CompileOptions::default();
options.register_external_effect(
    "TestFfi",
    "add",
    HostFnSig { params: vec![HostType::Int, HostType::Int], ret: HostType::Int },
)?;
```

#### ABI safety and signature matching

Important notes:

- For bytecode v0, external effect signatures must also be ABI-safe (same primitive set as host
  imports). The compiler will reject non-ABI-safe signatures when compiling to bytecode.
- The compiler currently does **not** typecheck that your registered external-effect signature
  matches the interface method signature in the program. Keep them aligned (ideally define them in
  one place in your embedding codebase).

---

## Producing a bytecode module

### Compile from source

The simplest embedding approach is “compile then run”:

```rust
use rusk_compiler::{CompileOptions, compile_file_to_bytecode_with_options};
use rusk_host::std_io;
use std::path::Path;

let mut options = CompileOptions::default();
std_io::register_host_module(&mut options);

let module = compile_file_to_bytecode_with_options(Path::new("path/to/program.rusk"), &options)?;
```

If you already have the source text in memory:

```rust
use rusk_compiler::{CompileOptions, compile_to_bytecode_with_options};

let module = compile_to_bytecode_with_options(source_text, &options)?;
```

### Load/store `.rbc`

For “compile once, run many times” embedding, serialize bytecode to `.rbc` and load it later:

```rust
use rusk_bytecode::{from_bytes, to_bytes};

let bytes: Vec<u8> = to_bytes(&module)?;
let loaded = from_bytes(&bytes)?; // includes verification
```

If you construct or mutate an `ExecutableModule` in memory, you can also explicitly verify it:

```rust
use rusk_bytecode::verify_module;

verify_module(&module)?;
```

---

## Embedding the VM

### Create a VM

To run bytecode, create a `Vm` from an `ExecutableModule`:

```rust
use rusk_vm::Vm;

let mut vm = Vm::new(module.clone())?;
```

Notes:

- `Vm::new` starts execution at `module.entry` (normally `main`).
- `Vm::new` requires the entry function to take 0 parameters (for `fn main()`).

If your program defines `fn main(argv: [string])`, pass argv at construction time:

```rust
use rusk_vm::Vm;

// Host-provided command line arguments.
// Convention: argv[0] is the full path of the executed file, or "" if there is no file.
let argv = vec![String::new(), "arg1".to_string(), "arg2".to_string()];

let mut vm = Vm::new_with_argv(module.clone(), argv)?;
```

All strings in `argv` must be valid UTF-8. Hosts should perform lossy conversion when reading
platform/OS arguments if necessary.

### Install host imports

Host imports are declared inside the bytecode module, and identified by a stable
`rusk_bytecode::HostImportId`.

At runtime, you must provide implementations via `Vm::register_host_import`.

#### Install by name

The ergonomic pattern is:

1. Look up the `HostImportId` by name using the module.
2. Register a closure (or any `HostFn` implementation).

```rust
use rusk_vm::{AbiValue, HostError, Vm};

if let Some(id) = module.host_import_id("std::println") {
    vm.register_host_import(id, |args: &[AbiValue]| match args {
        [AbiValue::String(s)] => {
            println!("{s}");
            Ok(AbiValue::Unit)
        }
        other => Err(HostError { message: format!("std::println: bad args: {other:?}") }),
    })?;
}
```

#### Install a whole host set

Prefer reusing `rusk-host` where possible. For example:

```rust
use rusk_host::std_io;

std_io::install_vm(&module, &mut vm);
```

#### Runtime validation behavior

The VM validates host calls at runtime:

- Arity must match the signature in the module.
- Each argument must be ABI-safe and match the expected `AbiType`.
- The return value must match the declared return `AbiType`.

If any of those checks fail, the VM traps with a descriptive error.

#### Reentrancy rule

While a host import is executing, the VM is considered “in a host call”.

- Calling back into the VM (e.g. calling `vm_step` from inside a host function) will cause a trap:
  “vm re-entered during host call”.

Design intent: host imports are synchronous leaf operations, not recursive VM entrypoints.

---

### Drive execution with `vm_step`

Execution is driven by repeatedly calling:

```rust
use rusk_vm::{StepResult, vm_step};

let step: StepResult = vm_step(&mut vm, /* fuel */ None);
```

`vm_step` is a small state machine:

- `StepResult::Done { value }`: program finished successfully.
- `StepResult::Trap { message }`: unrecoverable runtime error.
- `StepResult::Request { effect_id, args, k }`: the VM suspended on an externalized effect.
- `StepResult::Yield { remaining_fuel }`: the VM used up its fuel budget (cooperative scheduling).

#### Fuel (cooperative scheduling / time slicing)

`vm_step(vm, fuel)` accepts:

- `None` to run until `Done` / `Trap` / `Request` (effectively “no limit”),
- `Some(n)` to run for at most `n` instruction steps before returning `Yield`.

This is useful to:

- avoid unbounded execution in a single host tick,
- integrate multiple VMs into a scheduler/event-loop,
- implement timeouts.

---

## Externalized effects: `StepResult::Request`

When an effect is performed and no in-language handler matches it, the VM either:

- traps (`unhandled effect`), or
- suspends and returns a `Request` if the performed effect was declared in the module’s external
  effect table.

On `Request`, the host receives:

- `effect_id: rusk_bytecode::EffectId` (stable table index),
- `args: Vec<AbiValue>` (ABI-safe payload),
- `k: rusk_vm::ContinuationHandle` (a handle representing “the rest of the computation”).

### Host-side effect dispatch

A common pattern is to resolve the effect name + signature from the module and then dispatch:

```rust
use rusk_vm::{AbiValue, StepResult, vm_drop_continuation, vm_resume, vm_step};

loop {
    match vm_step(&mut vm, None) {
        StepResult::Done { value } => break value,
        StepResult::Trap { message } => panic!("vm trapped: {message}"),
        StepResult::Yield { .. } => continue,
        StepResult::Request { effect_id, args, k } => {
            let decl = module
                .external_effect(effect_id)
                .unwrap_or_else(|| panic!("unknown external effect id {}", effect_id.0));

            let resume_value = match (decl.interface.as_str(), decl.method.as_str(), args.as_slice()) {
                ("TestFfi", "add", [AbiValue::Int(a), AbiValue::Int(b)]) => AbiValue::Int(a + b),
                other => {
                    let _ = vm_drop_continuation(&mut vm, k);
                    panic!("unhandled external effect request: {other:?}");
                }
            };

            vm_resume(&mut vm, k, resume_value).expect("resume");
        }
    }
}
```

### Resuming vs cancelling

Once you receive a `Request`, the VM is **suspended**:

- Calling `vm_step` again without resuming/cancelling produces a trap:
  “vm is suspended; call resume/drop first”.

You have two choices:

1. **Resume** with a value:
   - `vm_resume(&mut vm, k, value)`
   - The value becomes the result of the `@Interface.method(...)` expression in the Rusk program.
2. **Cancel** the continuation:
   - `vm_drop_continuation(&mut vm, k)`
   - The VM transitions to a trapped “cancelled” state (useful for aborting).

Both operations validate the continuation handle; resuming with a stale/wrong handle returns a
`VmError::InvalidContinuation`.

### Limitations

Current external-effect behavior has intentional v0 limits:

- **Non-generic effects only.** The VM currently only externalizes effects where the performed
  interface has **no runtime type arguments** (i.e. no `interface Foo<T> { ... }` externalization
  yet).
- **ABI-safe args/return only.** Use `bytes` or `string` to encode complex values.
- **One outstanding request per VM.** While suspended, the VM cannot make progress until the host
  resumes or cancels it.

For deeper background on how effects and continuations work in Rusk, see:
`docs/delimited-continuations-and-scoped-effects.md`.

---

## Pinned Continuations: host-storable continuation handles

Rusk supports **first-class, one-shot delimited continuations** as language-level values (type
`cont(<param>) -> <ret>`). These can be captured in effect handlers, stored in variables, and
resumed later.

Starting with the "Tail-resume from host" feature (2026-02-20), **continuation values can cross
the VM/host ABI boundary** as opaque handles. This enables embeddings where:

- A Rusk program captures a continuation and passes it to the host (via a host import call or
  externalized effect result).
- The host stores the continuation handle(s) in host-owned state (e.g. a scheduler, event loop, or
  custom registry).
- The host can later **resume** the continuation (either by passing the handle back to Rusk code,
  or by using the host-driven tail-resume API).
- The host can explicitly **drop** unused continuations to release resources.

This is particularly useful for embeddings that need to integrate Rusk continuations with native
event loops, async runtimes, or other host-side scheduling mechanisms.

### Host functions with continuation signatures

To declare host functions that accept or return continuations, use `HostType::Cont` in the
signature:

```rust
use rusk_compiler::{
    CompileOptions, HostFnSig, HostFunctionDecl, HostModuleDecl, HostType, HostVisibility,
};

let mut options = CompileOptions::default();

options.register_host_module(
    "host",
    HostModuleDecl {
        visibility: HostVisibility::Public,
        functions: vec![
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "store_cont".to_string(),
                sig: HostFnSig {
                    params: vec![HostType::Cont {
                        param: Box::new(HostType::Int),
                        ret: Box::new(HostType::Int),
                    }],
                    ret: HostType::Unit,
                },
            },
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "take_cont".to_string(),
                sig: HostFnSig {
                    params: Vec::new(),
                    ret: HostType::Cont {
                        param: Box::new(HostType::Int),
                        ret: Box::new(HostType::Int),
                    },
                },
            },
        ],
    },
)?;
```

In Rusk code, these host functions can be called naturally:

```rusk
interface E { fn boom() -> int; }

fn main() -> int {
    // Capture a continuation and pass it to the host for storage
    match @E.boom() {
        @E.boom() -> k => { host::store_cont(k); 0 }
        x => x
    };

    // Later: retrieve the continuation from the host and resume it
    let k = host::take_cont();
    k(42)
}
```

### How continuation values cross the ABI boundary

When a `Value::Continuation` crosses the VM/host boundary (as an argument or return value), the VM
automatically **pins** the continuation in an internal GC-rooted table and converts it to:

- `AbiValue::Continuation(ContinuationHandle)`

Where `ContinuationHandle` is an opaque, generation-checked handle:

```rust
use rusk_vm::{AbiValue, ContinuationHandle};

// In a host import implementation:
if let Some(id) = module.host_import_id("host::store_cont") {
    vm.register_host_import(id, |args: &[AbiValue]| match args {
        [AbiValue::Continuation(handle)] => {
            // Store the handle in host state (e.g. a Vec or HashMap)
            store_continuation_somewhere(*handle);
            Ok(AbiValue::Unit)
        }
        _ => Err(/* ... */)
    })?;
}
```

When `AbiValue::Continuation(handle)` flows back into the VM (e.g. returned from a host import),
the VM looks up the pinned continuation and converts it back to `Value::Continuation`.

**Important properties:**

- **Pinned continuations are GC-rooted**: while the host holds a handle, the captured Rusk
  frames/values remain alive (will not be collected).
- **Handles are opaque and VM-specific**: they have no meaning outside the VM that issued them.
- **One-shot semantics are preserved**: resuming a continuation (in-language or from the host)
  consumes the underlying state; further resume attempts fail with `InvalidContinuation`.
- **Generation checks prevent use-after-free**: if a handle's slot is reused, stale handles are
  rejected.

### Host-driven tail resumption

The host can directly resume a pinned continuation using:

```rust
use rusk_vm::{vm_resume_pinned_continuation_tail, AbiValue};

vm_resume_pinned_continuation_tail(&mut vm, handle, AbiValue::Int(42))?;
```

This is a **tail-resume** operation:

- It consumes the one-shot continuation state.
- It splices the captured continuation segment *on top of* the current VM stack (as-if tail-resuming
  from a host handler frame that is not represented in the VM).
- The continuation's final return value is **discarded** (there is no "second half" host handler
  frame to receive it). If the VM stack is empty, the resumed continuation becomes the new entry
  computation and its return value becomes the program result as usual.
- The operation is **schedule-only**: it does not step the VM. The host must subsequently call
  `vm_step` to drive execution.

Example host-side resume loop:

```rust
use rusk_vm::{StepResult, vm_step, vm_resume_pinned_continuation_tail, AbiValue};

// After storing a continuation handle from a host import call:
vm_resume_pinned_continuation_tail(&mut vm, stored_handle, AbiValue::Int(42))?;

loop {
    match vm_step(&mut vm, None) {
        StepResult::Done { value } => break value,
        StepResult::Trap { message } => panic!("vm trapped: {message}"),
        StepResult::Yield { .. } => continue,
        StepResult::Request { .. } => { /* handle externalized effects */ }
    }
}
```

**Why tail-only?**

A non-tail host resume would require the host to keep state for the "second half" of a host effect
handler, which is outside the scope of the current embedding model. Tail-resume semantics avoid
this by consuming the continuation without returning control to a host handler frame.

### Dropping pinned continuations

If the host decides not to resume a continuation (e.g. cancelling a request or cleaning up on
shutdown), it must explicitly drop the handle to release the pinned resources:

```rust
use rusk_vm::vm_drop_pinned_continuation;

vm_drop_pinned_continuation(&mut vm, handle)?;
```

After dropping, the handle becomes invalid and the captured continuation state is eligible for
garbage collection (unless still reachable from the running VM).

**Important:**

- Dropping a continuation is **not** an error; it is a normal cleanup operation (equivalent to
  abandoning a continuation in Rusk code).
- Attempting to drop the same handle twice, or dropping an already-consumed handle, returns an
  error.

---

## Generic specialization (advanced)

Most embeddings can ignore this section.

The bytecode VM supports Rusk generics by passing implicit runtime `TypeRep` values to generic
functions. For some embeddings, it can be useful to implement **specific instantiations** of a
generic function in the host (e.g. a fast path).

`rusk-vm` exposes this via:

- `Vm::intern_type_rep` to build the VM’s internal `TypeRepId` for a type constructor, and
- `Vm::register_generic_specialization` to route calls to a host import when a specific set of
  type arguments is present.

High-level idea:

1. Your program calls a generic bytecode function `F<T>(...)`.
2. At runtime, the VM can observe the concrete `TypeRep` arguments for that call.
3. If the host registered `(F, [type_args...]) -> host_import_id`, the VM calls that host import
   instead of interpreting the function body.

Important constraints enforced by the VM:

- The target function must actually be generic (`generic_params > 0`).
- `type_args.len()` must match the function’s generic arity.
- The host import signature must match the *value* parameters (type-rep params are excluded).

If you need this, read the implementation in `crates/rusk-vm/src/lib.rs` and expect the API to
evolve (this is a v0 runtime).

---

## Troubleshooting

### Compile-time errors

- **Unknown host function**
  - Symptom: compilation fails during name resolution/typechecking.
  - Fix: register the function’s prototype in `CompileOptions` via `register_host_module(...)`.

- **“host import … is not ABI-safe for bytecode v0”**
  - Symptom: compilation fails while producing bytecode.
  - Fix: restrict host function signatures to ABI-safe primitives for code meant to run on
    `rusk-vm`, or extend the VM/ABI to support the types you need.

- **“external effect … has non-ABI-safe signature for bytecode v0”**
  - Symptom: `register_external_effect(...)` works but compiling to bytecode fails.
  - Fix: restrict external effect signatures to ABI-safe primitives.

### Runtime traps

- **“missing host import implementation: `<name>`”**
  - You compiled a module that imports a host function, but didn’t register it via
    `Vm::register_host_import`.

- **“vm is suspended; call resume/drop first”**
  - You received `StepResult::Request` and called `vm_step` again without resuming/cancelling.

- **“vm re-entered during host call”**
  - A host import attempted to call back into the VM (directly or indirectly). Host imports must
    be synchronous leaf operations.

- **”unhandled effect: Interface.method”**
  - The program performed an effect that had no in-language handler, and it was not declared as an
    externalized effect in the module.

- **”invalid continuation handle”** (when using `vm_resume_pinned_continuation_tail` or
  `vm_drop_pinned_continuation`)
  - The continuation handle is stale (already consumed or dropped), has the wrong generation, or
    belongs to a different VM.
  - Ensure the handle is used only once (one-shot) and that you're not attempting to resume/drop
    after a previous consume operation.
