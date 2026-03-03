# Ergonomic Host Integration API (Compiler + VM)

## Summary

This proposal introduces an ergonomic, **typed** host integration layer on top of the existing
compiler/VM “host imports” and “externalized effects” APIs.

Key improvements:

1. **Typed ABI conversions** so hosts can write `Ok(42)` instead of `Ok(AbiValue::Int(42))`, and
   decode arguments without verbose `match` ladders.
2. **Generic host import registration** so `Vm::register_host_import` can accept typed closures
   (tuples as arguments) rather than forcing `&[AbiValue]`.
3. **Generic host module declarations** so `HostModuleDecl` / `HostFunctionDecl` can be built from
   Rust types (e.g. `(String,) -> ()`) instead of explicit `HostType` vectors.
4. **One-step external effect dispatch** by building an `EffectId -> handler` table once at startup
   and indexing it directly on `StepResult::Request`.

This proposal is **API-focused**: it is intended to be implemented as new public helpers/builders
and trait impls. It does **not** change bytecode format, VM semantics, or compiler lowering rules.

---

## Motivation

### 1) `AbiValue` conversions are repetitive and error-prone

Today a host import typically looks like:

```rust
vm.register_host_import(id, |args: &[AbiValue]| match args {
    [AbiValue::String(s)] => Ok(AbiValue::Unit),
    other => Err(HostError { message: format!("bad args: {other:?}") }),
})?;
```

The host is forced to:

- decode `&[AbiValue]` manually,
- encode the return value manually,
- produce bespoke “bad args” errors for every import.

### 2) Compiler-side host declarations are verbose

Declaring a host module currently requires building nested structs:

- `HostModuleDecl { visibility, functions: vec![HostFunctionDecl { name, visibility, sig }, ...] }`
- `HostFnSig { params: vec![HostType::...], ret: HostType::... }`

This is mechanically straightforward but very boilerplate-heavy for embedder code and `rusk-host`
installers.

### 3) Externalized effect dispatch is multi-step per request

The recommended host loop typically does:

1. receive `StepResult::Request { effect_id, args, k }`,
2. look up `module.external_effect(effect_id)` to get `(interface, method, sig)`,
3. match on `(interface, method)` (strings),
4. validate/decode `args`,
5. compute a result and call `vm_resume`.

Even though `EffectId` is already a stable numeric table index, hosts still end up doing repeated
lookups and string matching. This is both verbose and avoidable.

---

## Goals

- Make host imports and external effects **pleasant to implement** in Rust.
- Reduce boilerplate in:
  - `AbiValue` argument decoding and return value encoding,
  - host module declarations (`HostModuleDecl`, `HostFunctionDecl`, `HostFnSig`),
  - external effect dispatch (`StepResult::Request` loops).
- Keep the **existing low-level APIs** available for advanced use cases.
- Ensure typed helpers remain aligned with bytecode v0 ABI constraints:
  - unit/bool/int/float/string/bytes/continuation-handle only.

## Non-goals

- Changing bytecode format (`.rbc`) or ABI surface types.
- Changing VM operational semantics for `CALL_HOST`, `perform`, `resume`, or handler matching.
- Introducing async effects at the VM boundary (hosts can do async scheduling themselves; this
  proposal focuses on the typed, synchronous API surface).
- Adding support for non-ABI-safe values crossing the VM boundary (arrays/tuples/structs/etc.).

---

## Proposal

### 1) ABI conversions: `From<T> for AbiValue` + typed decoding

Add standard library conversion traits for the ABI-safe types so host code can use `.into()` and
typed extraction instead of hand-written pattern matches.

#### 1.1 `From<T> for AbiValue`

Implement `From` for common Rust types:

- `impl From<()> for AbiValue`
- `impl From<bool> for AbiValue`
- `impl From<i64> for AbiValue`
- `impl From<f64> for AbiValue`
- `impl From<String> for AbiValue`
- `impl From<&str> for AbiValue` (allocating)
- `impl From<Vec<u8>> for AbiValue`
- `impl From<&[u8]> for AbiValue` (allocating)
- `impl From<ContinuationHandle> for AbiValue`

This directly enables the user-requested pattern:

```rust
let v: AbiValue = 123_i64.into();
```

#### 1.2 Fallible decoding: `AbiValue::decode<T>()` + `AbiDecodeError`

For decoding, prefer a fallible, typed API that can carry a helpful error.

Because of Rust orphan rules, we cannot implement `TryFrom<&AbiValue>` for primitives like `i64`.
Instead, use:

- `AbiValue::decode::<T>() -> Result<T, AbiDecodeError>`
- `AbiDecode` trait implemented for ABI-safe Rust types (`i64`, `String`, `Vec<u8>`, ...)

This supports straightforward argument checks when needed:

```rust
let n: i64 = args[0].decode()?;
```

#### 1.3 Tuple decoding: `AbiArgs`

Because host imports receive `&[AbiValue]`, provide a trait to decode argument lists into tuples:

```rust
pub trait AbiArgs<'a>: Sized {
    fn decode(args: &'a [AbiValue]) -> Result<Self, AbiDecodeError>;
}
```

Implementations would be provided for tuples up to a fixed arity (e.g. 0–8 or 0–16):

- `()` decodes from `[]`
- `(T1,)` decodes from `[v1]`
- `(T1, T2)` decodes from `[v1, v2]`
- ...

This avoids per-function `match` on the full args slice while remaining Rust-stable (no variadic
generics required).

> Note: A “borrowed args” flavor (e.g. decoding `&str` or `&[u8]` without cloning) can be added
> later if we want to optimize allocations. This proposal keeps the first iteration simple and
> fully owned.

---

### 2) Typed host imports: `Vm::register_host_import_typed`

Add a new VM API that accepts a typed closure and handles ABI decode/encode automatically.

#### API sketch

```rust
impl Vm {
    pub fn register_host_import_typed<Args, Ret>(
        &mut self,
        id: HostImportId,
        f: impl FnMut(Args) -> Result<Ret, HostError> + 'static,
    ) -> Result<(), VmError>
    where
        for<'a> Args: AbiArgs<'a>,
        Ret: Into<AbiValue>,
    { /* adapter over existing register_host_import */ }
}
```

Usage:

```rust
vm.register_host_import_typed(id, |(s,): (String,)| -> Result<(), HostError> {
    /* ... */
    Ok(())
})?;
```

Ergonomic win:

- no `&[AbiValue]` in user code,
- no explicit `AbiValue::Unit` returns,
- argument arity/type mismatches become standardized decode errors (and can include the host import
  name for debugging).

#### Relationship to the existing API

- `Vm::register_host_import` remains the low-level escape hatch.
- `register_host_import_typed` is implemented as a small adapter that:
  - decodes `args` to `Args`,
  - calls the typed closure,
  - converts `Ret` into `AbiValue`.

No changes are required to the VM’s internal calling convention or runtime validation rules.

---

### 3) Generic host module declarations (compiler API)

Add builders and type-driven helpers so embedder code does not manually construct `HostType` /
`HostFnSig`.

#### 3.1 `HostFnSig::of::<Args, Ret>()`

Add a generic constructor restricted to ABI-safe types:

```rust
impl HostFnSig {
    pub fn of<Args, Ret>() -> Self
    where
        Args: HostParamTypes,
        Ret: HostReturnType,
    { /* builds Vec<HostType> + ret */ }
}
```

Where `HostParamTypes` is implemented for tuples of ABI-safe element types, and each element type
maps to a `HostType`:

- `()` → `HostType::Unit`
- `bool` → `HostType::Bool`
- `i64` → `HostType::Int`
- `f64` → `HostType::Float`
- `String` → `HostType::String`
- `Vec<u8>` → `HostType::Bytes`
- `Cont<P, R>` → `HostType::Cont { param: P, ret: R }`

This makes it hard to accidentally declare non-ABI-safe host imports (the type system rejects it
by missing trait bounds).

#### 3.2 `HostModuleDecl` builder with default visibility

Add a builder that reduces repetition (especially function visibility):

```rust
HostModuleDecl::public()
    .function::<(String,), ()>("print")
    .function::<(String,), ()>("println")
    .build()
```

Design points:

- Module visibility is set once (common case: `public`).
- Functions default to the module’s visibility unless overridden.
- Function names accept `impl Into<String>` so callers can pass `&'static str`.

The existing struct types remain as the “data model”; the builder is a convenience layer.

#### 3.3 Typed external effect declarations

Add a typed wrapper around `CompileOptions::register_external_effect`:

```rust
impl CompileOptions {
    pub fn register_external_effect_typed<Args, Ret>(
        &mut self,
        interface: impl Into<String>,
        method: impl Into<String>,
    ) -> Result<(), String>
    where
        Args: HostParamTypes,
        Ret: HostReturnType,
    {
        self.register_external_effect(interface, method, HostFnSig::of::<Args, Ret>())
    }
}
```

This ensures effect declarations and host-side handlers can share the same Rust type signature
notation.

---

### 4) One-step external effect dispatch: `EffectDispatchTable`

Add a small VM-facing helper type that builds a dense `Vec` indexed by `EffectId` and stores the
registered handlers.

This mirrors how host imports are already dispatched by `HostImportId`.

#### API sketch

```rust
pub struct EffectDispatchTable {
    // length == module.external_effects.len()
    handlers: Vec<Option<Box<dyn HostFn>>>,
}

impl EffectDispatchTable {
    pub fn new(module: &ExecutableModule) -> Self;

    pub fn register_typed<Args, Ret>(
        &mut self,
        module: &ExecutableModule,
        interface: &str,
        method: &str,
        f: impl FnMut(Args) -> Result<Ret, HostError> + 'static,
    ) -> Result<(), String>
    where
        for<'a> Args: AbiArgs<'a>,
        Ret: Into<AbiValue>;

    pub fn dispatch(&mut self, effect_id: EffectId, args: &[AbiValue]) -> Result<AbiValue, HostError>;
}
```

Host loop becomes:

```rust
StepResult::Request { effect_id, args, k } => {
    let resume_value = effects.dispatch(effect_id, &args)?;
    vm_resume(&mut vm, k, resume_value)?;
}
```

That is the desired “one step to find the handler” behavior: `effect_id` indexes directly into the
table, with no per-request string matching.

#### Optional: name-based registration happens once

`register_typed` can do the string-to-id resolution exactly once at startup:

- `module.external_effect_id(interface, method)` → `EffectId`
- store handler at `handlers[effect_id.0 as usize]`

If the module does not contain that external effect, registration can either:

- return an error (strict mode), or
- ignore it (useful when sharing a host handler set across multiple modules).

The API can expose both behaviors:

- `register_*` (strict)
- `try_register_*` (best-effort)

---

### 5) Optional: helper to “drive VM until done/trap/yield”

For hosts that want an even higher-level API for the common synchronous case, add a helper function
that automatically handles externalized effects via `EffectDispatchTable`:

```rust
pub fn vm_step_with_effects(
    vm: &mut Vm,
    fuel: Option<u64>,
    effects: &mut EffectDispatchTable,
) -> Result<StepResult, StepWithEffectsError>
```

Semantics:

- it repeatedly calls `vm_step(vm, fuel)` until it returns `Done`, `Trap`, or `Yield`,
- when it sees `Request`, it dispatches to `effects`, calls `vm_resume`, and continues,
- if dispatch or resume fails, it returns `Err(...)` and leaves the VM suspended so the caller can
  decide whether to resume or cancel.

Fuel is applied per internal `vm_step` segment (between effect suspensions).

This keeps effect handling “out of band” (still host-driven) while removing boilerplate in the
typical “run to completion” embedding scenario.

---

## Examples

### Example A: `_std_host::print` / `println` host imports

#### Today (representative)

- Manually declare `HostModuleDecl` and `HostFnSig` using `HostType`.
- Manually pattern match ABI args and return `AbiValue`.

#### Proposed

Compiler side:

```rust
use rusk_compiler::{CompileOptions, HostModuleDecl};

options.register_host_module(
    "_std_host",
    HostModuleDecl::public()
        .function::<(String,), ()>("print")
        .function::<(String,), ()>("println")
        .build(),
)?;
```

VM side:

```rust
use rusk_vm::Vm;

if let Some(id) = module.host_import_id("_std_host::print") {
    vm.register_host_import_typed(id, |(s,): (String,)| {
        /* write to stdout */
        Ok(())
    })?;
}
```

### Example B: externalized effects dispatch

#### Today (representative)

```rust
let decl = module.external_effect(effect_id).unwrap();
match (decl.interface.as_str(), decl.method.as_str(), args.as_slice()) {
    ("TestFfi", "add", [AbiValue::Int(a), AbiValue::Int(b)]) => { /* ... */ }
    _ => { /* ... */ }
}
```

#### Proposed

Setup once:

```rust
let mut effects = EffectDispatchTable::new(&module);
effects.register_typed::<(i64, i64), i64>(&module, "TestFfi", "add", |(a, b)| Ok(a + b))?;
effects.register_typed::<(String,), String>(&module, "TestFfi", "echo", |(s,)| Ok(s))?;
```

Handle requests:

```rust
StepResult::Request { effect_id, args, k } => {
    let v = effects.dispatch(effect_id, &args)?;
    vm_resume(&mut vm, k, v)?;
}
```

---

## Compatibility / Migration

This can be implemented incrementally:

1. Add `From<T> for AbiValue` and decoding traits.
2. Add `Vm::register_host_import_typed` as an adapter over existing `register_host_import`.
3. Add `HostFnSig::of::<Args, Ret>()` and the `HostModuleDecl` builder.
4. Add `EffectDispatchTable` (pure host-side helper).

The existing APIs remain available. Once the typed layer is adopted by `rusk-host` and examples,
the old “manual struct construction + manual AbiValue matching” patterns can be deprecated (or kept
as low-level power-user interfaces).

Given the repository’s current stage, breaking changes are acceptable if they significantly reduce
API surface complexity (e.g. replacing ad-hoc patterns with one canonical typed pattern).

---

## Open Questions

1. **Tuple arity limit**: implement `AbiArgs` / `HostParamTypes` for up to N args (8? 16?).

8 should be enough, we can think about some kind of macro to simplify this later.

2. **Continuation typing**:
   - Implemented with a `Cont<P, R>` type in both layers:
     - `rusk_compiler::Cont<P, R>` is a compile-time signature marker (`cont(P) -> R`).
     - `rusk_vm::Cont<P, R>` is a runtime wrapper around `ContinuationHandle` that preserves the
       same type parameters for typed host APIs.

3. **Error typing**: should decode errors be `HostError`, a dedicated `AbiDecodeError`, or both?

Decoding errors are a distinct category and may want to carry different information (e.g. expected
vs. actual types), so a separate error type makes sense (with `From<AbiDecodeError> for HostError`
for easy `?` propagation).

4. **Registration strictness**: should effect handler registration error if the module does not
   declare the effect, or should it be a no-op by default?

Return an error; callers can explicitly ignore it if they want best-effort registration across
multiple modules.
