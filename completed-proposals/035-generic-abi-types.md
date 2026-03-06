# Generic ABI-Safe Nominal Types (Closed Generic Instantiations at VM ↔ Host Boundary)

Date: 2026-03-06  
Status: Draft

## Summary

Rusk’s VM/host ABI currently allows:

- primitives (`unit`, `bool`, `int`, `float`, `byte`, `char`, `string`, `bytes`, `cont(P) -> R`)
- structural composites (`[T]`, `(T0, T1, ...)`)
- nominal composites **only when monomorphic** (`struct S`, `enum E` with **no** type arguments)

This means common library enums like `Option<T>` / `Result<T, E>` cannot appear in:

- `extern fn` host import signatures
- externalized effect signatures (`StepResult::Request` / `vm_resume`)

This proposal extends the notion of **ABI-eligible types** to allow **closed generic instantiations**
of nominal types, i.e. nominal types with **no unbound type arguments**:

- ✅ `Option<bytes>`
- ✅ `Result<string, int>`
- ✅ `Vec<Result<int, string>>` (if `Vec<T>` is a Rusk `struct`/`enum` and the instantiation is closed)
- ❌ `Option<T>` (unbound type arg)
- ❌ `Result<T, E>` (unbound type args)
- ❌ `Option<Iface>` / `Option<fn(...) -> ...>` (not ABI-eligible)

Key idea: **generic values may cross the ABI boundary iff their type arguments are fully known at
compile time and each argument is itself ABI-eligible.**

---

## Motivation

Without `Option<T>` / `Result<T, E>` at the ABI boundary, host APIs are forced into awkward shapes:

- Use sentinel values (e.g. empty string for “none”) → ambiguous / error-prone.
- Trap the VM for recoverable errors → poor ergonomics and composability.
- Encode results into `bytes`/`string` and decode → boilerplate and overhead.

These are exactly the types that *should* be easy to exchange across the boundary:

- optional values (`Option<T>`)
- fallible results (`Result<T, E>`)
- domain-specific generic containers when instantiated with ABI-safe payloads

This aligns with the “reified type arguments” runtime model: applied types like
`Result<string, int>` already exist in the type system and as runtime `TypeRep` nodes; the ABI
restriction is the missing link. (See `completed-proposals/005-generic-rework.md` and
`BYTECODE_SPEC.md` §3.)

---

## Goals

- Allow nominal generic instantiations with **closed** ABI-safe type arguments in:
  - `extern fn` signatures
  - externalized effect signatures
- Preserve strong VM/host boundary validation:
  - traps on arity mismatch
  - traps on type mismatch (including generic argument mismatches)
- Keep the host-side inspection and construction story coherent:
  - host can observe the instantiated type arguments of a value
  - host can construct `Option<bytes>` / `Result<string, int>` without ad-hoc serialization
- Avoid “runtime polymorphic ABI”:
  - no unbound type variables in ABI types
  - no “any generic instantiation accepted” unless explicitly declared (`any`)

---

## Non-goals

- Allowing **generic `extern fn`** declarations (still forbidden; signatures remain monomorphic as
  *functions*).
- Allowing unbound type variables in ABI types (`Option<T>`).
- Adding a stable cross-language C ABI; this remains a VM embedding ABI.
- Adding host-defined nominal types (host inventing new struct/enum identities).
- Solving higher-kinded type arguments at the ABI boundary.

---

## Design

### 1) Terminology: “ABI-closed” types

Define **ABI-closed** types as types with no remaining inference/generic variables:

- no `T` generic parameters (`Ty::Gen`)
- no inference vars (`Ty::Var`)
- no `Self`
- no associated-type projections (`T::Assoc`)

This is stricter than “monomorphic” in the traditional sense, because Rusk has reified generics:
an applied type like `Result<string, int>` is ABI-closed even though it uses a generic definition.

### 2) ABI eligibility rule for nominal type applications

Update the ABI-eligibility rule for nominal types (structs/enums) from:

> “Nominal ABI types must be monomorphic: no type arguments.”

to:

> “Nominal ABI types may be applied to type arguments **only if** the instantiation is ABI-closed
> and all type arguments are ABI-eligible.”

Concretely, a nominal application `N<A0, A1, ...>` is ABI-eligible iff:

1. `N` resolves to a `struct` or `enum` definition.
2. Generic arity matches: `arity(N) == len(args)`.
3. For each `Ai`: `Ai` is ABI-eligible **and** ABI-closed.
4. After substituting the type arguments into the nominal’s fields/variant field types, **all
   reachable field types** are ABI-eligible.

Notes:

- This naturally allows nested closed instantiations (`Option<Result<int, string>>`).
- ABI eligibility remains recursive and structural: a generic instantiation is only as ABI-eligible
  as what it contains.

### 3) ABI type representation changes (`HostType` / `AbiType`)

We need to represent “nominal type + arguments” in the ABI signature format.

#### 3.1 MIR / tooling surface (`rusk_mir::HostType`)

Extend `HostType` to include type arguments for nominal types:

```rust
// Illustrative shape (exact naming TBD)
pub enum HostType {
  // ...
  Struct { name: String, args: Vec<HostType> },
  Enum   { name: String, args: Vec<HostType> },
  // ...
}
```

For non-generic types, `args` is empty.

This is used for:

- host import signatures (`HostFnSig`)
- externalized effect signatures
- ABI schema emission (see §5)

#### 3.2 Bytecode ABI surface (`rusk_bytecode::AbiType`)

Extend `AbiType` similarly:

```rust
pub enum AbiType {
  // ...
  Struct { type_id: TypeId, args: Vec<AbiType> },
  Enum   { type_id: TypeId, args: Vec<AbiType> },
  // ...
}
```

This allows the ABI to distinguish `Result<string, int>` from `Result<int, string>` at runtime.

### 4) Bytecode format changes

`BYTECODE_SPEC.md` §3.1 currently defines:

- `struct(TypeId)`
- `enum(TypeId)`

This proposal changes them to include an argument list payload:

- `struct(TypeId, args...)`
- `enum(TypeId, args...)`

Encoding sketch (tag + payload):

- `Struct`: `type_id: u32`, then `args_len: len`, then `args: [AbiType; args_len]`
- `Enum`: `type_id: u32`, then `args_len: len`, then `args: [AbiType; args_len]`

This is a breaking change to `.rbc` encoding and requires bumping the bytecode ABI version.

### 5) ABI schema support for generic nominals

To let the host inspect/construct generic structs/enums, the ABI schema format must express field
types that mention the nominal’s generic parameters.

Example schema for `enum Option<T> { Some(T), None }` needs to reference “type parameter 0”.

Two viable approaches:

#### Option A — Add schema-only type variables

Introduce a *schema type* that can reference generic parameters, e.g. `AbiSchemaType`, and use it
inside `AbiSchema` instead of `AbiType`:

```rust
pub enum AbiSchemaType {
  // all AbiType cases...
  TypeParam(u32), // only valid in schemas
}
```

Then:

- `AbiSchema` entries are keyed by the nominal `TypeId` (the definition).
- For non-generic structs/enums, no `TypeParam` appears.
- For generic structs/enums, `TypeParam(i)` may appear in field positions.

Host-side validation substitutes `TypeParam(i)` with the actual `args[i]` from the instantiated
`AbiType::Struct { .. }` / `AbiType::Enum { .. }`.

Pros:
- Single schema per definition (no per-instantiation explosion).
- Host reflection remains predictable and relatively small.

Cons:
- Adds a second “type language” for schemas (but this is already conceptually true today).

#### Option B — Emit specialized schemas per instantiated ABI type

Emit `abi_schemas` entries only for the concrete instantiated types that appear in host import /
external effect signatures (e.g. emit a schema for `Option<bytes>`).

This requires changing the schema table key from `TypeId` to something like:

- `(TypeId, args...)`, or
- a `TypeRepId`-like key, or
- a new “ABI nominal instance id”.

Pros:
- Schema stays in terms of plain `AbiType` (no `TypeParam`).

Cons:
- More bytecode format change surface.
- More complexity and potentially larger `.rbc` files.

**Recommendation:** Option A (schema-only type parameters) is the simplest coherent model for v1.5.

### 6) Runtime value validation & representation

Generic values already exist in the VM heap as:

- `HeapValue::Struct { type_id, type_args: Vec<TypeRepId>, fields }`
- `HeapValue::Enum { type_id, type_args: Vec<TypeRepId>, variant, fields }`

Currently the host boundary rejects `type_args != []` (“generic values are not ABI-eligible in v1”).

With this proposal:

- `AbiType::Struct { type_id, args }` / `AbiType::Enum { type_id, args }` are valid expectations.
- When converting VM `Value` → `AbiValue` under an expected `AbiType`, the VM validates:
  - `type_id` matches
  - the runtime `type_args` match the expected `args` (after converting each `AbiType` arg to a
    runtime `TypeRepId`)
- When converting `AbiValue` → VM `Value`, the VM validates the same invariant to prevent host-side
  forging/mismatches.

#### Note on `AbiValue::ty()`

Host code frequently relies on `AbiValue::ty()` for validation (e.g. struct/enum construction).
To support generic nominals, ABI values that refer to structs/enums should carry their instantiated
ABI type arguments, so `ty()` can return a fully-instantiated `AbiType` and comparisons work.

Design direction:

- extend `AbiStructRef` / `AbiEnumRef` to carry `args: Vec<AbiType>` (or a boxed slice) alongside
  `type_id`
- have `AbiValue::ty()` return `AbiType::Struct { type_id, args }` / `AbiType::Enum { ... }`

### 7) HostContext API changes (reflection + construction)

To make generic instantiations usable from host code:

- Reflection:
  - `enum_type_name` remains stable (returns the definition name, e.g. `core::result::Result`)
  - add `enum_type_args(&AbiEnumRef) -> &[AbiType]`
  - add `struct_type_args(&AbiStructRef) -> &[AbiType]`
- Construction:
  - extend `alloc_struct` / `alloc_enum` to accept a fully-instantiated nominal ABI type, not only
    a bare type name.
    - e.g. `alloc_enum_typed(AbiType::Enum { type_id, args }, variant, fields)`

This avoids ad-hoc per-type constructors while remaining fully typed and validated.

### 8) User-facing examples

Rusk-side host imports become dramatically nicer:

```rusk
mod env {
  extern fn get(key: string) -> Option<string>;
}

mod fs {
  extern fn read(path: string) -> Result<bytes, string>;
}
```

Host-side (Rust) can return values without `bytes` packing:

- `Ok(bytes)` / `Err(string)` for `Result<bytes, string>`
- `Some(string)` / `None` for `Option<string>`

---

## Compatibility & versioning

- This is a breaking change to the bytecode `.rbc` encoding of `AbiType` and likely `AbiSchema`.
- The project explicitly treats backwards compatibility as a non-goal at this stage; bumping the
  bytecode ABI version is acceptable.
- Source-level `extern fn` syntax does not change; only ABI eligibility expands.

---

## Alternatives considered

1) **Status quo**: keep generics out of ABI.
   - simplest, but forces serialization/sentinels and blocks idiomatic APIs.

2) **Special-case only `Option<T>` / `Result<T, E>` in `AbiType`** as dedicated ABI tags.
   - reduces generality and creates a “two class” system where some generics are structural and
     others nominal.
   - still useful as a minimal step, but doesn’t address user-defined generic containers.

3) **Send runtime `typerep` values across the ABI** and treat them as part of the signature.
   - makes the ABI more reflective/dynamic, increases surface area, and complicates embedders.
   - `typerep` is currently explicitly non-ABI-safe and is better kept internal.

4) **Monomorphize generic instantiations into distinct nominal IDs** (intern `Option<bytes>` as its
   own `TypeId`).
   - complicates runtime dispatch/type identity and bloats module tables.

---

## Decisions

This proposal’s “open questions” are decided as follows for the initial implementation:

1) **`cont(P) -> R` as a generic type argument: allowed**
   - `cont(P) -> R` is already ABI-safe, and it remains represented as an opaque pinned handle.
   - The ABI type does not encode `P`/`R` at runtime; those remain compile-time only.

2) **Diagnostics: add explicit ABI-eligibility failures**
   - ABI validation reports *why* a type is rejected (unbound generic, inference var, assoc proj,
     interface/fn type, generic arity mismatch, etc.), rather than a generic “unsupported host
     type” message.

3) **Host reflection: expose instantiated type arguments as `AbiType`**
   - Host APIs reflect generic instantiations as declarative `AbiType` trees (including type
     arguments), not VM-internal `TypeRep` handles.
   - The VM internally converts `AbiType` ↔ `TypeRepId` for runtime validation of generic
     struct/enum values crossing the boundary.
