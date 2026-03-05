# Complex ABI Types (byte / char / array / struct / enum / tuple) Across the VM ↔ Host Boundary

Status: Draft (Proposed)

Date: 2026-03-05

## Summary

Extend the VM/host ABI so **Rusk values beyond primitives** can cross the boundary without manual
packing/unpacking into `bytes` or long “field lists”.

This proposal adds:

- leaf primitives: `byte`, `char` (in addition to the existing ABI primitives)
- composite types: `array`, `tuple`, `struct`, `enum`

to:

- **host imports** (`CallTarget::Host`)
- **externalized effects** (`StepResult::Request` / `vm_resume`)

Scope is intentionally limited to **types defined on the Rusk side** (i.e., types present in the
compiled module). **Host-defined types are out of scope** for now.

The primary user-visible change is that a host function can accept/return a Rusk-defined composite
type directly (e.g. `Point`, `Result<int, string>`, `(int, int)`, `[Point]`), instead of requiring
bespoke serialization or manual field flattening.

---

## Motivation

Today the bytecode ABI boundary (`BYTECODE_SPEC.md` §3) only supports:

- `unit`, `bool`, `int`, `float`, `string`, `bytes`, `continuation`

It does **not** support:

- `byte`, `char`
- arrays (`[T]`)
- tuples (`(T0, T1, ...)`)
- structs / enums

As a result, when Rusk code and the embedding host both “speak” a composite type, they must still
do extra work to transfer it:

1. **Manual flattening**: pass many arguments (one per field) and rebuild on the other side.
2. **Ad-hoc serialization**: encode to `bytes` (or `string`) and decode in the host.

Both approaches are:

- verbose and error-prone (field order / versioning issues)
- slower than necessary (extra allocation and copying)
- hostile to API ergonomics (host APIs cannot be expressed naturally)

Enabling composite ABI types makes host surfaces feel like real APIs instead of byte-pipe
protocols.

---

## Goals

- Allow `byte` / `char` plus **Rusk-defined** `struct`, `enum`, `tuple`, and `array` values to be
  passed:
  - from VM → host (arguments),
  - from host → VM (return values),
  - and across `StepResult::Request` boundaries for external effects.
- Eliminate the need for user-written “pack/unpack” code (manual flattening or ad-hoc `bytes`
  serialization) in the common case.
- Preserve VM safety invariants:
  - no use-after-free of VM objects by the host,
  - no VM reentry during host calls (keep existing rule),
  - clear runtime validation and trapping on ABI mismatches.
- Keep the initial design **simple and implementable** with the current runtime type metadata.

---

## Non-goals

- **Host-defined types**: the host inventing new nominal types that Rusk code can reference.
- A stable cross-language ABI intended for C/FFI consumption.
- Arbitrary graph serialization or “deep clone” semantics for cyclic object graphs.
- Introducing implicit conversions between different shapes (no “auto packing/unpacking” between
  `struct` and `(field0, field1, ...)`).

---

## Design: Limited First, With a Path to “Arbitrary”

The prompt asks whether composite ABI support should be **arbitrary** or **limited**.

This proposal recommends a **limited v1** that is immediately useful and avoids the hardest
problems (full schema reflection, generic instantiation metadata, and cyclic deep-copy), while
leaving a clear upgrade path.

### Why “limited” first?

Rusk composite values are **reference-like** at runtime (heap objects referenced by handles). They
can be large and can form cyclic graphs. A fully “arbitrary by-value” ABI would require:

- a canonical deep serialization format for all heap objects,
- cycle handling (object identities / backreferences),
- schema metadata for every reachable nominal type (including generics),
- and substantial complexity in the VM/host adapters.

Instead, we can achieve the main ergonomic goal (“no manual pack/unpack”) by passing **opaque
references** to composite values plus **typed accessors**.

---

## Proposed ABI Surface (v1)

### 1) Declare host imports in Rusk source via `extern fn`

Remove the Rust-side “host function declaration configuration” mechanism (the `HostModuleDecl` /
`CompileOptions::register_host_module(...)` model). Instead, host-callable functions are declared
directly in Rusk source using `extern fn` items inside normal modules.

This keeps Rusk host interop in the language itself: the host surface is visible in source code,
can be reviewed, and is typechecked like any other API.

#### Syntax (new item form)

```ebnf
ExternFnItem := "extern" "fn" Ident "(" ExternParams? ")" "->" Type ";" ;
ExternParams := ExternParam ("," ExternParam)* (",")? ;
ExternParam  := Ident ":" Type ;
```

#### Rules (v1)

- All parameter and return types must be explicit (no inference in declarations).
- No generics on `extern fn` in v1 (monomorphic signatures only).
- The declared signature must be **ABI-eligible** (§5); otherwise it is a compile-time error.
- The fully-qualified path name (e.g. `std::println`) is the host import name stored into
  MIR/bytecode.

Example:

```rust
mod std {
    extern fn print(s: string) -> unit;
    extern fn println(s: string) -> unit;
}
```

Compilation model:

- Name resolution/typechecking treat `extern fn` like a normal function declaration with no body.
- Lowering turns calls to extern functions into `CallTarget::Host(...)` and emits a host import
  entry with the declared ABI signature.
- The compiled module therefore fully declares the set of host imports it needs; the embedding
  host must install implementations for these names at runtime.

Recommended workflow:

- Put commonly-used host surfaces in the **sysroot as normal Rusk modules** that contain `extern fn`
  declarations (e.g. `std`, `wasi`, `env`).
- Embedders decide which of these host imports to actually implement; missing implementations are
  detected as a runtime error (preflight or trap on call).

### 2) Extend `AbiType` (bytecode-level) to include `byte`/`char` and composites

Extend `rusk_bytecode::AbiType` beyond the current primitives to support:

- leaf primitives:
  - `Byte`
  - `Char`
- structural composites:
  - `Array(Box<AbiType>)`
  - `Tuple(Vec<AbiType>)`
- nominal composites (Rusk-defined):
  - `Struct(TypeId)`
  - `Enum(TypeId)`

Notes:

- `TypeId` refers to entries in the module’s `type_names` table (types defined in the program /
  sysroot).
- `Array(Box<AbiType>)` and `Tuple(Vec<AbiType>)` are *structural* (not interned as nominal types).
- This is a breaking change to the `.rbc` encoding of `AbiType` (and thus the bytecode ABI
  version); that is acceptable at the current stage of the project.
- `AbiType` becomes a recursive structure and will no longer be a tiny `Copy` enum.

### 3) Introduce `AbiValue` variants for composites as **opaque object references**

At runtime, composite values already exist as heap objects in the VM (`struct` / `tuple` / `enum`).
The key change is to allow these heap objects to be surfaced through the ABI boundary as **opaque
references**, instead of forcing them to be serialized to `bytes`.

Proposed additions to `rusk_vm::AbiValue`:

- `Byte(u8)`
- `Char(char)`
- `Array(AbiArrayRef)`
- `Tuple(AbiTupleRef)`
- `Struct(AbiStructRef)`
- `Enum(AbiEnumRef)`

Where each `Abi*Ref` is a safe, VM-validated handle to a heap object.

Sketch (illustrative only):

```rust
pub struct AbiTupleRef {
    pub handle: GcRef,
    pub readonly: bool,
    pub len: u32,
}

pub struct AbiArrayRef {
    pub handle: GcRef,
    pub readonly: bool,
    pub len: u32,
}

pub struct AbiStructRef {
    pub handle: GcRef,
    pub readonly: bool,
    pub type_id: TypeId,
}

pub struct AbiEnumRef {
    pub handle: GcRef,
    pub readonly: bool,
    pub type_id: TypeId,
}
```

Notes:

- These references are *opaque* to user code; the only supported operations are via host APIs
  described below.
- The handle is a `GcRef` (index + generation). Stale handles must be rejected by the VM.
- `readonly` is preserved so the host cannot mutate through a readonly view (consistency with
  `BYTECODE_SPEC.md` §2.4).
- `AbiValue::Byte` is an unsigned 8-bit value.
- `AbiValue::Char` represents a Unicode scalar value; the VM must reject invalid code points on
  host→VM boundaries (trap on mismatch).

### 4) Host API: a `HostContext` for inspecting/constructing composite ABI values

To avoid copying and to support cyclic graphs, the host must be able to **inspect** composite
values through the VM, rather than receiving fully materialized “deep” Rust structures.

Introduce a host-call context type, passed to host imports and effect handlers:

```rust
pub struct HostContext<'vm> { /* borrows VM heap + module */ }
```

And update the host function trait to receive it:

```rust
pub trait HostFn {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue])
        -> Result<AbiValue, HostError>;
}
```

`HostContext` exposes safe accessors for composite values:

- Array:
  - `cx.array_len(&AbiArrayRef) -> usize`
  - `cx.array_get(&AbiArrayRef, idx) -> Result<AbiValue, HostError>`
  - (optional v1) `cx.array_set(...)` only when `readonly == false`
- Tuple:
  - `cx.tuple_len(&AbiTupleRef) -> usize`
  - `cx.tuple_get(&AbiTupleRef, idx) -> Result<AbiValue, HostError>`
- Struct:
  - `cx.struct_type_name(&AbiStructRef) -> &str`
  - `cx.struct_get(&AbiStructRef, field: &str) -> Result<AbiValue, HostError>`
  - (optional v1) `cx.struct_set(...)` only when `readonly == false`
- Enum:
  - `cx.enum_type_name(&AbiEnumRef) -> &str`
  - `cx.enum_variant(&AbiEnumRef) -> (&str /*variant*/, usize /*arity*/)`
  - `cx.enum_get(&AbiEnumRef, idx) -> Result<AbiValue, HostError>`

Construction helpers (host → VM) should also be available:

- `cx.alloc_array(items: Vec<AbiValue>) -> Result<AbiValue, HostError>`
- `cx.alloc_tuple(items: Vec<AbiValue>) -> Result<AbiValue, HostError>`
- `cx.alloc_struct(type_name: &str, fields: Vec<(&str, AbiValue)>) -> Result<AbiValue, HostError>`
- `cx.alloc_enum(type_name: &str, variant: &str, fields: Vec<AbiValue>) -> Result<AbiValue, HostError>`

These constructors validate:

- the named type exists in the module,
- field/variant names are valid,
- arity matches,
- and field values match the ABI-eligibility rules below.

### 4.1) Handle validity / lifetime rules

Composite ABI values are references into the VM heap. They are:

- **VM-local**: handles are only meaningful within the `Vm` instance they came from.
- **GC-validated**: the VM must reject stale `GcRef` values (generation mismatch / freed slot).
- **Reachability-dependent**: if the VM garbage-collects an object and reuses its slot, an old
  handle must become invalid (and be rejected on use).

In v1 we do **not** guarantee “host-storable object handles” across arbitrary time; the intended
use is:

- inspect the object during the host call / external effect handling, or
- pass the handle back to the VM (round-trip) without deep copying.

If longer-lived handles are needed, add a dedicated **pinning table** (similar to
`ContinuationHandle`) as a follow-up.

### 4.2) Typed adapters (ergonomics)

This proposal is most valuable when host code can work with composite types ergonomically, e.g.:

```rust
fn log_point(cx: &mut HostContext<'_>, (p,): (Point,)) -> Result<(), HostError> { ... }
```

However, the VM’s `TypeId` values are **module-specific** (interned IDs), so “typed ABI type
matching” cannot rely on hard-coded `TypeId` constants.

Rusk is intended as an **embedded script language**, so the recommended approach is **runtime
resolution by name/path** (no extra compile-time tools):

- Provide a host-side type spec (`AbiTypeSpec`) that can name nominal types by path, e.g.
  `"Point"` / `"foo::Point"`.
- At host import installation time, resolve these names to the module’s concrete `TypeId` values
  (via `ExecutableModule::type_id(...)`) and validate that the module-declared host import
  signatures match what the host expects.

Typed host adapters can then be built on top of this resolution mechanism, but remain optional:
hosts that prefer minimal coupling can always operate directly on `(HostContext, AbiValue)` with
field/element accessors.

### 5) ABI eligibility rules (what composite types are allowed?)

To keep v1 implementable and predictable, not all Rusk types can be used at the boundary.

Define a recursive predicate **`abi_eligible(T)`** over Rusk types:

Allowed leaf types:

- `unit`, `bool`, `int`, `float`, `byte`, `char`, `string`, `bytes`, `cont(P)->R`

Allowed composite types:

- array types (`[T]`) where `T` is `abi_eligible`
- tuple types where every element type is `abi_eligible`
- struct types where every field type is `abi_eligible`
- enum types where every variant field type is `abi_eligible`

Explicitly *not* ABI-eligible in v1:

- `typerep`, function values, references, interfaces, and other VM-internal types

This yields a practical “data transfer object” subset suitable for host interop.

### 6) Runtime validation behavior

Host calls and external effects must validate that ABI values match declared signatures.

With composite types, validation becomes structural:

- `AbiType::Array(elem)` expects an `AbiValue::Array` (and enforces `elem` when elements are
  produced/consumed).
- `AbiType::Struct(type_id)` expects an `AbiValue::Struct` whose `type_id` matches.
- `AbiType::Enum(type_id)` expects an `AbiValue::Enum` whose `type_id` matches.
- `AbiType::Tuple([t0, t1, ...])` expects an `AbiValue::Tuple` whose `len` matches; each element
  is validated against `ti` when accessed/decoded (either eagerly at boundary or lazily by
  typed decoding).

Traps / host errors:

- If the VM is about to call a host import with an argument value that cannot be represented as an
  `AbiValue` under the declared signature (e.g. contains a non-ABI-eligible field), execution traps.
- If the host returns a value that does not match the declared return `AbiType`, execution traps.

This preserves the existing “mismatch traps” contract in `BYTECODE_SPEC.md` §3.2.

---

## Bytecode / Module Metadata Changes

### 1) `.rbc` encoding changes for `AbiType`

`AbiType` is no longer a single `u8` tag. It becomes a recursive tagged encoding, for example:

- `Unit` / `Bool` / `Int` / `Float` / `Byte` / `Char` / `String` / `Bytes` / `Continuation` remain
  simple tags
- `Struct` encodes `tag + type_id(u32)`
- `Enum` encodes `tag + type_id(u32)`
- `Array` encodes `tag + AbiType(elem)`
- `Tuple` encodes `tag + len + repeated AbiType`

The exact tags and wire format should be recorded in `BYTECODE_SPEC.md` (new ABI version).

### 2) Optional: include an “ABI schema” table for host-side reflection and validation

To support host-side reflection and robust runtime checks, the compiler may emit an ABI schema
table into `ExecutableModule`, keyed by `TypeId`, describing:

- struct field order + field ABI types
- enum variants + per-variant field ABI types

This schema is not strictly required for the VM to execute (it already carries field name layouts
for structs), but it is useful for:

- validating host expectations against the module at startup,
- enabling generic debugging/logging of ABI values on the host side,
- implementing `HostContext` helpers that can do richer checks (e.g. arity/field name validation)
  without re-deriving schema from other tables.

If adopted, the schema should be restricted to only the types that are reachable from ABI
signatures to keep module size bounded.

---

## Examples

### Example 1: Pass a `struct` to the host (no manual packing)

Rusk:

```rust
struct Point { x: int, y: int }

mod _demo_host {
    extern fn log_point(p: Point) -> unit;
}

fn main() -> unit {
    let p = Point { x: 1, y: 2 };
    _demo_host::log_point(p);
}
```

Host implementation (conceptual):

```rust
let id = module
    .host_import_id("_demo_host::log_point")
    .expect("compiled module must declare _demo_host::log_point");

vm.register_host_import(id, move |cx, args| {
    let AbiValue::Struct(p) = &args[0] else { /* trap */ };
    let x: i64 = cx.struct_get(p, "x")?.decode()?;
    let y: i64 = cx.struct_get(p, "y")?.decode()?;
    eprintln!("Point({x}, {y})");
    Ok(AbiValue::Unit)
})
```

### Example 2: Pass-through without cloning

Rusk:

```rust
struct Blob { data: bytes }

mod _demo_host {
    extern fn identity_blob(b: Blob) -> Blob;
}

fn main() -> unit {
    let b = Blob { data: b"hi" };
    let b2 = _demo_host::identity_blob(b);
    // `b2` is the same object graph (subject to readonly rules).
}
```

Host:

```rust
// returns the same ABI value handle; no bytes serialization needed
fn identity_blob(_cx: &mut HostContext<'_>, (b,): (AbiValue,)) -> Result<AbiValue, HostError> {
    Ok(b)
}
```

### Example 3: Arrays of ABI-eligible composites

Rusk:

```rust
struct Point { x: int, y: int }

mod _demo_host {
    extern fn centroid(points: [Point]) -> Point;
}

fn main() -> unit {
    let ps = [Point { x: 0, y: 0 }, Point { x: 2, y: 0 }];
    let c = _demo_host::centroid(ps);
    // ...
}
```

Host implementation (conceptual):

```rust
vm.register_host_import(id, move |cx, args| {
    let AbiValue::Array(points) = &args[0] else { /* trap */ };
    let n = cx.array_len(points);
    // Iterate via `cx.array_get(points, i)` without copying the whole array.
    // Build the return `Point` via `cx.alloc_struct("Point", ...)`.
    Ok(cx.alloc_struct("Point", vec![("x", 1.into()), ("y", 0.into())])?)
})
```

---

## Alternatives Considered

1) **Keep ABI primitives-only and recommend serialization to `bytes`**
   - simplest, but keeps the current pain point and performance overhead

2) **Flatten composites into multiple ABI parameters**
   - avoids introducing composite ABI value kinds, but is brittle, verbose, and requires manual
     packing/unpacking whenever the shape changes

3) **By-value deep encoding of composite graphs**
   - works for trees, but becomes complex for cyclic object graphs and tends to recreate a
     serialization format inside the ABI

---

## Open Questions / Future Work

- **Mutability**: should the host be able to mutate composite values through the ABI?
  - v1 suggestion: preserve `readonly` and allow mutation only when not readonly, via explicit
    `HostContext` methods.
- **Pinned object handles**: do we need a “host-storable” handle table for composite objects
  (analogous to `ContinuationHandle`) to keep objects alive across multiple host calls?
- **Generic nominal types**: allow `Struct(TypeId, type_args: Vec<AbiType>)` once ABI schema can
  represent generic field types and/or runtime validation is defined.
- **Versioning**: optionally attach a stable hash to each ABI-exposed nominal type definition so a
  host can verify it is interacting with the expected schema.

---

## Implementation Plan (Incremental)

1. Add `extern fn` item syntax + semantics, and remove the compiler-side host module declaration
   injection path (`HostModuleDecl`, `CompileOptions::register_host_module`, etc.).
2. Extend MIR host import metadata so declared extern functions carry ABI signatures that include
   `byte`/`char` and composite shapes.
3. Extend `rusk_bytecode::AbiType` and `.rbc` encoding to represent `byte`/`char`/array/tuple/struct/enum
   in signatures.
4. Extend `rusk_vm::AbiValue` with `Byte`/`Char` and composite reference variants (`Array`/`Tuple`/`Struct`/`Enum`),
   and add `HostContext` for safe inspection and construction.
5. Update VM host import and external effect boundary checks to understand composite ABI values
   (including readonly enforcement for mutation).
6. Add integration tests under `tests/` that:
   - pass/return `byte` and `char`,
   - pass/return arrays of ABI-eligible elements,
   - pass/return struct/enum/tuple values,
   - round-trip values across `StepResult::Request` / `vm_resume`.
