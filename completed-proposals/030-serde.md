# Serde for Rusk: compile-time derived serialization / deserialization

Date: 2026-02-21

Status: **Draft**

This proposal is about a **mechanism** (how user types participate in serialization), not an
on-the-wire format. Formats (binary/text, schema tagging, versioning policy, canonicalization,
etc.) can be layered on top once the mechanism exists.

---

## Summary

Introduce a **zero-reflection, compile-time derived** serialization system for Rusk that is:

- **Interface based**: `Serialize` / `Deserialize` live in `core::serde`.
- **Compiler-synthesized** by opt-in `derive` declarations (Rust-like ergonomics, but built-in).
- **Zero-cost at runtime** in the sense that no runtime type metadata or reflection is required; the
  compiler generates normal Rusk code that walks the type structure.
- **Format-agnostic**: implementations target a small serializer/deserializer *data model* API,
  allowing different formats to exist as libraries/hosts.

Key user experience:

```rusk
use core::serde::{Serialize, Deserialize};

struct Point { x: int, y: int }

derive Serialize + Deserialize for Point;
```

The compiler expands the `derive` request into normal `impl` blocks.

---

## Motivation

Rusk programs and libraries need a principled way to convert values to/from an external
representation for:

- persistence (state files, caches)
- IPC/RPC (message passing, host interop)
- debugging / tooling (structured dumps)
- tests (golden files)

A runtime-reflection approach (Java style) is undesirable for Rusk’s goals:

- it adds runtime footprint and complexity to the VM
- it weakens encapsulation and expands the security surface
- it makes performance and determinism harder to reason about

Instead, Rusk should follow a Rust/Swift-like model: **interfaces + compiler synthesis**.

---

## Design goals

1. **No runtime reflection**: serialization must not depend on runtime field lists, names, or type
   metadata.
2. **Opt-in**: a type is serializable only if the user (or stdlib) opts in explicitly.
3. **Composable**: user-defined structs/enums compose from serializable fields/variants.
4. **Predictable dispatch**: static calls for concrete types; dynamic dispatch only where Rusk
   already requires it (e.g., interface-constrained generics in the erased generics model).
5. **Extensible backends**: multiple formats can be implemented without changing the compiler.
6. **Clear failure behavior**: deserialization is fallible and should return a typed error rather
   than trapping by default.

---

## Non-goals (initial scope)

- Selecting or standardizing a particular encoding format.
- Stable schema evolution / versioning policy (field tags, rename rules, migrations).
- VM snapshotting / heap graph serialization (object identity, cycles, deduplication).
- A general procedural macro system (user-defined compiler plugins).
- Making “everything serializable by default”, including functions, continuations, host handles, etc.

---

## Core mechanism

### 1) Standard library interfaces

Add a `core::serde` module in the sysroot that defines (names are placeholders):

- `interface Serializer { ... }`
- `interface Deserializer { ... }`
- `interface Serialize { readonly fn serialize(s: Serializer) -> Result<unit, SerdeError>; }`
- `interface Deserialize { static fn deserialize(d: Deserializer) -> Result<Self, SerdeError>; }`
- `enum SerdeError { ... }` (or a struct; exact shape is a library choice)

Notes:

- `Serialize::serialize` is `readonly` by default (serialization should not mutate the value).
- `Deserialize::deserialize` is an **associated function** (no receiver).

This is intentionally *format-agnostic*: `Serializer` / `Deserializer` define a minimal event-based
API over a language-level data model (primitives, sequences, maps, struct fields, enum variants).
The exact API surface can be iterated later; the key point is the **shape**:

- user types implement `Serialize` / `Deserialize`
- formats implement `Serializer` / `Deserializer`

### 2) Allow `static fn` in interfaces (small language extension)

To support Rust-like “associated functions” (required for `Deserialize`, and also broadly useful
for patterns like `Default::default()`), extend the language so an interface may declare:

```rusk
interface Deserialize {
    static fn deserialize(d: Deserializer) -> Result<Self, SerdeError>;
}
```

And an impl may provide the corresponding `static fn` body:

```rusk
impl Deserialize for Point {
    static fn deserialize(d: Deserializer) -> Result<Point, SerdeError> { ... }
}
```

Dispatch / call syntax for static interface functions:

- Primary (fully explicit): `I::f::<T>(args...)`
  - Example: `Deserialize::deserialize::<Point>(d)`
- `Type::f(...)` remains reserved for **inherent** static methods only (to avoid ambiguity).

Effect namespace interaction:

- Only **instance** interface methods participate in effect operation naming (`@I.op(...)`).
- `static fn` interface members are **not** effect operations.

### 3) `derive` as compiler-supported opt-in codegen

Add a new item form:

```rusk
derive Serialize + Deserialize for Point;
```

Semantics:

- `derive ... for TypeName;` is a pure compile-time directive.
- The compiler expands it into one or more normal `impl` blocks.
- In the initial stage, only derives for `core::serde::Serialize` and `core::serde::Deserialize`
  are supported (hard-coded in the compiler).
  - Unknown derives are a compile error.
  - Future: allow library-defined derives when/if a macro system exists.

Placement:

- The `derive` item must appear in the same module as the target type definition (at least in the
  initial stage), so expansion can reliably access type structure and avoid cross-module
  compilation-order issues.

---

## What the compiler generates (informal)

The derive expansion is conceptually “as if the user wrote” the impls below.

### Structs

For a named-field struct:

```rusk
struct Point { x: int, y: int }
derive Serialize + Deserialize for Point;
```

Expand to:

- `impl Serialize for Point { ... }` that serializes fields in declaration order.
- `impl Deserialize for Point { ... }` that reads fields and constructs `Point { x: ..., y: ... }`.

For a newtype struct:

```rusk
struct UserId(int);
derive Serialize + Deserialize for UserId;
```

Expand to serialize/deserialize the wrapped `.0` field.

### Enums

For:

```rusk
enum Shape {
    Circle(int),
    Rect(int, int),
    Unit,
}

derive Serialize + Deserialize for Shape;
```

Expand to:

- `Serialize`: a `match self { ... }` that emits a variant tag + variant payload.
- `Deserialize`: read a tag, then read payload and construct the corresponding variant.

### Generics (erased type args model)

For:

```rusk
struct Box<T> { value: T }
derive Serialize + Deserialize for Box;
```

Expand to:

```rusk
impl<T: Serialize> Serialize for Box<T> { ... }
impl<T: Deserialize> Deserialize for Box<T> { ... }
```

Where the bounds are **inferred** by the derive algorithm:

- Each generic parameter that appears in a serializable position in the type’s fields/variants
  gets a `: Serialize` (or `: Deserialize`) bound.
- Bounds are not added for parameters that do not appear (or only appear under non-derived
  positions, if such exist later via adapters).

This matches Rusk’s current “no specialization by concrete type args” restriction: one impl per
nominal type name, valid for all instantiations.

---

## Customization model (v0 minimal, extensible later)

Initial stage (v0) deliberately keeps customization minimal:

- Derive uses the type’s declared structure (field list, variant list).
- If a type needs custom behavior, users write a manual `impl Serialize for T` / `impl Deserialize
  for T`.

Future directions (not required for v0):

1. **Adapters (“with”)**:
   - Allow a field to opt into a custom codec type, e.g. `with = core::serde::hex`.
2. **Skipping/defaulting**:
   - `skip`: omit a field from serialization; require a default at deserialization.
3. **Renaming**:
   - rename fields/variants for text formats and/or schema-stable tags.
4. **General attribute syntax**:
   - If Rusk later adds a general-purpose attribute system, serde customization should reuse it.

This proposal intentionally does not lock in a particular attribute syntax.

---

## Safety / robustness notes (mechanism-level)

Even without committing to a format, the mechanism should support safe decoders:

- `Deserializer` APIs should be able to report errors (`Result<_, SerdeError>`), not trap.
- Libraries/hosts should be able to enforce budgets (max length, max depth, max total elements),
  but those limits live in the specific `Deserializer` implementation, not in derived code.

This keeps the compiler’s role small: it generates structural traversal code only.

---

## Implementation strategy (phased)

1. **Stdlib surface (core-only)**
   - Introduce `core::serde` with the minimal interfaces and `SerdeError`.
   - Provide manual impls for primitives + core container types (`Option`, `Result`, arrays, etc.).

2. **Language support**
   - Add `static fn` interface members and matching impl support.
   - Add `derive ... for Type;` parsing + AST representation.

3. **Compiler derive expansion**
   - Implement derive expansion for `Serialize` and `Deserialize` only.
   - Generate explicit impls (no runtime metadata).
   - Add compile errors for:
     - missing field/variant support
     - unsupported types (functions, continuations, host-only handles)
     - recursive cycles are allowed structurally (they compile), but may still be runtime-infinite if
       the value graph is cyclic; graph handling is out of scope.

4. **Tests**
   - Add compiler tests that assert derive expansion works for:
     - structs, newtypes, enums
     - generics (bounds inference)
     - nested/recursive types
     - error paths (unknown derive, unsupported field types)

---

## Open questions

1. **Call syntax for static interface functions**: is `I::f::<T>(...)` acceptable, or should Rusk
   also allow `T::f(...)` to resolve to trait static members when unambiguous?

   We should accept `T::f(...)` for ergonomics, but only when it is unambiguous.

2. **Where does `derive` live long-term**: should it become a general attribute system
   (`#[derive(...)]`) once Rusk has attributes, or remain a standalone item?

   We should make it a general attribute system in the long term, so make sure the short-term implementation can evolve into that without breaking changes.

3. **Serialization of interface-typed values**: do we want a separate mechanism (`DynSerialize`)
   that includes dynamic type tags, or is it intentionally out of scope?

   The `DynSerialize` can be added later as a separate interface, but it is out of scope for v0.

4. **Error propagation style**: should deserialization prefer returning `Result` values, or throwing
   via `core::result::Throw<E>` effects (with `try { ... }.catch(...)`), or support both?

   Let return Results, keep effect out of the initial design for simplicity.

5. **`Option` vs JSON `null` / field omission**: should `Option<T>` map to JSON `null` (and/or
   “missing field”), or should it always use the structural enum encoding?

   Keep `Option<T>` as a regular enum in the format-agnostic `core::serde` model (tag + payload),
   so other formats don’t inherit JSON-specific semantics.

   JSON-specific needs are addressed in `std::json` via wrappers:

   - `std::json::NullOr<T>` for explicit JSON `null`
   - `std::json::OmitOr<T>` for missing object fields
   - Combine as `std::json::OmitOr<std::json::NullOr<T>>` to distinguish missing / null / value.
