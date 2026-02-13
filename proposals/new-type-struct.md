# New-Type Structs Proposal

## Summary

Add **new-type structs** (a restricted subset of tuple structs) to Rusk:

- Declaration: `struct Name(T);`
- Construction: `Name(v)` (supports turbofish: `Name::<T>(v)`)
- Field access by index: `x.0`
- Destructuring pattern: `Name(p)` (also supports qualified paths: `mod::Name(p)`)

New-type structs are **nominal** wrapper types around a single field and **do not** introduce any
implicit conversions to/from their underlying field type.

This proposal intentionally does **not** include multi-field tuple structs. (See the existing
`proposals/tuple-struct.md` for a larger design.)

---

## Motivation

Rusk already supports:

- primitive types (`int`, `string`, ...)
- structural tuples (`(T0, T1, ...)`)
- named-field nominal structs (`struct Point { x: int, y: int }`)

What’s missing is a lightweight way to introduce **domain-specific nominal types** without inventing
fake field names:

```rust
struct UserId(int);
struct FileFd(int);
struct Meters(int);
```

These wrappers are useful for:

- making APIs self-documenting
- preventing accidental mixing of semantically distinct values that share the same representation
- enabling multiple effect “slots” of the same underlying type via distinct type identities (e.g.
  `State<UserId>` vs `State<FileFd>`)

---

## Goals

- Provide a minimal, easy-to-implement nominal wrapper form for a **single** field.
- Reuse existing expression/pattern surface syntax where possible:
  - call syntax for construction (`Name(v)`)
  - tuple-index field access (`.0`)
  - constructor-like pattern (`Name(p)`)
- Keep the type system predictable:
  - **no implicit coercions** between `Name` and `T`
  - explicit access via `.0` or pattern destructuring
- Preserve runtime type identity for `is` / `as?` and effect identity (new-type structs are normal
  structs in the nominal type system).

---

## Non-goals

- Multi-field tuple structs (`struct Pair(A, B);`) — deferred to `proposals/tuple-struct.md`.
- Unit-like structs with `struct Foo;` syntax.
- Any implicit conversions (“auto packing/unpacking”).

---

## Proposed design

### 1) Syntax: struct item form

Extend the `struct` item grammar to allow either:

- the existing named-field form, or
- a **new-type** form with a single field type and a trailing semicolon.

```ebnf
StructItem     := "struct" Ident GenericParams? ( NamedStructBody | NewTypeStructBody ) ;
NamedStructBody:= "{" FieldList? "}" ;
NewTypeStructBody := "(" Type ")" ";" ;
```

Examples:

```rust
struct UserId(int);
struct Box<T>(T);
```

### 2) Construction: call-like syntax

New-type structs construct with call syntax:

```rust
let id = UserId(42);
let b = Box(123);
let b2 = Box::<int>(123);
```

This is only valid for new-type structs; named-field structs continue to use struct literals:

```rust
struct Point { x: int, y: int }
let p = Point { x: 1, y: 2 };
```

### 3) Field access: `.0`

The single field of a new-type struct is accessed using the existing indexed field syntax:

```rust
let id = UserId(42);
let n: int = id.0;
```

Assignment is allowed on non-`readonly` values:

```rust
let id = UserId(1);
id.0 = 2;
```

### 4) Patterns: `Name(p)`

New-type structs can be destructured in patterns using constructor-like syntax:

```rust
match UserId(42) {
    UserId(n) => n
}
```

Qualified paths are allowed:

```rust
mod m { struct UserId(int); }

match m::UserId(1) {
    m::UserId(n) => n
}
```

### 5) Type system behavior

New-type structs are **nominal** types:

- `UserId` is distinct from `int`, even though it has one `int` field.
- there are no implicit conversions between `UserId` and `int`.
- generic instantiations keep the nominal type:
  - `Box<int>` is distinct from `int`.

Runtime type operations keep nominal identity:

- `UserId(1) is UserId` is `true`.
- `UserId(1) is int` is rejected because `int` is not a nominal `is` target.

---

## Implementation sketch

- Parser:
  - accept `struct Name(Type);` as a new `struct` body form.
  - accept `Path("(" ... ")")` patterns as a “constructor pattern” and resolve in the typechecker
    as either enum-variant patterns or new-type struct patterns.
- Typechecker:
  - treat `Name(expr)` calls as new-type construction when `Name` resolves to a new-type struct.
  - `.0` field access and assignment on a new-type struct resolves to its single field type.
- Lowering/MIR:
  - lower `Name(expr)` directly to `MakeStruct` with one field named `".0"`.

---

## Test plan

Add fixtures and integration tests covering:

- construction + `.0` access + assignment
- generic inference and turbofish on construction
- pattern destructuring (qualified and unqualified)
- “no implicit conversion” type errors
- error messages for using call syntax on named-field structs

