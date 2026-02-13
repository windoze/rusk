# Tuple Structs Proposal

## Summary

Add **tuple structs** (Rust-style) to Rusk:

- Declaration: `struct Name(T0, T1, ...);`
- Construction: `Name(v0, v1, ...)`
- Destructuring pattern: `Name(p0, p1, ...)`
- Field access and assignment by index: `x.0`, `x.1`, ...

Tuple structs are **nominal** types (unlike tuple types), but their fields are **positional** (unlike current
named-field structs).

---

## Motivation

Rusk already has:

- structural tuples: `(T0, T1, ...)` and `(v0, v1, ...)`
- nominal named-field structs: `struct Point { x: int, y: int }`
- nominal enums with tuple variants: `enum Option<T> { Some(T), None }`

What’s missing is the “in-between” form: a **nominal wrapper** with **positional fields**.

Tuple structs unlock a few common patterns without forcing “fake names” onto fields:

1. **Newtype wrappers** (domain types)
   - `struct UserId(int);`
   - makes it impossible to accidentally pass a raw `int` where an ID is expected
2. **Opaque but lightweight nominal aggregates**
   - `struct Rgb(int, int, int);`
   - fields are meaningful by position and typically not referenced by name
3. **Better ergonomics for generic wrappers**
   - `struct Tagged<T>(T, string);`
   - convenient for helpers like `map`, `zip`, etc.

This feature matches the project’s “syntax close to Rust” goal while remaining implementable with the current
compiler/MIR architecture.

---

## Goals

- Add a first-class tuple-struct form that is:
  - easy to parse (minimal grammar impact)
  - easy to lower (no new MIR primitive required)
  - consistent with existing `enum` tuple variants and tuple field access (`.0`)
- Preserve Rusk’s left-to-right evaluation guarantees.
- Keep typechecking rules predictable, especially around `readonly` views.

---

## Non-goals (for this proposal)

- “Unit-like structs” with semicolon syntax (`struct Foo;`). Rusk already supports an empty named struct:
  `struct Foo {}`. Adding a semicolon form can be a follow-up.
- Adding named arguments / record update syntax for structs.
- Adding pattern features beyond what tuples already support (e.g. multiple `..`).

---

## Proposed design

### 1) Syntax: tuple struct declarations

Extend the `struct` item grammar to allow either a named-field body (existing) or a tuple-field body (new).

**Current (v0.4)**:

```ebnf
StructItem     := "struct" Ident GenericParams? "{" FieldList? "}" ;
Field          := Ident ":" Type ;
```

**Proposed**:

```ebnf
StructItem     := "struct" Ident GenericParams? ( NamedStructBody | TupleStructBody ) ;
NamedStructBody:= "{" FieldList? "}" ;
TupleStructBody:= "(" TypeList? ")" ";" ;
```

Notes:

- The trailing `;` matches Rust and avoids “dangling” item endings.
- `TypeList` reuses the existing tuple/enum-variant list form and allows a trailing comma.
- `struct Name();` (zero-field tuple struct) is allowed but not particularly useful; it is included for
  completeness and uniformity.

Examples:

```rust
struct UserId(int);
struct Point2(int, int);
struct Pair<A, B>(A, B);
```

### 2) Construction: call-like syntax

Tuple structs construct with call syntax:

```rust
let id = UserId(42);
let p = Point2(10, 20);
let pair = Pair(1, true);
```

#### Type arguments

Because construction uses the existing call syntax, tuple-struct construction can use the existing turbofish
when needed:

```rust
let p = Pair::<int, bool>(1, true);
```

#### Name resolution and ambiguity

Tuple-struct construction uses the same surface syntax as a normal call, so the compiler must resolve whether
`Name(...)` means:

- a function call, or
- tuple struct construction.

To keep this predictable and Rust-like:

1. Declaring a tuple struct `Name` also declares a **constructor value** `Name` in the value namespace with
   signature `fn(T0, T1, ...) -> Name`.
2. It is a compile-time error to declare another value item with the same fully-qualified name as the tuple
   struct constructor (same rule-of-thumb as “enum variant names behave like associated constructors”).

This keeps calls unambiguous: `Name(...)` always refers to the constructor once the tuple struct exists, unless
shadowed by a local binding (as with any other value).

### 3) Field access: `.0`, `.1`, ...

Tuple struct fields are accessed using the existing indexed-field syntax:

```rust
let p = Point2(10, 20);
let x = p.0;
let y = p.1;
```

Tuple struct fields participate in assignment the same way tuple fields do today:

```rust
let p = Point2(10, 20);
p.0 = 11;
```

#### Bounds and errors

- Accessing an out-of-bounds index is a type error when the tuple struct type is known.
- As with tuple field access on inference variables, the compiler may infer a minimal arity where possible, but
  for tuple structs arity is always known from the struct definition.

#### `readonly` view behavior

Accessing a field through a `readonly` view returns a `readonly` view of the field when the field type is
reference-like (arrays/structs/enums/tuples), matching existing behavior for named structs and tuples.

### 4) Patterns: tuple struct destructuring

Add a new pattern form for tuple structs:

```rust
match p {
    Point2(x, y) => { ... }
}
```

#### Proposed grammar shape (source level)

Today, `Ident "(" ... ")"` is reserved for enum patterns and requires at least `Enum::Variant(...)`.
To support both enum variants and tuple structs (including qualified paths like `foo::Point2(…)`), treat this
as a general “constructor pattern” and resolve it in the typechecker:

```ebnf
CtorPat        := Path "(" (Pattern ("," Pattern)*)? (",")? ")" ;

Pattern        := "_"
               | Ident
               | LiteralPat
               | TuplePat
               | CtorPat
               | StructPat
               | ArrayPat ;
```

Typechecking rule:

- If `Path` resolves to an enum variant constructor, the pattern is an enum-variant pattern.
- Else if `Path` resolves to a tuple struct type, the pattern is a tuple-struct pattern.
- Otherwise: type error (“expected enum variant or tuple struct constructor”).

This mirrors Rust’s “tuple struct patterns and tuple variants share the same surface syntax”.

#### Rest patterns (`..`) inside tuple struct patterns

Tuple patterns support a single `..` rest marker. Tuple struct patterns can reuse the same rule:

```rust
struct Quad(int, int, int, int);

match Quad(1, 2, 3, 4) {
    Quad(a, ..mid, d) => { /* mid is (2, 3) */ }
}
```

Even though tuple structs have fixed arity, rest patterns are still useful for “prefix/suffix” destructuring
without naming intermediate fields.

---

## Type system impact

Tuple structs introduce a second struct “shape”:

- named-field struct: `struct S { x: T, y: U }`
- tuple-field struct: `struct S(T, U);`

Both are nominal types:

- `S` is distinct from `(T, U)` even if the tuple struct fields are `(T, U)`.
- runtime checks `is S` / `as? S` work the same way as existing structs (nominal identity, including type args).

### Method and interface behavior

Tuple structs support `impl` blocks and interface impls exactly like named structs:

```rust
impl UserId {
    fn as_int() -> int { self.0 }
}
```

No changes are required to receiver rules (`self`, `readonly fn`, etc.).

---

## MIR lowering (recommended approach)

No new MIR instruction is required.

Lower tuple struct construction to `make_struct` with synthetic field names:

- field `0` becomes `".0"`
- field `1` becomes `".1"`
- and so on

This reuses existing MIR capabilities:

- `make_struct <StructName> { field: <op>, ... }`
- `get_field` / `set_field` (already supports `".<n>"` format for tuples; using the same key format for tuple
  structs keeps the lowering uniform)

Tuple struct patterns lower to struct destructuring patterns over the same synthetic field keys.

This approach also preserves nominal type identity without requiring “nominal tuples” in MIR/runtime.

---

## Backwards compatibility

This is an additive feature:

- `struct Name(T, U);` is currently a parse error, so accepting it does not change meaning of existing valid code.
- `Name(v0, v1)` currently only typechecks if `Name` resolves to a function value; after this proposal it may also
  typecheck as tuple struct construction.

The only potential source incompatibility is **name collisions** if we adopt the “constructor occupies value
namespace” rule:

- code that previously defined both a type `Name` and a function `Name` would still compile today
  (different namespaces)
- after adding a tuple struct `Name`, defining a function `Name` in the same module should become a compile-time
  error

This is consistent with Rust’s model and prevents hard-to-explain call ambiguity.

---

## Drawbacks

- Adds another struct form to learn and document.
- Requires parser/typechecker changes to treat `Path(...)` in patterns as resolution-based rather than syntactically
  “always enum”.
- Requires updating field-access rules so `.0` can apply to tuple structs as well as tuples.

---

## Alternatives considered

1. **Use named structs for everything**
   - Works, but forces artificial names (`_0`, `first`, `value`, …) and makes newtype wrappers noisier.
2. **Represent tuple structs as tuple values at runtime**
   - Would require MIR/runtime support for nominal identity on tuples (or a new “nominal tuple” object kind).
   - More invasive than necessary.
3. **Introduce a distinct literal syntax**
   - e.g. `Name#(v0, v1)` or `Name@(v0, v1)` to avoid call ambiguity
   - avoids value-namespace concerns but is less Rust-like and adds new punctuation

---

## Implementation plan (compiler)

1. **Parser**
   - Extend `struct` item parsing to accept the tuple-body + `;` form.
   - Extend pattern parsing to accept `Path(...)` as a constructor pattern.
2. **AST / HIR**
   - Represent struct defs as either named-field or tuple-field.
   - Represent constructor patterns distinctly (or as a path + args node that typechecking resolves).
3. **Name resolution**
   - Decide and enforce the collision rule for tuple struct constructor names.
4. **Typechecker**
   - Add tuple struct construction in call checking.
   - Extend field access and assignment to allow `.N` on tuple structs (bounds checking, generic substitution,
     `readonly` behavior).
   - Add tuple struct pattern checking and lowering.
5. **Lowering to MIR**
   - Lower tuple struct construction to `make_struct` with `".N"` field keys.
   - Lower tuple struct patterns to struct destructuring with the same keys.
6. **Docs**
   - Update `RUSK_SPEC.md` (grammar + semantics).
   - If needed, clarify in `MIR_SPEC.md` that struct field keys may include `".<n>"`.

---

## Test plan

Add tests in `tests/` and/or `crates/rusk-compiler` covering:

- parsing: `struct Name(T);`, `struct Name(T, U);`
- construction: `Name(1)`, `Name(1, 2)`, and generic `Name::<T>(...)`
- field access: `x.0`, `x.1`, out-of-bounds diagnostics
- assignment: `x.0 = ...` and readonly rejection
- patterns:
  - `match x { Name(a, b) => ... }`
  - rest pattern behavior if included (`Name(a, ..mid, c)`)
- name collisions: rejecting `fn Name(...)` when `struct Name(...)` exists (if adopted)
