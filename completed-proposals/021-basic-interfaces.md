# Basic Interfaces: `core::ops`, `core::iter::Iterator`, `core::fmt::ToString`
#
# Date: 2026-02-17
# Status: Draft
#
# This proposal is intentionally “library-surface heavy”: it defines a small set of
# compiler-recognized interfaces that language features desugar to.

## Summary

Add a minimal set of **standard interfaces** that bind directly to existing / planned language
features:

1. **Operator interfaces** under `core::ops` (`Add`, `Sub`, `Mul`, `Div`, `Rem`, `Neg`, `Not`, `Lt`,
   `Gt`, `Le`, `Ge`, `Eq`, `Ne`).
2. An **`Iterator` interface** under `core::iter`, used by `for` loops and other iteration
   constructs.
3. A **`ToString` interface** under `core::fmt`, used by formatted strings (`f"..."`).

Key rule: **primitives still lower to VM intrinsics / bytecode instructions** (fast paths), but
non-primitive operator / iteration / formatting constructs become ordinary (statically resolved)
interface calls.

This turns several “hard-coded” language behaviors into a small, explicit “lang item” surface area,
making user-defined types more expressive while keeping performance and simplicity for built-ins.

---

## Motivation

Today (v0.4 behavior as implemented in the compiler/VM):

- Operators (`+ - * / %`, comparisons, `!`, unary `-`) are effectively **primitive-only**.
- `for` loops are effectively **array-only** (desugaring uses `core::intrinsics::into_iter` +
  `core::intrinsics::next`).
- `f"..."` interpolation uses a monolithic `core::intrinsics::to_string`, which makes formatting:
  - hard to customize per type
  - hard to evolve (format specifiers, non-allocating formatting, etc.)

As a result, user-defined types can define methods, but they cannot participate in core syntactic
features in a principled way.

This proposal introduces **three small interfaces** that the compiler treats specially when
lowering syntax sugar, aligning Rusk with the general “interfaces drive abstraction” direction.

---

## Design goals

1. **Bind syntax to library**: operators / `for` / `f"..."` lower to ordinary calls.
2. **Fast primitives**: primitives still use direct VM ops / intrinsics (no dynamic dispatch).
3. **Minimal surface**: keep it small; avoid Rust’s full `std::ops` / `fmt` complexity for now.
4. **Do not pollute global namespace**: these names should not be auto-imported into every module.
5. **Predictable readonly behavior**: pure operations should be callable on `readonly T`.

---

## Non-goals (initial scope)

- Cross-type operators (`int + float`, `Vec<T> + Slice<T>`, etc.).
- Assignment operators (`+=`, `*=`...) and user-defined short-circuiting (`&&`, `||`).
- A full formatting system (`Display`, `Formatter`, format specifiers, width/precision/alignment).
- Rust-like `IntoIterator` blanket impls (Rusk currently has no “impl for all implementers”).

---

## Proposal

### 1) `core::ops`: operator interfaces

Introduce the following interfaces in a new built-in module `core::ops`.

All operator methods are declared as `readonly fn` so they are callable on both `T` and
`readonly T` receivers (operators should not require mutation).

```rusk
// In `core::ops`:
interface Add { readonly fn add(other: Self) -> Self; }
interface Sub { readonly fn sub(other: Self) -> Self; }
interface Mul { readonly fn mul(other: Self) -> Self; }
interface Div { readonly fn div(other: Self) -> Self; }
interface Rem { readonly fn rem(other: Self) -> Self; }

interface Neg { readonly fn neg() -> Self; }
interface Not { readonly fn not() -> Self; }

interface Lt { readonly fn lt(other: Self) -> bool; }
interface Gt { readonly fn gt(other: Self) -> bool; }
interface Le { readonly fn le(other: Self) -> bool; }
interface Ge { readonly fn ge(other: Self) -> bool; }

interface Eq { readonly fn eq(other: Self) -> bool; }
interface Ne { readonly fn ne(other: Self) -> bool; }
```

#### 1.1 Operator lowering rules

For primitive types (`int`, `float`, `bool`, `string`, `bytes`, `unit`):

- Keep the existing lowering to:
  - dedicated VM instructions (e.g. `IntAdd`, `IntEq`, `BoolNot`), and/or
  - `core::intrinsics::*` host calls (e.g. `float_add`, `string_eq`, ...).

For non-primitive types:

- Lower operators to **explicit interface calls** (not method-call sugar) using fully-qualified
  names:

| Syntax | Lowers to |
|---|---|
| `a + b` | `core::ops::Add::add(a, b)` |
| `a - b` | `core::ops::Sub::sub(a, b)` |
| `a * b` | `core::ops::Mul::mul(a, b)` |
| `a / b` | `core::ops::Div::div(a, b)` |
| `a % b` | `core::ops::Rem::rem(a, b)` |
| `-x` | `core::ops::Neg::neg(x)` |
| `!x` | `core::ops::Not::not(x)` |
| `a < b` | `core::ops::Lt::lt(a, b)` |
| `a > b` | `core::ops::Gt::gt(a, b)` |
| `a <= b` | `core::ops::Le::le(a, b)` |
| `a >= b` | `core::ops::Ge::ge(a, b)` |
| `a == b` | `core::ops::Eq::eq(a, b)` |
| `a != b` | `core::ops::Ne::ne(a, b)` |

#### 1.2 Typechecking rules (non-primitive path)

For the initial stage, keep it deliberately simple and predictable:

- Binary operators require `left` and `right` to have the **same type** `T`.
- The operator is valid iff `T` has an `impl` for the corresponding `core::ops::*` interface.
- Result types:
  - arithmetic: `T`
  - unary `-` / `!`: `T`
  - comparisons / equality: `bool`

If no suitable impl exists, report an error such as:

> `operator '+' requires an impl of core::ops::Add for T`

#### 1.3 Notes on dynamic dispatch and generics

Many of these operator methods mention `Self` in argument/return position. Under the current
“Self-only” rule (§6.1 in `RUSK_SPEC.md`), such methods are **not dynamically dispatchable**.

This proposal does not attempt to change object-safety rules. Concretely:

- `a + b` for a receiver typed as an interface value (or as an interface-constrained generic)
  may remain unsupported initially.
- Operator overloading is still useful for concrete nominal types (the common case), and for code
  that can be monomorphized in the future.

If/when we want `T: Add` generic code to work without monomorphization, we can revisit this with a
dedicated “operator dictionaries” mechanism or a relaxation of Self-only constraints.

---

### 2) `core::iter::Iterator`: the iteration protocol

Standardize the `Iterator` interface as a core library surface item:

```rusk
// In `core::iter`:
interface Iterator {
  type Item;
  fn next() -> Option<Self::Item>;
}
```

Notes:

- `next` is **not** `readonly`: advancing an iterator is stateful and must be allowed to mutate the
  iterator value.
- Because the return type mentions only `Self::Item` (an associated type), it remains eligible for
  dynamic dispatch under the current rules.

#### 2.1 `for` desugaring (new)

Update the definitional desugaring of `for` to be expressed in terms of `core::iter::Iterator`.

The loop should:

1. evaluate the iterable expression once
2. obtain an iterator value `it`
3. repeatedly call `Iterator::next(it)`

Proposed desugaring shape:

```rusk
let __iterable = iter_expr;
let __it = /* see 2.2: “obtaining an iterator” */;
loop {
  match core::iter::Iterator::next(__it) {
    Option::Some(x) => { body; }
    Option::None => break;
  }
}
```

#### 2.2 Obtaining an iterator (no `IntoIterator` layer for now)

Because Rusk does not currently support blanket impls such as “`impl IntoIterator for all I:
Iterator`”, this proposal keeps the “into-iter” step as a **compiler-recognized intrinsic** for a
small set of built-in iterable types.

The compiler determines `__it` as follows:

1. If `iter_expr` has a type that implements `core::iter::Iterator`, use it directly:
   - `let __it = __iterable;`
2. Else if `iter_expr` is one of the built-in iterable container types:
   - arrays: `[T]` and `readonly [T]`
   - `string` and `readonly string`
   - `bytes` and `readonly bytes`
   then lower to a built-in intrinsic that constructs a dedicated iterator state object.

This keeps the surface small, avoids putting iteration cursor state inside the container value
itself, and preserves the existing “readonly array yields readonly elements” behavior.

Concretely, this proposal recommends:

- Keep `core::intrinsics::into_iter<T>(arr: [T]) -> core::intrinsics::ArrayIter<T>` (already
  present), and allow calling it with `readonly [T]` as well (current VM behavior).
- Extend the same mechanism for:
  - `string` → `core::intrinsics::StringIter`
  - `bytes` → `core::intrinsics::BytesIter`

and implement `core::iter::Iterator` for each `*Iter` struct.

#### 2.2.1 Alternative: “containers implement `Iterator` directly”

The original idea (and it is viable) is:

- implement `core::iter::Iterator` directly for:
  - `[T]` / `readonly [T]`
  - `string` / `readonly string`
  - `bytes` / `readonly bytes`
- make `for x in expr { ... }` always treat `expr` as the iterator and call `expr.next()`.

This avoids an explicit `IntoIterator` layer and makes `.next()` directly available on those
container values.

However, this approach forces one of two design choices:

1) **Embed cursor state into the container value’s runtime representation**, or
2) **Mutate the container’s heap object** to store cursor state.

Both options have drawbacks for v0.4:

- It complicates the runtime model (“arrays are containers, but also cursors”).
- It interacts poorly with `readonly` (you want `for` to accept `readonly [T]`, but `next` is
  stateful).
- It makes it harder to have multiple independent iterators over the same container without
  copying / cloning semantics becoming subtle.

Because the current VM already cleanly models iteration with a dedicated `ArrayIter<T>` state
object (cursor lives in the iterator, not the array), this proposal recommends keeping that
approach and extending it to `string` / `bytes`.

#### 2.3 Item types for built-in iterables

This proposal defines the element type for built-ins as:

- `[T]` iterates `T`
- `readonly [T]` iterates `readonly T` (preserve current behavior)
- `bytes` iterates `int` (0..=255)
- `string` iterates `int` (Unicode scalar value)

If we later add a `char` type, `string` iteration can switch to `char` without changing the
interface.

---

### 3) `core::fmt::ToString`: stringification for `f"..."` interpolation

Introduce a minimal formatting interface:

```rusk
// In `core::fmt`:
interface ToString {
  readonly fn to_string() -> string;
}
```

#### 3.1 `f"..."` desugaring (new)

Replace the current “always call `core::intrinsics::to_string`” rule with a `ToString`-based rule:

`f"prefix {e1} middle {e2} suffix"` lowers to:

1. evaluate `e1`, `e2` left-to-right
2. convert each via `core::fmt::ToString::to_string(ei)`
3. concatenate via `core::intrinsics::string_concat`

This gives users a principled hook for formatting their own types, while keeping concatenation as
a primitive operation.

#### 3.2 Required baseline impls

To keep `f"..."` usable out of the box, the core library surface must provide `ToString` impls for:

- `unit`
- `bool`
- `int`
- `float`
- `string` (identity / clone)
- `bytes` (e.g. `b"..."`-style or hex; bikeshed)

Implementation strategy is intentionally open:

- Either implement `ToString` methods by calling existing `core::intrinsics::to_string` (easy),
- or split `core::intrinsics::to_string` into typed intrinsics over time.

#### 3.3 Why `ToString` (and not `Display` + `Formatter`) right now?

`ToString` is the smallest useful hook that:

- matches current `f"..."` capabilities (no format specifiers today),
- avoids introducing a new “formatter object protocol” prematurely,
- is easy to implement for primitives and user-defined types.

Once we want formatting flags (e.g. `{x:08}`) or non-allocating formatting, we can layer:

- `core::fmt::Display`
- `core::fmt::Formatter`
- `ToString` as a convenience default method (`to_string` allocates by formatting into a buffer)

without changing the fundamental desugaring model.

---

## Guide-level examples

### A) Operator overloading for a user-defined type

```rusk
use core::ops::Add;
use core::ops::Eq;

struct Point { x: int, y: int }

impl Add for Point {
  readonly fn add(other: Point) -> Point {
    Point { x: self.x + other.x, y: self.y + other.y }
  }
}

impl Eq for Point {
  readonly fn eq(other: Point) -> bool {
    self.x == other.x && self.y == other.y
  }
}

fn main() -> int {
  let a = Point { x: 1, y: 2 };
  let b = Point { x: 3, y: 4 };
  let c = a + b;
  if c == Point { x: 4, y: 6 } { 1 } else { 0 }
}
```

### B) A custom iterator type usable in `for`

```rusk
use core::iter::Iterator;

struct Range { cur: int, end: int }

impl Iterator for Range {
  type Item = int;
  fn next() -> Option<int> {
    if self.cur < self.end {
      let out = self.cur;
      self.cur = self.cur + 1;
      Option::Some(out)
    } else {
      Option::None
    }
  }
}

fn main() -> int {
  let total = 0;
  for x in Range { cur: 0, end: 5 } {
    total = total + x;
  };
  total
}
```

### C) Custom stringification for `f"..."` interpolation

```rusk
use core::fmt::ToString;

struct User { id: int, name: string }

impl ToString for User {
  readonly fn to_string() -> string {
    // minimal formatting: `User(id=..., name=...)`
    core::intrinsics::string_concat(
      "User(id=",
      core::intrinsics::string_concat(
        core::fmt::ToString::to_string(self.id),
        core::intrinsics::string_concat(
          ", name=",
          core::intrinsics::string_concat(self.name, ")")
        )
      )
    )
  }
}

fn main() -> int {
  let u = User { id: 1, name: "alice" };
  std::println(f"user = {u}");
  0
}
```

---

## Compatibility and migration notes

- These interfaces should **not** be added to `core::prelude` (auto-import) initially.
  - Reason: user code commonly defines interfaces with names like `Add` already (see fixtures).
  - Operator desugarings and `for`/`f"..."` lowering should use fully-qualified `core::...` names.
- `Option` remains special-cased as “available everywhere” as today; `Iterator` depends on it.

---

## Implementation plan (follow-up work; not part of this proposal’s patch)

1. **Inject built-in module definitions**:
   - Add `core::ops`, `core::iter`, `core::fmt` as built-in modules (like `core::intrinsics`).
   - Add interface defs for operator traits, `Iterator`, `ToString`.
2. **Typechecker changes**:
   - Extend unary/binary operator typing to:
     - keep primitive fast paths
     - otherwise require the corresponding `core::ops::*` impl
   - Extend `for` typing to:
     - accept `Iterator` directly
     - accept arrays/string/bytes via intrinsic “into-iter” construction
3. **Lowering changes**:
   - Lower non-primitive operators to explicit interface calls.
   - Lower `for` to call `core::iter::Iterator::next` (and only use intrinsics for iterator construction).
   - Lower `f"..."` interpolation to `core::fmt::ToString::to_string`.
4. **VM/host intrinsics**:
   - Add iterator structs + intrinsics for `string` and `bytes` iteration (or extend `into_iter`).
5. **Tests/fixtures**:
   - Operator overloading on structs (add/eq).
   - `for` over `string` and `bytes`.
   - `f"..."` with a user-defined `ToString`.
