# Never / Bottom Type (`!`)

Date: 2026-02-18  
Status: **Draft**

This proposal introduces a **never type** (a.k.a. bottom type) spelled `!`.

The immediate motivation is to make “abortive control flow” APIs ergonomic and sound:

- `panic(...)` should return `!` (instead of being generic/polymorphic with ad-hoc special cases).
- Effect-based `throw` should return `!` (instead of `unit`), so user code can write `throw(e)` in
  value position without dummy expressions.

The design is intentionally minimal: `!` is a primitive, uninhabited type that can **coerce to any
type** in expression typing, allowing diverging expressions to fit anywhere.

---

## Motivation

### 1) Ergonomic `throw` with effects

Today, a pure-Rusk `try/catch` encoding using effects typically needs a dummy expression after
`throw` because `throw` must have some return type (currently `unit`):

```rusk
try {
    throw(e);
    0 // dummy, just to satisfy the `T` of the try block
}.catch |e| { ... }
```

If `throw(e)` has type `!`, the dummy value disappears and the code becomes “obviously correct”:

```rusk
try {
    throw(e) // type `!`, coerces to any `T`
}.catch |e| { ... }
```

Additionally, if the effect operation itself is typed as returning `!`, it becomes **statically
abortive**: handlers cannot meaningfully `resume` the continuation because they would have to
produce a value of type `!`.

### 2) Remove special-case hacks around `panic`

The sysroot currently declares `panic` as a polymorphic intrinsic (`panic<T>(...) -> T`).

This is expressive, but it interacts poorly with “reified generics” and type inference, and it
already required compiler special-casing to avoid unconstrained type variables.

With a real never type:

- `panic(msg: string) -> !` is the natural signature.
- No special-case inference is needed.
- `panic(...)` can appear in any value position via `!`-to-`T` coercion.

### 3) Better typing for common control-flow shapes

Once `!` exists, “diverging branches” become first-class:

```rusk
fn f(b: bool) -> int {
    if b { 1 } else { panic("no") }
}
```

Without a bottom type, typing such expressions tends to require either:
- syntactic restrictions, or
- ad-hoc “diverging expression” detection in many places.

---

## Goals

1. Add a primitive type spelled `!` that represents an **uninhabited** type.
2. Allow expressions of type `!` to appear in any expression context via coercion (`!` is bottom).
3. Change `core::intrinsics::panic` to return `!` (and remove its generic parameter).
4. Enable effect-based `throw` APIs to return `!` for better ergonomics and stronger typing.
5. Keep runtime implications minimal: `!` has no runtime values.

---

## Non-goals

- A full “unreachable code” analysis pass (nice-to-have, not required to land `!`).
- Adding new surface syntax beyond the `!` type itself.
- Changing the effect semantics model (handlers, delimitation, one-shot continuations).

---

## Design

### 1) Syntax and grammar

Add `!` as a type literal in type positions.

In the spec type grammar, extend primitive types:

```text
PrimType := "unit" | "bool" | "int" | "float" | "byte" | "char" | "string" | "bytes" | "!" ;
```

Notes:
- `!` already exists as the token for unary `not` in expression position; there is no ambiguity in
  a type context.
- No new keyword is introduced.

### 2) Type meaning: `!` is bottom / never

`!` is an **uninhabited** type:
- There is no runtime value of type `!`.
- Any expression that successfully evaluates to a value of type `!` is considered impossible; the
  only way to produce type `!` is to diverge (trap, abortive effect, infinite loop, etc.).

### 3) Coercion rule: `!` coerces to any type

In expression typing, `!` should be accepted where any type `T` is expected:

- If an expression has type `!`, it may be treated as having type `T` for any `T`.
- This is not a runtime conversion; it is justified because that expression never produces a value.

Practically, this means:

- `if` and `match` typing should accept diverging branches/arms without forcing the whole
  expression to become `!`.
- `let x: T = panic(...)` typechecks.
- `return_ty` unification accepts `!` (a function body may end with a diverging expression).

### 4) Type inference / unification behavior

Rusk largely uses unification-style type inference. A naive “`!` is just another nominal type”
does not work well because unification would accidentally *fix* type variables to `!` too early.

Proposed unification/coercion rule (conceptual):

- `unify(!, T) = T`
- `unify(T, !) = T`
- `unify(!, !) = !`

And crucially for inference variables:

- `unify(?X, !) = ?X` (do **not** bind `?X := !`; `!` provides no information)
- `unify(!, ?X) = ?X`

This matches the “bottom provides no constraints” intuition and allows expressions like:

```rusk
let x = if cond { panic("x") } else { 1 }; // x: int
```

### 5) Standard library / sysroot changes

#### 5.1 `panic`

Change the sysroot intrinsic declaration:

- From: `pub intrinsic fn panic<T>(msg: string) -> T;`
- To: `pub intrinsic fn panic(msg: string) -> !;`

Rationale:
- `panic` is semantically diverging.
- Avoids polymorphic `panic<T>` interacting with reified generics.

#### 5.2 Effect-based `throw`

For typed exceptions encoded as effects (e.g. `Throw<E>`), the operation should return `!`:

```rusk
pub interface Throw<E> {
    fn throw(e: E) -> !;
}

pub fn throw<E>(e: E) -> ! {
    @Throw.throw(e)
}
```

This gives two benefits:

1) **Ergonomics**: `throw(e)` can be written directly in value position.
2) **Static abortiveness**: a handler continuation would require an argument of type `!`, making
   resumption impossible in well-typed code.

### 6) MIR / bytecode / runtime representation

At runtime, `!` has **no values**, but the compiler/VM should still account for it.

Two reasonable implementation strategies:

#### Strategy A (preferred): represent `!` explicitly in IR types

- Add `Type::Never` (or similar) in MIR/bytecode type enums.
- Allow locals and function signatures to mention `Never`.
- If execution ever attempts to `return` a `Never` value (which should be impossible), trap with an
  internal error.

This keeps the IR honest and improves diagnostics.

#### Strategy B (minimal): erase `!` to `unit` in IR

- Treat `!` as compile-time-only, lowering it to `unit` in MIR/bytecode.
- Rely on the fact that well-typed programs never produce a value of type `!`.

This is simpler, but it risks confusing debug tooling and can hide type bugs.

### 7) Interaction with exhaustiveness and control-flow checking

- `match` exhaustiveness: `match` on a scrutinee of type `!` is vacuously exhaustive, but this
  proposal does not require changing the surface rule that `match` must have at least one arm (and
  at least one value arm if effect arms exist). This can be revisited later.
- `break`/`continue`/`return`: these are currently statements in Rusk, not expressions. This
  proposal does not change that. Future work could introduce expression forms whose type is `!`
  (e.g. `return expr` as an expression), but that is out of scope.

---

## Examples

### `panic` in value position

```rusk
fn f(x: int) -> int {
    if x > 0 { x } else { panic("x must be positive") }
}
```

### `throw` without dummy values

```rusk
fn parse_int(s: string) -> int {
    if s == "" { throw("empty") } else { 123 }
}
```

### Pure-Rusk `try/catch/finally` becomes cleaner

With `throw: E -> !`, a pure-Rusk `try { ... }.catch { ... }.finally { ... }` can typecheck without
artificial tail expressions after `throw`.

---

## Drawbacks / tradeoffs

- Adds a new primitive type and special-case coercion/unification behavior.
- Some diagnostic logic may need to treat `!` specially (e.g. “cannot infer type” errors should not
  be triggered merely because one branch diverges).

---

## Alternatives considered

1) **Keep `panic<T> -> T` and special-case diverging calls**
   - Requires ad-hoc rules in type inference to avoid unconstrained type variables.
   - Makes effect-based `throw` ergonomics worse (still needs dummy return values).

2) **Introduce a keyword type like `never` instead of `!`**
   - Slightly simpler tokenization, but less conventional and less ergonomic.
   - The user-facing request prefers `!`.

---

## Implementation plan (sketch)

1) Spec: update `RUSK_SPEC.md` type forms to include `!`.
2) Front-end:
   - Extend type parser to accept `!` in type contexts.
   - Add `Never` to the compiler’s type representation.
   - Implement bottom-type coercion in unification and branch typing (`if` / `match`).
3) Sysroot:
   - Change `core::intrinsics::panic` signature to `-> !`.
   - Update any pure-Rusk `throw` encodings to `-> !`.
4) MIR/bytecode:
   - Prefer `Type::Never` and ensure VM traps if it is ever materialized.
5) Tests:
   - Add typechecking tests for `if`/`match` with diverging arms.
   - Add a “throw in value position” test with no dummy tail expression.

