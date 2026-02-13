# Destructuring `let` (Pattern Bindings)

## Summary

Rusk already supports **patterns** in:

- `match` value arms
- effect handler arm parameters
- function parameters

This proposal extends `let` statements so the left-hand side can be a **pattern**, enabling
destructuring bindings such as tuples/arrays/structs/enums/new-type structs directly in locals.

## Motivation

- **Ergonomics**: avoid temporary locals + `match` just to extract fields/elements.
- **Consistency**: reuse the *existing* pattern syntax and semantics already implemented for
  parameters and `match`.
- **Expressiveness**: allow concise binding of multiple locals, including “rest” patterns (`..`) and
  new-type struct patterns.

## Proposed Syntax

### Grammar change (RUSK_SPEC.md)

Current:

```ebnf
LetStmt := "let" Ident (":" Type)? ("=" Expr)? ";" ;
```

Proposed:

```ebnf
LetStmt := "let" Pattern (":" Type)? ("=" Expr)? ";" ;
```

Notes / restrictions:

- `let x;` and `let x: T;` continue to mean “declare an uninitialized local”.
- If the pattern is **not** a simple binding (`Ident`) or wildcard (`_`), an initializer is required:
  - allowed: `let (a, b) = expr;`
  - rejected: `let (a, b);`
- The proposal does **not** change pattern syntax; it reuses §3.7 “Patterns” as-is.

### Pattern syntax reminder (rest)

Rusk’s existing rest pattern syntax is:

- `..` to ignore the middle/rest
- `..name` to bind the middle/rest

So the “rest binding” examples in this proposal use `..a` / `..rest` (consistent with §3.7).

## Semantics

### Evaluation and binding

For `let pat = expr;`:

1. Evaluate `expr` **exactly once**.
2. Pattern-match the resulting value against `pat`.
3. If the match succeeds, introduce bindings for every identifier bound by the pattern.
4. If the match fails, **trap** at runtime.

This matches the already-specified behavior for **parameter patterns**:
> “Parameters may use destructuring patterns; if a parameter pattern does not match at runtime, the
> function traps.”

### “Irrefutability”

Unlike Rust, this proposal does **not** require `let` patterns to be statically irrefutable.
Rusk patterns already support literal patterns (e.g. `true`, `1`, `"x"`) and those are currently
allowed in parameter positions, with runtime trapping on mismatch.

For consistency, `let` uses the same rule: any pattern is permitted, and mismatch traps.

### Binding kind

Every identifier bound inside the pattern behaves as if it were introduced by an ordinary `let`:

- the binding is **rebindable** (it is not `const`)
- it does not implicitly create a readonly view (that is only for `readonly`)
- scope/visibility follow the normal statement scope rules

### Type annotations

This proposal preserves `let`’s existing type annotation surface form:

- `let pat: T = expr;` ascribes the *scrutinee* type `T` (i.e. the type of `expr`), then checks
  `pat` against `T`.
- `let pat = expr;` infers the scrutinee type from `expr`, then checks `pat` against it.

Bound identifiers get their types from the positions they bind in the pattern (as already done for
`match`/parameter patterns).

## Examples

Tuple destructuring:

```rusk
let (a, b) = (10, 20);               // a = 10, b = 20
let (a, .., b) = (10, 20, 30, 40);   // a = 10, b = 40
let (..a, b) = (1, 2, 3);            // a = (1, 2), b = 3
```

Array destructuring:

```rusk
let [a, .., b] = [1, 2, 3, 4, 5];    // a = 1, b = 5
let [.., a] = [1, 2, 3];             // a = 3
let [..a, _] = [1, 2, 3];            // a = [1, 2]
```

New-type struct destructuring (constructor pattern):

```rusk
struct UserId(int);
let UserId(a) = UserId(123);         // a = 123
```

Nested patterns:

```rusk
let (UserId(id), [first, ..rest]) = (UserId(7), [10, 20, 30]);
// id = 7, first = 10, rest = [20, 30]
```

## Lowering / Implementation Sketch

The compiler already lowers parameter patterns by compiling a single-case MIR `switch` with a
default trap block. `let` can reuse the same strategy:

1. Lower `expr` into a fresh temporary local `tmp`.
2. Build:
   - an `ok` block whose parameters correspond to pattern-bound values (in the compiler’s existing
     “bind names” order),
   - a `trap` block with `Trap { message: "let pattern match failed" }` (message bikeshed).
3. Emit:
   - `Switch { value: tmp, cases: [pat -> ok], default: trap }`.
4. In the `ok` block, bind each name to the corresponding block param local, then continue.

Fast paths:

- `let x = expr;` stays as the current direct local init (no `switch` needed).
- `let _ = expr;` evaluates `expr` for side effects and discards the value.

## Diagnostics

### Compile-time

- If a non-trivial pattern is used without an initializer:
  - error: “destructuring `let` requires an initializer” (exact wording TBD)

### Runtime

- If the pattern match fails:
  - trap with a clear message (recommendation: `let pattern match failed`)

This mirrors parameter-pattern trapping.

## Compatibility

This is an **additive** syntax change:

- existing programs continue to parse/behave the same
- new valid programs are enabled (`let (a, b) = ...;`, etc.)

## Test Plan (when implemented)

- Parser tests:
  - accepts all examples above
  - rejects `let (a, b);` and `let [a, b];`
- Typechecker tests:
  - correct binding types for tuple/array/new-type patterns
  - `readonly` propagation rules remain unchanged (only through `readonly` bindings/params)
- Runtime tests:
  - pattern mismatch traps (e.g. `let 1 = 2;`)

## Open Questions

- Should `const` and `readonly` statements also accept patterns for symmetry?
  - e.g. `const (a, b) = pair();`, `readonly [head, ..tail] = xs;`
  - This proposal keeps scope to `let` but the same mechanism generalizes cleanly.
- Should the runtime trap message be shared with parameter-pattern failures or distinct?
