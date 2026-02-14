# Destructuring Bindings (Pattern Bindings for `let` / `const` / `readonly`)

## Summary

Rusk already supports **patterns** in:

- `match` value arms
- effect handler arm parameters
- function parameters

This proposal extends local binding statements (`let`, `const`, `readonly`) so the left-hand side
can be a **pattern**, enabling destructuring bindings such as
tuples/arrays/**named-field structs**/enums/new-type structs directly in locals.

## Motivation

- **Ergonomics**: avoid temporary locals + `match` just to extract fields/elements.
- **Consistency**: reuse the *existing* pattern syntax and semantics already implemented for
  parameters and `match`.
- **Expressiveness**: allow concise binding of multiple locals, including “rest” patterns (`..`) and
  new-type struct patterns, plus named-field struct patterns with field shorthands / renames.
- **Intent**: destructured names are frequently not meant to be reassigned; supporting `const` and
  `readonly` makes that intent explicit without changing the pattern syntax itself.

## Proposed Syntax

### Grammar change (RUSK_SPEC.md)

Current:

```ebnf
LetStmt := "let" Ident (":" Type)? ("=" Expr)? ";" ;
ConstStmt := "const" Ident (":" Type)? "=" Expr ";" ;
ReadonlyStmt := "readonly" Ident (":" Type)? "=" Expr ";" ;
```

Proposed:

```ebnf
LetStmt := "let" Pattern (":" Type)? ("=" Expr)? ";" ;
ConstStmt := "const" Pattern (":" Type)? "=" Expr ";" ;
ReadonlyStmt := "readonly" Pattern (":" Type)? "=" Expr ";" ;
```

Notes / restrictions:

- `let x;` and `let x: T;` continue to mean “declare an uninitialized local”.
- If the pattern is **not** a simple binding (`Ident`) or wildcard (`_`), an initializer is required:
  - allowed: `let (a, b) = expr;`
  - rejected: `let (a, b);`
- `const` and `readonly` statements continue to require an initializer (as in `RUSK_SPEC.md`):
  - allowed: `const (a, b) = expr;`, `readonly S{x, ..} = expr;`
  - rejected: `const x;`, `readonly x;`
- The proposal does **not** change pattern syntax; it reuses §3.7 “Patterns” as-is.

### Pattern syntax reminder (rest)

Rusk’s existing rest pattern syntax is:

- `..` to ignore the middle/rest
- `..name` to bind the middle/rest

So the “rest binding” examples in this proposal use `..a` / `..rest` (consistent with §3.7).

For **named-field struct patterns**, `..` is supported only as an ignore marker (per §3.7):

- allowed: `Point{x, ..}` (ignore unspecified fields)
- rejected: `Point{..rest}` (struct `..` cannot bind)

## Semantics

### Evaluation and binding

For `KW pat = expr;` where `KW ∈ { let, const, readonly }`:

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

For consistency, all three binding forms use the same rule: any pattern is permitted, and mismatch
traps.

### Binding kind

Every identifier bound inside the pattern uses the binding kind of the statement keyword:

- `let`: **rebindable**
- `const`: **not rebindable** (but does not deep-freeze referenced objects)
- `readonly`: **not rebindable** and introduces a **readonly view** for each binding (attempting to
  mutate through the binding is rejected by the typechecker and also traps at runtime)

In other words, `const (a, b) = expr;` makes **both** `a` and `b` const bindings; `readonly pat = e;`
makes **every** identifier bound by `pat` a readonly binding/view. Mixing binding kinds within a
single pattern is not part of this proposal.

Scope/visibility follow the normal statement scope rules.

### Type annotations

This proposal preserves each binding statement’s existing type annotation surface form:

- `let pat: T = expr;`, `const pat: T = expr;`, `readonly pat: T = expr;` ascribe the *scrutinee*
  type `T` (i.e. the type of `expr`), then check `pat` against `T`.
- `let pat = expr;`, `const pat = expr;`, `readonly pat = expr;` infer the scrutinee type from
  `expr`, then check `pat` against it.

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

Named-field struct destructuring:

```rusk
struct S { x: int, y: int }

let S{x: a, y: b} = S{x: 1, y: 2};   // a = 1, b = 2

// Field shorthand + ignoring the rest:
let S{x, ..} = S{x: 1, y: 2};        // x = 1
```

Nested patterns:

```rusk
let (UserId(id), [first, ..rest]) = (UserId(7), [10, 20, 30]);
// id = 7, first = 10, rest = [20, 30]
```

Const destructuring:

```rusk
const (a, b) = (10, 20);             // a/b cannot be rebound
const UserId(id) = UserId(123);      // id cannot be rebound
```

Readonly destructuring:

```rusk
// readonly bindings prevent rebinding AND prevent mutation through the binding
readonly (xs, ys) = ([1, 2], [3, 4]);
xs[0] = 9;                           // rejected (and would trap if reached)
```

## Lowering / Implementation Sketch

The compiler already lowers parameter patterns by compiling a single-case MIR `switch` with a
default trap block. `let`/`const`/`readonly` can reuse the same strategy:

1. Lower `expr` into a fresh temporary local `tmp`.
2. Build:
   - an `ok` block whose parameters correspond to pattern-bound values (in the compiler’s existing
     “bind names” order),
   - a `trap` block with `Trap { message: "binding pattern match failed" }` (message bikeshed).
3. Emit:
   - `Switch { value: tmp, cases: [pat -> ok], default: trap }`.
4. In the `ok` block, bind each name to the corresponding block param local, then continue.

Fast paths:

- `let x = expr;`, `const x = expr;`, `readonly x = expr;` stay as direct local init (no `switch`
  needed).
- `_` patterns evaluate `expr` for side effects and discard the value.
- `readonly` lowering can reuse existing MIR `as_readonly` behavior: either convert the scrutinee
  once before matching, or insert `as_readonly` conversions for each bound identifier in the `ok`
  block.

## Diagnostics

### Compile-time

- If a non-trivial pattern is used in a `let` without an initializer:
  - error: “destructuring `let` requires an initializer” (exact wording TBD)
- `const` / `readonly` without initializers are already rejected by the grammar.

### Runtime

- If the pattern match fails:
  - trap with a clear message (recommendation: `binding pattern match failed`)

This mirrors parameter-pattern trapping.

## Compatibility

This is an **additive** syntax change:

- existing programs continue to parse/behave the same
- new valid programs are enabled (`let (a, b) = ...;`, `const (a, b) = ...;`, `readonly (a, b) = ...;`, etc.)

## Test Plan (when implemented)

- Parser tests:
  - accepts all examples above
  - rejects `let (a, b);`, `let [a, b];`, `const (a, b);`, `readonly [a, b];`
- Typechecker tests:
  - correct binding types for tuple/array/new-type patterns
  - `const` bindings are not assignable
  - `readonly` bindings behave like existing `readonly` locals/params (readonly view + no rebinding)
- Runtime tests:
  - pattern mismatch traps (e.g. `let 1 = 2;`, `const 1 = 2;`, `readonly 1 = 2;`)

## Open Questions

- Should we eventually support mixing binding kinds *within* a single pattern?
  - e.g. `let (const a, readonly b, c) = ...;` (not proposed here)
- For `readonly pat = expr;`, should the compiler apply `as_readonly` to the scrutinee once, or to
  each bound binding individually? (Semantics should be equivalent; this is intended to be an
  implementation detail.)
- Should the runtime trap message be shared with parameter-pattern failures or distinct?
