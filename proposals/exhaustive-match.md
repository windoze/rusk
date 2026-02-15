# Proposal: Exhaustive `match` (Value Arms)

Date: 2026-02-15  
Status: Implemented

This proposal makes `match` safer and more predictable by enforcing **compile-time exhaustiveness**
for **value arms**. Today, the compiler lowers a missing-case `match` to a runtime trap
(`"non-exhaustive match"`). This change turns that into a compile-time error, while keeping MIR and
bytecode unchanged for now.

---

## 1. Requirements (as proposed)

- `match` should have **exhaustive value arms**; an unexhaustive match should be a **compile error**
  instead of a runtime trap. **Effect arms are out of scope.**
- If the exhaustiveness of `match` cannot be ensured at compile-time, the compiler should enforce a
  `_ => ...` arm to handle all unmatched values.
- This change should be a **compiler** change; we should keep **MIR and bytecode unchanged** for
  now.

---

## 2. Motivation

Current lowering inserts a default branch that traps at runtime when no value arm matches (the
emitted trap message is `"non-exhaustive match"`). This has a few downsides:

- Failures are discovered late (at runtime) instead of early (at compile time).
- Missing cases can hide in rarely-executed paths and regress silently.
- It is harder to refactor enums/ADTs safely without compiler help.

Enforcing exhaustiveness moves this class of failure into the compiler and aligns with the “Rust-
like” design goal described in `RUSK_SPEC.md`.

---

## 3. Scope / Non-goals

### In scope

- Exhaustiveness for **value arms** in `match` expressions.
- Diagnostics for non-exhaustive matches (compile-time errors).
- A conservative rule that requires an explicit `_ => ...` when exhaustiveness cannot be proven.

### Out of scope

- **Effect arms**: no exhaustiveness requirements for `@Iface.method(...)` arms in this proposal.
- Pattern usefulness / unreachable-arm diagnostics (e.g. “this arm is unreachable” warnings).
- Any change to MIR/bytecode formats, instruction sets, or semantics.
- Changing the runtime behavior of other pattern matches (e.g. `let pat = e;` trapping remains as-is
  unless separately proposed).

---

## 4. Proposed semantics

### 4.1 What “exhaustive” means

For a `match scrutinee { ... }`, consider only **value arms** (`Pattern => ...`), ignoring effect
arms. The set of value arms is **exhaustive** if, for every runtime value `v` that `scrutinee` can
evaluate to, at least one value-arm pattern matches `v`.

If not exhaustive, compilation fails with a `non-exhaustive match` error.

### 4.2 Interaction with effect arms (explicitly out of scope)

Effect arms may appear alongside value arms (handlers + value matching). This proposal:

- **does not** require effect arms to be “exhaustive” over any effect set, and
- **does** require that value arms remain exhaustive for the scrutinee’s returned value.

In other words: effect arms are ignored for the value-exhaustiveness check.

### 4.3 `_ => ...` catch-all arm requirement

Some scrutinee domains are (practically) infinite or not enumerable by the compiler (e.g. `int`,
`string`, `bytes`, `float`, most user-defined structs with unconstrained fields, arrays of arbitrary
length). For such types, exhaustiveness usually cannot be demonstrated by listing “all values”.

For those cases, the compiler should require a catch-all value arm in the canonical form:

```rusk
_ => { ... }
```

This arm guarantees there is no runtime “fall-through to trap”.

Note: an irrefutable value pattern (one that always matches the scrutinee type, such as a plain
binding `x`) is also a catch-all in practice; the compiler may treat it as satisfying the same need.
The diagnostic guidance should still suggest `_ => ...` as the most explicit form.

---

## 5. Exhaustiveness checking (v1: conservative rules)

The first implementation should bias toward **soundness + simplicity** rather than completeness
relative to Rust’s full pattern-matching algorithm.

### 5.1 Classify scrutinee type

After type inference resolves the scrutinee type `T`, classify `T` into:

1) **Finite / enumerable** (we can prove exhaustiveness by covering all constructors):
   - `unit`
   - `bool`
   - nominal `enum` types (closed set of variants)

2) **Non-finite / non-enumerable (v1)**:
   - `int`, `float`, `string`, `bytes`
   - arrays (arbitrary length)
   - nominal `struct` types (unless an irrefutable arm exists)
   - tuples (unless an irrefutable arm exists)
   - other cases where the compiler can’t confidently enumerate all possibilities in v1

This classification is an implementation detail; it exists to decide whether we do a “finite coverage
check” or enforce `_ => ...`.

### 5.2 Accept immediately if there is an irrefutable value arm

If any value arm is **irrefutable** for `T`, the match is exhaustive regardless of `T`’s domain.
Examples of irrefutable patterns (non-exhaustive list):

- `_`
- `name` (binding pattern)
- tuple destructuring like `(a, b)` when `T` is a 2-tuple and both subpatterns are irrefutable
- struct destructuring like `Point { x, y }` (or `{ .. }`) when `T` is exactly `Point`
- array pattern with a single rest `[..rest]` when `T` is an array type

This provides ergonomic exhaustive matches on product types without requiring a redundant `_ => ...`.

### 5.3 Finite coverage check (unit / bool / enum)

If `T` is finite/enumerable and no irrefutable arm exists:

- `unit`: exhaustive iff there is an arm matching `()`.
- `bool`: exhaustive iff there is an arm matching `true` and an arm matching `false`.
- `enum E`: exhaustive iff, for every variant `E::V`, there exists an arm that matches `E::V(...)`.
  - For payload variants, the simplest v1 requirement is that the payload subpatterns are
    themselves irrefutable for their expected types (e.g. `E::V(_)`, `E::V(x)`, `E::V(a, b)` with
    irrefutable `a`/`b` patterns).
  - If users want to split a variant by value (e.g. `Some(0)` vs `Some(_)`), they must still cover
    the remainder so that the variant is fully covered.

This is intentionally conservative; it can be refined later to support more complex “sum-of-products”
coverage without forcing `_`.

### 5.4 Otherwise, require `_ => ...`

If `T` is not in the finite/enumerable set and there is no irrefutable arm, the compiler emits a
compile error unless the match includes an explicit wildcard arm `_ => ...`.

This directly implements the “if exhaustiveness cannot be ensured at compile time, require `_`”
requirement.

---

## 6. Examples

### 6.1 `bool` (finite)

```rusk
match b {
  true => 1,
  false => 0,
}
```

This compiles (exhaustive).

```rusk
match b {
  true => 1,
}
```

This becomes a compile error: missing `false`.

### 6.2 `int` (non-finite; requires catch-all)

```rusk
match n {
  0 => "zero",
  1 => "one",
  _ => "many",
}
```

This compiles.

```rusk
match n {
  0 => "zero",
  1 => "one",
}
```

This becomes a compile error suggesting `_ => ...`.

### 6.3 Struct destructuring (irrefutable arm)

```rusk
match p {
  Point { x, y } => x + y,
}
```

This should compile: the pattern is irrefutable for `Point`.

### 6.4 Enum variants (finite, must cover all variants)

```rusk
match opt {
  Option::Some(x) => x,
  Option::None => 0,
}
```

This compiles.

If `Option::None` is missing, it becomes a compile error (instead of relying on runtime trap).

### 6.5 With effect arms (ignored for exhaustiveness)

```rusk
match do_io() {
  @IO.println(msg) => { /* handle effect */ },
  x => x,
}
```

This is exhaustive because `x => x` is irrefutable. Effect arms do not change the value-arm
exhaustiveness rule.

---

## 7. Diagnostics

Baseline diagnostic:

- Error message: `non-exhaustive match`

Preferred improvements (optional in v1, but desirable):

- For `bool`, tell the user which literal is missing.
- For enums, list missing variants by name (at least the top-level variant).
- For “require `_`” cases, include a fix-it style suggestion: “add `_ => ...`”.

---

## 8. Implementation sketch (compiler-only; MIR/bytecode unchanged)

### Where the check lives

Implement the exhaustiveness check in the compiler front-end (type checking), so it runs before MIR
lowering:

- `crates/rusk-compiler/src/typeck.rs`: inside/after `typecheck_match(...)`, once `scrutinee_ty` is
  resolved and value-arm patterns are typechecked.

### What stays unchanged

- MIR lowering keeps emitting the default “trap” terminator for non-exhaustive matches
  (`"non-exhaustive match"`), as it does today.
- Bytecode lowering and the interpreter/VM remain unchanged.

This proposal’s intent is that the runtime trap becomes unreachable for well-typed programs, without
changing the IR formats.

---

## 9. Testing plan

Add compiler tests covering:

- `bool` match missing `false` → compile fail
- enum match missing a variant → compile fail
- `int` match without `_` (and without an irrefutable arm) → compile fail
- `int` match with `_` → compile pass
- struct match with a single irrefutable destructuring pattern → compile pass
- match with effect arms + exhaustive value arms → compile pass

---

## 10. Alternatives considered

1) **Status quo (runtime trap)**  
   Keep lowering-only checking. Simple, but misses compile-time guarantees and encourages fragile
   code.

2) **Always require `_ => ...` for every match**  
   Very easy to implement, but noisy for enums/bools and undermines one of `match`’s main benefits.

3) **Full Rust-style exhaustiveness + usefulness**  
   Best UX long-term, but significantly more complex. This proposal intentionally starts with a
   conservative subset and leaves room for refinement.

---

## 11. Open questions / follow-ups

- Should the language specification (`RUSK_SPEC.md`) be updated immediately after implementation to
  explicitly state that non-exhaustive value matches are a compile-time error?
- Should `name => ...` (binding) be treated exactly like `_ => ...` for “required catch-all” cases,
  or should the compiler require `_` specifically to keep intent explicit?
- How far should v1 go on nested-pattern exhaustiveness inside enum payloads (e.g. product-of-sums)?
  The conservative choice is to require `_` in such cases until we implement a richer algorithm.
