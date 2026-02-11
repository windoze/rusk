## Checked runtime type tests and downcasts: `is` and `as?`

### Summary

Add two expression forms to Rusk:

- `expr is T -> bool`: a runtime type test (primarily for values whose static type is an `interface`).
- `expr as? T -> Option<T>`: a checked cast that returns `Option::Some(value)` on success and
  `Option::None` on failure (no trap on failure).

These features are intended to complement the existing `as` cast, which is an explicit *interface
upcast* that is checked statically and is a runtime no-op.

---

## Motivation

With runtime dynamic dispatch, it is common to store values behind an `interface` type:

```rusk
// Assume: `Dog` is a `struct`, and `impl Animal for Dog { ... }` exists.
let a: Animal = (Dog { name: "Fido" } as Animal);
```

Today, once a value is “behind” an interface, the language offers no way to:

1. check the dynamic concrete type (`Dog` vs `Cat`), or
2. check whether the dynamic value also implements some other interface, or
3. safely recover a concrete type (`Dog`) without introducing an unchecked cast or making
   `match` patterns accept interface-typed scrutinees.

This proposal adds a small, explicit surface for “RTTI-like” checks without introducing implicit
subtyping into inference.

---

## Design goals

1. **Safe and explicit**:
   - checked casts return `Option` and never trap on failure.
2. **Minimal interaction with inference**:
   - avoid implicit subtyping; keep interface casts explicit (`as` / `as?`).
3. **Useful for interface values**:
   - support `interface -> struct/enum` downcasts
   - support `interface -> interface` cross-casts (dynamic “implements?” checks)
4. **Preserve `readonly` views**:
   - successful casts preserve `readonly`ness of the input value.
5. **Deterministic semantics**:
   - compile-time evaluation is allowed only as an optimization; observable results must match the
     runtime check.

## Non-goals (initial scope)

- Flow-sensitive “smart casts” / type narrowing for `if x is T { ... }`.
- Checked casts that distinguish generic arguments (type arguments are erased at runtime in v0.4).
- Adding a trapping/unchecked downcast operator.

---

## Language surface changes

### 1) New keyword / token

- Add reserved keyword `is`.
- Add token `?` (used only in `as?` for now).

### 2) Grammar and precedence

`as?` is a cast-form (same precedence as the existing `as` cast).
`is` is a type-test operator with precedence similar to comparisons (lower than `+`/`*`, higher
than `&&`/`||`).

Proposed EBNF changes (relative to `RUSK_SPEC.md` §3.6):

```ebnf
// Allow a chain of interface upcasts (`as I`), optionally ending in one checked cast (`as? T`).
CastExpr       := UnaryExpr ( "as" Type )* ( "as" "?" Type )? ;

// Insert a type-test expression layer above additive expressions, so `x + y is T` parses as
// `(x + y) is T` (like other languages with `is`).
TypeTestExpr   := AddExpr ( "is" Type )? ;

// Update comparisons to operate on TypeTestExpr instead of AddExpr.
CmpExpr        := TypeTestExpr ( ( "<" | "<=" | ">" | ">=" ) TypeTestExpr )* ;
```

---

## Typing and semantics

### `expr is T`

- Type: `bool`
- `expr` is evaluated exactly once.
- The check succeeds if `expr`’s **dynamic** value matches `T`:
  - if `T` is a nominal `struct`/`enum` type: dynamic nominal type equality
  - if `T` is an `interface` type: dynamic “implements interface” check

### `expr as? T`

- Type: `Option<T>` (or `Option<readonly T>` if `expr` is `readonly _`)
- `expr` is evaluated exactly once.
- If the checked cast succeeds, it yields `Option::Some(casted_value)`.
- If it fails, it yields `Option::None`.
- The operator must not trap on cast failure.

### Restrictions (v0.4)

Because generic arguments are erased at runtime, the runtime cannot soundly distinguish values of
`Foo<int>` from values of `Foo<float>` if both exist. To keep this feature sound and implementable:

- In v0.4, require that the target type `T` in `is`/`as?` is **runtime-checkable**, which means:
  - `T` is a non-generic nominal type (`struct`/`enum`), or
  - `T` is a non-generic interface type.

This matches what the runtime can reliably observe today: nominal type identity and interface
implementation via method tables.

`is`/`as?` may still be accepted for non-runtime-checkable targets (e.g. targets with generic
arguments) **only when the compiler can prove the result statically** (so no runtime check is
required). Otherwise, it must be rejected as a type error.

### Compile-time folding (optional)

If the compiler can prove the result statically, it may constant-fold:

- `expr is T` to `true`/`false`
- `expr as? T` to `Option::Some(expr)` / `Option::None`

This must be an optimization only; observable behavior must match the runtime check.

---

## Examples

```rusk
interface Animal {}
interface Drawable {}

struct Dog { name: string }

impl Animal for Dog {}
impl Drawable for Dog {}

fn test_is(a: Animal) -> unit {
  if a is Dog {
    // This is a boolean test only (no smart-cast in v0.4).
    // Use `as?` to obtain a `Dog`.
  } else {
    // Not a Dog.
  }
}

fn test_as_question(a: Animal) -> unit {
  match a as? Dog {
    Option::Some(dog) => {
      // `dog` has static type `Dog` here.
      dog.name;
    }
    Option::None => {
      // Cast failed.
    }
  }
}

fn interface_cross_cast(x: Drawable) -> unit {
  // Runtime “implements?” check:
  if x is Animal {
    // Dynamic value also implements `Animal`.
  }

  match x as? Animal {
    Option::Some(a) => {
      // `a` has static type `Animal` here.
    }
    Option::None => {
      // Dynamic value does not implement `Animal`.
    }
  }
}
```

---

## Required spec updates

- `RUSK_SPEC.md`:
  - §2.2 Keywords: add `is`.
  - §2.3 Operators and Delimiters: add `?` (or explicitly document it as part of `as?`).
  - §3.6 Expressions:
    - add `is` / `as?` to precedence + EBNF
    - define typing/semantics for `is` and checked casts, including `readonly` preservation.

---

## Implementation sketch (repo-aligned, non-normative)

- Parser:
  - recognize `is` as a keyword in expressions.
  - recognize `as?` as `as` followed by `?` in cast position.
- Typechecker:
  - validate that the target `T` is a struct/enum/interface type.
  - reject casts that would require checking erased generic arguments at runtime (unless provable).
  - set the result type to `bool` for `is` and `Option<T>` / `Option<readonly T>` for `as?`.
- Lowering/runtime (one possible approach):
  - use the runtime’s dynamic nominal type name (already used for `vcall`) to implement:
    - nominal equality tests (`Dog` vs `Cat`)
    - interface implementation tests (“does dynamic type implement `I`?”) by consulting method
      metadata (or a derived `(type, interface) -> bool` table).
