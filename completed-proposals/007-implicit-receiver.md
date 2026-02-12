## Implicit receiver (`self`) in method definitions + `readonly fn` + default interface methods + explicit `static fn`

### Summary

This proposal changes how methods are *defined* (not how they are resolved) by:

1. Making the receiver **implicit** in `impl` method definitions (both inherent `impl T { ... }` and interface impls `impl I for T { ... }`).
2. Introducing a **reserved `self` identifier** that refers to the implicit receiver inside instance methods.
3. Adding a **method-level** `readonly` modifier: `readonly fn m(...) { ... }` means `self` is a readonly view inside the method.
4. Allowing **default methods** in `interface` items by permitting method bodies: `fn m(...) -> T { ... }`.
5. Introducing an explicit **`static`** modifier for receiver-less methods in inherent impls: `static fn m(...) { ... }`.
   - **Static methods are forbidden in `interface` items.**
   - **Static methods are forbidden in `impl I for T { ... }` blocks.**
   - Static methods are callable **only** as `Type::method(...)` (never as `value.method(...)`).

The goal is to reduce boilerplate (`self: Type` everywhere), clarify instance-vs-static intent, and make interfaces easier to evolve via default methods.

---

## Motivation

Today, method bodies in `impl` blocks must explicitly declare the receiver as a first parameter:

```rusk
impl Point {
  fn sum(self: Point) -> int { self.x + self.y }
}

impl Add for Point {
  fn add(self: Point, n: int) -> int { self.sum() + n }
}
```

Problems with this style:

- **High verbosity / redundancy**: the receiver type is already known from the `impl` header, but must be repeated.
- **High churn**: renaming a type or changing impl generics forces touching every method signature.
- **Mismatch with interface declarations**: `interface` method signatures already omit the receiver; impls must “re-introduce” it.
- **Receiver mutability is clunky**: expressing “this method does not mutate” requires typing-level `readonly` on the receiver parameter.
- **No explicit static/instance boundary**: “receiver-less method inside `impl`” is currently just “a function that happens not to take a receiver”; the language surface does not say so directly.

The proposed changes address these with a small number of syntactic and typing rules.

---

## Design goals

1. **Erase boilerplate**: no `self: Type` in method definitions.
2. **Keep method calls familiar**: `x.m(...)`, `I::m(x, ...)`, and `T::m(...)` remain valid shapes.
3. **Make mutability intent explicit**: `readonly fn` communicates “this does not mutate `self`”.
4. **Enable interface evolution**: default methods allow adding behavior without forcing every impl to immediately update.
5. **Make static methods explicit**: `static fn` clearly means “no receiver”.

---

## Non-goals (initial scope)

- Adding Rust-like ownership/borrowing (`&self` / `&mut self`); this proposal sticks to Rusk’s `readonly` view model.
- Adding `super` / “call the default implementation” syntax.
- Allowing static methods in interfaces (explicitly forbidden by this proposal).
- Introducing ad-hoc overloading or signature-based dispatch.

---

## Guide-level explanation

### 1) Instance methods: receiver is implicit, `self` is always in scope

In an `impl` block, a method definition no longer includes the receiver parameter:

```rusk
struct Point { x: int, y: int }

impl Point {
  fn sum() -> int { self.x + self.y }
}
```

Calling stays the same:

```rusk
let p = Point { x: 1, y: 2 };
p.sum()
```

### 2) `readonly fn`: `self` is readonly inside the method

Use `readonly fn` when a method requires `self` to be treated as a readonly view:

```rusk
impl Point {
  readonly fn sum() -> int { self.x + self.y }
}
```

Inside `readonly fn`, attempts to mutate through `self` are rejected by the typechecker (and remain protected at runtime by the existing readonly-view mechanism).

**Call ergonomics (proposed)**:
- `readonly fn` is callable on both `Point` and `readonly Point` receivers (the call site may implicitly create a readonly view when needed).
- A non-readonly instance method is *not* callable on a `readonly T` receiver.

This matches the intuition that “mutable can be used where readonly is required”, but not vice-versa.

### 3) `static fn`: no receiver, callable only as `Type::method(...)`

Static methods are receiver-less inherent impl members and must be marked with `static`:

```rusk
impl Point {
  static fn origin() -> Point { Point { x: 0, y: 0 } }
}

let p = Point::origin(); // ok
```

Static methods cannot be called with a value receiver:

```rusk
let p = Point::origin();
p.origin(); // error: static methods must be called as `Point::origin()`
```

### 4) Default methods in interfaces

Interfaces may provide default implementations:

```rusk
interface Add {
  fn add(n: int) -> int;

  readonly fn add_twice(n: int) -> int {
    self.add(n) + self.add(n)
  }
}
```

An impl may omit methods that have defaults:

```rusk
impl Add for Point {
  fn add(n: int) -> int { self.sum() + n }
  // add_twice omitted => uses default
}
```

---

## Reference-level design

### A) Surface syntax

#### A.1 Keywords / reserved identifiers

- Add `static` as a keyword (or contextual keyword limited to `impl` members).
- Reserve `self` as an identifier **inside instance method bodies**:
  - In an instance method body, `self` always refers to the receiver.
  - `self` cannot be shadowed by locals, parameters, patterns, or `let` bindings.
  - In a `static fn` body, `self` is not in scope.

Whether `self` becomes a fully reserved keyword everywhere or a contextual keyword is an implementation choice; the minimal rule required by this proposal is “cannot be declared/shadowed where it denotes the receiver”.

#### A.2 Grammar changes (EBNF sketch)

Current spec models `ImplMember := FnItem`. This proposal splits impl members into method items:

```ebnf
InterfaceMember :=
    ("readonly")? "fn" Ident GenericParams? "(" ParamList? ")" ReturnType?
    ( ";" | Block ) ;

ImplMember :=
    MethodDef ;

MethodDef :=
    MethodQualifiers? "fn" Ident GenericParams? "(" ParamList? ")" ReturnType? Block ;

MethodQualifiers :=
    ("readonly" | "static") ;
```

Additional constraints:

- `static` is allowed **only** in inherent impls: `impl Type { ... }`.
- `static` is forbidden in `impl Interface for Type { ... }`.
- `static` is forbidden in `interface { ... }` items.
- `readonly static fn` (both qualifiers together) is rejected as meaningless.

### B) Desugaring and internal function signatures

Although the receiver is not written, the compiler/typechecker still treats instance methods as functions whose **first parameter is the receiver**.

#### B.1 Inherent impl methods

Given:

```rusk
impl Point {
  fn sum() -> int { ... }
}
```

Desugar (conceptually) to an internal function signature:

```text
Point::sum(self: Point) -> int
```

For `readonly fn`, the receiver parameter is a readonly view:

```text
Point::sum(self: readonly Point) -> int
```

For `static fn`, **no receiver is inserted**:

```text
Point::origin() -> Point
```

#### B.2 Interface impl methods

Given:

```rusk
impl Add for Point {
  fn add(n: int) -> int { ... }
}
```

Desugar to:

```text
impl::Add::for::Point::add(self: Point, n: int) -> int
```

`readonly fn` similarly yields `self: readonly Point`.

`static fn` is rejected in this context.

### C) Method call resolution and typing

This proposal does **not** change the high-level *name resolution* order:

1. Inherent methods on the receiver’s nominal type
2. Interface methods (if unambiguous)
3. Otherwise: error

It does change what “counts” as a method candidate:

- **Dot calls** (`recv.m(args...)`) consider **only instance methods** (methods with a receiver).
- **Static methods** are never considered by dot-call resolution.

#### C.1 Calling a readonly method on a mutable receiver

When a method is declared `readonly fn`, calls are permitted on a non-readonly receiver by implicitly producing a readonly view for the duration of the call:

```rusk
let p = Point { x: 1, y: 2 };
p.sum(); // ok even if `sum` is `readonly fn`
```

This is equivalent to (conceptually):

```rusk
readonly tmp = p;
tmp.sum()
```

#### C.2 Calling a mutable method on a readonly receiver

A non-readonly instance method call is rejected when the receiver expression has type `readonly T`:

```rusk
readonly p = Point { x: 1, y: 2 };
p.mutate(); // error if `mutate` is not `readonly fn`
```

### D) Default interface methods

#### D.1 Declaration forms

An interface method is either:

- **required** (ends in `;`), or
- **defaulted** (has a body block).

`readonly fn` may be used for either form.

#### D.2 Implementation obligations

For `impl I for T { ... }`:

- Every **required** method in `I`’s full method set (including inherited methods) must be implemented.
- A **defaulted** method may be omitted; if omitted, calls dispatch to the interface’s default body.
- A defaulted method may be overridden by providing a method body in the impl.

#### D.3 Inheritance interaction (initial rule)

Rusk identifies interface methods by **(origin interface, method name)**.

To keep this canonical-id system simple, this proposal adopts the initial rule:

- Default bodies attach to the method’s **origin interface**.
- Subinterfaces do not “override” inherited method defaults without redeclaring a new method (which would be a *different* canonical method id).

In other words: if `J: I`, and `I` provides a default for `I::foo`, then `J` inherits that default for the same canonical method `I::foo`.

Allowing subinterfaces to override inherited defaults is left as a future extension because it interacts with canonical method ids, ambiguity rules, and diamond inheritance.

### E) Static methods: additional restrictions

To satisfy “static methods are only type-qualified”:

- `static fn` can only be invoked via a type path: `Type::method(...)`.
- If a call is written `value.method(...)` and `method` resolves to a static method, that is an error (even if the value’s static type is known).

To satisfy “static methods only in the type’s own impl”:

- Static methods are allowed only in inherent impls `impl Type { ... }`.
- Static methods are disallowed in interface impl blocks `impl I for Type { ... }`.

---

## Drawbacks

- **More implicitness**: `self` is “magically” available in method bodies, and the receiver is not visible in the signature.
- **Keyword pressure**: reserving `self` (and adding `static`) can break code that used those identifiers.
- **Default methods add complexity**: interface evolution becomes easier, but the compiler model must support “missing impl method => use default body”.

---

## Rationale and alternatives

### Alternative 1: Keep a receiver parameter, but infer its type

Instead of removing the receiver parameter entirely, allow:

```rusk
impl Point {
  fn sum(self) -> int { self.x + self.y }
}
```

Pros: explicit receiver presence; avoids reserving `self` as strongly.
Cons: still noisy; still special-cases a parameter position; less consistent with interface signatures (which currently omit receiver entirely).

### Alternative 2: Make methods readonly by default

Flip the default so `fn` is readonly and `mut fn` (or similar) is required for mutation.

Pros: encourages immutability.
Cons: larger ecosystem churn; doesn’t match current Rusk conventions where mutability is opt-in via `readonly` views.

### Alternative 3: No `static` keyword; infer from absence of receiver

This is how things effectively work today (receiver-less impl members exist).

Pros: fewer keywords.
Cons: with an implicit receiver model, “no parameters” would become ambiguous (instance method with zero args vs static method), and accidental dot-callability becomes harder to reason about.

---

## Prior art

- **Rust**: explicit receiver parameter in method signatures (`self`, `&self`, `&mut self`), default trait methods, and associated functions (`fn new()` in `impl` blocks).
- **C++/Java/C#**: implicit receiver (`this`), explicit `static` methods, default interface methods (Java).
- **Swift**: implicit `self` in many contexts, explicit when capturing in closures; separate `static` members.

Rusk’s key difference is modeling receiver mutability via **readonly views** rather than borrow types.

---

## Unresolved questions

1. **Is `self` globally reserved, or contextual?**
   - Minimal: reserve it only as the implicit receiver name in instance method bodies.
   - Stricter: make it a full keyword everywhere (simplifies parser/lexer; more breaking).
2. **Exact coercion rules for readonly receivers**
   - This proposal recommends allowing `T` to flow to `readonly T` specifically for readonly-method receivers.
   - Should similar coercions be generalized to argument passing (non-receiver positions)?
3. **Default-method “super calls”**
   - Should there be a way to call an interface’s default body from an override?
4. **Default methods in diamond inheritance**
   - With the “origin interface owns the default body” rule, diamonds are straightforward, but subinterface overriding is not addressed.

