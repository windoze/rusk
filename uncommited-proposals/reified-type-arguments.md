# Reified type arguments (non-erased generics runtime model)

NOTE: This document is superseded by `proposals/generic-rework.md` and is kept for history.

Status: **Draft (proposed)** — intended to be implemented **before** `proposals/generic-bound.md`.

This proposal introduces **reified type arguments**: generic type arguments of kind `Type` are
available at runtime as internal “type representations” (`TypeRep`), rather than being erased.

Reified type arguments are primarily motivated by the post-v0.4 roadmap around:

- generic interfaces + effects (`proposals/generic-interface-and-others.md`), and
- multiple-bounds generics (`proposals/generic-bound.md`).

The main benefit is that runtime mechanisms that must make distinctions between instantiated types
can do so soundly and precisely (e.g. `I<int>` vs `I<string>`), without requiring full
monomorphization.

---

## Background (what exists today)

In v0.4:

- Generic type arguments are **erased at runtime** (`RUSK_SPEC.md` currently states this).
- Dynamic dispatch is keyed by `(dynamic_receiver_type, canonical_method_id)` (see
  `completed_proposals/DYN_DISPATCH.md`).
- Runtime type tests / checked casts (`is` / `as?`) intentionally reject targets with generic
  arguments because the runtime cannot distinguish them reliably.
- Interfaces also act as effect namespaces; effect identity is currently monomorphic (interface
  name + method name).

These constraints are coherent for v0.4, but they become a bottleneck once we want:

- instantiated interface types as first-class types (`I<T>`),
- typed effects expressed as generic interfaces (`Yield<T>`, `Throw<E>`, …), and
- “generic virtual methods” (dynamic dispatch + generic interface methods) with predictable
  semantics.

This proposal introduces a runtime foundation that can support those features.

---

## Summary

### Core idea

Every type argument of kind `Type` that is passed to a generic function/method is also passed at
runtime as an internal `TypeRep` value.

Conceptually, a generic item:

```rusk
fn id<T>(x: T) -> T { x }
```

is compiled to a runtime function that receives an extra argument:

```text
fn id(type_T: TypeRep, x: Value) -> Value
```

The source language does not necessarily expose `TypeRep` as a user-visible type; it can remain
internal, used only by:

- dynamic dispatch,
- effect identity + handler matching,
- runtime type tests/casts (`is` / `as?`), and
- (optionally) future reflection-like features.

### Reification is not specialization

Reified type arguments enable runtime distinctions, but they do **not** require (or imply):

- conditional impl selection (a.k.a. “partial specialization”),
- overlap resolution beyond what the coherence rules permit, or
- selecting different impl bodies based on method type arguments.

Those are separate features and should be staged explicitly.

---

## Goals

1. **Enable runtime distinctions for instantiated types**:
   - `Foo<int>` is distinguishable from `Foo<string>` at runtime when necessary.
2. **Make generic interfaces + effects implementable**:
   - effect identity can incorporate interface instantiation args (`@Yield<int>.yield` is distinct
     from `@Yield<string>.yield`).
3. **Support generic virtual methods**:
   - dynamically-dispatched interface method calls can pass method type arguments at runtime.
4. **Keep staging discipline**:
   - allow an initial implementation that does *not* introduce specialization-like behavior.

---

## Non-goals (initial stage)

- Exposing full runtime reflection to user code (no `typeof(T)` / `Type<T>` surface required yet).
- Reifying higher-kinded type constructor parameters (`F<_>`) at runtime.
- Specialization / conditional impls (e.g. `impl I<int> for S` and `impl I<string> for S` both
  applying to the same `S`) unless explicitly adopted later with a coherence model.
- Making a particular optimization strategy mandatory (monomorphization remains optional).

---

## Proposed design

### 1) Runtime type representations (`TypeRep`)

Introduce an internal runtime value representing a type:

- `TypeRep` is **comparable** and **hashable** (at least internally).
- `TypeRep` is **canonicalized** (interned) so equality checks are cheap and stable.

Types that should have a `TypeRep` in the initial stage:

- primitives (`int`, `bool`, …)
- nominal types (structs/enums/interfaces), including **applied** types:
  - `Vec<int>`
  - `Result<string, int>`
  - `I<int>`

Types that are *not required* to be reified initially:

- higher-kinded type constructors (`F<_>`)
- function/continuation types (may be added later if useful)

Readonly view types (`readonly T`) should *not* be a distinct runtime type; a `TypeRep` represents
the underlying runtime type, while `readonly` remains a compile-time view/type system feature.

### 2) Passing type arguments at runtime

For each generic parameter of kind `Type` on a function/method, the compiler passes an internal
`TypeRep` argument at runtime.

This applies to:

- generic function calls
- generic interface method calls (static and dynamically-dispatched)
- effect operations that mention instantiated interface types

We do **not** propose passing `TypeRep` for:

- higher-kinded generic parameters (`F<_>`) in the initial stage (reject or erase those uses where
  runtime needs them).

### 3) Dynamic dispatch with generic interfaces (and generic interface methods)

This section targets the “generic virtual method” question.

#### 3.1 Dispatch key should not depend on method type arguments

For a dynamically-dispatched call:

```rusk
// receiver has static type `I<T>` (or `I<int>`, etc)
x.method<U>(args...)
```

the selected implementation should depend on:

- the receiver’s **dynamic concrete type**, and
- the interface method’s **canonical identity** (origin interface + method name),
  
and **must not** depend on the method’s type arguments `U`.

Rationale: if method type arguments participate in dispatch selection, the language is no longer
“parametric generics + dynamic dispatch”; it becomes a form of specialization/overloading on type
arguments, which has coherence and predictability implications and should be staged explicitly.

Instead, method type arguments are treated as extra runtime parameters *passed to* the selected
implementation.

#### 3.2 What runtime data flows in a virtual generic call?

Lower a virtual call to a `vcall`-like runtime operation that carries:

- the receiver value,
- a canonical method id (`(origin_interface_fqn, method_name)`),
- the instantiated **interface type arguments** (for `I<T>` this is the list `[T]`),
- the instantiated **method type arguments** (for `method<U>` this is the list `[U]`),
- the ordinary value arguments.

Conceptual lowering:

```text
vcall(
  recv,
  method_id = (origin_iface, "method"),
  iface_type_args = [type_T],
  method_type_args = [type_U],
  args = [ ... ]
)
```

The runtime dispatch table returns a single implementation function (based on dynamic receiver
type + method id), and that function receives the `TypeRep`s as hidden parameters.

#### 3.3 Do interface instantiation args participate in dispatch selection?

This is the key staging choice that separates “reification” from “specialization”.

**Initial stage (recommended): interface args do *not* participate in dispatch selection.**

- Dispatch key remains:
  - `(dynamic_receiver_type, canonical_method_id)`
- Interface instantiation args are still passed to the implementation as hidden parameters, but
  they do not change which impl body is chosen.

This requires a coherence restriction consistent with the “no specialization yet” direction:

- allow only “forall instantiations” impl headers, e.g. `impl<T> I<T> for S<T> { ... }`
- reject “specialized” impl headers like `impl I<int> for S { ... }` in the initial stage

**Later stage (optional): include interface args in the dispatch key.**

If we ever want multiple impls that differ only by interface type args to coexist, then reified
type args make it possible to key dispatch by:

- `(dynamic_receiver_type, interface_type_args, canonical_method_id)`

However, this becomes a conditional-impl / specialization-like feature and needs a full overlap
resolution model. This proposal does not define that model.

---

## Effects: type-precise operation identity

Reified type arguments are especially valuable for effects because effect identity is a *runtime*
lookup problem (handler matching).

### Proposed effect identity (post-v0.4)

When generic interfaces are supported (see `proposals/generic-interface-and-others.md`), allow
effect operations to name an instantiated interface type:

```rusk
@Yield<int>.yield(42)
@Throw<string>.throw("err")
```

With reified type arguments, the runtime can treat these as distinct operations by defining effect
identity as:

```text
(origin_interface_fqn, interface_type_args, method_name)
```

So `@Yield<int>.yield` is distinct from `@Yield<string>.yield` because the `interface_type_args`
list differs.

Notes:

- This proposal does not require making method-generic parameters part of effect identity; the
  common “typed effects” encoding uses generic *interfaces* (`Yield<T>`) rather than generic effect
  methods. If we later add method-generic effect calls, we can revisit whether method type args
  must be included in the effect id.

---

## Runtime type tests and checked casts (`is`, `as?`)

With reified type arguments, the runtime can soundly check instantiated nominal/interface types,
so we can relax the v0.4 restriction that targets must be monomorphic.

Examples that become runtime-checkable:

```rusk
let x: Foo<int> = ...;
if x is Foo<int> { ... }

let y: I<string> = ...;
match y as? I<string> { ... }
```

Design notes:

- `readonly` should remain disallowed as an `is` / `as?` target (it is a view, not a runtime type).
- For interface casts/tests, the check is about “implements this instantiated interface type”.
  Under the initial-stage coherence restriction (no specialization), this likely reduces to the
  same check as “implements the interface definition”, but the type arguments are still available
  for later stages and for effect identity consistency.

---

## Higher-kinded type parameters (HKTs)

Rusk supports limited HKTs at the type level (`F<_>`), but this proposal intentionally does **not**
reify HKTs at runtime in the initial stage.

Constraints:

- `TypeRep` is only required for type arguments of kind `Type` (arity 0).
- Features that would require runtime HKTs (e.g. passing a type constructor as a runtime value) are
  rejected or deferred.

This matches the staging decision in `proposals/generic-bound.md` to reject bounds on HKTs for now.

---

## Alternatives considered

### A) Monomorphization

Compile a specialized copy of each generic function/method for each concrete instantiation.

Pros:

- Often best runtime performance (static calls, inlining, constant-prop).
- No runtime representation of types required.

Cons:

- Compile time and code size can grow quickly for generic-heavy code.
- Harder to combine with “script → MIR → interpret” workflows unless specialization is staged and
  cached carefully.
- Separate compilation / dynamic loading becomes more complex.

### B) Reified type arguments (this proposal)

Compile a single generic body and pass type arguments at runtime as `TypeRep` values.

Pros:

- Avoids code-size blowups; single body per generic item.
- Makes it possible for runtime mechanisms to distinguish instantiations (key for typed effects and
  more precise runtime casts/tests).
- Naturally supports “generic virtual methods” by passing method type args into the dispatched
  implementation.

Cons:

- Runtime overhead (type passing + possible dictionary/vtable lookups).
- Requires runtime support for canonical type representations and equality/hashing.
- Needs a careful boundary between “internal type reps” and user-visible reflection to preserve a
  predictable, sound language model.

---

## Implementation sketch (repo-aligned, non-normative)

This is a deliberately high-level sketch; exact details depend on MIR and interpreter design.

1. **Runtime representation**
   - Add a runtime value kind for `TypeRep` (interned).
   - Add constructors for applied types (nominal type id + list of arg `TypeRep`s).

2. **Compiler lowering**
   - Extend generic function/method calls to pass hidden `TypeRep` arguments.
   - Extend dynamically-dispatched calls (`vcall`) to additionally carry:
     - interface instantiation args
     - method instantiation args

3. **Effects**
   - Extend effect call/handler matching to incorporate interface instantiation args in the effect
     operation id.

4. **`is` / `as?`**
   - Allow runtime-checking of instantiated nominal and interface types by comparing `TypeRep`
     (including arguments).

5. **Spec updates**
   - Update `RUSK_SPEC.md` to remove/adjust the “types are erased at runtime” statement and define
     what runtime distinctions are observable (via `is` / `as?` / effect identity / dispatch).

---

## Open questions

1. **User-visible reflection**:
   - Should `TypeRep` remain completely internal, or do we add a controlled surface (e.g. a `Type<T>`
     value) later?

2. **Which type forms are representable**:
   - Do we reify function types, tuples, arrays, `readonly` (probably not), etc.?

3. **Coherence vs specialization**:
   - Do we permanently keep “forall instantiations only” impl headers, or do we later add a model
     for specialized impl selection keyed by interface type args?

4. **Dictionary passing for bounded generics**:
   - Once `proposals/generic-bound.md` is implemented, do bounded generics use:
     - dynamic lookup by `(TypeRep, interface)` to obtain a dictionary, or
     - compile-time-selected dictionaries passed as hidden parameters?
