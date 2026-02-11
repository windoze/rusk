# Generics rework: generic interfaces, reified type arguments, and bounds

Status: **Draft (consolidated proposal)** — supersedes:

- `proposals/generic-interface-and-others.md`
- `proposals/reified-type-arguments.md`
- `proposals/generic-bound.md`

This document merges the three proposals above into a single coherent design, since they are
deeply coupled (generic interfaces ↔ effects ↔ dispatch ↔ generics runtime model ↔ bounds).

---

## Background (v0.4 current behavior)

In v0.4, the compiler and runtime model are intentionally restrictive:

- Generic type arguments are **erased at runtime** (per current `RUSK_SPEC.md` text).
- Dynamic dispatch (`vcall`) is keyed by `(dynamic_receiver_type, canonical_method_id)` (see
  `completed_proposals/DYN_DISPATCH.md`).
- Runtime type tests / checked casts (`is` / `as?`) reject targets with generic arguments because
  the runtime cannot distinguish them soundly under erasure.
- Interfaces also act as effect namespaces; effect identity is monomorphic (interface name +
  method name).

The compiler currently rejects (with explicit errors):

- generic interfaces: `interface I<T> { ... }`
- generic interface methods: `interface I { fn foo<U>(...) -> ...; }`
- generic methods inside interface impls: `impl I for S { fn foo<U>(...) { ... } }`
- type arguments in generic constraints and super-interface references

These restrictions are coherent for v0.4, but they block a set of post-v0.4 features that want
generic interfaces as first-class types and effects as generic namespaces (e.g. `Yield<T>`).

---

## Summary

This proposal introduces a staged “generics rework” that, in its initial complete form, provides:

1. **Reified type arguments (runtime foundation)**:
   - Generic type arguments of kind `Type` are available at runtime as an internal `TypeRep`.
   - Generic calls (including virtual calls and effects) pass hidden `TypeRep` parameters.

2. **Generic interfaces (and generic interface methods)**:
   - Allow interface types `I<T>` in the type system, in bounds, and in casts.
   - Allow generic interface methods `fn m<U>(...) -> ...` and implement them in `impl`.
   - Preserve a “no specialization yet” coherence rule (initial stage).

3. **Typed effects via generic interfaces**:
   - Extend effect call/arm syntax to accept instantiated interface types:
     - `@I<T>.foo(...)` and `@I.foo(...)` (with inference)
   - Define effect identity to include instantiated interface type arguments:
     - `(origin_interface_fqn, interface_type_args, method_name)`

4. **Generic bounds (multi-interface constraints)**:
   - Allow `T: I + J + K` bounds on generic parameters (initially for `fn` generics).
   - Allow method-call sugar under bounds only when resolution is unique by canonical method id;
     reject ambiguity in the initial stage (disambiguation syntax deferred).

Key staging decisions:

- This proposal **does not introduce specialization / conditional impl selection** in the initial
  stage. In particular, impls that differ only by concrete interface type arguments are rejected.
- Higher-kinded type constructor parameters (`F<_>`) are **not reified at runtime initially**, and
  bounds on HKTs are rejected initially.

---

## Goals

1. Support generic interfaces as first-class types: `I<T>`, bounds, and `impl`.
2. Provide a runtime model that can soundly distinguish instantiated types where needed:
   - `Foo<int>` vs `Foo<string>`, `I<int>` vs `I<string>`.
3. Make typed effects expressible using generic interfaces:
   - `Yield<T>`, `Throw<E>`, `Await<T>`, etc.
4. Support predictable dynamic dispatch for generic interfaces and generic interface methods.
5. Add multiple-interface bounds for ergonomic, composable generic APIs.
6. Keep staging discipline: deliver a coherent initial feature set without implying specialization.

---

## Non-goals (initial stage)

- Specialization / conditional impls / overlap models beyond current coherence:
  - no “pick impl body based on type args”
  - no `impl I<int> for S { ... }` coexisting with `impl I<string> for S { ... }`
- Mandatory monomorphization (still optional as an optimization strategy, not a semantic feature).
- Reifying higher-kinded type parameters (`F<_>`) at runtime.
- User-visible reflection APIs (e.g. `typeof(T)`, `Type<T>`) in the initial stage.
- Disambiguation syntax for ambiguous method names across multiple bounds (deferred).

---

## Guide-level design

### Surface syntax overview (Rusk)

This document uses **Rusk** surface syntax (not Rust):

- `struct` items always use `{ ... }` (no `struct S;`).
- Interface member signatures do **not** include the receiver; impl methods do.
- There is no `&self` / `mut self` — the receiver is the first parameter (`self: Type`), optionally
  `readonly self: Type`.

### Generic interfaces

```rusk
interface I<T> {
    fn foo() -> T;
}

struct GS<T> { value: T }

impl<T> I<T> for GS<T> {
    fn foo(self: GS<T>) -> T { self.value }
}
```

### Generic methods on interfaces

```rusk
interface Pair<T> {
    fn pair<U>(u: U) -> (T, U);
}

struct GS<T> { value: T }

impl<T> Pair<T> for GS<T> {
    fn pair<U>(self: GS<T>, u: U) -> (T, U) { (self.value, u) }
}
```

### Typed effects via generic interfaces

```rusk
interface Yield<T> { fn yield(value: T) -> unit; }

fn gen_one_infer<T>(x: T) -> unit { @Yield.yield(x) }
fn gen_one_explicit<T>(x: T) -> unit { @Yield<T>.yield(x) }
```

### Multi-interface bounds

```rusk
interface Hash { fn hash() -> int; }
interface Show { fn show() -> string; }

fn show_and_hash<T: Hash + Show>(x: T) -> (string, int) {
    (x.show(), x.hash())
}
```

---

## Core design

### 1) Reified type arguments (`TypeRep`) — runtime foundation

We introduce an internal runtime value representing a type:

- `TypeRep` is comparable and hashable (at least internally).
- `TypeRep` is canonicalized/interned so equality is cheap and stable.

In the initial stage, `TypeRep` must exist for:

- primitives (`int`, `bool`, …)
- nominal types (structs/enums/interfaces), including applied types:
  - `Vec<int>`
  - `Result<string, int>`
  - `I<int>`

Types not required to be reified initially:

- higher-kinded type constructors (`F<_>`)
- function/continuation types (may be added later if useful)

Readonly view types (`readonly T`) are not distinct runtime types; `readonly` remains a compile-time
view/type-system feature.

#### Passing type arguments at runtime

For each generic parameter of kind `Type` on a function/method, the compiler passes a hidden
`TypeRep` argument at runtime.

Conceptually:

```rusk
fn id<T>(x: T) -> T { x }
```

lowers to a runtime function like:

```text
fn id(type_T: TypeRep, x: Value) -> Value
```

This applies to:

- generic function calls
- generic interface method calls (static and dynamically-dispatched)
- effect operations that mention instantiated interface types

---

### 2) Generic interfaces — typing + coherence

Generic interface types `I<T>` are allowed in:

- type positions (values, fields, locals)
- bounds (`T: I<int>`)
- super-interface references (`interface J<T>: I<T> { ... }`)
- casts/tests (`as I<int>`, `is I<int>`, `as? I<int>`) once runtime support exists

#### Coherence model (initial stage: “no specialization yet”)

To keep semantics predictable and avoid an overlap/specialization model, we impose a coherence
restriction consistent with both erased and reified generics:

- Allow only “forall instantiations” impl headers where the interface type arguments are the impl’s
  own type parameters (in order), e.g.:
  - `impl<T> I<T> for GS<T> { ... }`
- Reject “specialized” impl headers such as:
  - `impl I<int> for S { ... }`
  - `impl I<string> for S { ... }`

This restriction ensures there is never more than one applicable impl that differs only by
interface type arguments, and it keeps dispatch semantics independent of specialization.

Later stages may relax this only if we adopt a full conditional-impl / overlap resolution model.

---

### 3) Dynamic dispatch for generic interfaces (and generic interface methods)

Generic interfaces should be usable as value types (like monomorphic interfaces today) and should
trigger runtime dynamic dispatch when the static receiver type is an interface.

Example:

```rusk
interface I<T> { fn foo() -> T; }
struct GS<T> { value: T }
impl<T> I<T> for GS<T> { fn foo(self: GS<T>) -> T { self.value } }

fn use_interface<T>(x: I<T>) -> T { x.foo() }
```

#### Canonical method identity

As in the existing dynamic dispatch proposal, interface methods have a canonical identity:

- `(origin_interface_fqn, method_name)`

If `J<T>: I<T>` and `foo` is declared in `I<T>`, then `foo`’s canonical identity is `I::foo` even
when accessed through `J`.

#### Dispatch selection rule (initial stage)

For a dynamically-dispatched call where the receiver is interface-typed, the selected implementation
depends on:

- the receiver’s **dynamic concrete type**, and
- the method’s **canonical identity**,

and must **not** depend on:

- the method’s type arguments, nor
- the interface instantiation type arguments,

in the initial stage.

Rationale: allowing type-argument-dependent dispatch selection is specialization/overloading. We
explicitly do not introduce that in the initial stage.

#### Runtime data flow in a virtual generic call

Lower a virtual call to a `vcall`-like operation that carries:

- the receiver value
- a canonical method id (`(origin_interface_fqn, method_name)`)
- the instantiated **interface type arguments** (as `TypeRep` list)
- the instantiated **method type arguments** (as `TypeRep` list)
- the ordinary value arguments

Conceptual lowering:

```text
vcall(
  recv,
  method_id = (origin_iface, "method"),
  iface_type_args = [type_T, ...],
  method_type_args = [type_U, ...],
  args = [ ... ]
)
```

The runtime dispatch table returns a single implementation function based on:

- `(dynamic_receiver_type, canonical_method_id)`

and that function receives the `TypeRep`s as hidden parameters.

Later stage (optional): include interface args in the dispatch key:

- `(dynamic_receiver_type, interface_type_args, canonical_method_id)`

This is only valid if we also adopt a coherent specialization model. This proposal does not define
that model.

---

### 4) Effects with generic interfaces

Effects are a key enabler for desugaring higher-level features like:

- generators (typed `yield<T>`)
- try/catch (typed `throw<E>`)
- async/await (typed `await<T>`)

These encodings want effects to carry type parameters.

#### Syntax: allow instantiated interface types in effect calls/arms

Support effect calls and handler arms with an instantiated interface type on the left side:

```rusk
@I<int>.foo(...)
```

If the interface type arguments can be inferred, they may be omitted:

```rusk
@I.foo(...)
```

and similarly in effect arms:

```rusk
match expr {
    @I<int>.foo(x) => resume(...)
    v => v
}
```

This is sufficient to express typed effects like `Yield<T>`, `Throw<E>`, `Await<T>` while keeping
operation signatures precise.

Method-generic effect call syntax such as `@I.foo<U>(...)` is explicitly deferred; we can revisit
it once we have experience with interface-instantiation effect typing.

#### Type-argument inference

For `@I.foo(args...)` where `I` is generic, treat missing interface type arguments as inference
variables and unify against the selected operation signature to solve them (same spirit as generic
argument inference for normal calls; see §8.5 of `RUSK_SPEC.md`).

If inference fails (no solution or multiple ambiguous solutions), require explicit type arguments
using `@I<T>.foo(...)`.

#### Effect identity (type-precise operations)

With reified type arguments, effect identity is defined as:

```text
(origin_interface_fqn, interface_type_args, method_name)
```

Consequences:

- `@I<int>.foo(...)` is a different operation than `@I<string>.foo(...)`.
- If `J<T>: I<T>` and `foo` originates in `I<T>`, then `@J<int>.foo(...)` is treated as the same
  operation as `@I<int>.foo(...)` (origin interface canonicalization + same instantiated args).

#### Grammar note

To support `@I<T>.foo(...)`, the effect-call grammar must accept generic arguments on the interface
path. A simple approach is to parse the callee as a type-path rather than a value-path:

- Change effect calls from `@ PathExpr . Ident ( ... )` to `@ PathType . Ident ( ... )`.
- Apply the same idea to effect arms.

---

### 5) Runtime type tests and checked casts (`is`, `as?`)

With reified type arguments, the runtime can soundly check instantiated nominal/interface types, so
we can relax the v0.4 restriction that targets must be monomorphic.

Examples that become runtime-checkable:

```rusk
let x: Foo<int> = ...;
if x is Foo<int> { ... }

let y: I<string> = ...;
match y as? I<string> { ... }
```

Notes:

- `readonly` remains disallowed as an `is` / `as?` target (it is a view, not a runtime type).
- For interface casts/tests, the check is “implements this instantiated interface type”. Under the
  initial-stage coherence restriction (no specialization), this often reduces to “implements the
  interface definition”, but we keep the type arguments available for consistency with effect
  identity and future stages.

---

### 6) Generic bounds (multi-interface constraints)

This section specifies how generic parameters can be constrained by one or more interface bounds,
so that generic code can call interface methods in a type-safe way.

#### Surface syntax

Allow a `+`-separated list of interface bounds:

```rusk
fn f<T: I + J>(x: T) -> int { x.foo() }
```

Reference grammar sketch:

```text
GenericParam := Ident HktArity? Bounds? ;
Bounds       := ":" InterfaceType ("+" InterfaceType)* ;
```

Where `InterfaceType` is a type expression resolving to an interface type:

- `I`
- `I<T>`
- `core::iter::Iterator<int>`

#### Initial restrictions

1. Bounds apply only to arity-0 type parameters:
   - `T: I + J` is allowed
   - `F<_>: I` is rejected for now (defer)
2. Each bound element must resolve to an interface type.
3. Duplicate bounds are allowed syntactically but are deduplicated semantically.

#### Constraint satisfaction

Given `T: I<A, ...> + J<B, ...>`, an instantiation `T := S` is valid if `S` implements every bound
interface type.

The precise meaning of “implements `I<A, ...>`” depends on the chosen coherence model. Under this
proposal’s initial-stage coherence rule, the type checker must be able to decide the predicate, and
runtime mechanisms must have enough information to support dynamic behavior (dispatch/effects/tests)
consistently.

#### Bound inheritance / implication

If an interface bound is a subinterface of another, it implies the super bound. For example:

```rusk
interface I { fn foo() -> int; }
interface J: I { fn bar() -> int; }

fn g<T: J>(x: T) -> int { x.foo() } // ok: J implies I
```

This implication also participates in ambiguity elimination (diamond duplication is not ambiguity).

#### Method-call typing under bounds (ambiguity rule)

For a receiver expression `x` of type `T` with bounds `T: B1 + ... + Bn`, a method-call sugar:

```rusk
x.m(args...)
```

is accepted iff:

1. Collect all methods named `m` available from each bound `Bi` (including inherited methods).
2. Deduplicate candidates by canonical method identity:
   - `(origin_interface_fqn, method_name)`
3. If empty: error “unknown method”.
4. If more than one canonical method remains: error “ambiguous method” (initial stage).
5. Otherwise, typecheck against the unique selected signature.

Disambiguation syntax (“fully-qualified call/impl”) is explicitly deferred.

#### Where bounds are allowed (staging)

Initial stage:

- Allow bounds on `fn` generic parameters.

Explicitly deferred:

- Bounds on `impl` generics:
  - `impl<T: I> J for Wrap<T>` is a conditional impl / partial specialization feature, not merely
    “a bound for using methods inside a body”. We should not accept this without a clear model for
    conditional impl selection.

Open for later:

- Bounds on `struct` / `enum` / `interface` generics (and when they are enforced).

---

## Implementation sketch (repo-aligned, non-normative)

1. **Runtime support for `TypeRep`**
   - Add a runtime value kind for `TypeRep` (interned).
   - Add constructors for applied types (nominal type id + list of arg `TypeRep`s).

2. **Compiler lowering**
   - Extend generic calls to pass hidden `TypeRep` arguments.
   - Extend `vcall` to additionally carry:
     - interface instantiation args (`TypeRep` list)
     - method instantiation args (`TypeRep` list)

3. **Effects**
   - Extend effect call/handler matching to incorporate interface instantiation args in the effect
     operation id.
   - Implement `@PathType.method(...)` parsing and typing.

4. **`is` / `as?`**
   - Allow runtime-checking of instantiated nominal and interface types by comparing `TypeRep`
     (including arguments).

5. **Spec updates**
   - Update `RUSK_SPEC.md` to remove/adjust the “types are erased at runtime” statement and define:
     - what runtime distinctions are observable (via `is` / `as?` / effect identity / dispatch)
     - the generic-interface + bounds typing rules
   - Update `MIR_SPEC.md` as needed for new runtime intrinsics/ops (`TypeRep`, extended `vcall`,
     extended effect ids).

---

## Staging plan (recommended)

This is a suggested ordering; it is not a normative requirement, but it is intended to keep the
language coherent at every intermediate step.

1. **Reified type arguments foundation**
   - `TypeRep` runtime support
   - hidden `TypeRep` parameters in generic calls
   - spec updates removing “runtime erasure” as a blanket statement

2. **Generic interfaces + coherence restrictions**
   - accept `interface I<T>`
   - accept `impl<T> I<T> for S<T>`
   - accept generic methods in interfaces and their impls

3. **Dynamic dispatch update**
   - extend `vcall` lowering to carry interface/method type args (without changing dispatch key)

4. **Typed effects syntax + runtime identity**
   - parse/typecheck `@I<T>.foo` and `@I.foo` (with inference)
   - extend effect identity to include interface instantiation args

5. **Generic bounds**
   - accept `T: I + J` (fn generics)
   - implement unique-by-canonical-id method resolution under bounds

6. **Deferred ergonomics**
   - disambiguation syntax for ambiguous bound methods
   - explicit generic instantiation syntax in value paths (if needed)
   - bounds on HKTs (if desired)
   - conditional impls / specialization model (if desired)

---

## Open questions

1. **User-visible reflection**
   - Should `TypeRep` remain completely internal, or do we add a controlled surface (e.g. `Type<T>`)
     later?

2. **Which type forms get `TypeRep`**
   - Do we eventually reify function types, tuples, arrays, etc.?

3. **Coherence vs specialization**
   - Do we permanently keep “forall instantiations only” impl headers?
   - If we relax it, what is the overlap/selection model and how does it interact with dispatch
     keys and effect identity?

4. **Effect + generic methods**
   - Do we ever need method-generic effect operations (`@I.foo<U>(...)`)?
   - If so, do method type args participate in effect identity, or are they purely runtime params?

5. **Disambiguation syntax for bounds**
   - What is the surface syntax to select a method origin when multiple bounds contribute the same
     method name?
   - Do we need qualification inside impl bodies as well?

6. **Dictionary passing strategy for bounded generics**
   - Under `T: I + J`, do calls compile to:
     - runtime lookup by `(TypeRep, interface)` to obtain dictionaries/vtables, or
     - compile-time-selected dictionaries passed as hidden parameters?
   - How does this interact with separate compilation / incremental compilation?

7. **Where clauses / formatting**
   - Do we add a `where` clause for readability once multiple bounds are common?

8. **Bounds on HKTs**
   - Do we eventually want bounds directly on type constructors (`F<_>: I`) or only patterns like
     `Functor<F<_>>`?

