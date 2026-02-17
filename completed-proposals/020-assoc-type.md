# Proposal: Minimal Associated Types in Interfaces

Date: 2026-02-16
Status: Draft

This proposal adds **associated types** (a.k.a. “assoc types”) to `interface` with a deliberately
minimal feature set aimed at keeping:

- interface authoring ergonomic for “output-like” types (e.g. iterators),
- dynamic dispatch predictable, and
- the compiler implementation incremental.

This proposal is intended to be implemented **after** `proposals/self-type.md`, and assumes `Self`
is available inside interfaces.

---

## 1. Background (Why We Need Assoc Types)

Rusk already supports generic interfaces:

```rust
interface Boxed<T> { fn get() -> T; }
```

However, the current initial-stage coherence restriction in `RUSK_SPEC.md` §6.4 rejects impl
headers that “pick” concrete interface type arguments, e.g.:

```rust
// Rejected today (specializes interface args):
impl Iterator<string> for LineIterator { ... }
```

This makes common “output-like” interfaces awkward: many real-world patterns want a *non-generic*
type to determine a single output type (e.g. an iterator yields exactly one `Item` type).

Associated types solve this by letting the impl choose the output type **once per implementing
type**, without requiring impl-header specialization.

---

## 2. Goals / Non-goals

### Goals

1. Allow an `interface` to declare associated types of kind `Type` (arity 0).
2. Require `impl Interface for Type` to define each associated type exactly once.
3. Allow interface methods and defaults to mention associated types via `Self::Assoc`.
4. Keep dynamic dispatch usable by requiring associated types to be **fixed on interface value
   types** (so method signatures are concrete at call sites).
5. Provide a minimal, explicit way to refer to associated types in generic code without a full
   trait solver.

### Non-goals (for this proposal)

- Associated type bounds: `type Item: Show;`
- Default associated types: `type Item = ...;`
- Generic associated types (GAT): `type Item<T>;`
- Higher-kinded associated types (associated type constructors).
- A full Rust/Swift-equivalent existential / “object safety” model.
- Specialization / overlapping impls beyond the current stage.

---

## 3. User-Facing Semantics

### 3.1 Declaring Associated Types in an Interface

Inside an `interface`, declare an associated type with:

```rust
interface Iterator {
  type Item;
  fn next() -> Option<Self::Item>;
}
```

Rules:

- Associated types are **type members** of the interface.
- In this proposal, associated types are **arity-0 only** (they denote a `Type`, not a type
  constructor).
- Associated types participate in interface inheritance (see §3.5).

### 3.2 Defining Associated Types in an `impl`

An interface impl must define each associated type with:

```rust
impl Iterator for LineIterator {
  type Item = string;

  fn next() -> Option<string> { ... }
}
```

Rules:

- Every associated type in the interface’s **full associated type set** (including inherited ones)
  must be defined exactly once in the impl.
- Defining an unknown associated type name is an error.
- Defining the same associated type more than once is an error.

Associated type definitions are compile-time only; they generate no runtime code.

### 3.3 Referring to Associated Types

This proposal intentionally keeps projection syntax minimal and explicit.

#### Inside an interface (and its impls)

Use `Self::Assoc`:

```rust
interface Iterator {
  type Item;
  fn next() -> Option<Self::Item>;
}
```

This requires `proposals/self-type.md` (`Self` in interfaces).

#### Outside (generic code)

Use **qualified projection**:

```rust
Iterator::Item<T>
```

Interpretation: “the `Item` associated type from `Iterator` for implementing type `T`”.

Examples:

```rust
fn first<T: Iterator>(it: T) -> Option<Iterator::Item<T>> {
  it.next()
}
```

Notes:

- We do **not** introduce `<T as Iterator>::Item` in this minimal proposal.
- We also do **not** introduce `T::Item` (too ambiguous: multiple interfaces could define `Item`).
- The `Iterator::Item<T>` form fits the existing `PathType` grammar (`TypeArgs` on path segments)
  with minimal parser changes.

### 3.4 Interface Value Types Must Bind Associated Types

An interface type used as a **first-class value type** is an existential (“some concrete type
implementing this interface”). For associated-type-carrying interfaces, leaving associated types
unspecified makes many method signatures unusable (the call site can’t name the return type).

To avoid “existential but unusable” interface values, we require:

> If an interface declares associated types, then using that interface as a value type must bind
> all of them.

We introduce associated type bindings on interface types:

```rust
Iterator{Item = string}
```

and with interface generics:

```rust
Graph<int>{Node = int, Edge = (int, int)}
```

Associated type binding syntax:

```text
IfaceTypeWithAssocBindings := PathType "{" AssocBinding ("," AssocBinding)* (",")? "}" ;
AssocBinding              := Ident "=" Type ;
```

Rules:

- The binding list must assign **every** associated type in the interface’s full associated type
  set exactly once.
- Extra bindings are rejected.
- Order is irrelevant.

Upcasts bind associated types explicitly:

```rust
let it: Iterator{Item = string} = line_iter as Iterator{Item = string};
```

Typechecking rule for `as` upcasts:

- `x as I{...}` succeeds only if the static type of `x` implements `I` and the associated type
  definitions of that impl satisfy the requested bindings.

### 3.5 Inheritance

Interfaces inherit associated types from super-interfaces:

```rust
interface J: I {
  type Extra;
}
```

Rules:

- The full associated type set of an interface includes all associated types declared in its
  transitive super-interfaces.
- As with methods in v0.4, multiple inheritance is rejected if two unrelated origins introduce the
  same associated type name.
- `impl J for T` must define associated types from both `J` and its inherited interfaces.

### 3.6 Dynamic Dispatch and “Object Safety”

We keep the spirit of `proposals/self-type.md`:

- Dyn-dispatchability is determined by **the method signature**, not by the method body.

We extend the model for associated types:

1. `Self` **by value** outside the receiver remains non-dyn-dispatchable (as in `self-type.md`).
2. `Self::Assoc` is allowed in method signatures and may be dynamically dispatched **when the
   receiver’s interface type binds the assoc type** (so the signature becomes concrete).

Concretely:

- For `x: Iterator{Item = string}`, `x.next()` is allowed and returns `Option<string>`.
- The bare value type `Iterator` is rejected (per §3.4), so we avoid the “you can write it but can’t
  call anything meaningful” pitfall.
- For constrained generics, calls are typed in terms of projections:
  - `T: Iterator` allows `t.next()` with return type `Option<Iterator::Item<T>>`.

### 3.7 Effects (`@Iface.method(...)`)

Rusk interfaces are also used as effect namespaces (§7 in `RUSK_SPEC.md`). Associated types are
defined *per impl* of an interface for a type, but effects have **no receiver**, therefore no `Self`
and no impl context.

To keep effects simple and well-defined:

> Effect calls must not target methods whose signature mentions `Self` or any associated types.

This is aligned with the intuition that effect interfaces should describe concrete operation
signatures.

---

## 4. Compiler / Runtime Design (Incremental)

### 4.1 AST and Parsing

Add a new interface member kind:

```rust
interface I {
  type Assoc;
  fn m() -> Self::Assoc;
}
```

Add a new impl member kind for interface impls:

```rust
impl I for T {
  type Assoc = int;
  fn m() -> int { ... }
}
```

Extend the type grammar to allow assoc bindings on interface types:

```rust
I{Assoc = int}
I<int>{Assoc = string}
```

### 4.2 Type Representation

Introduce a type form for associated type projections, conceptually:

- `Ty::AssocProj { iface, assoc, self_ty, iface_args }`

User-facing spellings lower to this:

- `Self::Assoc` inside `interface I<...>` lowers to `Ty::AssocProj { iface = I<...>, assoc = Assoc, self_ty = Ty::SelfType }`.
- `I<...>::Assoc<T>` lowers to the same with `self_ty = T`.

### 4.3 Typechecking Rules

#### Interface checking

While checking an interface:

- Collect declared associated types (with origin tracking, like methods).
- Validate that `Self::Assoc` references a known associated type in this interface’s full set.

#### Impl checking

While checking `impl I<...> for T`:

1. Collect all `type Assoc = ...;` items.
2. Validate:
   - completeness (must cover all required associated types),
   - no extras, and
   - no duplicates.
3. Substitute associated types when validating method signatures:
   - An interface method signature mentioning `Self::Assoc` must match the impl signature after
     substituting `Self := T` and each `Self::Assoc := <impl-provided type>`.

#### Using projections in generic code

- `I::Assoc<T>` is well-formed if (and only if) there is a bound in scope establishing `T: I` (or
  `T: I{Assoc = ...}`), so the projection is meaningful.
- The typechecker may normalize a projection when `T` is a concrete nominal type with a known impl.
- Otherwise it remains a symbolic projection type used for equality/unification.

This keeps the feature usable without requiring a full trait solver.

### 4.4 Lowering and Runtime

No MIR or VM changes are required for basic associated types.

Associated types affect:

- compile-time method signature substitution, and
- typechecking of interface object types with assoc bindings.

Dynamic dispatch remains a lookup from receiver dynamic type to the selected impl method body.

**Runtime `is` / `as?` targeting `I{Assoc = ...}` is deferred** in this minimal proposal:

- Accept `is/as?` with plain interface instantiations (`I<T>`) as today.
- Reject `is/as?` when the target interface type includes assoc bindings, unless/until we add runtime
  metadata to check them.

---

## 5. Diagnostics

Add clear errors for:

1. Missing associated type definitions in an interface impl.
2. Duplicate associated type definitions in an interface impl.
3. Unknown associated type name in `type Name = ...;`.
4. Unknown associated type referenced as `Self::Name` inside an interface.
5. Using an interface-with-associated-types as a value type without binding all assoc types.
6. Upcast `as I{...}` where the bindings don’t match the impl-provided assoc types.
7. Effect calls to methods mentioning `Self` or associated types.

---

## 6. Testing Plan

Add tests under `tests/` to cover:

1. **Basic assoc type impl**
   - Define `interface Iterator { type Item; fn next() -> Option<Self::Item>; }`
   - Implement it for a concrete type with `type Item = string;`.
2. **Generic projection typing**
   - `fn first<T: Iterator>(it: T) -> Option<Iterator::Item<T>> { it.next() }` compiles.
3. **Interface object with binding**
   - `let it: Iterator{Item = string} = concrete as Iterator{Item = string};`
   - Calling `it.next()` typechecks and returns `Option<string>`.
4. **Inheritance**
   - Super-interface declares `type A;`, sub-interface adds `type B;`.
   - Impl must define both.
5. **Diagnostics**
   - Missing `type Item = ...;` in impl errors.
   - `Iterator` as a value type errors (requires `{Item = ...}`).

---

## 7. Spec Updates

Update `RUSK_SPEC.md`:

- §3.2.4 (`interface` grammar): add `type Name;` members.
- §3.2.5 (`impl` grammar): allow `type Name = Type;` members in `impl Interface for Type`.
- §3.4 (`Type` grammar): allow `PathType { Assoc = Type, ... }` on interface types.
- §6 (“Interfaces and Methods”):
  - specify associated type semantics,
  - specify interface value type well-formedness rule (assoc bindings required),
  - specify how projections are written (`Iface::Assoc<T>`),
  - specify the restriction on effect calls for methods mentioning `Self`/assoc types.

No immediate `MIR_SPEC.md` changes are required.

---

## 8. Alternatives Considered

### A) “Unique full specialization” for generic interfaces

Allow `impl Iterator<string> for LineIterator` but enforce “at most one specialization per
implementing type”.

Pros:
- No new syntax for assoc members.

Cons:
- Still requires impl overlap/coherence reasoning as soon as other generic features interact.
- Makes output-like parameters look like input-like generic parameters, harming API clarity.

### B) Treat assoc types as extra hidden generic parameters

Pros:
- Reuses existing generics machinery.

Cons:
- Leaks into surface syntax quickly (users effectively have to pass “hidden” parameters).
- Makes trait objects awkward without named binding syntax anyway.

### C) Allow `T::Item` projection

Pros:
- Short syntax.

Cons:
- Ambiguous when multiple interfaces define `Item`.
- Requires additional disambiguation syntax, pushing complexity elsewhere.

This proposal picks explicit `Iface::Assoc<T>` projections as the simplest consistent rule.

---

## 9. Open Questions

1. Should we allow *partial* assoc bindings on interface value types (and restrict which methods are
   callable), or is “bind all or reject” better for ergonomics?
2. Should `is/as?` support `I{Assoc = ...}` targets by adding runtime metadata for associated type
   values? If yes, how should generic types and type equality be represented at runtime?
3. Should associated types be allowed in effect signatures if the assoc bindings are fully supplied
   on the `@I{...}.op(...)` path (or should effects stay strictly receiver-free and impl-free)?
4. Should we allow associated type RHS to mention other associated types, and if so, what cycle
   detection / normalization rules should we adopt?

---

## 10. Summary

This proposal introduces associated types in a minimal, implementable form:

- `interface` can declare `type Name;`
- `impl Interface for Type` defines `type Name = ...;`
- Methods use `Self::Name` inside interfaces/impls
- Generic code uses `Iface::Name<T>` projections
- Interface value types must bind associated types: `Iface{Name = Ty}`

The design intentionally postpones bounds/defaults/GAT and keeps dynamic dispatch predictable by
making interface value types fully specify associated types.

