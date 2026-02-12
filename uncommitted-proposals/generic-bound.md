# Generic bounds (multi-interface constraints)

NOTE: This document is superseded by `proposals/generic-rework.md` and is kept for history.

Status: **Completed (proposal)** — implementation is planned **after**:

- `proposals/generic-interface-and-others.md` (in progress)
- `proposals/reified-type-arguments.md` (draft)

This document specifies how Rusk generic parameters can be constrained by **one or more interface
bounds**, so that generic code can call interface methods in a type-safe way.

This proposal intentionally depends on the “generic interfaces / generic ergonomics” work. In
particular, it assumes we will support interface types like `I<T>` and reference those interface
types in bounds. See:

- `proposals/generic-interface-and-others.md` (generic interfaces, coherence model, effects)

---

## Summary

Allow generic parameters to declare **multiple interface bounds**:

```rusk
fn f<T: I + J>(x: T) -> int { x.foo() }
```

Inside the generic body:

- `T` is treated as implementing every interface in its bound set.
- method-call sugar `x.m(...)` is accepted only if it resolves to **exactly one canonical method**
  across the bound set; otherwise it is rejected for now.

Key staging decisions:

- **Multiple bounds is required** in the initial feature.
- **If method-call ambiguity exists, reject** (no disambiguation syntax in the initial stage).
  Adding “fully-qualified call/impl” disambiguation is explicitly deferred.
- **Higher-kinded parameters in bounds are rejected** initially (defer to later stage).
- The runtime representation of generics is not fixed here; we should **consider not erasing
  generics** (this interacts with generic interfaces, coherence, and dispatch).
- Optimization is optional and may be staged.

---

## Motivation

Generic code frequently needs to express requirements like:

- “`T` is printable + hashable”
- “`T` is an iterator and also cloneable”
- “`T` supports effects in interface `Yield<T>` / `Throw<E>`”

Single-interface bounds are often insufficient for real APIs; multiple bounds let us keep
interfaces small and composable without forcing “mega-interfaces” whose only purpose is bundling.

---

## Design goals

1. **Multiple bounds**: `T: I + J + K`.
2. **Deterministic method-call sugar**: accept `x.m(...)` only when there is a unique resolution.
3. **No implicit subtyping**: bounds do not create implicit conversions; casts remain explicit.
4. **Compatible with generic interfaces**: bounds can name instantiated interface types (e.g.
   `T: Iterable<int>`), once generic interfaces exist.
5. **Implementable in stages**: we can reject ambiguous cases now and add disambiguation syntax
   later.

---

## Non-goals (initial stage)

- Disambiguation syntax for ambiguous method names across bounds (“fully-qualified call/impl”).
- Bounds on higher-kinded type constructor parameters (`F<_>: ...`).
- Negative bounds / specialization / overlapping-impl resolution tricks.
- Any required optimization strategy (monomorphization vs vtables vs dictionary passing).

---

## Guide-level design

### Declaring bounds

```rusk
interface Hash { fn hash() -> int; }
interface Show { fn show() -> string; }

fn show_and_hash<T: Hash + Show>(x: T) -> (string, int) {
    (x.show(), x.hash())
}
```

### Ambiguity rule (initial stage)

If two bounds contribute a method with the same name but different **canonical origin**, method-call
sugar is ambiguous and rejected:

```rusk
interface I { fn foo() -> int; }
interface J { fn foo() -> int; }

fn bad<T: I + J>(x: T) -> int {
    x.foo() // rejected: ambiguous `foo` (I::foo vs J::foo)
}
```

For now, the program is rejected; we do **not** introduce a “fully-qualified call/impl” mechanism
in this initial stage. That feature is deferred.

Note: ambiguity should *not* be triggered by diamond duplication where the **origin interface is the
same**. For example, if `J: I`, then `I::foo` appearing via both `I` and `J` is the same canonical
method.

---

## Reference-level design

### Surface syntax

We extend generic parameter declarations to allow a `+`-separated list of bounds:

```text
GenericParam := Ident HktArity? Bounds? ;
Bounds       := ":" InterfaceType ("+" InterfaceType)* ;
```

Where `InterfaceType` is a type expression that must resolve to an interface type:

- `I`
- `I<T>`
- `core::iter::Iterator<int>`

### Initial restrictions

1. **Bounds only apply to arity-0 type parameters**:
   - `T: I + J` is allowed
   - `F<_>: I` is rejected for now (defer)
2. Each bound element must resolve to an **interface type** (not `struct`/`enum`).
3. Duplicate bounds are allowed syntactically but should be deduplicated semantically.

### Constraint satisfaction

Given `T: I<A, ...> + J<B, ...>`, an instantiation `T := S` is valid if `S` implements every bound
interface type.

The precise meaning of “implements `I<A, ...>`” depends on the generic-interface/coherence model
chosen in `proposals/generic-interface-and-others.md` (e.g. erased generics with restrictions vs a
non-erased model). This proposal does not re-specify that model; it only requires that the type
checker can decide the predicate.

### Bound inheritance / implication

If an interface bound is a subinterface of another, it implies the super bound. For example:

```rusk
interface I { fn foo() -> int; }
interface J: I { fn bar() -> int; }

fn g<T: J>(x: T) -> int { x.foo() } // ok: J implies I
```

This implication should also participate in ambiguity elimination by canonical origin (diamond
duplication is not ambiguity).

### Method-call typing under bounds

For a receiver expression `x` of type `T` with bounds `T: B1 + ... + Bn`, a method-call sugar
expression:

```rusk
x.m(args...)
```

is accepted iff:

1. Collect all methods named `m` available from each bound `Bi` (including inherited methods).
2. Deduplicate candidates by **canonical method identity**:
   - `(origin_interface, method_name)`
3. If the set is empty: error “unknown method”.
4. If the set contains more than one canonical method: error “ambiguous method” (initial stage).
5. Otherwise, typecheck arguments against the unique selected signature.

### Dispatch and optimization

This proposal does not require a particular compilation strategy.

Possible implementations include:

- monomorphization (if we choose a non-erased generics model),
- dictionary passing,
- vtable dispatch keyed by canonical method id (works well with interfaces and inheritance).

**Optimization is optional**: the compiler may choose to statically resolve calls when it can, but
it is not required by this proposal.

---

## Error behavior (examples)

### Missing bound

```rusk
fn bad<T>(x: T) -> int { x.foo() } // rejected: unknown method `foo` on `T`
```

### Unsatisfied bound at instantiation

```rusk
interface I { fn foo() -> int; }
struct S {}

fn needs_i<T: I>(x: T) -> int { x.foo() }

fn main() -> int { needs_i(S {}) } // rejected: `S` does not implement `I`
```

### Ambiguous method name across bounds

```rusk
interface I { fn foo() -> int; }
interface J { fn foo() -> int; }

fn bad<T: I + J>(x: T) -> int { x.foo() } // rejected: ambiguous `foo`
```

---

## Open questions

1. **Generics runtime model**:
   - Do we keep “erased generics”, or do we move to a non-erased model?
   - Two primary non-erased directions:
     - **Monomorphization** (compile a specialized copy per concrete instantiation)
     - **Reified type arguments** (carry type arguments at runtime; a single generic body can branch
       or look up dictionaries/vtables using the runtime types)
   - Pros / cons to consider:
     - **Monomorphization**
       - Pros:
         - Often best runtime performance (static calls, inlining, constant-prop, no generic
           indirection at runtime).
         - Straightforward typing story: instantiations are fully concrete; good for “static world”
           reasoning.
       - Cons:
         - Code size / compile time can grow quickly (generic-heavy code explodes).
         - Requires a compilation pipeline capable of late specialization (not always compatible
           with “script → MIR → interpret” workflows unless staged carefully).
         - Interacts sharply with dynamic loading / separate compilation (harder to share one
           compiled artifact across unknown instantiations).
     - **Reified type arguments**
       - Pros:
         - Single compiled generic body (smaller code size; less compile-time blowup).
         - Enables runtime reflection-like features (e.g. `is`/`as?` that can distinguish `Foo<int>`
           from `Foo<string>`), if we want them.
         - Can support “generic interface instances as values” with runtime dispatch keyed by
           `(dynamic_type, interface_type_args, method_id)` if the design chooses that route.
       - Cons:
         - Runtime overhead (carrying type args, dictionary/vtable lookups, harder to inline).
         - More moving parts in the runtime and MIR/VM (type representations, hashing/equality of
           types, canonicalization).
         - Semantics questions: which type operations are observable, and how do we keep programs
           deterministic/sound?
   - How does the choice affect:
     - coherence / overlap rules for `impl` (especially for `impl I<T> for S` forms),
     - dynamic dispatch tables (do type args participate in lookup keys?),
     - effect identity for generic interfaces (e.g. is `@Yield<int>.yield` distinct from
       `@Yield<string>.yield`?).

2. **Disambiguation syntax (“fully-qualified call/impl”)**:
   - When we allow ambiguity, what is the surface syntax to select a method origin?
   - Do we need qualification in *impl* bodies as well (e.g. implementing two `foo` methods from
     different origins)?
   - How does this interact with interface inheritance and canonical method identity?

3. **Where bounds are allowed**:
   - Only on `fn` generics, or also on `struct`/`enum`/`interface` generics?
   - For now, **skip bounds on `impl` generics**:
     - `impl<T: I> J for Wrap<T>` behaves like a *conditional impl* / *partial specialization*,
       not just “a bound for using methods inside a body”.
     - We do not yet have a language-level model for partial specialization / conditional impl
       selection, so accepting bounds on `impl` generics prematurely risks locking in semantics.
   - If allowed on nominal types, when are they checked (at type application, at construction, or
     both)?

4. **`where` clauses / formatting**:
   - Do we introduce a `where` clause for readability once multiple bounds are common?

5. **Bounds on HKTs**:
   - Today we reject `F<_>: ...`. Do we eventually want:
     - bounds directly on type constructors, or
     - the HKT-in-interface-parameter pattern (`Functor<F<_>>`) only?

6. **Generic methods + bounds**:
   - If interface methods can be generic (see `proposals/generic-interface-and-others.md`), do we
     want additional restrictions when a generic method is selected via bounds?
