# Generic interfaces (and other generic ergonomics)

This doc proposes adding support for **generic interfaces** (and potentially generic interface
methods) and clarifies how they should interact with:

- erased generics (v0.4 design choice),
- runtime dynamic dispatch (`vcall`), and
- algebraic effects (`@Interface.method(...)`).

---

## Current status (v0.4)

The compiler currently rejects:

- generic interfaces: `interface I<T> { ... }` (`"interfaces cannot be generic in v0.4"`)
- generic interface methods: `interface I { fn foo<U>(...) -> ...; }` (`"interface methods cannot be generic in v0.4"`)
- generic methods inside interface impls: `impl I for S { fn foo<U>(...) { ... } }`
  (`"impl methods cannot be generic in v0.4 interface impls"`)
- type arguments in generic constraints and super-interface references

This proposal describes what we want instead (likely post-v0.4).

---

## Goals

1. Allow defining and using generic interfaces in the type system: `I<T>`, bounds, and `impl`.
2. Keep compatibility with **erased generics** (type arguments do not affect runtime representation).
3. Make generic interfaces work with dynamic dispatch in the same model as monomorphic interfaces.
4. Decide (or explicitly stage) how generic interfaces interact with effects.

Non-goals for an initial iteration:

- monomorphization/specialization of impls by concrete type arguments (unless we explicitly choose
  to change the erased-generics constraint model)
- full “polymorphic effects” (unless we explicitly choose to extend effect identity/typechecking)

---

## Proposed surface syntax (Rusk)

This section rewrites the examples in **Rusk syntax** (not Rust):

- `struct` items always use `{ ... }` (no `struct S;` form).
- interface member signatures do **not** include the receiver; impl methods do.
- there is no `&self` / `mut self` — the receiver is written as the first parameter
  (`self: Type`, optionally `readonly self: Type`).

### Basic usage

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

---

## Coherence model under erased generics

In v0.4, **type arguments are erased at runtime**, and the implementation already enforces “erased
impl args” for nominal types: you generally cannot write impls that differ only by concrete type
arguments because they would be indistinguishable at runtime.

To support generic interfaces without changing that model, we should extend the same idea to
generic interface impls:

- Disallow “specialized” interface impl headers such as:
  - `impl I<int> for S { ... }`
  - `impl I<string> for S { ... }`
- Allow only “forall instantiations” impl headers where the interface arguments are the impl’s own
  type parameters (in order), e.g.:
  - `impl<T> I<T> for GS<T> { ... }`

Rationale:

- runtime dispatch tables are keyed by `(dynamic_type_name, canonical_method_id)`
- if the runtime cannot distinguish `I<int>` from `I<string>`, allowing both would create
  ambiguous/overlapping entries for the same method id

If we ever want to support `impl I<int> for S` and `impl I<string> for S` simultaneously, we need a
different design (e.g. monomorphization, or a trait-object-like representation that carries type
arguments/vtable selection).

---

## Interaction with dynamic dispatch

Generic interfaces should be usable as value types (like monomorphic `interface` values today) and
should trigger runtime dynamic dispatch when the static receiver type is an interface.

Example:

```rusk
interface I<T> { fn foo() -> T; }
struct GS<T> { value: T }
impl<T> I<T> for GS<T> { fn foo(self: GS<T>) -> T { self.value } }

fn use_interface<T>(x: I<T>) -> T { x.foo() }

fn main() -> int {
    let gs = GS { value: 42 };
    let i = (gs as I<int>);
    use_interface(i)
}
```

Dispatch rule sketch:

- concrete nominal receiver type (`GS<int>`, `Point`, …): compiler can statically resolve the impl
  and emit a direct call (optionally)
- interface-typed receiver (`I<int>`, `I<T>`, or constrained generic `T: I<int>`): emit `vcall`
  using the canonical `(origin_interface, method_name)` identity

### Generic methods + dynamic dispatch (decision needed)

If we keep the “erased generics” runtime model (single function body, no runtime type parameters),
then *method* generic parameters do not inherently block dynamic dispatch: the dispatched function
can still be called with concrete runtime values.

However, generic methods interact with effects (next section), so a staged approach may still be
practical:

- allow generic interface methods for normal method calls first
- decide later whether generic interface methods are permitted as effect operations

---

## Interaction with effects (main open issue)

Generic effects are a key enabler for desugaring higher-level control-flow features like:

- generators (a typed `yield<T>` operation),
- try/catch (a typed `throw<E>` operation), and
- async/await (a typed `await<T>` operation).

To make these usable, effect operations need **generic value type parameters** in their signatures.

Today (v0.4), effect calls are identified by:

- an interface name (origin interface for inherited methods), and
- a method name

And effect call syntax is:

```rusk
@Interface.method(args...)
```

There is no way to write type arguments in `@...` syntax today, and the effect typing rules assume
each effect operation has a monomorphic signature.

Generic interfaces/methods would imply *polymorphic* effect operations unless we change effect
identity and/or restrict usage.

### Decision: allow `@I<T>.foo(...)` (post-v0.4)

This proposal should support effect calls/handlers with an *instantiated interface type* on the
left side:

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

This is the minimal feature needed to express typed effects like `Yield<T>`, `Throw<E>`, `Await<T>`
in a way that keeps effect signatures precise (instead of forcing dynamic/erased “one-size-fits-all”
effect signatures).

The alternative syntax putting type arguments on the method, e.g. `@I.foo<T>(...)`, is explicitly
postponed: we can decide later whether it is needed and how it should interact with method-generic
parameters and parsing.

In other words, the generic parameters that matter most for effect ergonomics are the **interface
type arguments** (the “operation family”), not necessarily method-generic parameters.

Example (generator-style `yield`):

```rusk
interface Yield<T> { fn yield(value: T) -> unit; }

fn gen_one_infer<T>(x: T) -> unit { @Yield.yield(x) }

fn gen_one_explicit<T>(x: T) -> unit { @Yield<T>.yield(x) }
```

### Type-argument inference

Interface type arguments in effect calls/arms should be inferred using the same mechanism as
generic argument inference for normal function calls (§8.5 in `RUSK_SPEC.md`):

- For a call `@I.foo(args...)`, if `I` is generic, treat its missing type arguments as inference
  variables and unify against the selected operation signature to solve them.
- For an effect arm `@I.foo(pats...) => ...`, treat missing interface type arguments similarly:
  typecheck the patterns and the arm body under the operation signature, and solve the interface
  type arguments via unification (using literal-pattern types and any constraints induced by uses of
  bound names in the arm body).
- Expected-type context should contribute constraints just like it does for normal calls.
- If inference fails (no solution or multiple ambiguous solutions), require explicit type arguments
  with `@I<T>.foo(...)`.

Note: as with normal generics, inferred type arguments may reference in-scope generic parameters
(e.g. `T` in `gen_one<T>`), even though they are erased at runtime.

### Effect identity (generic instantiation effects)

Under this design, effect identity is still based on the **origin interface** and method name, but
now also includes the instantiated interface type arguments:

- `@I<int>.foo(...)` is a different effect from `@I<string>.foo(...)`.
- If `J<T>: I<T>` and `foo` is declared in `I<T>`, then `@J<int>.foo(...)` should be treated as the
  same effect as `@I<int>.foo(...)` (origin interface + same instantiated arguments).

This keeps the existing “origin interface determines the operation” rule, while making generic
effect operations type-correct and useful.

### Syntax/grammar sketch

To support `@I<T>.foo(...)`, the effect-call grammar needs to accept generic arguments on the
interface path. A simple approach is to switch the callee from an expression path to a type path:

- Change `EffectCall` from `@ PathExpr . Ident ( ... )` to `@ PathType . Ident ( ... )`.
- Apply the same idea to effect arms (`@ PathType . Ident ( pats... ) ...`).

This keeps the generic arguments in the “type world” (where they already exist and are well-defined
in the grammar).

---

## Other generic ergonomics (related, but separable)

- Generic argument inference for normal function calls (e.g. `id(42)`) already exists; we should
  ensure the same inference works for interface method calls and for any future generic effect-call
  syntax.
- There is currently no dedicated, disambiguated syntax for explicit generic arguments in *value
  paths* (expressions). If we want explicit instantiation in calls/handlers (especially effects),
  we likely need to introduce one (bikeshedding required).
