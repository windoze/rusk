# Proposal: `Self` in Interfaces + Per-Impl Specialized Default Methods

Date: 2026-02-16
Status: Draft

This proposal introduces `Self` as a special type placeholder inside `interface` definitions and
updates codegen so that **default methods remain dynamically dispatchable based on their signature**
even when their bodies call **`Self`-only** methods (methods that mention `Self` in non-receiver
positions). This is achieved by specializing default method bodies **per `impl`**, analogous to how
Rust monomorphizes default trait methods per implementing type.

The core motivation is to keep the “signature-only dyn-dispatchability” mental model (predictable
usability) while supporting expressive `Self`-returning / `Self`-taking APIs.

---

## 1. Background (Current State)

Rusk currently supports:

- `interface` definitions with optional default method bodies (see `RUSK_SPEC.md` §6).
- Dynamic dispatch when the static receiver type is an interface type, or a generic constrained by
  an interface (implemented via MIR `VCall`).
- Static dispatch when the static receiver type is a concrete nominal type (struct/enum), resolved
  to a stable implementation function name like `impl::<Iface>::for::<Type>::<method>`.

Important compiler notes (current implementation):

- Default interface method bodies are compiled once into internal functions:
  - `$default::<IfaceFqn>::<method>`
- Missing default methods in an `impl` generate per-impl wrappers that *forward* to the `$default`
  function.
- Dynamic dispatch uses a dispatch table `Module.methods` keyed by `(dynamic_type_name, method_id)`
  where `method_id` is `"<origin_iface>::<method>"`. (This behaves like a vtable entry, even if the
  representation is a map.)

This architecture works well for “normal” interface methods, but it breaks down once we introduce
`Self` as a placeholder for the implementing type:

- A method whose signature contains `Self` cannot be called through an interface-typed value
  (“object safety” / existential typing issue).
- If default methods are compiled once with an **interface-typed** receiver, they cannot safely call
  `Self`-only methods unless those methods are dynamically callable. But we explicitly do *not* want
  `Self`-only methods in dynamic dispatch.

Therefore, we need a way for default method bodies to be compiled in a context where `Self` is known
to be the concrete implementing type.

---

## 2. Goals / Non-goals

### Goals

1. Add `Self` as the placeholder for “the implementing type” in interface method signatures.
2. Define dyn-dispatchability (a.k.a. “object safety”) primarily as a **signature property**, not a
   body property.
3. Support default methods that:
   - are dyn-dispatchable by signature, and
   - call `Self`-only methods in their bodies,
   by compiling them as per-impl specialized implementations.
4. Keep runtime representation and dispatch mechanism simple:
   - `VCall` still dispatches through `Module.methods`.
   - No changes required to MIR/VM beyond “which entries exist”.

### Non-goals (for this proposal)

- Associated types, higher-kinded interface generics, or advanced trait solver features.
- A fully Rust-equivalent method lookup algorithm.
- Backwards compatibility guarantees (repo explicitly allows breaking changes).

---

## 3. User-Facing Semantics

### 3.1 `Self` Type

Inside an `interface` definition, `Self` denotes the implementing type of that interface.

Example:

```rust
interface Clone {
  fn clone() -> Self;
}
```

In `impl Clone for Point { ... }`, `Self` is `Point`.

`Self` is only introduced for interface contexts in this proposal:

- Allowed in interface method signatures.
- Allowed in interface default method bodies.
- Not specified (yet) for inherent `impl Type { ... }` blocks.

### 3.2 “Self-only” Methods

Definition:

- A method is **`Self`-only** if `Self` appears anywhere in its signature **outside the receiver**.
  This includes nested positions such as `Vec<Self>`, `fn(Self) -> int`, etc.
- `Self` appearing implicitly as the receiver (`self`) does **not** make a method `Self`-only.

Examples:

```rust
interface Eq {
  fn eq(other: Self) -> bool; // Self-only (parameter position)
}

interface Factory {
  fn new() -> Self; // Self-only (return position)
}

interface Debug {
  fn debug() -> string; // dyn-safe (no Self outside receiver)
}
```

### 3.3 Dyn-dispatchability Rule (Signature-only)

We define a method as **dyn-dispatchable** (eligible for `VCall` / “in vtable”) iff:

1. The receiver is an instance receiver (`self`), as today.
2. The method signature contains **no `Self` occurrences outside the receiver**.

Consequences:

- `Self`-only methods:
  - can be called when the static receiver type is a concrete nominal type (static dispatch),
  - cannot be called when the static receiver type is an interface type (`I<...>`) or a generic
    constrained by an interface (`T: I`), because those call sites require `VCall`.
- Non-`Self` methods remain callable via `VCall`.

### 3.4 Default Methods: Specialized per `impl`

Key semantic rule:

> A default method is dyn-dispatchable if and only if its **signature** is dyn-dispatchable, even
> if its body calls `Self`-only methods.

This requires a codegen rule:

> Default method bodies are compiled into per-impl specialized implementations (thunks), so the body
> executes in a context where `Self` is the concrete implementing type.

#### Example (Main Motivator)

```rust
interface CloneEq {
  fn clone() -> Self;         // Self-only (not dyn-dispatchable)
  fn eq(other: Self) -> bool; // Self-only (not dyn-dispatchable)

  fn clone_eq() -> bool {     // dyn-dispatchable (ret bool)
    self.clone().eq(self.clone())
  }
}
```

Behavior:

- `x: CloneEq` (interface-typed value):
  - `x.clone()` is rejected (not dyn-dispatchable).
  - `x.clone_eq()` is allowed and dynamically dispatched.
  - Inside `clone_eq`, the calls to `clone()` and `eq(...)` are statically dispatched because the
    executing code is specialized for the concrete implementing type.

---

## 4. Compiler / Runtime Design

### 4.1 High-Level Compilation Strategy

For each `impl Interface for Type`:

- For every method explicitly implemented in the impl:
  - compile as today: `impl::<Iface>::for::<Type>::<method>`.
- For every method omitted but provided as a default in the origin interface:
  - generate **a real method body** for `impl::<Iface>::for::<Type>::<method>` by specializing the
    default method body for `Type`.

Crucially:

- We do **not** rely on a shared `$default::<Iface>::<method>` implementation for execution.
- The dispatch table for dynamic calls (`Module.methods`) points at the per-impl function id for
  dyn-dispatchable methods (including those whose body originated from a default).

### 4.2 Dispatch Table Population (Excluding Self-only Methods)

Today, every interface method implementation ends up in the module dispatch table.

With `Self`:

- The compiler must only populate `Module.methods` for dyn-dispatchable methods.
- Self-only methods should still exist as callable functions (for static dispatch), but must not be
  reachable via `VCall`.

This implies we need per-method metadata:

- `dyn_dispatchable: bool` computed from the final interface method signature (including inherited
  methods, after origin canonicalization).

### 4.3 Typechecking and “Where Does `Self` Live?”

We need to make `Self` usable inside:

1) interface method signatures (to validate impl signatures),
2) default method bodies (so they can call `Self`-only methods),
3) `impl` method bodies (so the implementing method can use `Self` naturally).

There are two viable internal representations:

#### Option A (Recommended): Add a dedicated type `Ty::SelfType`

- Extend the internal type language with a `Self` placeholder:
  - `Ty::SelfType` (name bikesheddable)
- During typechecking of interface items:
  - `Self` is allowed and lowered to `Ty::SelfType`.
- During typechecking of an `impl Interface for Type`:
  - validate that substituting `Ty::SelfType := <Type>` makes the interface signature match the impl
    signature.
- During per-impl specialization:
  - substitute `Ty::SelfType := <Type>` before lowering decisions (or typecheck directly under the
    substituted signature).

Pros:

- Keeps “Self-ness” explicit in types.
- Enables signature-only dyn-dispatchability checks without relying on AST heuristics.

Cons:

- Requires implementing substitution / display / hashing logic for the new type variant.

#### Option B: Encode `Self` as a reserved generic parameter

- Treat `Self` as a synthetic generic param at index 0 with a bound of “implements this interface”.
- This reuses constrained-generic method resolution.

Pros:

- Potentially reuses existing machinery.

Cons:

- Interacts poorly with Rusk’s current lowering strategy for generics (`Ty::Gen` currently lowers to
  `VCall` in many places, which is the opposite of what we want inside specialized thunks).
- Makes “is this method dyn-dispatchable?” harder to explain and implement.

This proposal recommends Option A.

### 4.4 Default Method Specialization Mechanics

We want the per-impl specialized default method body to behave like:

> “As if the user wrote this method in the impl.”

That suggests generating a synthetic `FnItem` for each missing default, whose body is the default
body AST from the origin interface, and then running the normal typecheck + lowering pipeline on it.

However, the current compiler architecture stores expression types in a single global `TypeInfo`
keyed by `Span`. That makes naive AST reuse impossible: the same default-body span would be used for
multiple different specialized implementations (different receiver types).

Therefore, implementing per-impl specialization requires one of the following:

#### Option 1 (Recommended): Make `TypeInfo` function-scoped

Change `TypeInfo` from:

- `HashMap<Span, Ty>`

to something that includes function identity, e.g.:

- `HashMap<(String /*fn_name*/, Span), Ty>`
  - and similarly for `call_type_args` / `method_type_args` / effect metadata.

Then:

- The same syntactic span can appear in multiple functions without collisions.
- Synthetic functions generated during compilation (match helpers, thunks, default specializations)
  can be typechecked independently and lowered correctly.

Pros:

- Correct-by-construction for cloned AST bodies.
- Improves compiler robustness beyond this feature (synthetic codegen already exists).

Cons:

- Requires touching a number of lookups in lowering to pass the current function name.

#### Option 2: “Freshen spans” when cloning default bodies

- Clone the default method body AST and rewrite all `Span`s so they don’t collide.

Pros:

- Avoids a larger `TypeInfo` refactor.

Cons:

- Makes diagnostics harder: spans no longer map cleanly back to original source.
- Requires pervasive span rewriting logic for every AST node kind.

This proposal recommends Option 1.

### 4.5 Method Resolution inside Default Method Bodies

We must prevent a semantic footgun:

- Today, default bodies are effectively typechecked as if `self: <interface type>`, so `self.m()`
  cannot resolve to inherent methods (interfaces have no inherent methods).
- With per-impl specialization, `self` becomes a nominal concrete type, and Rusk’s global resolution
  rule (inherent methods before interface methods) would cause:
  - `self.m()` to start resolving to an inherent method on `Type`, if one exists.

This would silently change the meaning of default bodies, depending on the implementing type.

We want default bodies to be stable interface-level behavior. Therefore we propose:

> In interface method bodies (including defaults), method-call sugar on `self` resolves to the
> current interface method set before considering inherent methods on the implementing type.

Two implementation approaches:

1) **Typechecker mode flag**: when typechecking a default body, set a flag “prefer interface methods
   for receiver `self`”, and adjust lookup accordingly.
2) **AST rewrite**: rewrite `self.m(args...)` to `Iface::m(self, args...)` for calls that resolve to
   interface methods in the template, ensuring stable meaning.

This proposal prefers the typechecker-mode approach because it preserves spans and avoids rewrites,
but either is acceptable if it keeps semantics stable and diagnostics good.

### 4.6 Lowering and Dynamic Dispatch

No MIR changes are required. The effect is entirely:

- which functions exist, and
- which methods are present in `Module.methods`.

Lowering behavior for specialized defaults:

- Calls to interface methods from within the specialized body will usually become **static calls**
  because the receiver is a nominal type (`Type`), and the compiler already resolves these to
  `impl::<Iface>::for::<Type>::<method>`.
- Calls to `Self`-only methods are also static calls (by construction they must resolve to an impl
  method).
- The entrypoint of a dyn-dispatchable default method is still dynamically dispatched via `VCall`
  from interface-typed receivers, and resolves to the per-impl specialized function.

---

## 5. Diagnostics and Error Messages

We should provide clear errors in these situations:

1) Calling a `Self`-only method via interface value:
   - `x: I`
   - `x.self_only()` should error: “method `I::self_only` is not dynamically dispatchable because it
     mentions `Self` in its signature; call requires a concrete receiver type”.

2) Calling a `Self`-only method via constrained generic receiver:
   - `fn f<T: I>(t: T) { t.self_only(); }`
   - If generics remain erased/lowered to `VCall`, reject similarly.

3) A default method is dyn-dispatchable by signature but cannot be specialized:
   - This should be an internal error; specialization must be supported for correctness once `Self`
     exists. If it fails, error should clearly indicate a compiler bug and point to the interface
     method.

4) Ambiguity / method resolution changes in default bodies:
   - If we implement the “prefer interface methods” rule, add a targeted diagnostic if a call is
     ambiguous or tries to resolve to inherent unexpectedly.

---

## 6. Testing Plan

Add integration tests under `tests/` (or unit tests near typechecker/lowering where appropriate) to
cover:

1) **Self-only exclusion from dyn dispatch**
   - Define interface with `fn clone() -> Self`.
   - Ensure `x: I` cannot call `clone()`.

2) **Dyn-dispatchable default calls Self-only**
   - Interface declares:
     - `fn clone() -> Self` (Self-only)
     - `fn eq(other: Self) -> bool` (Self-only)
     - `fn clone_eq() -> bool { ... }` (dyn-safe)
   - Provide an impl for a struct.
   - Upcast to interface type and call `clone_eq()`; verify it executes and returns expected value.

3) **Default body doesn’t accidentally call inherent**
   - Define `struct S` with an inherent method `fn debug() -> string`.
   - Define `interface I` with method `fn debug() -> string` and default method that calls
     `self.debug()`.
   - Ensure the default resolves to the interface method (expected behavior per §4.5).

4) **Inherited defaults**
   - Interface `J: I` inherits a default dyn-safe method that calls `Self`-only from `I`.
   - Ensure impl of `J` for `S` specializes correctly and dyn call works.

---

## 7. Spec Updates

Update `RUSK_SPEC.md` §6 (“Interfaces and Methods”) to specify:

- The meaning and scope of `Self` in interfaces.
- The dyn-dispatchability (“object safety”) rule: `Self` in non-receiver positions excludes a method
  from dynamic dispatch.
- The default method specialization rule: dyn-dispatchability depends only on the signature, and
  default bodies are specialized per impl.
- Method-call resolution rule inside interface method bodies (if we adopt the “prefer interface
  methods for `self`” tweak).

Optionally add a short note to `MIR_SPEC.md` clarifying that `VCall` only targets dyn-dispatchable
interface methods; other interface methods are compile-time-only / statically dispatched.

---

## 8. Alternatives Considered

### A) Body-based dyn-dispatchability (“if it calls Self-only, make it static-only”)

Pros:

- Simplifies codegen: shared `$default` bodies remain valid.

Cons:

- Surprising and fragile: a refactor inside a default body can silently change whether it is
  callable on an interface object.
- Requires call-graph analysis or conservative approximations.

### B) Require explicit opt-out: `static fn` / `where Self: Sized`-style marker

Pros:

- Predictable and explicit.

Cons:

- Still blocks the intuitive pattern where a dyn-safe default method wants to use `Self` internally.

### C) Keep `$default` shared but allow “private vtable entries”

Pros:

- Avoids specialization.

Cons:

- Reintroduces `Self`-only methods into dynamic dispatch via backdoor, undermining the object-safety
  model and making the interface object API less clear.

This proposal chooses per-impl specialization because it matches well-known semantics and yields the
best usability.

---

## 9. Open Questions

1) Should `Self` also be allowed in:
   - inherent impl blocks (`impl Type { ... }`)?
     - It should be allowed, and it should not cause any issues and Self is known in this context.
   - free functions? (probably not)
     - No, free function doesn't have a meaningful `Self`.
2) How should `Self` appear in diagnostics and pretty-printing? (`Self` vs the concrete type name in
   specialized contexts)
   - The most important thing is to clearly tell user when `Self` is incorrectly used in a dynamic context.
3) What is the long-term plan for generics:
   - Are they erased forever, or will we eventually monomorphize? This affects whether calling a
     `Self`-only method on `T: I` could ever be supported.
     - Let's postpone it to the later stages.
4) Do we want to forbid name collisions that would otherwise create ambiguity between inherent and
   interface methods in default bodies, or is the “prefer interface methods for self” rule sufficient?
   - In long term we should support `T::method` for disambiguating, but right now we can forbid name collisions for simplicity in the current stage.

---

## 10. Summary

This proposal makes `Self` usable in interfaces while keeping dynamic dispatch predictable:

- **Dyn dispatch eligibility is determined by signature**.
- **Self-only methods are statically dispatched only** (not part of `VCall`/dispatch table).
- **Default methods are specialized per impl**, so dyn-safe defaults can call Self-only methods
  internally.

The main compiler engineering requirement is enabling per-impl specialization without corrupting
type information, which is best addressed by making `TypeInfo` function-scoped rather than globally
keyed by spans.

