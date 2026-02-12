# Generics Rework Plan (Generic Interfaces, Reified Type Args, Bounds)

This plan implements `proposals/generic-rework.md` in the current repo (`rusk` v0.1.0).

The current implementation is “v0.4-style” in spirit:
- generic type arguments are erased at runtime
- interfaces used for effects must be monomorphic
- `is` / `as?` reject targets with type arguments
- constrained generics support only a single interface constraint
- interface methods (and impl methods in interface impls) cannot be generic

The goal of this work is to deliver the **initial complete stage** of the proposal:
- runtime foundation: **reified** type arguments of kind `Type` via an internal `TypeRep`
- **generic interfaces** as first-class types (`I<T>`)
- **generic interface methods** and their impls
- **typed effects** using instantiated interface types (`@I<T>.foo`, `@I.foo` with inference)
- **multi-interface bounds** (`T: I + J + K`) for `fn` generics
- keep the staging discipline: **no specialization**, **no HKT reification**, **no method-generic effect calls**

---

## 0) Acceptance Criteria (Definition of Done)

1. Parsing + AST supports:
   - `T: I + J + K` bounds (including instantiated interface types)
   - effect calls and effect patterns with instantiated interface types: `@I<T>.foo(...)`
2. Typechecking supports:
   - `interface I<T> { ... }`
   - `interface` inheritance with instantiated supers (e.g. `interface J<T>: I<T> { ... }`)
   - generic interface methods (`fn m<U>(...) -> ...`)
   - interface impls for generic interfaces (coherent, no specialization)
   - multi-interface bounds + method-call sugar disambiguation rule
   - typed effects with instantiated interface types and inference for omitted interface args
   - `is` / `as?` targets may be instantiated nominal/interface types (no `readonly` targets)
3. MIR + interpreter supports reified type args:
   - internal, comparable `TypeRep`
   - allocations of generic structs/enums carry their instantiated type args
   - `is` / `as?` use runtime type checks that include type args
   - effect identity includes instantiated interface type args
4. Compiler lowers:
   - hidden `TypeRep` parameters for type-parameter generics (kind `Type`)
   - effect calls/handlers carry interface type args into MIR
   - `is` / `as?` lower to runtime checks over `TypeRep`
5. Tests:
   - new fixtures cover generic interfaces, generic interface methods, typed effects, multi-bounds, and runtime checks with type args
   - all existing tests continue to pass, with fixture updates where behavior changed

---

## 1) Staging / Implementation Order

### Stage 1 — Runtime `TypeRep` foundation (reification)

**MIR changes**
- Add a `TypeRep` runtime value category.
- Add MIR support to *construct* type representations:
  - `const` literal for primitive/nominal `TypeRep` leaves (interned)
  - `make_type_rep` for applied nominal types (`Name<...>`)
- Update nominal allocation ops (`make_struct`, `make_enum`) to carry instantiated type args.
- Update `is_type` / `checked_cast` to accept an instantiated target type representation.

**Interpreter changes**
- Add an internal, interned `TypeRep` table (cheap equality / hashing).
- Store instantiated type information in heap objects (struct/enum).
- Implement runtime type tests:
  - exact nominal type equality for `struct`/`enum` (including args)
  - interface tests check “implements instantiated interface type” (initially under coherence/no-specialization restrictions)
- Update effect matching to include interface instantiation args in operation identity.

### Stage 2 — Generic interfaces + generic interface methods

**Typechecker**
- Lift v0.4 bans:
  - allow `interface I<T>`
  - allow type args in super-interface references
  - allow generic methods in interfaces
  - allow generic methods in interface impls
- Update interface inheritance computation:
  - inherited method signatures must be substituted into the child interface’s generic environment
  - canonical origin method identity remains `(origin_interface_fqn, method_name)`
- Keep coherence restriction (no specialization):
  - reject impl headers that select by concrete interface args (e.g. `impl I<int> for S`)

**Compiler**
- Update method-call lowering to work with generic interfaces/methods (signatures now have type params).

### Stage 3 — Typed effects via generic interfaces

**Parser/AST**
- Parse effect callee as a type path: `@PathType.method(...)` (also for effect match arms).

**Typechecker**
- Typecheck effect calls with instantiated interface types:
  - `@I<T>.foo(args...)`
  - `@I.foo(args...)` infers missing interface args by unifying against the operation signature
- Effect identity uses canonical origin + instantiated interface args + method name.
- Explicitly reject *method-generic* effect operations for now (`@I.foo<U>(...)` stays deferred).

**Compiler/MIR**
- Lower effect calls/handlers so MIR carries interface type args and runtime compares them.

### Stage 4 — Multi-interface bounds

**Parser/AST**
- Change generic constraints from a single `T: I` to `T: I + J + K` (each element is an interface type).

**Typechecker**
- Store bounds as a list (dedup semantically).
- Constraint checking at inference finalization must verify all bounds.
- Method-call sugar on bounded generics:
  - collect candidates from all bounds
  - dedup by canonical origin id
  - require uniqueness or error (disambiguation syntax deferred)

**Compiler**
- Bounded-generic method calls lower to `vcall` using the unique canonical method id.

### Stage 5 — Update specs + tests/fixtures

- Update `RUSK_SPEC.md`:
  - remove blanket “types are erased at runtime”
  - define what is runtime-checkable/observable: `is`/`as?`, effect identity, dispatch
  - update effect grammar to use `PathType` in `@...`
  - document multi-interface bounds + ambiguity rule
- Update `MIR_SPEC.md` for:
  - `TypeRep` runtime values
  - extended `perform`/handler identity with interface instantiation args
  - updated `is_type` / `checked_cast`
- Update and add fixtures:
  - existing compile-error fixtures that are no longer errors
  - new fixtures for each new capability
- Run:
  - `cargo fmt`
  - `cargo test`
  - `cargo clippy -- -D warnings`

---

## 2) Concrete Work Items (Per Module)

### `src/ast.rs`
- `GenericParam`: replace `constraint: Option<PathType>` with `bounds: Vec<PathType>`.
- `Expr::EffectCall` and `EffectPattern`: replace `interface: Path` with `interface: PathType`.

### `src/parser.rs`
- Parse `T: I + J + K` bounds in generic params.
- Parse `@` effect callee using `parse_path_type()` (not `parse_path_expr()`).

### `src/typeck.rs`
- Generic params:
  - lower bounds as instantiated interface types (and reject bounds on HKTs)
  - inference constraints carry instantiated interface types
- Interfaces:
  - allow generic interfaces and generic interface methods
  - super-interface references may have type args
  - compute inherited method signatures with substitution
- Impl items:
  - allow `impl<T> I<T> for S<T>`-style headers (coherent restriction, no specialization)
  - allow generic impl methods matching generic interface methods
- Effects:
  - typecheck `@I<T>.foo` and infer args for `@I.foo`
  - same for effect match arms
- Bounds:
  - method-call resolution under multiple bounds with uniqueness-by-origin rule
- Runtime checks:
  - allow `is` / `as?` targets to include type args

### `src/mir.rs`
- Add `Type::TypeRep` (optional annotation).
- Add `ConstValue::TypeRep` literal.
- Add `Instruction::MakeTypeRep`.
- Update:
  - `Instruction::IsType` / `CheckedCast` to take a `TypeRep` operand/constant instead of monomorphic `Type`
  - `Instruction::MakeStruct` / `MakeEnum` to carry instantiated type args (or a full `TypeRep`)
  - `EffectId` / `HandlerClause` / `Perform` to incorporate interface instantiation args

### `src/interpreter.rs`
- Implement interned `TypeRep` table.
- Carry type args in heap objects for structs/enums.
- Implement updated runtime checks and updated effect identity matching.
- Update `vcall` to forward hidden type args when required.

### `src/compiler.rs`
- Add a lowering helper to produce `TypeRep` values for types (`Ty -> TypeRep`).
- Add hidden `TypeRep` parameters for arity-0 generic params (kind `Type`).
- Update call sites to pass hidden `TypeRep` args for generic calls.
- Update effect call and handler lowering to include interface instantiation args.
- Update `is` / `as?` lowering to use `TypeRep`.
- Ensure lambdas capture required `TypeRep` values (since closure ABI is fixed).

---

## 3) Fixtures / Tests To Add

### Update existing fixtures
- `fixtures/114_generic_interface_compile_error.rusk` → should become an `ok` fixture demonstrating generic interfaces compile and run.
- `fixtures/132_as_question_generic_target_compile_error.rusk` → should become an `ok` fixture demonstrating `as? Option<int>` works and is type-arg precise.

### New fixtures (names TBD)
- Generic interface + dynamic dispatch:
  - `interface Boxed<T> { fn get() -> T; }`
  - `struct Box<T> { v: T }`
  - `impl<T> Boxed<T> for Box<T> { fn get(self: Box<T>) -> T { self.v } }`
  - call via interface type and via method sugar
- Generic interface method:
  - `interface Pair<T> { fn pair<U>(u: U) -> (T, U); }`
  - validate inference of `U`
- Typed effects:
  - `interface Yield<T> { fn yield(value: T) -> unit; }`
  - use `@Yield<int>.yield(1)` and handler arm `@Yield<int>.yield(x) => ...`
  - include an inference case `@Yield.yield(1)` (ok) and a non-inferable case `@Yield.yield(x)` (compile error)
- Multi-interface bounds:
  - `fn f<T: Hash + Show>(x: T) -> int { ... }`
  - include an ambiguous bound-method-name compile error fixture
- Runtime `is` / `as?` with type args:
  - `Option::Some(1) is Option<int>` is true
  - `Option::Some(1) is Option<string>` is false

---

## 4) Notes / Explicit Deferrals (Initial Stage)

These are intentionally **not** implemented in this plan (aligning with proposal non-goals):
- Specialization / conditional impl selection / overlap resolution beyond current coherence checks.
- Reification of higher-kinded type constructor parameters (`F<_>`).
- Method-generic effect call syntax (`@I.foo<U>(...)`).
- Disambiguation syntax for ambiguous method names across multiple bounds.

