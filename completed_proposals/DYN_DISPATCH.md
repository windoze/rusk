## Runtime dynamic dispatch for `interface` + interface inheritance

### Summary

This proposal introduces **runtime dynamic dispatch** for `interface` methods, allows an `interface`
name to be used **as a value type directly**, and adds **interface inheritance** (including multiple
inheritance and diamond graphs).

Target user-facing examples:

```rusk
interface I { fn foo(); }
interface J: I { fn bar(); }
interface K: I { fn baz(); }
interface L: J + K { fn boo(); } // diamond: `I::foo` appears once

struct S {}

impl L for S {
  fn foo(self: S) { ... } // implementation of I::foo (shared by J/K/L views)
  fn bar(self: S) { ... }
  fn baz(self: S) { ... }
  fn boo(self: S) { ... }
}

fn main() {
  let s = S {};
  (s as I).foo(); // dynamic dispatch to S::foo
  (s as J).bar(); // dynamic dispatch to S::bar
  (s as K).baz(); // dynamic dispatch to S::baz
  (s as L).boo(); // dynamic dispatch to S::boo
}
```

### Context: what exists today (repo findings)

**Interfaces exist today, but calls are statically resolved**:

- Parser + AST support `interface I { fn m(...); }` and `impl I for T { ... }` with no inheritance.
- `RUSK_SPEC.md` §6.1 explicitly defines `Interface::method(...)` as statically resolved.
- Typechecker builds:
  - `ProgramEnv.interfaces: BTreeMap<String, InterfaceDef>`
  - `ProgramEnv.interface_methods: BTreeMap<(type_name, interface_name, method_name), fn_name>`
- Compiler lowers `Interface::method(recv, ...)` to a direct `call <impl_fn>(...)` by looking up
  `env.interface_methods` using the **receiver’s nominal type**.

**There is already a MIR/runtime hook we can reuse**:

- MIR has `Instruction::VCall { obj, method, args }` and `Module.methods: (type_name, method) -> fn`.
- Interpreter supports `vcall`, but currently:
  - it expects the receiver to be a `struct` heap object (`HeapValue::Struct`)
  - the compiler never emits `VCall` and never populates `Module.methods`

This proposal is largely about “wiring up” missing infrastructure, plus adding inheritance/casts.

---

## Design goals

1. **Dynamic dispatch** when the *static* receiver type is an `interface` (or an interface-constrained
   generic), without requiring monomorphization (type args are erased in v0.4).
2. **`interface` as a type**: variables, parameters, arrays, etc. can be typed as `I`.
3. **Interface inheritance**, including:
   - single inheritance: `interface J: I { ... }`
   - multiple inheritance: `interface L: J + K { ... }`
   - diamond graphs: inherited methods should not require duplicate implementations.
4. Preserve existing ergonomics:
   - `x.foo()` remains supported and should not become “more ambiguous” due to inheritance.
5. Keep runtime safe: the repo is `#![forbid(unsafe_code)]`.

## Non-goals (initial scope)

- Generic interfaces (still rejected in v0.4).
- Default methods, method bodies in interfaces.
- Overload/override systems (no “override” keyword, no signature-based overloading).
- Downcasting from `I` to `S` (could be added later with RTTI helpers).
- Disambiguation syntax for colliding inherited method names (see “Open questions”).

---

## Core idea

Treat an interface method as identified by a **canonical method id**:

```
(declaring_interface_fqn, method_name)
```

Derived interfaces *include* inherited methods, but they do not “redeclare” them. In a diamond
graph, `I::foo` is still the **same canonical method**, reached via multiple paths.

At runtime, dynamic dispatch resolves using:

```
(dynamic_receiver_type_fqn, canonical_method_id) -> implementing_function_name
```

This is the “vtable” concept for the interpreter, implemented as a lookup table in the MIR module.

---

## Language surface changes

### 1) Interface inheritance syntax

Extend `interface` items to optionally list super-interfaces:

```ebnf
InterfaceItem := "interface" Ident GenericParams?
                 ( ":" InterfaceRef ( "+" InterfaceRef )* )?
                 "{" InterfaceMember* "}" ;

InterfaceRef  := PathType ; // v0.4: must resolve to a non-generic interface
```

Notes:
- `+` is chosen to match the example. (Alternative: `,` like Rust trait bounds; not proposed here.)
- Duplicate super-interfaces are allowed syntactically but should be deduplicated during resolution.
- Cycles are rejected (direct or indirect).

### 2) `as` cast expression for interface upcasts

Add an expression form:

```rusk
expr as I
```

Rules:
- The RHS type must resolve to an `interface` type.
- The cast is accepted if the source type is known to implement the target interface
  (including via interface inheritance).
- The cast is a **runtime no-op** in v0.4 (no wrapper allocation), but it changes the static type.

This is the minimal surface needed for `(s as I).foo()` without introducing implicit subtyping
into the unification-based type system.

### 3) Dynamic dispatch trigger points

Dynamic dispatch is used when the call target cannot be resolved to a single impl at compile time:

- Calling a method on a value whose static type is an interface:
  - `(x: I).foo()` uses dynamic dispatch.
- Calling an interface method with a receiver that is not a concrete nominal type:
  - `I::foo(x)` where `x: T` and `T: I` uses dynamic dispatch.

Compile-time optimization is optional:
- If the receiver is a concrete nominal type `S`, `I::foo(s)` may remain a direct call.

---

## Interface inheritance rules

### Building the full method set

For each interface `X`, compute `X.all_methods`, a map:

```
method_name -> { origin_interface, signature }
```

Algorithm (conceptual):
1. Start with `X`’s own declared methods: each has `origin_interface = X`.
2. For each super-interface `B` in `X.supers`:
   - for each `(name -> {origin, sig})` in `B.all_methods`:
     - if `name` is not present in `X.all_methods`, insert it
     - if `name` is present:
       - if both entries have the same `origin_interface` (diamond duplication), keep one
       - otherwise, reject as a name conflict (v0.4 restriction)

This yields:
- Diamond merging “just works” for inherited methods.
- Unrelated interfaces that both declare `foo` cannot be multiply-inherited in v0.4 unless we add
  disambiguation syntax.

### Subinterface relation

Define `X` is a subinterface of `Y` if:
- `X == Y`, or
- `X` transitively inherits from `Y`.

This is needed for:
- `ensure_implements_interface` when the value is already an interface type:
  - `J as I` is valid if `J: I`.
- Potential future implicit upcasts.

---

## Semantics of `impl` with inherited interfaces

### Requirement: “Implementing L implements all its supers”

`impl L for S { ... }` must make `S` implement:
- `L`
- all transitive super-interfaces of `L` (`J`, `K`, `I` in the example)

### What methods must appear in the `impl` block?

In v0.4, require the `impl` block to provide **exactly one implementation per method name**
in `L.all_methods` (including inherited ones).

Rationale:
- Matches the example (single block implements the whole interface family).
- Avoids introducing a multi-trait coherence system in the first iteration.

Method signature checks:
- Same rules as today:
  - method cannot be generic
  - parameter count is `1 + declared_params`
  - (recommended) check parameter/return types match the interface signature exactly

### How to populate the dispatch table

For each required method `m` in `L.all_methods`:
- declare an internal MIR function name for the implementation, e.g.:
  - `impl::<L>::for::<S>::<m>`
- record it in `ProgramEnv.interface_methods` under the **canonical method id**:
  - key: `(S, origin_interface, m)`
  - value: `impl::<L>::for::<S>::<m>`

This ensures that:
- `I::foo`, `J::foo`, `K::foo`, and `L::foo` all canonicalize to `(I, foo)` and share one impl.
- A type “implements” a derived interface if it has entries for every method in the derived
  interface’s `all_methods` (by origin ids).

Coherence rule (recommended for v0.4):
- Disallow overlapping impls that would provide the same canonical method id twice
  for the same type. (Example: `impl I for S` and `impl L for S` both define `foo`.)

---

## Compiler and runtime plan (by subsystem)

### A) Lexer / Parser / AST

Files:
- `src/ast.rs`
- `src/parser.rs`
- (optional) `RUSK_SPEC.md` updates (separate PR or included)

Changes:
1. Extend `InterfaceItem` to include:
   - `supers: Vec<PathType>` (or similar)
2. Parse optional inheritance list in `parse_interface_item`:
   - after name + generics, accept `:` then `PathType` separated by `+`
3. Add `Expr::As { expr, ty, span }` (cast expression).
4. Parse `as` as an infix operator with appropriate precedence.

Deliverables:
- New AST structures.
- Parser tests (via fixture compile tests) for the new syntax.

### B) Module resolution

Files:
- `src/modules.rs`

No new resolution features are required, but the typechecker must resolve each super-interface
path into a fully-qualified interface name using `ModuleResolver::resolve_type_fqn(...)`.

### C) Type environment (`ProgramEnv`) and interface metadata

Files:
- `src/typeck.rs`

Changes:
1. Extend `InterfaceDef` to store:
   - `supers: Vec<String>` (resolved FQNs)
   - `own_methods: BTreeMap<String, InterfaceMethodSig>` (optional split)
   - `all_methods: BTreeMap<String, {origin, sig}>` (computed)
2. Add a new env-build pass:
   - resolve `InterfaceItem.supers` to FQNs
   - detect cycles in the interface graph
   - compute `all_methods` for every interface (topo sort or DFS + memoization)
   - enforce “no conflicting inherited method names” in v0.4

### D) Typechecking rules

Files:
- `src/typeck.rs`

Changes:
1. Implement typing for `Expr::As`:
   - typecheck operand
   - resolve the target type and ensure it is an interface
   - accept if:
     - operand type is a concrete type that implements the interface, or
     - operand type is an interface that is a subinterface of the target
2. Update `typecheck_interface_method_call` (`Interface::method(recv, ...)`):
   - allow inherited methods by looking up in `iface_def.all_methods`
   - canonicalize the method id to its `origin_interface` for later lowering
3. Update method-call sugar typing (`x.foo()`):
   - if receiver type is an interface `I`, resolve `foo` *within `I.all_methods` only*
   - if receiver type is a constrained generic `T: I`, resolve within `I.all_methods`
   - for concrete receiver types, keep current behavior but fix inheritance-induced ambiguity:
     - multiple candidate interfaces are OK if they canonicalize to the same origin method id
4. Update `ensure_implements_interface` / `type_implements_interface`:
   - when `ty` is an interface type `J`, treat `J` as implementing any super-interface it inherits
   - when checking an interface `X`, check all required methods in `X.all_methods` via canonical ids

### E) MIR + lowering strategy

Files:
- `src/mir.rs`
- `src/compiler.rs`
- (optional) `MIR_SPEC.md` updates

Preferred approach: reuse existing MIR `vcall`.

1. Choose a canonical method encoding for `Instruction::VCall.method`, e.g.:
   - `"I::foo"` where `I` is the origin interface FQN
2. Lower dynamic interface calls to `VCall`:
   - `Interface::method(recv, args...)`:
     - evaluate `recv` to an operand
     - emit `VCall { obj: recv, method: origin_id, args: args... }`
   - `x.foo(args...)` on `x: I` similarly lowers to `VCall` using the origin id
3. Keep static dispatch as an optimization only:
   - if receiver is a known nominal type `S` and a unique impl exists, still emit `Call`
   - otherwise fall back to `VCall`
4. Populate `Module.methods` during compilation:
   - for each `(type_name, origin_iface, m) -> impl_fn` in `env.interface_methods`:
     - insert `((type_name, format!("{origin_iface}::{m}")), impl_fn)` into `module.methods`

### F) Interpreter/runtime

Files:
- `src/interpreter.rs`
- (optional) `src/mir.rs` if `VCall` needs richer ids

Changes:
1. Update `vcall` receiver handling to support all nominal heap objects that can implement interfaces:
   - structs (`HeapValue::Struct`) and enums (`HeapValue::Enum`) at minimum
   - define how to obtain the “dynamic type name” for enums (`enum_name`)
2. Keep the dispatch lookup as:
   - `module.methods.get(&(dynamic_type_name, method_id_string))`
3. Improve runtime trap messages:
   - include interface + method and receiver dynamic type.

No GC changes are required if interface values are a type-only cast (no wrapper allocation).

---

## Testing plan

Use the existing fixture harness (`tests/fixtures.rs`) and add new fixtures.

### Runtime + semantics fixtures

1. `fixtures/XXX_dyn_dispatch_simple.rusk`
   - single interface `I`, struct `S`, `impl I for S`, cast `(s as I).foo()`
2. `fixtures/XXX_dyn_dispatch_inheritance_diamond.rusk`
   - the example diamond (`I`, `J: I`, `K: I`, `L: J+K`)
   - ensure:
     - `(s as I).foo()` works
     - `(s as J).foo()` works (inherited method)
     - `(s as L).foo()` works (inherited through diamond)
3. `fixtures/XXX_dyn_dispatch_generic_constraint.rusk`
   - `fn f<T: I>(x: T) { I::foo(x); }` should compile and run

### Compile error fixtures

1. Cycle detection:
   - `interface A: B { } interface B: A { }` => compile_error “cycle”
2. Conflicting inherited method names:
   - `interface A { fn foo(); } interface B { fn foo(); } interface C: A + B { }`
   - should be rejected in v0.4 unless disambiguation is added
3. Overlapping impls (if coherence rule is adopted):
   - `impl I for S { ... }` + `impl J: I for S { ... }` overlap on `I::foo`

---

## Spec/doc updates (follow-up work, but required for completeness)

1. `RUSK_SPEC.md`
   - Update §3.2.4 “Interfaces” grammar to include inheritance list
   - Update §6.1 “Interface Methods” to distinguish:
     - static dispatch on concrete receivers
     - dynamic dispatch when receiver is an interface value
   - Document `as` cast expression
2. `MIR_SPEC.md`
   - Clarify `vcall` is used for interface dynamic dispatch and define method id format

---

## Open questions / future extensions

1. **Method name collisions across multiple inherited interfaces**
   - v0.4 proposal: reject
   - future: add qualified method names in impls (e.g. `fn I::foo(self: S)`)
2. **Implicit upcasts**
   - allow `let x: I = s;` without `as`? This introduces subtyping into inference.
3. **Downcasts / RTTI**
   - `if x is S { ... }` / `x as? S` patterns would require runtime type checks.
4. **Interface impls for primitives**
   - would require boxing or a second representation for interface objects.
