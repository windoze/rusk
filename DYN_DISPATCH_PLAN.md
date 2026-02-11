## DYN_DISPATCH implementation plan (v0.4)

This plan implements `proposals/DYN_DISPATCH.md` end-to-end:

- Runtime dynamic dispatch for `interface` method calls when the *static* receiver type is an
  `interface` (or an interface-constrained generic).
- `interface` inheritance (single + multiple + diamond) with canonical method IDs.
- `as` cast expression for explicit upcasts to interfaces (`expr as I`).
- Full fixtures/tests coverage, and spec updates.

### 0) Guardrails / compatibility

- Keep `#![forbid(unsafe_code)]`.
- Preserve existing static dispatch for concrete receivers as an optimization.
- Keep existing surface syntax working (`x.foo()`, `I::foo(x, ...)`, `impl I for S { ... }`).
- No new features beyond the proposal scope (no generic interfaces, no default methods, no downcast).

---

## Phase 1: Front-end syntax (AST + parser)

1. **AST**
   - Extend `InterfaceItem` with `supers: Vec<PathType>`.
   - Add `Expr::As { expr: Box<Expr>, ty: TypeExpr, span: Span }`.

2. **Parser**
   - Parse optional inheritance list after `interface Name`:
     - `interface J: I { ... }`
     - `interface L: J + K { ... }`
   - Parse `as` as an infix operator with tight-ish precedence (binds before `+`, after unary/postfix).

Deliverable checks:
- New `.rusk` fixtures compile with the new syntax.

---

## Phase 2: Type environment + interface graph resolution (typeck env build)

3. **Extend `InterfaceDef` metadata**
   - Store resolved `supers: Vec<String>` (FQNs).
   - Keep `own_methods` (what the interface declares itself).
   - Compute and store `all_methods`:
     - `method_name -> { origin_iface_fqn, sig }`

4. **Interface graph validation**
   - Resolve each `InterfaceItem.supers` to interface FQNs (reject non-interface targets).
   - Reject cycles (direct/indirect).
   - Compute `all_methods` via DFS + memoization (or topo order).
   - Reject conflicting inherited method names in v0.4 (same name, different origin).
   - Diamond duplication is allowed iff the canonical origin is the same.

Deliverable checks:
- Cycle fixture fails with `compile_error` containing “cycle”.
- Conflicting inherited method names fixture fails with `compile_error` containing “conflict”.

---

## Phase 3: Typechecking changes (calls, casts, inheritance semantics)

5. **`expr as I` typing**
   - RHS must resolve to an interface type.
   - Accept if:
     - operand is a nominal type that implements the target interface (including via inheritance), OR
     - operand is an interface type `J` where `J` is a subinterface of `I`.
   - Cast is a runtime no-op: only affects the static type.

6. **Interface method resolution (static typing side)**
   - `I::method(recv, ...)`:
     - Allow methods in `I.all_methods` (including inherited).
     - Canonicalize method id to `(origin_iface, method)`.
   - Method-call sugar `recv.method(...)`:
     - If receiver is an interface type `I`, resolve only within `I.all_methods`.
     - If receiver is a constrained generic `T: I`, resolve within `I.all_methods`.
     - If receiver is a concrete nominal type, keep existing search across interfaces, but treat
       multiple candidates as OK if they canonicalize to the same origin method id (diamond case).
   - `ensure_implements_interface` / `type_implements_interface`:
     - Treat interface types as implementing their super-interfaces.
     - Treat `T: J` as implementing `I` if `J: I`.

Deliverable checks:
- `(s as I).foo()` typechecks.
- `(s as J).foo()` works when `foo` is inherited from `I`.
- `fn f<T: I>(x: T) { I::foo(x); }` typechecks.

---

## Phase 4: Lowering + MIR dynamic dispatch wiring

7. **Lowering strategy**
   - Use `Instruction::VCall` for dynamic interface calls.
   - Keep direct `Instruction::Call` for compile-time resolvable concrete receivers.
   - Encode canonical method IDs as strings: `"<origin_iface_fqn>::<method_name>"`.

8. **Populate `Module.methods`**
   - During compilation, build the dispatch table from `env.interface_methods`:
     - Key: `(dynamic_type_fqn, "<origin_iface>::<method>")`
     - Value: implementing function name.

Deliverable checks:
- Generated MIR for interface-typed calls includes `VCall`.
- `Module.methods` is non-empty for programs using interface impls.

---

## Phase 5: Interpreter/runtime support

9. **Interpreter `vcall`**
   - Determine receiver dynamic type name for:
     - `HeapValue::Struct { type_name, .. }`
     - `HeapValue::Enum { enum_name, .. }`
   - Resolve via `module.methods[(type_name, method_id)]`.
   - Improve trap messages for missing methods.

Deliverable checks:
- Dynamic dispatch works for structs (required).
- Dynamic dispatch works for enums (recommended; included in proposal).

---

## Phase 6: Tests + fixtures (must be comprehensive)

10. **Add runtime fixtures**
   - `116_dyn_dispatch_simple.rusk`
   - `117_dyn_dispatch_inheritance_diamond.rusk`
   - `118_dyn_dispatch_generic_constraint.rusk`

11. **Add compile error fixtures**
   - `119_interface_inheritance_cycle_compile_error.rusk`
   - `120_interface_inheritance_name_conflict_compile_error.rusk`
   - `121_interface_overlapping_impls_compile_error.rusk` (coherence rule)

12. **Add targeted Rust unit tests if needed**
   - If fixture coverage misses a detail (e.g., `Module.methods` contents), add a small unit test
     under `tests/` to assert `VCall` + table entries exist.

---

## Phase 7: Spec updates + polish

13. Update `RUSK_SPEC.md`
   - Grammar for interface inheritance.
   - `as` cast expression.
   - Clarify static vs dynamic dispatch rules.

14. Update `MIR_SPEC.md`
   - Specify `vcall` method-id format used for interface dispatch.

15. Tooling/verification
   - `cargo fmt`
   - `cargo test`
   - (Optional) `cargo clippy -- -D warnings` if it’s clean.

