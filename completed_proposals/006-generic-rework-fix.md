# Generic Rework Fix Plan (Bounds in Impl Methods, Multi-Segment Type Args, All-Effect Match Invalid)

This plan addresses three requested changes:

1) **Support bounded method generics in impls**
2) **Lift the “last segment only” type-arg restriction**
3) **Clarify that all-effect `match` is invalid** (because `match` must have a value result)

---

## Summary of Current State (Observed)

- Impl methods currently reject bounds on method generics.
- Type arguments are only accepted on the last segment of a type path.
- The typechecker rejects `match` expressions without at least one value arm; this already matches the intended semantics but should be reflected in the spec.

---

## Plan

### 1) Support bounded method generics in impls

**Goal**: Allow bounds on method-generic parameters in impl methods, provided they match the interface method signature’s generic bounds.

**Work items**
- **Typechecker / impl validation**
  - Remove the hard rejection for bounds on impl method generics.
  - Ensure bounds in impl method generics match (or are equivalent to) the interface method’s generic bounds after substitution and shifting into the impl’s generic environment.
  - Add a precise mismatch error (e.g., “method generic bounds do not match interface contract”).
- **Signature equality logic**
  - Extend the existing “params/return” equality checks to include method generic bounds comparison (after proper substitution/shift).
- **Tests/fixtures**
  - Add fixture(s) where interface method has bounded generics and impl matches.
  - Add a compile-error fixture where impl method bounds differ from the interface method bounds.
- **Spec updates**
  - Ensure `RUSK_SPEC.md` explicitly allows bounds on method generics in impls and requires them to match the interface method signature.

**Acceptance criteria**
- An interface with bounded method generics compiles with a conforming impl.
- A mismatch in method generic bounds produces a type error.

---

### 2) Lift “last segment only” type-arg restriction

**Goal**: Allow type arguments on any segment of a type path, matching the grammar in `RUSK_SPEC.md`.

**Work items**
- **Parser**
  - Ensure type paths are parsed with `GenericArgs?` on each segment (already implied by current `PathType` structure). No syntax changes expected.
- **Type lowering (typechecker)**
  - Remove the “last segment only” restriction in `lower_path_type` and related checks.
  - Extend resolution to handle instantiated intermediate segments (e.g., `A<T>::B<U>`). This likely requires:
    - Resolving the left-most segment,
    - Tracking module/type namespaces along the chain,
    - Re-resolving each segment in context with its own type args.
- **Compiler lowering / TypeRep**
  - Update `lower_type_rep_for_path_type` to allow type args on any segment.
  - Define the runtime `TypeRep` for nested type paths consistently with the updated resolution logic.
- **Effect paths**
  - Ensure effect interface paths (`@PathType.method`) use the new multi-segment type resolution.
- **Tests/fixtures**
  - Add fixture(s) for `A<T>::B<U>` with intermediate type args in both type positions and effect/interface positions.
- **Spec updates**
  - Confirm `RUSK_SPEC.md` already allows this; add a note that multi-segment type args are supported by implementation.

**Acceptance criteria**
- Type paths with args on intermediate segments resolve correctly.
- `TypeRep` lowering works for such paths.
- New fixtures pass.

---

### 3) All-effect `match` is invalid

**Goal**: Codify that a `match` must include at least one value arm; effect-only `match` is invalid.

**Work items**
- **Spec update**
  - Add a rule that `match` expressions must include at least one value arm.
  - Explain rationale: the value arms determine the `match` result type; effect arms only install handlers and do not produce the final value.
- **Tests/fixtures**
  - Add a compile-error fixture demonstrating all-effect `match` is rejected with a clear error message.

**Acceptance criteria**
- Spec clearly states the requirement.
- Compile-error fixture exists and matches the current behavior.

---

## Files to Update (Expected)

- `src/typeck.rs` (impl method bound checking; path type lowering changes)
- `src/compiler.rs` (type path → TypeRep lowering; effect lowering if needed)
- `RUSK_SPEC.md` (bounds in impl methods; multi-segment type args; match rule)
- New/updated fixtures under `fixtures/`
- Optional: `MIR_SPEC.md` if runtime `TypeRep` changes need clarification

---

## Risks / Open Questions

- Multi-segment type args may require rethinking module/type resolution order to avoid ambiguous paths.
- Interface inheritance with instantiated supers may need to interact with new path resolution.
- Bounds equivalence may require a normalization step (ordering, dedup, or origin canonicalization) to compare impl vs interface bounds reliably.

---

## Checklist (for later implementation)

- [ ] Implement bounded method generics in impls with proper matching.
- [ ] Remove last-segment-only restriction and update type path resolution.
- [ ] Update `TypeRep` lowering for multi-segment paths.
- [ ] Add fixtures for both features and failures.
- [ ] Update `RUSK_SPEC.md` to reflect both changes and the all-effect match rule.

