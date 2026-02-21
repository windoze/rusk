# Proposal: Static Devirtualization (VCall → Direct Call) + Small-Function Inlining Cooperation

Date: 2026-02-21  
Status: Draft

## Motivation

Rusk supports runtime dynamic dispatch for `interface` methods via MIR/bytecode `VCall`. This is
necessary when the receiver’s *static* type is an `interface` (or an interface-constrained generic),
but it is often unnecessarily expensive when the receiver’s *dynamic* type is effectively known at
the callsite.

We already have two relevant pieces of infrastructure:

- **Small-function inlining** (compiler-side, MIR-level) reduces call overhead in tight loops, but
  it cannot inline through `VCall` because the callee is not known statically.
- **VM “non-dynamic” `VCall` fast paths** (runtime-side) can speed up some hot cases (notably
  primitive receivers), but they still execute a `VCall` instruction and cannot expose the callee’s
  body to the compiler, so it cannot unlock additional MIR optimizations or inlining.

This proposal adds a compiler-side **static devirtualization** pass:

> When the compiler can prove that a `VCall` has exactly one possible target *and* all required
> runtime `TypeRep` arguments are already available at the callsite, rewrite it into a direct call
> (`CallId`/`CallTarget::Mir`), enabling the existing small-function inliner to inline the method
> body.

The intended end result for hot code is:

`VCall (dynamic dispatch)` → `CallId (direct)` → `inline_tiny_functions()` → “no call at all”.

## Background (Current State)

### What `VCall` represents

At the MIR level:

- `Instruction::VCall { obj, method, method_type_args, args, ... }`
  - chooses an implementation based on the receiver’s runtime type,
  - implicitly supplies the receiver’s nominal type arguments (as `TypeRep` values) to the callee,
  - then calls the resolved method implementation.

At the bytecode level:

- `Instruction::VCall` uses a per-type dispatch table (`vcall_dispatch[type_id]`) to find the
  callee function id.

### Where `VCall` comes from today

The compiler already uses **static dispatch** when the receiver’s static type is a concrete nominal
type (including primitives). `VCall` primarily remains in:

- calls on values whose *static* type is an `interface`,
- calls inside erased generics where the receiver is a type parameter constrained by an interface
  (e.g. `T: Hash`).

### Why a compiler-side devirtualization pass still matters

Runtime fast paths are valuable, but limited:

- They apply only to cases the VM explicitly recognizes (usually a small set of primitive methods).
- They don’t remove the `VCall` instruction, and they don’t enable compiler inlining.
- They can’t help user-defined interface methods on user-defined types.

Static devirtualization is a complementary optimization:

- It removes `VCall` when the target is provably unique.
- It unlocks MIR-level inlining and follow-on simplifications.

## Goals / Non-goals

### Goals

- **Eliminate dynamic dispatch overhead** when a `VCall`’s target is provably unique at the
  callsite.
- **Cooperate with the existing small-function inliner**:
  - replace `VCall` with a direct call form the inliner understands, then inline when eligible.
- Preserve semantics exactly:
  - do not change observable behavior, trapping behavior, or `readonly` propagation.
- Keep code size growth bounded and predictable using existing inlining thresholds.

### Non-goals (for this proposal)

- Full Rust-style monomorphization of generics.
- Interprocedural whole-program devirtualization (class hierarchy analysis, PGO, etc.).
- Language-level “smart casts” / type narrowing (this is an internal optimization only).
- Adding new user-visible syntax.

## Proposal

### 1) Define “all types known at the callsite” (operational definition)

We say a `VCall` is eligible for static devirtualization if:

1. The receiver is provably of **exactly one nominal runtime type** (`type_name`) at the program
   point of the `VCall`.
2. The callsite can supply the **receiver nominal type arguments** required by the impl wrapper as
   explicit runtime `TypeRep` operands *without* extracting them from the value at runtime.
   - For primitives this is trivially satisfied (receiver type args are empty).
   - For generic structs/enums this typically means the receiver value is known to have been
     constructed with known `type_args` operands in the current function (or after inlining).
3. The method’s `method_type_args` operands already exist (they do today; they are explicit operands
   in MIR).

This definition is intentionally conservative: if we cannot supply the receiver’s `TypeRep` args
explicitly, we do *not* devirtualize in v1 of this optimization.

### 2) Rewrite rule: `VCall` → direct `CallId`

Given an eligible `VCall`:

```
VCall { dst, obj, method, method_type_args, args }
```

Rewrite it to:

```
CallId {
  dst,
  func: CallTarget::Mir(resolved_fn_id),
  args: [
    receiver_type_args...,   // runtime TypeRep operands (explicit at callsite)
    method_type_args...,     // already present in MIR
    obj,                     // receiver value
    args...                  // remaining arguments
  ]
}
```

Resolution uses existing method metadata:

- MIR modules carry a `(type_name, method_id) -> FunctionId` map for `VCall` resolution.
- For interface methods, `method_id` is already canonicalized as `{origin_interface}::{method_name}`
  during lowering.

If resolution fails (no entry), keep the `VCall` unchanged. (If this was a guaranteed compile-time
error, it should already be diagnosed earlier; devirtualization should not introduce new user-facing
errors.)

### 3) Intra-procedural analysis to prove the receiver type

Because MIR locals do not carry full static types, the pass cannot rely on typechecker annotations
alone. Instead, it uses a small, conservative, intra-procedural analysis that tracks *proven*
runtime receiver types.

#### 3.1 Analysis domain

For each local (flow-sensitive), track a “receiver shape”:

- `Unknown` — cannot prove a unique runtime nominal type.
- `Known { type_name, type_args }`
  - `type_name: String` (nominal runtime type name, matching dispatch table keys),
  - `type_args: Vec<Operand>` representing the runtime `TypeRep` operands needed by impl wrappers.
- `Conflict` — multiple different `Known` facts merged; treat as non-devirtualizable.

Join at CFG merges:

- `Known(a) ⊔ Known(a) = Known(a)`
- `Known(a) ⊔ Known(b != a) = Conflict`
- `Unknown ⊔ Known(a) = Conflict` (conservative; we only accept “known on all paths”)
- `Unknown ⊔ Unknown = Unknown`

This is conservative but simple and safe.

#### 3.2 How we produce `Known` facts

We can derive `Known` in obvious cases:

- `Const` of a primitive value:
  - `int`, `bool`, `float`, `byte`, `char`, `string`, `bytes`, `unit`
  - `type_args = []`
- `MakeStruct { type_name, type_args, ... }`:
  - `Known { type_name, type_args }`
- `MakeEnum { enum_name, type_args, ... }`:
  - `Known { type_name = enum_name, type_args }`
- `AsReadonly`, `Copy`, `Move` propagate the shape unchanged.

Everything else defaults to `Unknown` unless proven otherwise.

#### 3.3 Optional refinement from runtime type tests (`is` / checked cast lowering)

Rusk has runtime type tests (`is`) and checked casts (`as?`), which lower to MIR `IsType` and
control flow.

Even without language-level “smart casts”, the compiler can still use the IR pattern to refine the
analysis internally:

- If we see a branch of the form:
  - `%t = IsType { value: %x, ty: <typerep for S> }`
  - `CondBr { cond: %t, then: ..., else: ... }`
- then within the `then` successor, `%x` can be treated as `Known { type_name = S, ... }` *if* the
  `TypeRep` operand is a known literal or otherwise statically-known nominal type.

This refinement is optional in v1. It provides a path to devirtualizing within common downcast
patterns (`if x is S { ... }`) without changing the user-facing type system.

### 4) Cooperation with small-function inlining

Static devirtualization and small-function inlining are mutually reinforcing:

- Devirtualization turns `VCall` into a direct call that the inliner can reason about.
- Inlining can expose concrete receiver constructions and `TypeRep` arguments, creating new
  devirtualization opportunities.

#### 4.1 Proposed pipeline order

At `--opt-level o1/o2` (or similar), run:

1. Call target resolution (existing).
2. Small-function inlining pass (existing).
3. **New: `devirtualize_vcalls()`**.
4. Small-function inlining pass again (existing, same thresholds).
5. Remaining MIR cleanups / constant resolution (existing).

Rationale:

- The first inline pass reduces indirections and can make receiver origins “local” (e.g. after
  inlining a small helper that constructs a value and returns it as an interface).
- Devirtualization then replaces eligible `VCall`.
- The second inline pass can inline the devirtualized method body if it is eligible.

#### 4.2 Inlining thresholds and safety

This proposal does not require changing inliner eligibility rules. Devirtualization simply creates
more `CallId` opportunities that may already be eligible under existing thresholds.

If we later want more wins, we can separately propose expanding inliner eligibility (e.g. allowing a
single intrinsic call), but that is out of scope here.

## Examples (Conceptual)

### Example A: Interface value backed by a known concrete type

```rusk
interface Hash { fn hash(self) -> int; }

fn use_hash(x: Hash) -> int {
  x.hash()
}

fn main() -> int {
  let x: Hash = (123 as Hash);
  use_hash(x)
}
```

Inside `main`, the receiver for `hash()` is always an `int` value wrapped as `Hash`. After inlining
small helpers, the compiler can often see that the `Hash`-typed local is sourced from a single
concrete type. Devirtualization can rewrite the `VCall` to a direct call to the `int` impl, and the
inliner can then inline the tiny impl body into the caller.

### Example B: Devirtualization under a runtime type test

```rusk
fn maybe_hash(x: Hash) -> int {
  if x is int {
    // even without user-visible smart casts, the compiler sees the `IsType`-guarded region
    // and can devirtualize `x.hash()` in the true branch.
    x.hash()
  } else {
    0
  }
}
```

The optimization is purely internal: it uses the IR shape of `IsType` to specialize dispatch inside
the guarded region, without changing what programs typecheck or how they behave.

## Evaluation Plan

### Metrics

- Count `VCall` instructions in optimized MIR/bytecode for selected benchmarks.
- Track runtime:
  - wall-clock for call-heavy benchmarks,
  - VM metrics such as `vcall_instructions` and (if still present) `vcall_fast_path_hits`.

### Candidate benchmarks

- `benchmarks/phase7_map_dict.rusk` (hash/eq in generic-ish code; should benefit when devirtualized
  callsites become visible after inlining).
- `benchmarks/phase4_call_dispatch` (call overhead stress test).

## Testing Plan

Add compiler/bytecode tests that assert structural properties of optimized output:

- **Devirtualization works**: a program with an interface-typed local sourced from a single concrete
  type yields optimized MIR/bytecode with `VCall` removed at that callsite.
- **Inlining happens**: if the resolved impl is “tiny” under the existing inliner thresholds,
  validate that the direct call is inlined (call removed).
- **Conservatism**: if the receiver may be one of multiple types, ensure the `VCall` remains (no
  unsound rewrite).

Where possible, pair structural tests with a behavioral test to ensure semantics remain unchanged.

## Risks / Tradeoffs

- **Miscompilation risk (soundness)**: the analysis must be conservative. Any uncertainty must leave
  the `VCall` intact.
- **Compile-time cost**: dataflow analysis adds work at `--opt-level`. Keep it small, intra-
  procedural, and gated by opt level.
- **Code size**: devirtualization can enable more inlining. Existing inliner limits should bound the
  blow-up; still, this should be monitored.
- **Debuggability**: more rewriting can make IR dumps harder to compare. Consider adding (optional)
  compiler flags to dump MIR before/after devirtualization when debugging.

## Alternatives / Related Work

- **VM-only approaches**:
  - primitive `VCall` fast paths (already implemented),
  - dispatch caching or id-based dispatch tables to reduce lookup overhead.
  These reduce runtime overhead but do not enable inlining or compiler-side simplifications.

- **Full specialization / monomorphization**:
  - Generate specialized versions of erased generic functions for concrete type arguments.
  This is a larger design space and can subsume many devirtualization wins, but it has bigger code
  size, compilation time, and runtime representation implications.

## Future Work

- **Broaden applicability via explicit “extract receiver type args” IR**:
  - If we add an IR instruction to extract a reference’s runtime type arguments as `TypeRep` values,
    we can devirtualize more `VCall`s even when receiver type args are not already materialized.
- **Interprocedural devirtualization**:
  - module-level or whole-program analysis for interface-typed values passed across call boundaries.
- **PGO-guided devirtualization and inlining**:
  - speculative devirtualization with guards for hot callsites, if/when we have profiling.

