# Proposal: Small-Function Inlining + “Non-dynamic” Dispatch Fast Paths

Date: 2026-02-19  
Status: Implemented  

## Motivation

The new `core::map::Map` benchmark (`benchmarks/phase7_map_dict.rusk`) is intentionally unfair to
Rusk (Python `dict` is highly optimized native code), but it is a good stress test for:

- tight loops,
- lots of small helper calls (`index_for`, `next_index`, etc.),
- interface method calls inside generic code (`Hash::hash`, `Eq::eq`).

Most recent results (macOS arm64, `--opt-level o2`) show `phase7_map_dict` is ~46x slower than
Python (~219ms vs ~4.7ms median, depending on run noise).

VM instruction metrics for a single run (`rusk-measure --metrics --warmup 0 --iters 1`) show:

- `executed_instructions`: 9.66M
- `call_instructions`: 714k
- `vcall_instructions`: 150k
- `vcall_fast_path_hits`: 150k (dominated by `Hash::hash` / `Eq::eq` in generic Map methods)

This is not “one thing”, but it does point to clear optimization opportunities:

- **Call overhead is high**: `call_instructions` is ~1M in a single benchmark run.
- **Dynamic dispatch overhead exists even when it is semantically unnecessary**:
  - `vcall_instructions` is ~150k in `phase7_map_dict`.
  - The hot case is `K = int`, where `Hash::hash` and `Eq::eq` are effectively known operations.

This proposal focuses on two complementary improvements:

1) a **more capable small-function inliner** (compiler-side, MIR-level), and
2) a **VM fast path for “non-dynamic” dynamic dispatch** (devirtualize `VCall` in common primitive
   cases).

The combination targets both main sources of overhead in `phase7_map_dict`:

- removing *avoidable* function frame pushes (inlining), and
- removing *avoidable* dispatch table lookups + wrapper calls (primitive `VCall` fast path).

## Background (Current State)

### The compiler already has a tiny inliner

`crates/rusk-compiler/src/compiler.rs` has `inline_tiny_functions()` which runs after call target
resolution.

It is currently extremely conservative:

- inlines only **single-block** functions,
- rejects bodies containing **any** `Call`/`CallId`/`VCall`/effects/etc,
- effectively only inlines “pure, straight-line” helpers.

This means it **does not inline** the kinds of helpers we want in `core::map`:

- `index_for` / `next_index` have control flow (`if`) → multi-block,
- many useful wrappers contain a single intrinsic call → currently rejected.

### Interface calls in generic code use `VCall`

For constrained generics (`T: Hash`), calls like `Hash::hash(x)` lower to `VCall` because the
receiver’s dynamic type is not statically known within the generic body.

This is correct semantically, but expensive, especially when:

- the receiver is a **primitive value** (`int`, `bool`, `string`, `bytes`, …), and
- the method is a known “core” interface method that maps to a VM intrinsic (e.g. `hash_int`,
  `int_eq`).

In `phase7_map_dict`, the type parameter `K` is instantiated with `int`, but the Map methods are
still compiled once (erased generics), so the runtime path still uses `VCall`.

## Goals / Non-goals

### Goals

- Reduce runtime overhead in tight loops by:
  - inlining small helpers (including small CFGs),
  - eliminating per-iteration call frame pushes where safe.
- Reduce overhead of `VCall` in cases that are “dynamic by implementation detail” but not dynamic by
  semantics:
  - `Hash::hash` on primitives,
  - `Eq::eq` / `Ne::ne` on primitives,
  - possibly basic `core::ops::*` on primitives (Add/Sub/etc) when reached via constrained generics.
- Keep changes compatible with the language semantics (no observable behavior changes).
- Keep code size blow-up bounded and predictable.

### Non-goals (for this proposal)

- Full Rust-style monomorphization of generics (possible future direction, but bigger scope).
- Whole-program profile-guided optimization.
- Inlining across effect handlers (`Perform`/`Resume`) or other complex control-flow constructs.

## Proposal A: Generalize Small-Function Inlining (Compiler / MIR)

### Why this matters for Map

`core::map` uses small helper functions (`index_for`, `next_index`, `buckets_for_elems`) and small
methods. Without inlining, each helper is a MIR call with:

- argument moves/copies,
- new frame allocation,
- return plumbing.

On a VM/interpreter, removing even “cheap” calls can be a big win.

### A1. Expand eligibility: multi-block functions

Extend the existing inliner to support functions with a small number of basic blocks and block
parameters.

Current implementation limits (tunable):

- `INLINE_MAX_BLOCKS`: 8
- `INLINE_MAX_INSTRUCTIONS`: 24 (sum across all blocks; terminators not counted)

Eligibility rejects:

- any call-like instruction (`Call*`, `VCall`, `ICall`),
- effect instructions (`PushHandler`, `Perform`, `Resume`, etc.),
- obvious allocation/mutation instructions (see A2),
- multi-return (`ReturnMulti`).

### A2. Keep the inliner conservative (no calls, no allocations)

In the initial implementation, the inliner remains intentionally conservative:

- it still rejects any callee that contains a call-like instruction (`Call*`, `VCall`, `ICall`),
- it also rejects obvious allocation/mutation ops (`MakeEnum`, `MakeStruct`, `IndexSet`, etc.)
  to avoid code-size blowups and to avoid interfering with MIR escape analysis.

This is sufficient to inline `core::map` helpers like `index_for` / `next_index` without changing
optimization behavior for `Option`-heavy code (which relies on unboxed-return transforms).

### A3. Core algorithm sketch (SSA with block params)

Rusk MIR uses block params (phi-like). A multi-block inliner must:

1. Split the caller block at the call site into:
   - `pre` (instructions before call),
   - `post` (instructions after call), turned into a new block.
2. Clone callee blocks into the caller with fresh `BlockId`s.
3. Map callee locals → fresh caller locals.
4. Map callee entry params → call arguments (including type reps).
5. Rewrite each callee `Return { value }` terminator into `Br { target: post, args: [value] }`
   where `post` expects one param (the return value) bound to the call’s `dst`.
6. Remap terminators (`Br`, `CondBr`, `Switch`) to the new block ids, remapping their argument
   operands via the local map.

This is standard SSA inlining; the main complexity is ensuring block-param arities match and
preserving evaluation order at the call site (split-before-inline ensures this).

### A4. Where in the pipeline

Keep it where it is today (after `resolve_call_targets()`), but consider running it:

- **before** peephole / bytecode optimization passes (so they see larger straight-line regions),
- and possibly **iteratively** (inline → simplify → inline again) with a low max iteration count.

## Proposal B: “Non-dynamic” `VCall` Fast Paths (VM)

Inlining is not sufficient for the `Hash::hash` / `Eq::eq` hot path inside generic Map methods,
because the call is currently a `VCall` (dynamic dispatch).

However, in `phase7_map_dict` the receiver is a primitive value (`int`), and the method is a known
core interface method. That is “dynamic” only because generics are erased, not because the runtime
needs true polymorphism.

### B1. Add a fast path for primitive receiver + known method

In the VM’s `VCall` execution, before doing `module.methods[(type_name, method)]` lookup:

1. Check whether the receiver is a primitive (`Value::Int`, `Value::Bool`, etc.).
2. Check whether `method` is one of a small set of canonical core interface method ids, e.g.:
   - `core::hash::Hash::hash`
   - `core::ops::Eq::eq`
   - `core::ops::Ne::ne`
   - (optionally) `core::ops::{Add,Sub,Mul,Div,Rem}::*` for `int`/`float`, etc.
3. If matched, directly execute the corresponding intrinsic or VM operation (no dispatch table
   lookup, no wrapper call frame).

This is essentially **devirtualization at runtime** with a small, stable dispatch table.

### B2. Why VM-level fast path is attractive

- It does not require monomorphization.
- It benefits *all* constrained-generic code that ends up calling `Hash::hash`/`Eq::eq` on primitives.
- It avoids per-call overhead that is currently quite expensive:
  - constructing/cloning `type_name` and `method` strings,
  - hashing/lookup in `module.methods`,
  - pushing a new frame for tiny wrapper impls.

### B3. Keep semantics identical

This must be exactly equivalent to the normal dynamic dispatch outcome.

Constraints:

- Only apply to primitives where the compiler already synthesizes an impl for the relevant
  interface method (which is already true for `Hash`/`Eq`/etc).
- For `readonly`, primitives are unaffected; the method bodies are readonly anyway.

### B4. Optional: dispatch caching / interning

Even without semantic devirtualization, `VCall` could be sped up by:

- interning type names and method ids at module load,
- storing `module.methods` in an id-based table,
- avoiding `String` clones in the hot loop.

This is a more general improvement, but also a larger refactor; it can be a follow-up.

## Implementation Notes (Done)

### Compiler (MIR inliner)

- Expanded `inline_tiny_functions()` to support inlining small multi-block CFGs by:
  - splitting the caller block at the callsite,
  - cloning callee blocks + remapping locals,
  - rewriting callee `Return` terminators to branch to a continuation block.
- Kept eligibility strict:
  - no calls/effects,
  - no obvious allocation/mutation instructions,
  - small instruction + block count limits.

### VM (primitive `VCall` fast path)

- Added a `VCall` fast path for primitive receivers for:
  - `core::hash::Hash::hash`
  - `core::ops::Eq::eq`
  - `core::ops::Ne::ne`
- Added `VmMetrics.vcall_fast_path_hits` and exposed it in `rusk-measure --json`.

### Tests

- Added a metrics test to ensure the fast path is actually taken for generic `Hash`/`Eq` calls.
- Added a compiler MIR test that verifies a small multi-block helper is inlined (call removed).

## Evaluation Plan

### Primary benchmark

- `benchmarks/phase7_map_dict.rusk`
  - Track wall-clock median runtime.
  - Track VM metrics:
    - `call_instructions` should drop significantly after inlining improvements.
    - `vcall_instructions` count may remain, but **time per vcall** should drop after VM fast path.

### Secondary benchmarks

- `phase4_call_dispatch` (call-heavy loop; inliner should help)
- `phase1_var_loop` (should not regress)
- `phase5_gc_epoch` (ensure no unexpected GC behavior changes)

## Risks / Tradeoffs

- **Code size increase**: inlining can balloon bytecode size; guard with strict thresholds + depth.
- **Compile time regression**: more inlining means more MIR rewriting; ensure opt-level gating.
- **VM complexity**: method-name-based fast paths are easy to grow; keep the set small and clearly
  justified (Hash/Eq first).

## Future Work (Out of Scope, but Related)

### Generic specialization / monomorphization (bytecode-level)

Longer-term, the “right” fix for “generic code calling primitive ops via `VCall`” is to generate
specialized bytecode for concrete instantiations (e.g. `Map<int,int>`), replacing `VCall` with
direct `CallId`/intrinsics and enabling even more inlining and constant folding.

This likely requires:

- representing specialization targets in the module (similar to existing host specialization),
- VM support to choose specialized bytecode functions,
- compiler support to discover instantiations + clone functions with type substitution.

This is a larger design and can be proposed separately if/when we want to commit to it.
