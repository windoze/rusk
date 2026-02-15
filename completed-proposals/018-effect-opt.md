# Effect Runtime Optimizations (Tail Resume, Continuation Capture, Handler Cache)

Date: 2026-02-15  
Status: Draft

This proposal describes a prioritized set of optimizations to reduce overhead in Rusk’s
effects/handlers implementation, primarily in the **bytecode VM** (`crates/rusk-vm`).

The goal is to improve performance in common “generator-like” and “control-flow-like” scenarios:

- generator/coroutine style `yield` / `resume` loops
- exception-like abortive effects (`try/catch` shape)
- stateful handlers (`use_state` shape)

The proposed work focuses on:

1) **Tail `resume`** (avoid stack/handler growth in generator loops)  
2) **One-shot continuation capture by move/split** (avoid cloning large stack slices)  
3) **Abortive handlers** (skip continuation capture when continuation is provably unused)  
4) **Handler lookup fast paths** (inline cache)  
5) **Lower allocation/identity overhead** (interning + small-buffer strategies)

This is a performance proposal only: it does not change surface language semantics.

---

## Motivation

Rusk ships an effects microbenchmark intended to resemble a generator:

- `benchmarks/phase6_effects_generator.rusk`

As of 2026-02-15, `benchmarks/results.md` shows a large gap vs the Python reference
(`benchmarks/phase6_effects_generator.py`). This benchmark is deliberately simplistic, but it
highlights that the current runtime pays substantial per-`perform` / per-`resume` costs.

The performance gap appears to be structural, not just “missing peephole opts”, because it is
dominated by:

- continuation capture cost (currently copies/clones stack slices),
- resumption strategy (currently tends to grow the VM stack/handler stack in tail-resume patterns),
- handler lookup cost (linear scan + string-heavy effect identity work).

Closing this gap is important because the same runtime mechanisms are expected to back:

- a `try/catch` implementation using effects,
- UI-style state effects,
- various “async-ish”/iterator-like libraries.

---

## Background: Semantics Requirements

This proposal must preserve the language semantics described in `RUSK_SPEC.md` (§7):

- **Handlers are delimited** to scrutinee evaluation (§7.4).
- Continuations are **one-shot** (§7.3).
- `match` with effect arms installs handlers only while evaluating the scrutinee (§7.2).
- Calling a continuation resumes scrutinee evaluation and then continues to value-arm matching:
  the call returns the “handled region result” (§7.3).

The reference compiler enforces delimitation by lowering `match` with effect arms into a helper
function (the helper frame becomes the “handler owning frame”). This property must remain true.

---

## Background: Current Bytecode VM Behavior (High Level)

The bytecode VM implements effects via:

- `PushHandler` / `PopHandler` (install/uninstall handler entries on `vm.handlers`)
- `Perform` (search handler stack; capture continuation; jump to handler clause)
- `Resume` (consume continuation token; inject effect return value; append captured frames/handlers)

Two key implementation details matter for performance:

1) **Continuation capture is currently copy-heavy**
   - `Perform` constructs a continuation by cloning a slice of `vm.frames` and a slice of
     `vm.handlers` into a `ContinuationState`.
   - Cloning frames implies cloning register vectors and values.

2) **Resumption currently behaves like a normal call**
   - `Resume` extends `vm.frames` and `vm.handlers` with the captured continuation.
   - The resumed computation eventually “returns to” the handler clause frame, because the handler
     clause is still on the stack.

This is semantically fine, but it is not always the most efficient operational model.

---

## The “Tail Resume” Nesting Problem

Consider the common generator handler shape:

```rusk
@Yield.yield(v) => {
  // do something with v
  resume(())
}
```

Logically, this is a loop:

1) perform `yield(v)`
2) handler processes `v`
3) resume continuation and keep going
4) repeat

However, operationally, the current VM behaves more like:

- “resume is a non-tail call”

so each cycle tends to:

- keep an extra handler activation around (waiting for the continuation to return),
- re-install handler entries via captured handler stacks,
- accumulate VM frames/handlers over many yields in a long-running generator loop.

This has multiple costs:

- extra handler entries to scan on later `perform`s,
- more memory traffic and allocations,
- worse locality.

The core observation: in generator-like code, `resume(k)` is often in **tail position**, so we
should be able to treat it like a tail call and **splice** stacks rather than stack-growing calls.

---

## Goals

1) **Asymptotic stability for generator loops**
   - tail-resume should not grow `vm.frames` / `vm.handlers` with the number of yields.

2) **Reduce per-`perform` overhead**
   - avoid cloning large frame/handler slices when capturing one-shot continuations.

3) **Reduce per-`perform` handler lookup overhead**
   - add a safe fast path for repeated effect calls under stable handler stacks.

4) **Preserve semantics** from `RUSK_SPEC.md` (§7), especially:
   - delimitation guarantee,
   - one-shot behavior,
   - deep handling as currently implemented (effects remain handled after resuming).

---

## Non-goals

- Multi-shot continuations.
- New surface syntax or semantic changes to effects/handlers.
- JIT/AOT compilation of bytecode.
- Reworking unrelated VM hot paths (this proposal is scoped to effects).

---

## Proposed Work Items (Prioritized)

### P0: Tail `resume` (stack splice / tail-call elimination)

**Problem:** When a continuation call is the last computation in a handler clause, the VM still
keeps the handler activation around and runs the continuation “on top of it”, which can cause
stack/handler growth in generator-like loops.

**Proposal:** Add a “tail resume” path that replaces:

- “resume continuation then return to handler”

with:

- “tail-resume: splice to continuation and do not return to the handler activation”

#### Tail position definition (v0)

Eligible when:

- the continuation call is within a handler clause body, and
- its result is returned directly as the clause result (no post-processing).

This covers typical generator handlers and many exception handlers that resume once.

#### Implementation options

Option A (preferred): explicit bytecode opcode

- Add `Instruction::ResumeTail { k, value }` (name bikesheddable).
- Compiler decides tail position precisely; VM executes a dedicated fast path.

Option B: VM-side pattern matching on instruction sequences (discouraged)

- Detect “`Resume` then immediately return that result” patterns in bytecode.
- This couples the VM to current lowering patterns and is fragile across compiler changes.

#### Semantics sketch

At runtime, a tail-resume should:

1) consume the continuation token (one-shot, unchanged),
2) inject the provided value into the suspended `perform` destination (unchanged),
3) discard the current handler activation frames/handlers that would otherwise remain waiting,
4) install the continuation frames/handlers as the current execution state.

The key is that the handler activation does not accumulate when it is tail-calling into the
continuation.

#### Expected impact

- Large win for generator-like loops (`yield`/`resume`).
- Reduced handler lookup cost due to stable handler stack depth.
- Reduced memory pressure from avoiding accumulation of frames/handlers.

---

### P0: One-shot continuation capture by move/split (avoid cloning large slices)

**Problem:** `Perform` currently clones `vm.frames[owner_depth..]` and rebased handler entries.
This is expensive in deep stacks or with large register files.

**Proposal:** Leverage one-shot continuations to **move** as much state as possible out of the VM
instead of cloning, while still snapshotting the handler owning frame correctly.

#### Key insight

Even with one-shot continuations, we generally need to snapshot the owning frame at capture time,
because the handler clause runs in that owning frame and may mutate registers.

But we do not need to clone everything:

- frames above the owning frame are not needed to run the handler clause and can often be moved out,
- handler entries at/above the owning depth can often be moved out as well.

So we can target:

- clone **one** frame (owning frame snapshot),
- move the rest via `Vec::split_off` and similar operations.

#### Expected impact

- Significant reduction in per-`perform` cost.
- Better scaling with call depth inside handled regions.

---

### P1: Elide continuation capture when the continuation is provably unused (abortive handlers)

**Problem:** Many effects are used in an exception-like way: the handler never resumes the
continuation. Capturing a continuation in those cases is pure overhead.

**Proposal:** If the compiler can prove the continuation binder is not used by a given effect arm,
compile that arm in an “abortive” mode where `Perform` does not capture a continuation at all.

This should benefit:

- `try/catch`-style patterns,
- early-exit control flow expressed via effects,
- “fallback value” handlers.

#### What “unused” means

Safe to elide capture when the continuation value is not observed:

- not called,
- not stored,
- not returned,
- not passed to any other function.

If it is referenced even once (e.g. stored in a data structure), we must capture it.

#### Implementation options

Option A (preferred): compile-time decision

- Extend handler clause metadata to indicate whether a continuation token is required.
- VM `Perform` can skip continuation creation when not required and jump to handler code directly.

Option B: VM heuristic (not recommended)

- Try to infer “continuation unused” at runtime from bytecode patterns.
  This is unreliable and hard to guarantee.

#### Expected impact

- Big win for exception-like effects (no continuation capture on hot path).

---

### P2: Handler stack inline cache (“last-called effect handler” fast path)

**Problem:** Handler lookup is currently a linear scan of `vm.handlers`, then `clauses`.
In tight loops, the same effect is often performed repeatedly under a stable handler stack.

**Proposal:** Add a small inline cache in the VM, e.g.:

- `last_effect_lookup: Option<(effect_key, handler_stack_version, handler_index, clause_index)>`

On `Perform`:

1) If cache is present and `handler_stack_version` matches:
   - check the cached clause first,
   - verify effect identity and pattern match,
   - dispatch directly on success.
2) Otherwise, fall back to full search and update the cache.

#### Invalidation / versioning

We need a lightweight invalidation mechanism, e.g.:

- `vm.handler_stack_generation: u32` incremented on:
  - `PushHandler` / `PopHandler`,
  - handler unwinds on `Return`,
  - `Resume` / tail-resume (stack splice).

Cache can also guard on `vm.handlers.len()` as a cheap sanity check.

#### Expected impact

- Reduced overhead for repeated effects in hot loops.
- Especially effective after P0 makes stacks stable (high cache hit rate).

---

### P2: Reduce effect identity overhead (interning / IDs, fewer allocations)

**Problem:** Effect identity in the VM hot path currently involves `String`/`Vec` construction and
comparison work.

**Proposal (directional):**

- Intern interface/method strings into stable IDs in the module/bytecode layer.
- Represent performed effects with compact IDs so `Perform` can compare integers instead of strings.

This can be staged:

1) VM-only interning table mapping “runtime effect identity” to a small integer.
2) If needed, evolve bytecode encoding to carry IDs directly.

---

### P2: Reduce small, repeated allocations (small buffers + fast paths)

**Problem:** Even after major structural changes, effects tend to allocate many small `Vec`s:

- argument vectors,
- bind vectors for patterns,
- interface type-arg vectors.

**Proposal (directional):**

- Use small-buffer strategies for common arities (0–2 args).
- Add fast paths for trivial patterns (`_`, `bind`) to avoid creating bind vectors at all.

This is lower priority than P0/P1 but can improve steady-state hot loop performance.

---

## Measurement Plan

We want changes to be measurable and attributable. The repo already supports VM instruction metrics:

- `cargo run --release --bin rusk-measure -- --backend bytecode --opt-level o2 --metrics ...`

### Existing benchmark

- `benchmarks/phase6_effects_generator.rusk` (primary)

### Recommended additional benchmarks (follow-up)

Add small, stable `.rusk` programs in `benchmarks/` for:

1) abortive effect (`try/catch`-like) without resumption
2) stateful handler (`use_state`-like) with small state and high operation counts

### Metrics to track

- wall-clock time (median across repeats)
- VM instruction counts:
  - `perform_instructions`, `resume_instructions`
  - `push_handler_instructions`, `pop_handler_instructions`
  - `call_instructions` (continuation-call lowering overhead)
- optional new VM counters (if useful for debugging/verification):
  - max `vm.frames.len()` and max `vm.handlers.len()` during run
  - handler-cache hit rate
  - continuation capture “bytes cloned” estimate

---

## Compatibility / Semantics

All proposed changes are intended to preserve:

- one-shot continuation behavior (`RUSK_SPEC.md` §7.3),
- delimitation guarantee (`RUSK_SPEC.md` §7.4),
- current deep-handling behavior (effects remain handled after resuming).

In particular:

- Tail-resume changes *how* we realize deep handling (splice vs stack growth), not what is handled.
- Abortive handler optimization applies only when the continuation value is not observed.

---

## Rollout Plan (Suggested)

1) Implement P0 tail-resume (prefer a dedicated opcode) + tests.
2) Implement P0 capture-by-move/split, measure across all effects benchmarks.
3) Implement P1 unused-continuation elision, validate with exception-like tests.
4) Implement P2 handler-cache + identity/alloc optimizations as incremental wins.

---

## Open Questions

1) Where should “tail position” be decided?
   - compiler (preferred) vs VM pattern detection.

2) Should tail-resume apply only to handler clauses, or to all continuation calls in tail position?
   - initial scope: handler clauses only.

3) How far should we go in changing bytecode encoding to reduce effect identity overhead?
   - start VM-only, then revisit bytecode encoding if needed.

4) Should we expose extra VM metrics (max stack depth, cache hits) in `VmMetrics`?
   - useful for regression testing and performance tuning.

