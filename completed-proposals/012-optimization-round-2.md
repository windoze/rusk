# Optimization Round 2: Interpreter Hot-Path + Core Intrinsics

## Summary

Current microbenchmarks show Rusk is **~13–22× slower than Python** on “basic VM work” (locals/loops,
field access, call dispatch) and **~70× slower** on the effects-based generator benchmark.

This proposal focuses on two near-term optimization batches:

1) **Remove avoidable per-call allocations** in the interpreter call path (especially argument
   packaging).
2) **Reduce “primitive op” overhead** by fast-pathing core intrinsics, then graduating to
   dedicated MIR opcodes and simple inlining for tiny call-heavy helpers.

The remaining topics are explicitly deferred as **Future Plan**:

- Effects/generators specific optimizations
- Longer-term “new execution engine” options (bytecode VM, JIT/AOT)

---

## Motivation

We now have a repeatable benchmark harness:

- `benchmarks/compare.py` produces `benchmarks/results.md` and `benchmarks/results.json`.

On the current baseline run (`benchmarks/results.md`), Rusk is significantly slower:

| benchmark | rusk/py (median) | what it stresses |
|---|---:|---|
| `phase1_var_loop` | ~19.8× | locals + while loop + int ops |
| `phase2_object_access` | ~13.9× | tuple/struct get/set |
| `phase3_closure_capture` | ~15.8× | closure call in tight loop |
| `phase4_call_dispatch` | ~13.0× | function call dispatch |
| `phase5_gc_epoch` | ~22.1× | allocation churn / GC |
| `phase6_effects_generator` | ~72.2× | yield/resume style control flow |

The key takeaway is that we are paying too much overhead per “small” operation.
Python is also an interpreter, but its hot path for:

- integer arithmetic
- comparisons
- local variable access
- function calls (especially builtins)

is heavily optimized C code with minimal allocation and minimal indirection.

Rusk’s interpreter currently executes MIR in Rust, but many “primitive ops” are implemented as
**host calls** (e.g. numeric operators lowered to `core::intrinsics::*`). That turns a single
`x = x + 1` into “evaluate args + allocate arg Vec + dispatch host closure + pattern match args”.

Example signal from the current results JSON:

- `phase1_var_loop` (n=200_000, iters=3) performs ~2.4M host calls (≈ 4 intrinsics per loop
  iteration × 200k × 3), implying *millions* of tiny argument vector allocations today.

This is the exact kind of overhead we should be able to eliminate in a targeted optimization pass.

---

## Goals

### Primary goals (this round)

- Make “basic loops” competitive with a typical bytecode VM baseline:
  - drastically reduce per-iteration overhead for `int` arithmetic + comparisons
  - remove per-call heap allocations for common call shapes
- Reduce `host_calls` and `call_instructions` for non-FFI-heavy programs by turning core ops into
  direct interpreter operations (or MIR opcodes).
- Improve `phase1` / `phase4` wall-clock time by a large constant factor (target: **5–10×**),
  without changing language semantics.

### Constraints

- Keep `#![forbid(unsafe_code)]` (interpreter and MIR crates currently forbid unsafe).
- Keep changes internal-only:
  - language source semantics unchanged
  - no new user-facing syntax/features
- MIR compatibility is not required for this round (we can evolve compiler + interpreter in lockstep
  if it simplifies the hot path).

---

## Non-goals (this round)

- Reworking the effect system / continuation model for generator performance.
- Adding a new bytecode format, a JIT, or an AOT compiler backend.
- Changing GC algorithms beyond what is strictly necessary for the proposed call/intrinsic changes.

---

## Part 1 — Remove per-call argument allocations

### Current state

Hot-path calls currently allocate an argument vector on every call:

- `eval_args()` in `crates/rusk-interpreter/src/interpreter.rs` allocates a fresh `Vec<Value>`
  (`Vec::with_capacity(args.len())`) and pushes evaluated operands.
  - See: `eval_args()` around `interpreter.rs:1988`.

This impacts:

- `Instruction::Call` (name-based dispatch)
- `Instruction::CallId` (ID-based dispatch)
- `Instruction::ICall` / `Instruction::VCall`
- `Instruction::Perform` (effect dispatch also builds vectors)

In tight loops, the number of calls can be “per arithmetic op”, leading to millions of tiny heap
allocations and memmoves.

### Proposal

#### 1A) Replace “return a new Vec” with “fill into a reusable buffer”

Introduce one or more interpreter-owned scratch buffers, e.g.:

- `call_args: Vec<Value>`
- (optional) `call_args2: Vec<Value>` for nested situations where a host call may re-enter the
  interpreter and also use the buffer (rare, but possible).

Then replace:

- `fn eval_args(...) -> Result<Vec<Value>, _>`

with:

- `fn eval_args_into(&mut self, frame: usize, ops: &[Operand], out: &mut Vec<Value>) -> Result<(), _>`

Call sites become:

- `out.clear(); out.reserve_exact(ops.len()); eval_args_into(..., out);`
- pass `&out` as `&[Value]` to host functions
- pass `&out` to MIR frame parameter initialization

This turns “allocate + push” into “clear + reuse capacity”, removing virtually all heap traffic
from argument packaging.

#### 1B) Avoid building a `Vec<Value>` for trivial arities (optional but cheap)

Common intrinsics are binary (`int_add`, `int_lt`, etc.). If we keep any “call-based intrinsic”
path temporarily, we can add a specialized path for 0/1/2 arguments:

- evaluate directly into fixed locals:
  - `let a = eval_operand(...)`
  - `let b = eval_operand(...)`
- call intrinsic handler with a small fixed slice (or a specialized `host2(a, b)` signature).

This can be staged:

1) start with buffer reuse (1A)
2) later, introduce specialized host signatures where it’s worth it

### Expected impact

- Large constant-factor win for `phase1_var_loop` and `phase4_call_dispatch` (and any code where
  “small ops” are implemented as calls).
- Reduced GC pressure indirectly (fewer allocations -> fewer GC triggers).
- Simpler profiling: `host_calls` becomes a better signal (less polluted by arg allocation cost).

### Rollout plan

1) Add reusable arg buffers to `Interpreter`.
2) Update all call sites (`CallId`, `Call`, `ICall`, `VCall`, `Perform`) to use `eval_args_into`.
3) Keep behavior identical; only change packaging.
4) Validate with:
   - `cargo test`
   - `benchmarks/compare.py --validate` (regenerate results)

---

## Part 2 — Make primitive ops cheap (intrinsics fast path → MIR opcodes → inlining)

Part 1 removes allocations, but it still leaves “primitive work” expressed as calls.
Part 2 reduces the remaining dispatch overhead.

### 2A) Immediate: interpreter fast-path for core intrinsics

Even before MIR changes, the interpreter can special-case the most common intrinsics when it sees:

- `CallTarget::Host(<id>)` where `<id>` corresponds to:
  - `core::intrinsics::int_add`
  - `core::intrinsics::int_sub`
  - `core::intrinsics::int_mul`
  - `core::intrinsics::int_eq` / `int_lt` / etc.
  - `core::intrinsics::bool_not`, `bool_eq`, …

This bypasses:

- host closure dispatch
- slice pattern matching inside `register_int_fns` in `crates/rusk-interpreter/src/corelib.rs`

Implementation sketch:

1) During module load / host import binding, build a small “intrinsic id table”:
   - `IntrinsicIds { int_add: Option<HostImportId>, int_lt: Option<HostImportId>, ... }`
2) In the `CallTarget::Host(id)` path, match `id` against that table:
   - if it’s a known intrinsic, execute directly in Rust using the evaluated operands
   - otherwise fall back to normal host call

This is safe, internal-only, and easy to validate.

### 2B) Preferred: introduce dedicated MIR opcodes for arithmetic + comparisons

Interpreter fast paths are a good staging tactic, but the cleaner end state is:

- Numeric operators and comparisons should be **MIR instructions**, not host calls.

Proposed MIR additions (initial set):

- `IntAdd { dst, a, b }`
- `IntSub { dst, a, b }`
- `IntMul { dst, a, b }`
- `IntDiv { dst, a, b }` (keep division-by-zero trap semantics)
- `IntMod { dst, a, b }`
- `IntLt/IntLe/IntGt/IntGe/IntEq/IntNe { dst, a, b }`
- `BoolNot { dst, v }`
- `BoolEq/BoolNe { dst, a, b }`

Then update compiler lowering (operators/desugarings in `crates/rusk-compiler`) to emit these ops
directly instead of `CallId` to `core::intrinsics::*`.

Benefits:

- reduces instruction count (no “call plumbing” for primitive ops)
- reduces runtime indirection (no host import lookup on hot path)
- makes the interpreter loop dramatically tighter and more cache-friendly

### 2C) Inline tiny MIR functions (call dispatch benchmark)

`phase4_call_dispatch` is dominated by call overhead (`inc(x) -> x + 1`).

Once primitive ops are cheap, call overhead becomes the next ceiling. A minimal, safe inliner can
remove most of the overhead for the common case:

- inline non-recursive functions
- small body threshold (e.g. <= N instructions / <= M blocks)
- no effect operations inside (or handle conservatively)
- no heap allocations in the inliner itself on hot paths (compile-time only)

This is a compiler-only optimization; interpreter remains unchanged.

The inliner can start very conservative and still deliver a measurable win for microbenchmarks and
real-world “helper-heavy” code.

### Expected impact

If we do 2A+2B, we should expect:

- `phase1_var_loop` to drop from “millions of host calls” to near-zero host calls.
- A major reduction in `call_instructions`, `host_calls`, and executed instruction count.
- Large wall-clock improvements; the goal is to eliminate most of the ~10–20× gap on
  arithmetic-heavy loops.

If we additionally do 2C, we should expect:

- `phase4_call_dispatch` to improve substantially (function call overhead removed for tiny helpers).

### Rollout plan

Staged to keep risk controlled:

1) Implement 2A (interpreter fast-path) behind an internal flag or as default if safe.
2) Add MIR opcodes (2B) + compiler lowering changes, keep the old host-call lowering as a fallback
   until tests pass.
3) Once stable, remove the old lowering for the covered ops.
4) Implement a conservative inliner (2C), initially opt-in (compiler flag), then default-on once
   validated.

---

## Measurement / Success Criteria

We should treat `benchmarks/compare.py` as the acceptance harness.

### Required checks

- `cargo test`
- `.venv/bin/python benchmarks/compare.py --validate`

### Success criteria (targets, not guarantees)

- `phase1_var_loop`: reduce rusk/py ratio from ~20× to **< 5×**.
- `phase4_call_dispatch`: reduce rusk/py ratio from ~13× to **< 5×**.
- `phase1_var_loop` metrics in `rusk-measure`:
  - `host_calls` drops by ~4× per loop iteration (ideally near-zero)
  - `call_instructions` drops similarly
- No semantic changes: all benchmarks still return `EXPECTED`.

---

## Future Plan (Explicitly deferred)

### A) Effects/generators specifically

The current generator benchmark uses general effect handling + continuation capture.
Python generators have specialized VM support, so a large gap is expected.

Future optimization directions:

- Compiler lowering of “generator-ish” functions into an explicit **state machine** (async/await style).
- Runtime work to reduce continuation capture costs (pooling/reuse, fewer allocations).
- Effect handler lookup optimization (better indexing than linear scan of clauses/handlers).

### B) Longer-term execution engine options

If we want Python-competitive or better performance broadly, we will likely need one of:

- A compact bytecode VM (smaller instructions, tighter dispatch loop, fewer pointer indirections).
- A JIT (e.g. Cranelift) for hot functions, with interpreter fallback.
- An AOT mode for stable scripts.

These options are intentionally out of scope for Optimization Round 2.

---

## Open questions

1) Do we want to preserve serialized `.mir` compatibility across versions, or treat MIR as strictly
   internal and versioned with the toolchain?
2) Should the interpreter keep host-call fallbacks for core intrinsics (for debugging), or fully
   remove them once MIR opcodes land?
3) How aggressive should the inliner be by default (thresholds, recursion handling, effects)?

