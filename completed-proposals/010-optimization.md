# Rusk Compiler + Interpreter Optimization Proposal

## Summary

This proposal outlines **concrete optimization opportunities** in the current Rusk toolchain based on
a read-through of:

- Compiler front-end: `crates/rusk-compiler/src/*` (notably `compiler.rs`, `typeck.rs`, `modules.rs`)
- MIR interpreter + GC: `crates/rusk-interpreter/src/*` (notably `interpreter.rs`, `gc.rs`)
- MIR data model: `crates/rusk-mir/src/lib.rs`

The focus is on changes that can materially improve:

- **Interpreter runtime throughput** (the biggest low-hanging fruit today)
- **Compiler compile-time** for medium/large programs
- **Memory usage / allocations** (compiler + interpreter)

This proposal does not implement changes; it describes a **measurement-first** plan and a staged set
of optimizations with increasing scope/risk.

---

## Motivation

Rusk is currently in a phase where the compiler and interpreter prioritize correctness and
readability. That is appropriate early on, but several current design choices are likely to become
hard bottlenecks as:

- programs grow larger
- the language grows more features (effects, generics, interfaces)
- the runtime is used for longer-running workloads

Optimizing now—especially by removing accidental algorithmic overhead—can preserve Rusk’s “fast
feedback” development experience and keep the interpreter viable as a reference runtime.

---

## Current Findings (Hotspots / Cost Centers)

### 1) Interpreter clones MIR functions/blocks/instructions frequently

In `crates/rusk-interpreter/src/interpreter.rs`, the interpreter often clones large MIR structures
to work around borrowing constraints.

Examples:

- **Function calls clone the full MIR function body**:
  - `Interpreter::call_function` does `self.function(func)?.clone()`
  - `Frame::new(function, ...)` only needs `function.name` and `function.locals`, and does *not*
    retain blocks/instructions, meaning most of the clone cost is thrown away.
- **Branching / entering blocks clones the full function body**:
  - `Interpreter::enter_block` clones the current function just to access `block.params`, then
    immediately discards it.
- **Main interpreter loop clones per-step**:
  - `Interpreter::run_loop` clones `Instruction` and `Terminator` each step (`clone()` on values
    that contain `String`/`Vec`/nested MIR structures).

This is likely the single largest performance issue today: it introduces substantial allocation and
copying work that scales with MIR size, not just executed instruction count.

### 2) Runtime object field access uses string keys + tree maps

Runtime heap objects use stringly-typed lookup structures:

- `HeapValue::Struct { fields: BTreeMap<String, Value>, .. }`
- `Instruction::GetField { field: String, .. }`

Additionally, tuple access parses a string field name at runtime:

- `GetField` / `SetField` on tuples use `field.strip_prefix('.').and_then(|s| s.parse::<usize>())`

This implies:

- `O(log n)` map lookup per struct field access (plus string comparisons)
- runtime parsing work for tuple field access
- additional allocations (strings stored in MIR and in heap values)

For object-heavy programs (records, closures as structs, AST-like data), this overhead is likely to
dominate.

### 3) GC does O(heap_slots) mark clearing every collection

In `crates/rusk-interpreter/src/gc.rs` (`MarkSweepHeap::collect_garbage`):

- every GC cycle clears marks by iterating over **all slots** (`for slot in &self.slots`), even if
  only a small fraction is live

This is a classic mark-sweep cost that can be improved (see “Mark epoch” below). It is especially
important if the interpreter becomes allocation-heavy (arrays, tuples, enums, structs, continuations).

### 4) Compiler closures/helpers capture *all visible bindings*

In `crates/rusk-compiler/src/compiler.rs`, closure and helper lowering currently uses:

- `FunctionLowerer::visible_bindings()` → merges all scopes into a `BTreeMap<String, VarInfo>`

This is then used for:

- `FunctionLowerer::lower_lambda_expr`: captures **all visible named bindings** into the closure
  env array, regardless of whether the lambda body references them.
- `FunctionLowerer::lower_match_via_helper`: captures all visible bindings into a helper function’s
  argument list for effectful matches.

This is simple and deterministic, but has potentially large costs:

- bigger closure env arrays → more allocations + copying at runtime
- larger helper signatures → more argument passing and local initialization
- capturing unused values → unnecessary retains (can increase GC pressure)

### 5) Compiler scope representation is map-heavy

`FunctionLowerer` stores:

- `scopes: Vec<BTreeMap<String, VarInfo>>`

Lookups (`lookup_var`) walk scopes from innermost to outermost, doing `BTreeMap::get` at each depth.

This design is fine for correctness but likely suboptimal when:

- there are many nested blocks
- name lookups are frequent (typechecked languages tend to be lookup-heavy)

### 6) Names are `String` across the pipeline (lexer → AST → typeck → MIR → interpreter)

Names are currently represented as `String` nearly everywhere:

- Lexer: `TokenKind::Ident(String)`, `TokenKind::String(String)`, `FStringPart::Text(String)`, etc.
- AST: `Ident { name: String }`, `Path { segments: Vec<Ident> }`
- Typeck: `TyCon::Named(String)`, env tables keyed by `String`
- MIR: function names, type names, field names, handler ids, method ids are strings
- Interpreter: uses these strings as hot-path keys

This is a common early-stage design, but it increases:

- allocation count
- hashing/comparison cost
- memory footprint

---

## Goals

### Near-term goals (high impact, low semantic risk)

- Remove accidental cloning of large MIR structures in the interpreter.
- Reduce interpreter allocation churn on hot paths.
- Establish a repeatable benchmark/profiling harness so changes are measurable.

### Medium-term goals (structural improvements)

- Improve runtime object representation for fast field/index access.
- Reduce compiler closure/helper capture to only what’s needed.
- Replace “stringly typed” runtime identifiers with interned IDs where beneficial.

### Non-goals (for this proposal)

- Implement a JIT.
- Implement a full optimizing compiler (inlining, LICM, etc.) before fixing data-structure overhead.
- Break no-std support for `rusk-interpreter` or `rusk-mir`.
- Preserve backward compatibility (MIR format, serialized modules, or crate APIs). At this stage we
  can change compiler/interpreter/MIR in lockstep and freely break interfaces to simplify and
  optimize.

---

## Proposal: Optimization Roadmap

### Phase 0 — Measurement First (Bench + Profiling)

Status: **DONE** (implemented in-tree)

Implemented artifacts:

- Compiler timing API:
  - `rusk_compiler::CompileMetrics`
  - `compile_to_mir_with_options_and_metrics`
  - `compile_file_to_mir_with_options_and_metrics`
- Interpreter counters:
  - `rusk_interpreter::InterpreterMetrics`
  - `Interpreter::{metrics, reset_metrics, take_metrics}`
- Measurement runner binary:
  - `src/bin/rusk-measure.rs` (`--json`, `--warmup N`, `--iters N`)
- Smoke tests to keep the instrumentation wired up:
  - `tests/phase0_metrics.rs`

Add a small, stable set of benchmarks and counters to track:

**Compiler metrics**

- parse time
- typecheck time
- MIR lowering time
- total `compile_file_to_mir_with_options` wall time

**Interpreter metrics**

- executed instruction count
- allocation count (already partly tracked via `allocations_since_gc`)
- GC cycles and time
- function calls and branches

**Benchmark inputs**

- reuse `fixtures/` programs and add a few “microbench” inputs (e.g., tight loops, many function
  calls, heavy struct/tuple access, heavy allocation)

Success criteria for Phase 0:

- consistent local runs (`cargo bench` or `cargo test -- --ignored` style)
- CI-friendly and deterministic results (within normal variance)

Notes:

- Criterion is an option, but a custom lightweight timer-based harness may be preferable to keep
  no-std constraints out of core crates.

### Phase 1 — Interpreter: Eliminate Large MIR Clones

Status: **DONE** (implemented in-tree)

Implemented artifacts:

- Interpreter stores the loaded MIR module as `Rc<Module>` and exposes `Interpreter::new_shared`
  for cheap reuse across runs.
- Hot loop executes borrowed MIR:
  - `execute_instruction(&Instruction)`
  - `execute_terminator(&Terminator)`
- Call/branch paths no longer clone full `Function` bodies (including handler dispatch and block
  entry).
- `Frame` stores function identifiers as `Rc<str>` so the hot loop avoids allocating/cloning
  function name strings per step.

The interpreter should treat the loaded `Module` as immutable program data and avoid cloning it on
execution paths.

Concrete changes (conceptual):

1. **Avoid cloning `Function` in calls**
   - Change `Frame::new` to accept only what it needs (e.g. locals count + a function identifier),
     rather than consuming an owned `Function`.
   - In `Interpreter::call_function`, avoid `self.function(func)?.clone()`.

2. **Avoid cloning `Function` in `enter_block`**
   - In `Interpreter::enter_block`, extract `block.params` while holding an immutable borrow, then
     drop it before mutating the frame locals.
   - Clone only `Vec<Local>` (small), not the entire function (large).

3. **Avoid cloning `Instruction`/`Terminator` in `run_loop`**
   - Restructure `execute_instruction` and `execute_terminator` to take `&Instruction` /
     `&Terminator` (borrowed), cloning only the pieces that must be owned (ideally none in common
     cases).

Expected impact:

- Major reduction in allocations and copying.
- Runtime throughput improvement likely to be large for call/branch-heavy programs, because today
  each call/branch can clone large MIR structures.

Risk level:

- Medium: mostly refactoring + borrowing strategy, but touches core execution loop and needs careful
  validation with existing tests.

### Phase 2 — Runtime Object Layout + Field/Index Fast Paths

Goal: make “object operations” fast without needing pervasive compiler optimizations.

Status: **DONE** (implemented in-tree)

Implemented artifacts:

- MIR module metadata:
  - `rusk_mir::Module::struct_layouts` (per-type field order)
- New MIR ops for index-based access:
  - `Instruction::StructGet` / `Instruction::StructSet`
  - `Instruction::TupleGet` / `Instruction::TupleSet`
- Interpreter runtime object representation:
  - heap `Struct` fields are stored as `Vec<Value>` in layout order (no per-object map)
  - `StructGet`/`StructSet` and `TupleGet`/`TupleSet` execute as direct `Vec` indexing fast paths
- Compiler lowering:
  - tuple `.n` lowering emits `TupleGet`/`TupleSet` (no runtime string parsing)
  - struct field lowering emits `StructGet`/`StructSet` with resolved field indices
  - internal lowering-only structs (`$Cell`, `$Closure`) also use `StructGet`/`StructSet`

Benchmark (local, `rusk-measure`, release build):

- Input: `benchmarks/phase2_object_access.rusk`
- Command:
  - `cargo run --release --bin rusk-measure -- --json --warmup 2 --iters 10 benchmarks/phase2_object_access.rusk`
- Baseline (before Phase 2):
  - `run.avg_ns = 479302895` (≈ 479ms)
- After Phase 2:
  - `run.avg_ns = 435812016` (≈ 436ms)
  - ≈ **9.1% faster** on this benchmark on this machine

Planned changes (now implemented):

1. **Replace struct fields `BTreeMap<String, Value>` with index-based storage**
   - Store struct fields as `Vec<Value>` in field order.
   - Add a per-type field-name → index table in the interpreter (or in MIR metadata) so `GetField`
     becomes an index lookup.

2. **Remove tuple field parsing**
   - Introduce explicit tuple indexing operations in MIR (e.g. `GetTupleIndex { idx }`) rather than
     encoding tuple indices as strings like `".0"`.
   - Alternatively, represent tuple “fields” as numeric in MIR `GetField`, but this is a bigger
     type-level change.

3. **Special-case internal runtime structs**
   - `$Closure` and `$Cell` are internal compiler-generated types. The interpreter can store them
     as specialized heap objects to avoid map lookups (e.g. `HeapValue::Closure { func, env }`).

Expected impact:

- Significant for programs with heavy struct/tuple access (including closures).

Risk level:

- Medium/High: changes span compiler lowering + MIR data model + interpreter object layout. Needs a
  compatibility strategy (see below).

### Phase 3 — Compiler: Capture Analysis for Lambdas and Helpers

Status: **DONE** (implemented in-tree)

Implemented artifacts:

- Compiler performs a free-variable analysis over the AST for:
  - lambda bodies
  - match-with-effects helper bodies
- Lambdas/helpers now capture **only the bindings that are actually referenced** (plus the
  internal `$typerep::` capture slots).
- Tests:
  - `tests/phase3_capture_analysis.rs`

Benchmark (local, `rusk-measure`, release build):

- Input: `benchmarks/phase3_closure_capture.rusk`
- Command:
  - `cargo run --release --bin rusk-measure -- --json --warmup 2 --iters 10 benchmarks/phase3_closure_capture.rusk`
- Baseline (before Phase 3):
  - `run.avg_ns = 246066495` (≈ 246ms)
- After Phase 3:
  - `run.avg_ns = 114329229` (≈ 114ms)
  - ≈ **53.5% faster** on this benchmark on this machine

### Phase 4 — Symbol Interning / ID-Based Names (Cross-cutting)

Status: **DONE** (implemented in-tree)

Implemented artifacts:

- MIR ids and tables:
  - `FunctionId` and `HostImportId` identifiers
  - `Module::functions` and `Module::host_imports` stored as indexed tables
  - name → id maps (`Module::{function_ids, host_import_ids}`)
  - virtual dispatch table now stores `FunctionId` values
- New MIR ops / literals:
  - `Instruction::CallId { func: CallTarget, ... }` for resolved call dispatch
  - `ConstValue::FunctionId(FunctionId)` for interned function references
- Compiler post-pass:
  - Resolves `call` targets into `CallId` using the module tables.
  - Resolves `ConstValue::Function(name)` into `ConstValue::FunctionId(id)`.
- Interpreter execution:
  - Stores the current function as a `FunctionId` in `Frame` (no per-step string lookup).
  - Dispatches `CallId` without string-key lookups.
  - Represents `Value::Function` as a `FunctionId` (so `icall` avoids string dispatch).

Benchmark (local, `rusk-measure`, release build):

- Input: `benchmarks/phase4_call_dispatch.rusk`
- Command:
  - `cargo run --release --bin rusk-measure -- --json --warmup 2 --iters 10 benchmarks/phase4_call_dispatch.rusk`
- Baseline (before Phase 4, after Phase 3):
  - `run.avg_ns = 454285645` (≈ 454ms)
- After Phase 4:
  - `run.avg_ns = 287352766` (≈ 287ms)
  - ≈ **36.7% faster** on this benchmark on this machine

### Phase 5 — GC Improvements (Mark Epoch + Policy)

Status: **DONE** (mark epoch implemented in-tree; adaptive threshold deferred)

Implemented artifacts:

- Mark epoch in the mark-sweep heap:
  - Slots store `marked_epoch: Cell<u32>` rather than a boolean.
  - Heap stores a `current epoch` and increments per GC cycle (with a wraparound fallback).
- Existing GC tests still pass:
  - `tests/gc.rs`

Benchmark (local, `rusk-measure`, release build):

- Input: `benchmarks/phase5_gc_epoch.rusk`
- Command:
  - `cargo run --release --bin rusk-measure -- --json --warmup 2 --iters 10 benchmarks/phase5_gc_epoch.rusk`
- Baseline (before Phase 5, after Phase 4):
  - `run.avg_ns = 2113890362` (≈ 2.114s)
- After Phase 5:
  - `run.avg_ns = 1990100037` (≈ 1.990s)
  - ≈ **5.8% faster** on this benchmark on this machine

---

## Breaking Changes Policy (Current Stage)

Backward compatibility is explicitly **not** a goal right now. That means:

- We can change the MIR data model and interpreter/compiler implementation together.
- We do not need upgrade paths for previously serialized MIR modules.
- Public crate APIs are allowed to change if that unlocks a substantially simpler or faster design.

Practical guidelines to keep iteration sane even while breaking things:

- Prefer “clean breaks” over compatibility layers: remove old representations once the new one
  lands.
- Keep a debug-friendly name table even if execution uses numeric IDs (helps diagnostics and
  tooling).
- Treat `serde` serialization as an internal/debug artifact unless/until we decide to stabilize a
  format.

---

## Validation Plan

For each phase:

- add targeted tests (unit/integration) covering:
  - function calls, branching, and block entry
  - struct/tuple field access
  - closures and captured variables (including shadowing)
  - GC behavior (dangling refs, resurrection prevention, generational correctness if added)
- run the full suite:
  - `cargo test`
  - plus benchmark harness where applicable

---

## Prioritized “Quick Wins” Checklist

These are good first implementations once Phase 0 metrics exist:

1. Interpreter: remove `Function` clones in `call_function` and `enter_block`.
2. Interpreter: change `execute_instruction` / `execute_terminator` to borrow instructions.
3. Interpreter: avoid cloning `frame.func` string in the hot loop (store an id or use `Rc<str>`).
4. Compiler: implement free-variable capture for lambdas (big runtime win on real code).
5. GC: implement mark epoch to avoid full slot clearing.
