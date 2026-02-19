# Proposal: ID-based Interning for Types/Methods (Dispatch Tables)

Date: 2026-02-19  
Status: Draft  

## Motivation

Rusk’s bytecode runtime currently uses **string-keyed** maps for several “dispatch-like” operations:

- `VCall` method dispatch: `module.methods[(dyn_type_name, method)]`
- interface implementation checks: `module.interface_impls[type].contains(interface)`
- struct layout lookup: `module.struct_layouts[type]`
- external effect lookup / matching (interface + method strings)

This is simple and correct, but it has predictable costs:

1. Hot-path allocations / clones (e.g. cloning type names out of heap objects).
2. Hot-path hashing / tree traversal on `String` keys.
3. Repeated storage of identical strings in instruction streams (`VCall.method` repeats a lot).

We already mitigated the hottest case in `core::map::Map` by adding a VM fast path for primitive
`Hash::hash` / `Eq::eq` / `Ne::ne`. That helps a lot, but it is intentionally narrow: it does not
speed up the general case where the receiver is a **struct/enum** and dispatch is truly needed.

An **ID-based interning** layer is a general improvement that benefits:

- all `VCall` dispatch (not just primitives),
- interface checks used by casts/type tests,
- any future optimizations that want fast method-id comparisons,
- memory footprint and `.rbc` size (deduplicate strings).

## Current State (Problem Statement)

### Bytecode data model

Today, bytecode stores names directly in instructions and metadata:

- `Instruction::VCall { method: String, .. }`
- `Instruction::MakeStruct { type_name: String, .. }` / `MakeEnum { enum_name: String, .. }`
- `ExecutableModule.methods: BTreeMap<(String, String), FunctionId>`
- `ExecutableModule.interface_impls: BTreeMap<String, BTreeSet<String>>`

### VM runtime behavior

In `VCall`, the VM:

1. Determines the receiver dynamic type name (`String`), and clones it.
2. Clones the `method` string from the instruction.
3. Looks up `(type_name, method)` in a `BTreeMap`.
4. Pushes a frame for the resolved wrapper/impl.

Even when semantics require dynamic dispatch, **the string work is still avoidable**: the runtime
doesn’t fundamentally need string keys to do method resolution.

## Goals / Non-goals

### Goals

- Replace string-keyed dispatch tables with **dense or near-dense numeric IDs**.
- Make `VCall` method resolution allocation-free and fast (integer key lookup).
- Deduplicate repeated strings in bytecode instructions and in `.rbc`.
- Keep language semantics unchanged (purely representational/performance).

### Non-goals (initially)

- Whole-program monomorphization / specialization (separate direction).
- Changing the surface language syntax or the generic system.
- Replacing effect dispatch hashing/caching (we can reuse the same idea later, but it’s not the
  first target).

## Proposed Design

### 1) Introduce interned IDs in `ExecutableModule`

Add interning tables to `rusk_bytecode::ExecutableModule`:

- `type_names: Vec<String>`
- `method_names: Vec<String>`

And use dense IDs:

- `TypeId(u32)` indexes into `type_names`
- `MethodId(u32)` indexes into `method_names`

Notes:

- Primitive “types” (`unit`, `bool`, `int`, …) should also have `TypeId`s so the VM can unify
  dispatch logic (primitives become “just another TypeId”).
- Keep the string tables for debugging and for preserving the current textual naming scheme.

### 2) Replace `methods` with an ID-based dispatch table

Replace:

- `methods: BTreeMap<(String, String), FunctionId>`

with something like:

- `methods_by_type: Vec<Vec<(MethodId, FunctionId)>>`

Where:

- `methods_by_type[type_id]` is a sorted small vector (by `MethodId`)
- lookup is `binary_search_by_key(method_id)`

Rationale:

- most types have few methods → small sorted vectors are cache-friendly
- avoids hashing while still fast
- encoding remains deterministic for `.rbc` (important for byte-identical roundtrips)

Alternatives (also viable):

- `Vec<BTreeMap<MethodId, FunctionId>>`
- `Vec<HashMap<MethodId, FunctionId>>` (fast lookup but needs care for deterministic encoding)

### 3) Make `VCall` reference method ids, not method strings

Change bytecode instruction:

- from: `VCall { method: String, ... }`
- to:   `VCall { method: MethodId, ... }`

This removes repeated string storage in instruction streams and makes dispatch use integer keys.

### 4) Store `TypeId` in heap objects (struct/enum)

To remove string work from the hot path entirely, the VM should not need to fetch a type name
string from heap objects during dispatch.

Change VM heap object headers for nominal objects:

- `HeapValue::Struct { type_name: String, .. }` → `HeapValue::Struct { type_id: TypeId, .. }`
- `HeapValue::Enum { enum_name: String, .. }`   → `HeapValue::Enum { type_id: TypeId, .. }`

The intern tables in `ExecutableModule` can still map `TypeId -> String` for diagnostics.

Allocation-time resolution:

- The fully ID-based end state updates allocation instructions to carry `TypeId` instead of a type
  name string.
- As an incremental step, the VM could translate `type_name -> type_id` at allocation time and
  store the id in the heap object (still removes strings from the dispatch hot path, but does not
  deduplicate instruction strings).

### 5) Optional extensions (follow-ups)

Once the infrastructure exists, the same intern pool can be applied to:

- `interface_impls`: `BTreeMap<TypeId, BTreeSet<InterfaceId>>`
- `struct_layouts`: `BTreeMap<TypeId, LayoutId>`
- `GetField/SetField` field names (rarely hot, but reduces `.rbc` size)
- effect identifiers (`Perform` / handler clauses) by interning `(interface, method)` into an
  `EffectNameId` and keeping `interface_args` as runtime type reps.

## `.rbc` format impact

This is a representational change to the serialized module. The `.rbc` encoder/decoder should:

- serialize `type_names` and `method_names` tables once,
- encode `TypeId`/`MethodId` as `u32`,
- encode dispatch tables using ids instead of strings,
- bump the `.rbc` version (e.g. `0.6` or `1.0`), since old decoders won’t understand the new layout.

Since `.rbc` is explicitly “internal stability” today, a version bump is acceptable.

## VM / Compiler work breakdown

### Compiler

- During interface wrapper synthesis / dispatch table population:
  - intern all dynamic type names (including primitives),
  - intern all method ids used for dispatch (both inherent and interface-qualified ids).
- Emit:
  - `VCall` with `MethodId`,
  - `MakeStruct`/`MakeEnum` with `TypeId` (preferred end state).
- Populate `methods_by_type` using ids.

### Bytecode + verifier

- Add new id types (`TypeId`, `MethodId`) to `rusk-bytecode`.
- Extend module verification:
  - validate that all ids are in range,
  - validate that dispatch table entries reference valid function ids.

### VM

- Update heap object representation to store `TypeId`.
- Update `VCall` execution:
  - compute receiver `TypeId` without allocations,
  - lookup `MethodId` in `methods_by_type[type_id]` with a binary search (or equivalent),
  - call resolved function.
- Update the existing primitive `VCall` fast path to match on `MethodId` instead of string
  comparisons (faster and less typo-prone).

## Evaluation Plan

### Benchmarks

Primary:

- `benchmarks/phase4_call_dispatch.rusk` (dispatch-heavy)
- `benchmarks/phase7_map_dict.rusk` (Map hot path; even with primitive fast path, other `VCall`s and
  dispatch-table operations should benefit in future variations)

Suggested additional benchmark (new):

- “struct-key map” benchmark: `Map<S, int>` where `S` is a small struct and `Hash/Eq` are
  implemented in Rusk. This forces non-primitive dispatch where the current fast path does not
  apply, and should show the win of id-based dispatch clearly.

### Metrics

- Add counters for:
  - `vcall_dispatch_table_lookups` (after inlining/fast paths, how many remain?)
  - optional: `vcall_method_binary_search_steps` (sanity for table density)
- Compare:
  - wall-clock medians,
  - instruction counts (should stay similar),
  - runtime per `VCall` should drop.

## Risks / Tradeoffs

- Migration complexity: touches bytecode format, compiler lowering, verifier, VM, and heap object
  representation.
- Debuggability: ids are less readable than strings; mitigate by keeping intern tables and exposing
  helper APIs to resolve ids back to names in debug output.
- Table design: a poor layout could regress performance; prefer simple sorted vectors first and
  validate with benchmarks.

## Alternatives / Incremental Steps

If we want a low-risk “step 0” before full id-based interning:

- Change `ExecutableModule.methods` from `BTreeMap<(String, String), FunctionId>` to a nested map:
  `BTreeMap<String, BTreeMap<String, FunctionId>>`.
- Then do lookups using `&str` borrowed keys, avoiding `String` cloning in `VCall`.

This does not reduce `.rbc` size and is not as fast as integer IDs, but it can provide an
immediate win with fewer moving parts.

