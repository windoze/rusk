# Rusk Bytecode Specification (v0.1 Draft)

This document specifies the **Rusk bytecode layer** as implemented by:

- `crates/rusk-bytecode` (`rusk_bytecode`): the bytecode module data model (`ExecutableModule`),
  verifier, and stable `.rbc` encode/decode.
- `crates/rusk-vm` (`rusk_vm`): the reference bytecode VM with a step-driven embedding API.

The bytecode is a **register-based**, dynamically typed instruction set, designed to be a compact,
portable execution format derived from MIR.

Notes on stability:

- The `.rbc` serialization format is versioned and deterministic, and is treated as **internal
  stability** for v0 (no forward-compat promise yet).
- The in-memory Rust enums/structs are not themselves a stable ABI; the serialized format is the
  intended interchange boundary.

---

## 1. Conventions

### 1.1 Determinism

Execution is deterministic given:

- a fixed bytecode module,
- deterministic host import implementations,
- deterministic external effect handling by the host.

### 1.2 Indexing and numbering

- **Registers** are numbered starting at `0`.
- **Program counters (PCs)** are instruction indices into a function’s `code: Vec<Instruction>`,
  starting at `0`.
- **FunctionId / HostImportId / EffectId / TypeId / MethodId** are dense `u32` indices into their
  respective module tables.

### 1.3 Traps

A **trap** is an unrecoverable runtime error. In the reference VM, a trap:

- transitions the VM into a trapped state, and
- causes `vm_step` to return `StepResult::Trap { message }`.

After trapping, repeated `vm_step` calls are idempotent and return the same `Trap`.

### 1.4 Operand evaluation order

Within a single instruction, operands are evaluated **left-to-right** (e.g. read `obj` before
reading `value` in a store). This matters for which error is reported first and for `move`, which
can uninitialize a register.

---

## 2. Runtime values

Bytecode execution is dynamically typed at runtime.

### 2.1 Value set

The reference VM supports these runtime value categories:

- **Primitives**:
  - `unit`
  - `bool`
  - `int` (signed 64-bit)
  - `float` (IEEE-754 `f64`)
  - `byte` (unsigned 8-bit)
  - `char` (Unicode scalar value)
  - `string` (UTF-8, immutable)
  - `bytes` (immutable byte sequence)
- **Type representations**: `typerep` (an interned runtime type representation)
- **References** (heap objects with identity):
  - `struct` instances
  - `enum` instances
  - `array` instances
  - `tuple` instances
- **Function references**: internal bytecode function IDs (used by `icall`)
- **Continuation tokens**: one-shot delimited continuations created by `perform` and consumed by
  `resume`

### 2.2 Uninitialized registers

Each register slot is either:

- **uninitialized**, or
- initialized with a value.

Reading an uninitialized register is a trap.

### 2.3 Copy vs move

The bytecode has explicit `copy` and `move`:

- `copy` duplicates a value into another register.
  - For reference values, this copies the reference (it does not deep-clone the heap object).
- `move` transfers a value and leaves the source register uninitialized.

### 2.4 Readonly reference views

Readonly is a **per-reference view restriction**, not a global deep freeze:

- A readonly reference may be read.
- Any write through a readonly reference traps.
- Readonly does not prevent mutation through other aliases that are not readonly.

Operations that read through a readonly reference (field/tuple/array loads, pattern binding) return
values in a readonly view when appropriate (i.e. references are re-tagged as readonly).

---

## 3. ABI boundary (VM ↔ host)

The embedding boundary is intentionally small and uses ABI-safe primitives only.

### 3.1 `AbiType`

The bytecode-level ABI supports:

- `unit`
- `bool`
- `int`
- `float`
- `string`
- `bytes`

These are the only types allowed in:

- **host import signatures** (called via `CallTarget::Host`),
- **externalized effect signatures** (returned via `StepResult::Request` and resumed via
  `vm_resume`).

### 3.2 Host import contract

Host imports are synchronous functions installed by the embedding host.

Normative requirements:

- **No VM reentry during host calls**: host import implementations must not call back into
  `vm_step`, `vm_resume`, or `vm_drop_continuation` while they are executing. (The reference VM
  enforces this and traps/errs on reentry.)
- Host import calls validate:
  - arity against the declared signature, and
  - argument and return `AbiType` matches.
  Type mismatches trap.

---

## 4. Program structure

### 4.1 `ExecutableModule`

A bytecode module contains:

- `functions: Vec<Function>`
- `function_generic_params: Vec<u32>`
  - parallel to `functions`; gives the number of leading runtime `typerep` parameters (generic
    arity) for each function.
- `host_imports: Vec<HostImport>`
- `type_names: Vec<String>`
  - interned type name table; `TypeId(u32)` indexes this table
  - must include primitive type names (`unit`, `bool`, `int`, `float`, `byte`, `char`, `string`,
    `bytes`)
- `method_names: Vec<String>`
  - interned method name table; `MethodId(u32)` indexes this table
- `vcall_dispatch: Vec<Vec<(MethodId, FunctionId)>>`
  - parallel to `type_names`; used by `vcall` dynamic dispatch
- `interface_impls: Vec<Vec<TypeId>>`
  - parallel to `type_names`; used by runtime interface checks (`is_type` / `checked_cast`)
- `struct_layouts: Vec<Option<Vec<String>>>`
  - parallel to `type_names`; struct field layouts by `TypeId`
- `external_effects: Vec<ExternalEffectDecl>`
  - the set of effects that may “bubble” to the host as `StepResult::Request`
- `entry: FunctionId`

The verifier (`rusk_bytecode::verify_module`) enforces table integrity and basic invariants.

### 4.2 `Function`

A function has:

- `name: String`
- `reg_count: u32` (total registers allocated for the function)
- `param_count: u32`
  - parameters are registers `0..param_count`
- `code: Vec<Instruction>` (linear instruction stream)

At function entry:

- registers `0..param_count` are initialized from call arguments,
- all other registers are uninitialized.

### 4.3 Representing generics (runtime `typerep` parameters)

Bytecode v0 represents generics by passing **runtime type arguments explicitly** as leading
parameters:

- Each function has a `generic_arity = module.function_generic_params[fn_id]`.
- The first `generic_arity` parameters of the function (registers `0..generic_arity`) are runtime
  `typerep` values.
- The remaining parameters are ordinary runtime values.

The verifier enforces `generic_arity <= param_count`.

The VM does not have a static type system; these conventions exist so that:

- generic calls can carry runtime type arguments when needed (e.g. for interface/effect identity),
- embeddings may optionally register specialized host implementations keyed by `(FunctionId,
  [TypeRepId...])`.

---

## 5. Execution model

### 5.1 Frames and PC

Execution proceeds with a call stack of **frames**. Each frame contains:

- `func: FunctionId`
- `pc: usize` (next instruction index)
- `regs: Vec<Option<Value>>` (length = `reg_count`)
- `return_dsts` (return destinations in the caller frame)
  - `None`: return value(s) are discarded
  - `One(Reg)`: single-register return (from `Call` / `ICall` / `VCall`)
  - `Multi([Reg...])`: multi-register return (from `CallMulti`)

Instruction fetch behavior:

1. Fetch instruction at `code[pc]`.
2. Increment `pc` by 1.
3. Execute the instruction (which may overwrite `pc` for control flow).

Falling off the end (`pc >= code.len()`) is treated as `return unit`.

### 5.2 VM states and the step API

The reference VM has these high-level states:

- **Running**: `vm_step` executes instructions.
- **Suspended**: `vm_step` traps with “vm is suspended”; the host must call `vm_resume` or
  `vm_drop_continuation` first.
- **Done**: `vm_step` returns `StepResult::Done { value }` idempotently.
- **Trapped**: `vm_step` returns `StepResult::Trap { message }` idempotently.

### 5.3 Fuel accounting

`vm_step(vm, fuel)` executes instructions until one of:

- the program finishes (`Done`),
- a trap occurs (`Trap`),
- an external effect is requested (`Request`),
- fuel is exhausted (`Yield`).

Fuel is decremented by **1 per executed instruction**, regardless of instruction type. Yielding
only happens **between** instructions; host calls are not interrupted mid-call.

---

## 6. Effects and continuations

Bytecode supports **scoped effect handlers** and **delimited continuations**.

### 6.1 Handler stack

Handlers are pushed/popped dynamically:

- `push_handler { clauses... }` pushes a handler entry associated with the current frame depth.
- `pop_handler` pops the top handler and traps if the handler stack does not match the current
  frame depth.

Each handler has a set of **clauses**. A clause includes:

- an effect identity (`interface`, runtime `typerep` interface args, `method`),
- argument patterns (for matching performed effect arguments),
- a target PC within the current function,
- `param_regs`: destination registers for pattern binds, optionally followed by one final register
  for the continuation token.

`param_regs` arity determines whether the clause is **abortive**:

- If `param_regs.len() == bind_count`, the clause does **not** receive a continuation token. The
  performed effect is handled **abortively**: the remainder of the computation after the `perform`
  is abandoned (as if its continuation were captured and then dropped).
- If `param_regs.len() == bind_count + 1`, the clause receives a continuation token in the final
  register and may resume it via `resume`.

### 6.2 `perform` (in-language handling)

`perform` evaluates an effect identity and arguments, then searches handlers from top to bottom for
the first matching clause:

- effect identity must match exactly (including runtime interface type arguments),
- arity must match,
- each argument pattern must match, collecting bind values.

On a match, `perform` transfers control to the handler clause. If the clause requests a
continuation token (`param_regs.len() == bind_count + 1`), `perform` captures a delimited
continuation; otherwise it may skip capturing and handle the effect abortively.

If the clause is **abortive** (`param_regs.len() == bind_count`), `perform`:

1. Unwinds the current stack/handler stack back to the handler owner frame.
2. Writes pattern binds into `param_regs[0..bind_count]`.
3. Sets the handler owner frame `pc` to the clause’s `target_pc`.

If the clause is **resumptive** (`param_regs.len() == bind_count + 1`), `perform`:

1. Captures a **delimited continuation** representing the remainder of execution from the handler
   owner frame up to the current top frame, including relevant nested handlers.
2. Clears the `perform` destination register in the captured top frame (if `dst` is present),
   ensuring it is uninitialized in the continuation.
3. Unwinds the current stack/handler stack back to the handler owner frame.
4. Writes pattern binds into `param_regs[0..bind_count]` and writes the continuation token into the
   last `param_reg`.
5. Sets the handler owner frame `pc` to the clause’s `target_pc`.

This makes the handler clause body run “in place”, with access to the continuation token when one
is requested.

### 6.3 `resume` (in-language resumption)

`resume` takes a continuation token and a resume value.

Operationally, `resume`:

1. Consumes the continuation token (continuations are **one-shot**; resuming twice traps).
2. If the continuation remembers a `perform` destination register, writes the resume value into
   that destination register in the captured top frame.
3. Splices the captured frames/handlers onto the current VM stack.
4. Sets the captured bottom frame’s `return_dst` to the `resume` instruction’s `dst`, so that when
   the resumed continuation eventually returns, its return value is written back into the caller
   frame.

### 6.4 Externalized effects (`StepResult::Request`)

If no in-language handler matches a `perform`, the VM may externalize the effect to the host:

- Only **monomorphic** effects (no runtime interface type args) are eligible for externalization.
- The effect must be declared in `module.external_effects` with an ABI-safe signature.

If externalized, `vm_step` returns:

```text
StepResult::Request { effect_id, args: Vec<AbiValue>, k: ContinuationHandle }
```

The VM enters the **Suspended** state.

The host must then choose one of:

- `vm_resume(vm, k, value)` to supply an ABI value back to the suspended `perform` destination
  register (if present), or
- `vm_drop_continuation(vm, k)` to cancel execution and trap the VM with a “cancelled” message.

Only one suspension is possible at a time in the reference VM. A `ContinuationHandle` becomes
invalid after being resumed or dropped (generation increments).

---

## 7. Pattern matching

Pattern matching is used by:

- `switch` cases, and
- effect handler clause argument patterns.

The pattern language is defined by `rusk_bytecode::Pattern`:

- wildcard `_`
- bind (captures the matched value)
- literal (a subset matches at runtime; see below)
- tuple patterns with optional rest binding
- array patterns with optional rest binding
- enum patterns
- struct patterns

### 7.1 Bind ordering

Bind values are collected in a deterministic left-to-right traversal:

- tuple/array: prefix binds, then rest bind (if any), then suffix binds
- enum: fields in order
- struct: fields in the order listed in the pattern

The `param_regs` list in `SwitchCase` and `HandlerClause` corresponds to this bind order.

For `HandlerClause`, if a continuation token is requested, it appears **after** all bind registers
(`param_regs[bind_count]`).

### 7.2 Literal matching subset

The reference VM’s literal pattern matcher supports equality on:

- `unit`, `bool`, `int`, `float`, `string`, `bytes`, and `FunctionId`.

Other literal forms may be present in serialized `.rbc` patterns, but currently do not match any
runtime value in the reference VM.

### 7.3 Readonly propagation

When matching through readonly references, field/element values and bind captures use readonly
views, preserving the “no writes through this view” restriction.

---

## 8. Instruction set (semantics)

This section defines operational semantics per instruction. All register indices must be in range
`0..reg_count` for the current function frame; out-of-range access traps.

### 8.1 Data movement and constants

- `Const { dst, value }`
  - Writes an immediate constant into `dst`.
  - `ConstValue::TypeRep(lit)` creates a `typerep` constructor value (no type args applied).
  - `ConstValue::Function(id)` creates a function reference usable by `icall`.
- `Copy { dst, src }`
  - Traps if `src` is uninitialized.
  - Writes a clone of `src` into `dst`.
- `Move { dst, src }`
  - Traps if `src` is uninitialized.
  - Moves the value from `src` to `dst` and leaves `src` uninitialized.
- `AsReadonly { dst, src }`
  - Traps if `src` is uninitialized.
  - If `src` is a reference, writes a readonly view; otherwise copies as-is.

### 8.2 Runtime type operations

- `MakeTypeRep { dst, base, args }`
  - Reads each `args[i]` as a `typerep` value.
  - Produces an interned applied `typerep(base, args...)`.
- `IsType { dst, value, ty }`
  - Reads `ty` as a `typerep` and writes `bool` indicating whether `value` matches that type.
- `CheckedCast { dst, value, ty }`
  - Performs `IsType` and returns an `Option` enum value:
    - `Option::<ty>::Some(value)` if the test succeeds, else `Option::<ty>::None`.

Type test notes (reference VM behavior):

- `struct`/`enum` tests compare `TypeId` **and** applied type args.
- `interface` tests require metadata in `module.interface_impls`.

### 8.3 Heap construction

- `MakeStruct { dst, type_id, type_args, fields }`
  - `type_args` must be `typerep` registers.
  - Uses `module.struct_layouts[type_id]` to determine field order.
  - `fields` is a list of `(field_name, reg)`; all layout fields must be provided exactly once.
  - Allocates and writes a struct reference.
- `MakeEnum { dst, enum_type_id, type_args, variant, fields }`
  - `type_args` must be `typerep` registers.
  - Allocates and writes an enum reference.
- `MakeArray { dst, items }`
  - Allocates and writes an array reference.
- `MakeTuple { dst, items }`
  - If `items` is empty, writes `unit` (tuple-0 is represented as `unit`).
  - Otherwise allocates and writes a tuple reference.

### 8.4 Field and element access

There are two styles: name-based and index-based.

- `GetField { dst, obj, field }`
  - `obj` must be a reference to a struct or tuple.
  - For structs: `field` is a field name resolved via `module.struct_layouts[receiver_type_id]`.
  - For tuples: `field` is a string of the form `".<index>"` (e.g. `".0"`).
- `SetField { obj, field, value }`
  - Like `GetField`, but mutates the referenced object.
  - Traps if `obj` is a readonly reference.

Index-based ops:

- `StructGet { dst, obj, idx }` / `StructSet { obj, idx, value }`
  - `obj` must be a struct reference; `idx` is a field index.
- `TupleGet { dst, tup, idx }` / `TupleSet { tup, idx, value }`
  - `tup` must be a tuple reference; `idx` is an element index.

Array ops:

- `IndexGet { dst, arr, idx }`
  - `idx` must be an `int`.
  - If `arr` is an array reference, reads element `idx` (0-based).
  - If `arr` is a `bytes` value, reads byte `idx` (0-based) and writes a `byte` to `dst`.
  - Traps on out-of-bounds or negative indices, or if `arr` is neither an array reference nor a
    `bytes` value.
- `IndexSet { arr, idx, value }`
  - Like `IndexGet`, but mutates the array (not supported for `bytes`).
  - Traps if `arr` is readonly.
- `Len { dst, arr }`
  - Writes the array length as an `int`.

Readonly propagation:

- Loads through readonly references return readonly views of reference-typed elements/fields.

### 8.5 Integer and boolean ops

These ops trap if operands are uninitialized or of the wrong type:

- `IntAdd`, `IntSub`, `IntMul`, `IntDiv`, `IntMod`
  - `IntDiv` traps on division by zero.
  - `IntMod` traps on modulo by zero.
  - Integer overflow wraps (two's complement).
- `IntLt`, `IntLe`, `IntGt`, `IntGe`, `IntEq`, `IntNe`
- `BoolNot`, `BoolEq`, `BoolNe`

### 8.6 Calls

- `Call { dst, func, args }`
  - `func` is one of:
    - `Bc(FunctionId)` — call another bytecode function
    - `Host(HostImportId)` — call a host import
    - `Intrinsic(Intrinsic)` — call a VM intrinsic
  - The callee’s `param_count` must match `args.len()` for bytecode calls.
  - `dst` is the return destination register in the caller frame; if `dst` is `None`, the return
    value is discarded.
  - Generic specialization (optional, embedding feature): if `func = Bc(fid)` and `fid` has
    `generic_arity > 0`, the VM may treat the first `generic_arity` arguments as runtime type args
    and dispatch to a host import specialization registered for `(fid, type_args)` instead of
    interpreting the bytecode body.
- `CallMulti { dsts, func, args }`
  - Multi-register return variant of `Call`, used by internal compiler optimizations (e.g. unboxed
    `Option<T>` returns).
  - `dsts` is the list of destination registers in the caller frame; its length must match the
    callee’s `ReturnMulti { values }` arity.
  - v0 restriction: `func` must be `Bc(FunctionId)` (host imports and intrinsics are not supported
    for multi-return calls).
- `ICall { dst, fnptr, args }`
  - `fnptr` must contain a function reference value.
  - Otherwise identical to `Call` into bytecode.
- `VCall { dst, obj, method, method_type_args, args }`
  - Dynamic dispatch on the receiver’s runtime `TypeId`:
    - receiver must be either:
      - a struct or enum reference, or
      - a primitive value (`unit`, `bool`, `int`, `float`, `byte`, `char`, `string`, `bytes`)
    - lookup uses `module.vcall_dispatch[receiver_type_id]` to resolve `method: MethodId` to a
      `FunctionId`
    - an implementation may provide equivalent fast paths for well-known core methods on primitive
      receivers (e.g. `core::hash::Hash::hash` on `int`), bypassing the dispatch table
    - receiver type args are empty for primitive receivers
  - The callee is invoked with argument list:
    1. receiver type args as leading `typerep` values,
    2. `method_type_args` as additional leading `typerep` values,
    3. the receiver value,
    4. `args`.

### 8.7 Effects and control flow

Effects:

- `PushHandler { clauses }`
- `PopHandler`
- `Perform { dst, effect, args }`
- `Resume { dst, k, value }`
- `ResumeTail { k, value }`

`ResumeTail` is a tail-call variant of `Resume`. It consumes and resumes the continuation like
`Resume`, but instead of returning to the current frame it replaces the current frame with the
captured continuation segment, preserving the current frame’s return destination(s). This is
equivalent to resuming in a `return` position (tail resume).

Control flow:

- `Jump { target_pc }`
- `JumpIf { cond, then_pc, else_pc }`
  - `cond` must be `bool`.
- `Switch { value, cases, default_pc }`
  - Evaluates cases in order; the first matching case wins.
  - For a matching case, pattern binds are written to `case.param_regs` in bind order and `pc` is
    set to `case.target_pc`.
  - If no case matches, `pc` is set to `default_pc`.

### 8.8 Termination

- `Return { value }`
  - Returns from the current frame.
  - If there is a caller frame and `return_dst` is set, writes the value into the caller register.
  - If returning from the entry frame, the return value must be ABI-safe; otherwise the VM traps.
- `ReturnMulti { values }`
  - Multi-register return variant of `Return`, used by internal compiler optimizations.
  - If there is a caller frame and multi-return destinations are set, writes each returned value
    into its corresponding caller register.
  - Returning multiple values from the entry frame is not supported and traps.
- `Trap { message }`
  - Traps immediately with the provided message.

---

## 9. Intrinsics

Intrinsics are VM-provided operations invoked via `CallTarget::Intrinsic`.

They are primarily compiler desugaring targets and are not part of the VM/host ABI.
Intrinsic calls trap on type/arity mismatch (“bad args”) or on intrinsic-specific errors.

The current intrinsic set includes:

- f-string helpers: `StringConcat`, `ToString`, `Panic`
- bool ops: `BoolNot`, `BoolEq`, `BoolNe`
- int ops: `IntAdd`, `IntSub`, `IntMul`, `IntDiv`, `IntMod`, `IntEq`, `IntNe`, `IntLt`, `IntLe`,
  `IntGt`, `IntGe`
- float ops: `FloatAdd`, `FloatSub`, `FloatMul`, `FloatDiv`, `FloatMod`, `FloatEq`, `FloatNe`,
  `FloatLt`, `FloatLe`, `FloatGt`, `FloatGe`
- primitive equality helpers: `StringEq`, `StringNe`, `BytesEq`, `BytesNe`, `UnitEq`, `UnitNe`
- primitive conversions: `IntToByte`, `IntTryByte`, `ByteToInt`, `IntToChar`, `IntTryChar`,
  `CharToInt`
- `bytes` helpers: `BytesGet`, `BytesLen`, `BytesSlice`, `BytesToArray`, `BytesFromArray`
- `string` helpers: `StringSlice`
- iterator protocol: `IntoIter`, `Next`, `StringIntoIter`, `StringNext`, `BytesIntoIter`,
  `BytesNext`
- array ops: `ArrayLen`, `ArrayLenRo`, `ArrayPush`, `ArrayPop`, `ArrayClear`, `ArrayResize`,
  `ArrayInsert`, `ArrayRemove`, `ArrayExtend`, `ArrayConcat`, `ArrayConcatRo`, `ArraySlice`,
  `ArraySliceRo`

---

## 10. `.rbc` serialization format (v0.6)

This section specifies the stable `.rbc` binary encoding for `ExecutableModule`.

### 10.1 Design goals

- Explicit, portable encoding (little-endian fixed-width integers).
- No reliance on Rust layouts or `serde`.
- Deterministic output: `encode -> decode -> encode` is byte-identical.

### 10.2 Primitive encodings

All integers are little-endian.

- `u8`: 1 byte
- `u16`: 2 bytes
- `u32`: 4 bytes
- `u64`: 8 bytes
- `i64`: 8 bytes (two’s complement)
- `f64`: encoded as `u64` IEEE-754 bit pattern

Common scalar aliases:

- `Reg`: `u32`
- `FunctionId` / `HostImportId` / `EffectId` / `TypeId` / `MethodId`: `u32`
- `pc` targets: `u32` (instruction indices)

Booleans:

- `bool`: encoded as `u8` where `0` = false, `1` = true (other values are invalid)

Length-prefixed sequences:

- `len`: `u32` element count
- `string`: `len(u32)` + UTF-8 bytes (no terminator)
- `blob`: `len(u32)` + raw bytes

Optional values:

- `Option<Reg>` is encoded as:
  - `0u8` for `None`
  - `1u8` for `Some`, followed by a `u32` register index

### 10.3 File header

Header layout:

- bytes `[0..8)`: magic `RUSKBC0\0` (ASCII; 8 bytes)
- bytes `[8..10)`: major version (`u16`)
- bytes `[10..12)`: minor version (`u16`)

Current version:

- major = `0`
- minor = `6`

The reference decoder requires an **exact** version match.

### 10.4 Module layout

After the header, the module is encoded as:

1. `functions_len: len`
2. `functions: [Function; functions_len]`
3. `function_generic_params_len: len`
4. `function_generic_params: [u32; function_generic_params_len]`
5. `host_imports_len: len`
6. `host_imports: [HostImport; host_imports_len]`
7. `type_names_len: len`
8. `type_names: [string; type_names_len]`
9. `method_names_len: len`
10. `method_names: [string; method_names_len]`
11. `vcall_dispatch_len: len`
12. `vcall_dispatch: repeat vcall_dispatch_len times { type_id: u32, method_id: u32, fn_id: u32 }`
    - `type_id` is a `TypeId` index into `type_names`
    - `method_id` is a `MethodId` index into `method_names`
    - `fn_id` is a `FunctionId` index into `functions`
13. `interface_impls_len: len`
14. `interface_impls: repeat interface_impls_len times { type_id: u32, ifaces_len: len, ifaces: [u32; ifaces_len] }`
    - `type_id` is a `TypeId` (dynamic type)
    - each `iface` is a `TypeId` naming an interface
15. `struct_layouts_len: len`
16. `struct_layouts: repeat struct_layouts_len times { type_id: u32, fields_len: len, fields: [string; fields_len] }`
17. `external_effects_len: len`
18. `external_effects: [ExternalEffectDecl; external_effects_len]`
19. `entry: u32` (FunctionId)

The decoder rejects trailing bytes after the module.

### 10.5 Tagged enums

This format uses tag bytes/words for enums. The tags are normative.

#### `AbiType` (`u8`)

| Tag | Variant |
| --- | ------- |
| 0 | `Unit` |
| 1 | `Bool` |
| 2 | `Int` |
| 3 | `Float` |
| 4 | `String` |
| 5 | `Bytes` |

#### Bytecode `ConstValue` (`u8`)

| Tag | Variant | Payload |
| --- | ------- | ------- |
| 0 | `Unit` | — |
| 1 | `Bool` | `bool(u8)` |
| 2 | `Int` | `i64` |
| 3 | `Float` | `f64` |
| 4 | `String` | `string` |
| 5 | `Bytes` | `blob` |
| 6 | `TypeRep` | `TypeRepLit` |
| 7 | `Function` | `u32` (FunctionId) |

#### `TypeRepLit` (`u8`)

| Tag | Variant | Payload |
| --- | ------- | ------- |
| 0 | `Unit` | — |
| 1 | `Bool` | — |
| 2 | `Int` | — |
| 3 | `Float` | — |
| 4 | `String` | — |
| 5 | `Bytes` | — |
| 6 | `Array` | — |
| 7 | `Tuple(arity)` | `u32` arity |
| 8 | `Struct(name)` | `string` |
| 9 | `Enum(name)` | `string` |
| 10 | `Interface(name)` | `string` |
| 11 | `Fn` | — |
| 12 | `Cont` | — |
| 13 | `Byte` | — |
| 14 | `Char` | — |
| 15 | `Never` | — |

#### Patterns (`u8`)

| Tag | Variant | Payload |
| --- | ------- | ------- |
| 0 | `Wildcard` | — |
| 1 | `Bind` | — |
| 2 | `Literal` | Bytecode `ConstValue` |
| 3 | `Tuple { prefix, rest, suffix }` | `len + prefix* + opt(rest) + len + suffix*` |
| 4 | `Enum { enum_name, variant, fields }` | `string + string + len + field*` |
| 5 | `Struct { type_name, fields }` | `string + len + (string + pat)*` |
| 6 | `Array { prefix, rest, suffix }` | `len + prefix* + opt(rest) + len + suffix*` |

`opt(rest)` is encoded as:

- `0u8` for `None`
- `1u8` for `Some`, followed by the nested pattern

#### Intrinsics (`u16`)

Intrinsics use `u16` tags `0..=75`:

- `0`: `StringConcat`
- `1`: `ToString`
- `2`: `Panic`
- `3`: `BoolNot`
- `4`: `BoolEq`
- `5`: `BoolNe`
- `6`: `IntAdd`
- `7`: `IntSub`
- `8`: `IntMul`
- `9`: `IntDiv`
- `10`: `IntMod`
- `11`: `IntEq`
- `12`: `IntNe`
- `13`: `IntLt`
- `14`: `IntLe`
- `15`: `IntGt`
- `16`: `IntGe`
- `17`: `FloatAdd`
- `18`: `FloatSub`
- `19`: `FloatMul`
- `20`: `FloatDiv`
- `21`: `FloatMod`
- `22`: `FloatEq`
- `23`: `FloatNe`
- `24`: `FloatLt`
- `25`: `FloatLe`
- `26`: `FloatGt`
- `27`: `FloatGe`
- `28`: `StringEq`
- `29`: `StringNe`
- `30`: `BytesEq`
- `31`: `BytesNe`
- `32`: `UnitEq`
- `33`: `UnitNe`
- `34`: `IntoIter`
- `35`: `Next`
- `36`: `ArrayLen`
- `37`: `ArrayLenRo`
- `38`: `ArrayPush`
- `39`: `ArrayPop`
- `40`: `ArrayClear`
- `41`: `ArrayResize`
- `42`: `ArrayInsert`
- `43`: `ArrayRemove`
- `44`: `ArrayExtend`
- `45`: `ArrayConcat`
- `46`: `ArrayConcatRo`
- `47`: `ArraySlice`
- `48`: `ArraySliceRo`
- `49`: `StringIntoIter`
- `50`: `StringNext`
- `51`: `BytesIntoIter`
- `52`: `BytesNext`
- `53`: `IntToByte`
- `54`: `IntTryByte`
- `55`: `ByteToInt`
- `56`: `IntToChar`
- `57`: `IntTryChar`
- `58`: `CharToInt`
- `59`: `BytesGet`
- `60`: `BytesSlice`
- `61`: `BytesToArray`
- `62`: `BytesFromArray`
- `63`: `StringSlice`
- `64`: `BytesLen`
- `65`: `StringFromChars`
- `66`: `StringFromUtf8`
- `67`: `StringFromUtf8Strict`
- `68`: `StringFromUtf16Le`
- `69`: `StringFromUtf16LeStrict`
- `70`: `StringFromUtf16Be`
- `71`: `StringFromUtf16BeStrict`
- `72`: `HashInt`
- `73`: `HashString`
- `74`: `HashBytes`
- `75`: `HashCombine`

#### Call targets (`u8`)

| Tag | Variant | Payload |
| --- | ------- | ------- |
| 0 | `Bc` | `u32` (FunctionId) |
| 1 | `Host` | `u32` (HostImportId) |
| 2 | `Intrinsic` | `u16` (Intrinsic tag) |

#### Instructions (`u8`)

Instruction opcodes are normative tags used by the `.rbc` encoding (v0.6):

- `0`: `Const`
- `1`: `Copy`
- `2`: `Move`
- `3`: `AsReadonly`
- `4`: `IsType`
- `5`: `CheckedCast`
- `6`: `MakeTypeRep`
- `7`: `MakeStruct`
- `8`: `MakeArray`
- `9`: `MakeTuple`
- `10`: `MakeEnum`
- `11`: `GetField`
- `12`: `SetField`
- `13`: `StructGet`
- `14`: `StructSet`
- `15`: `TupleGet`
- `16`: `TupleSet`
- `17`: `IndexGet`
- `18`: `IndexSet`
- `19`: `Len`
- `20`: `IntAdd`
- `21`: `IntSub`
- `22`: `IntMul`
- `23`: `IntDiv`
- `24`: `IntMod`
- `25`: `IntLt`
- `26`: `IntLe`
- `27`: `IntGt`
- `28`: `IntGe`
- `29`: `IntEq`
- `30`: `IntNe`
- `31`: `BoolNot`
- `32`: `BoolEq`
- `33`: `BoolNe`
- `34`: `Call`
- `35`: `ICall`
- `36`: `VCall`
- `37`: `PushHandler`
- `38`: `PopHandler`
- `39`: `Perform`
- `40`: `Resume`
- `41`: `Jump`
- `42`: `JumpIf`
- `43`: `Switch`
- `44`: `Return`
- `45`: `Trap`
- `46`: `ResumeTail`
- `47`: `CallMulti`
- `48`: `ReturnMulti`

Instruction payloads are encoded by writing each operand field in the order listed in the
instruction definition, using the primitive encodings from §10.2 (e.g. `u32` for registers and PC
targets, `string` for names, `len` for vectors, and `Option<Reg>` tags for optional destinations).

---

## 11. Versioning rules

The `.rbc` header contains `(major, minor)` versions. Currently:

- Any change to the `.rbc` binary encoding requires a version bump.
- The reference decoder requires an exact `(major, minor)` match; it does not currently accept
  newer minor versions.

Future versions may relax minor-version compatibility, but that is not currently guaranteed.
