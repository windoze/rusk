# Rusk MIR Specification (v0.2 Draft)

This document defines a mid-level intermediate representation (MIR) for Rusk.
MIR is a control-flow graph (CFG) with explicit basic blocks and explicit effect
handling (`push_handler` / `perform` / `resume`). The design goal for v0.2 is:

- **Self-contained and precise**: every instruction and terminator has defined
  operational semantics, including traps.
- **Interpreter-friendly**: the IR can be executed directly with a small runtime.
- **Lowering-friendly**: it is straightforward to lower Rusk constructs
  (`match`, effects, loops) into MIR.

This spec focuses on **dynamic interpretation semantics**. Implementations are
free to add static checking/verification (types, move tracking, dominance, …) as
an additional layer.

---

## 1. Goals

- **Structured CFG**: explicit basic blocks and terminators for fast execution.
- **Effect-aware**: handlers and `perform`/`resume` are first-class IR ops.
- **Low-overhead loops**: no implicit closure allocations for loops.
- **Language-aligned**: direct lowering from Rusk syntax and semantics.

---

## 2. Conventions

### 2.1 Determinism

MIR execution is deterministic given:
- the initial arguments to the entry function, and
- deterministic host functions.

All instructions evaluate operands **left-to-right**.

### 2.2 Identifiers

MIR uses the following identifier forms in its canonical textual format
(non-normative but recommended):

- **Function name**: `<ident>` (e.g. `main`, `process_data`)
- **Block label**: `<ident>` (e.g. `block0`, `block_log`)
- **Local**: `%<ident>` (e.g. `%0`, `%msg`, `%k`)
- **Virtual method id**: `<InterfaceFqn>::<method>` (e.g. `Logger::log`)

An implementation may intern these names.

The in-tree compiler performs interning at the MIR level for hot-path lowering and downstream
execution (the bytecode backend uses ID-based tables):

- functions are assigned dense `FunctionId` indices
- declared host imports are assigned dense `HostImportId` indices
- direct calls are resolved to a `CallTarget` (`mir(FunctionId)` / `host(HostImportId)`)

---

## 3. Program Structure

### 3.1 Module

A module contains:
- functions (required)
- optional interface/type metadata (optional, for tooling/validation/optimization)
- optional method-resolution metadata (optional, for `vcall`)
- optional declared host imports (optional, for embedding/validation)

In this implementation, method-resolution metadata is represented as a lookup table:

- `(dynamic_type_name, method_id) -> function_id`

Additionally, checked casts / runtime type tests against `interface` targets may use optional
interface-implementation metadata:

- `(dynamic_type_name, interface_name) -> bool`

Finally, a module may include optional **struct field layout metadata** to support
index-based struct storage and fast field operations:

- `type_name -> [field_name...]` (field order)

Finally, a module may include a set of **declared host function imports** (e.g. `std::println`),
including their (monomorphic) signatures. An interpreter may validate that all declared imports
are installed before executing any MIR.

Note: the in-tree implementation stores functions and host imports in indexed tables, with
additional name → id maps for diagnostics and host registration.

### 3.2 Function

A function body is a CFG of basic blocks:

```text
fn <name>(<params...>) -> <ret> {
  <block>...
}
```

The **entry block** is the first block defined in the function body (by textual
order). By convention, it is named `block0`.

Parameters are locals plus optional mutability/type info:

```text
fn f(readonly %x: int, %y: array) -> unit { ... }
```

### 3.3 Basic Block

A basic block has:
- a label (block name)
- an optional parameter list (block arguments)
- a list of instructions
- a single terminator

```text
block1(%x, %y):
  %z = copy %x
  return %z
```

Block parameters are assigned when control transfers into the block.

---

## 4. Values, Locals, and Mutability

### 4.1 Runtime Value Model

MIR values are dynamically typed at runtime. v0.2 requires support for:

- `unit`
- `bool`
- `int` (signed 64-bit, two's complement)
- `float` (IEEE-754 binary64)
- `byte` (unsigned 8-bit)
- `char` (Unicode scalar value)
- `string` (UTF-8)
- `bytes` (opaque byte vector)
- `typerep` (an internal, interned runtime type representation used for generic calls, effect
  identity, and runtime type tests / casts)
- reference values:
  - `struct` instances
  - `enum` instances
  - `array` instances
  - `tuple` instances
- `fn` references (for `icall`)
- continuation tokens (for `resume`)

Reference values have **identity** (two references may point to the same object).

### 4.2 Locals

Locals are per-function slots. Each slot may be:
- **uninitialized**
- **initialized** with a value

Reading an uninitialized local is a trap.

### 4.3 Copy vs Move

MIR has explicit `copy` and `move`.

- `copy` reads a source local and clones the value into the destination.
  - For reference values, `copy` creates a **new reference** to the same object
    (it does not deep-clone the object).
- `move` reads a source local, writes it to the destination, and leaves the
  source local **uninitialized**.

Implementations may add additional dynamic or static diagnostics (e.g.
use-after-move), but the operational semantics above are normative.

### 4.4 Readonly Views

Rusk supports `readonly` bindings as a **view restriction** (not necessarily a
deep freeze). MIR models this by allowing references to be tagged as readonly.

- Any write through a readonly reference traps.
- Readonly is **per reference view**; other aliases may still be mutable.

MIR provides `as_readonly` to create a readonly view.

---

## 5. Types (Optional)

MIR may carry types for optimization/debugging:

- primitives: `int`, `float`, `bool`, `byte`, `char`, `string`, `bytes`, `unit`
- composite: `struct`, `enum`, `array`, `tuple`
- callable: `fn`
- continuations: `cont(<param>) -> <ret>` (the value passed to `resume` and the value produced by `resume`)
- effects: `interface`

Type checking is optional; a conforming interpreter executes dynamically.

---

## 6. Operands, Literals, and Patterns

### 6.1 Operands

An operand is either:
- a local: `%x`
- a literal: e.g. `unit`, `true`, `123`, `"text"`, `[1, 2, 3]`

### 6.2 Literals

Literals may be nested:
- array literal: `[<lit>, ...]`
- tuple literal: `(<lit>, <lit>, ...)` (comma required; empty tuple is `unit`)
- struct literal: `Type { field: <lit>, ... }`
- enum literal: `Enum::Variant(<lit>, ...)`

Composite literals allocate fresh reference values when evaluated.

### 6.3 Patterns (for `switch` and handler clauses)

Patterns are a restricted subset of Rusk patterns.

Supported forms:
- `_` (wildcard)
- `bind` (a binding site; textual form uses a local, e.g. `%msg`)
- literals: `unit`, `true`, `123`, `"s"`, `b\"...\"`
- tuple destructuring: `(p1, p2, .., pk)` / `(p1, p2)` / `(..rest)`
- enum destructuring: `Enum::Variant(<pat>, ...)`
- struct destructuring: `Type { field: <pat>, ... }`
- array destructuring: `[p1, p2, .., pk]` / `[p1, p2]` / `[..rest]`

Tuple/array destructuring uses a single optional “rest” marker:
- If there is no rest marker, arity/length must match exactly.
- If there is a rest marker, the value must have length at least `prefix + suffix`.
- If the rest marker is a wildcard, the middle slice is ignored.
- If the rest marker is a bind, the middle slice is captured as a fresh value:
  - tuple rest capture allocates a new tuple containing the slice (or yields `unit` if the slice is empty)
  - array rest capture allocates a new array containing the slice (possibly empty)

Pattern matching is structural on composite values. For struct patterns:
- missing field traps **at match time** (treated as non-match is allowed by
  implementations, but v0.2 recommends trap to surface invalid MIR early).

Binding order is **left-to-right, depth-first**.

Determinism rules for map-like constructs:
- For struct patterns, fields are processed in the **textual order** they appear
  in the pattern.

When a pattern is used in `switch` or a handler clause:
- the sequence of bound values (in binding order) is passed as block arguments
  to the selected target block
- the target block must declare exactly that number of block parameters
  (names are for readability; arity is normative)

---

## 7. Instructions

Instructions are written in assignment form:

```text
%dst = <op> ...
```

Some instructions are statement-like and produce no value.

### 7.1 Constants and Binding

- `const`:
  - Syntax: `%dst = const <literal>`
  - Semantics: evaluate literal, write to `%dst`.

- `copy`:
  - Syntax: `%dst = copy %src`
  - Semantics: read `%src`, clone, write to `%dst`.
  - Trap: `%src` is uninitialized.

- `move`:
  - Syntax: `%dst = move %src`
  - Semantics: read `%src`, write to `%dst`, set `%src` to uninitialized.
  - Trap: `%src` is uninitialized.

- `as_readonly`:
  - Syntax: `%dst = as_readonly %src`
  - Semantics: if `%src` is a reference, create a readonly view; otherwise copy.
  - Trap: `%src` is uninitialized.

- `make_type_rep`:
  - Syntax: `%dst = make_type_rep <base> (<op_args...>)`
  - Semantics:
    - evaluate `<op_args...>` to `typerep` values
    - construct and intern an applied type representation from `<base>` and its arguments
    - write the resulting `typerep` value to `%dst`
  - Note: leaves such as `int`/`bool` and nominal type ids may be represented as `typerep` literals.

- `is_type`:
  - Syntax: `%dst = is_type <op_value> <op_type_rep>`
  - Semantics: runtime type test; writes `bool` into `%dst`.
    - the target type is provided as a runtime `typerep` value (interned)
    - for nominal targets (`struct`/`enum`): compare dynamic nominal type identity *including*
      instantiated type arguments
    - for `interface` targets: check that the dynamic type implements the interface and that the
      instantiated interface type arguments match (initial-stage coherence may allow this to be
      checked as a prefix of the dynamic type arguments)

- `checked_cast`:
  - Syntax: `%dst = checked_cast <op_value> <op_type_rep>`
  - Semantics: runtime checked cast; writes an `Option` enum value into `%dst`:
    - on success: `Option::Some(<op_value>)`
    - on failure: `Option::None`

### 7.2 Data and Fields

- `make_struct`:
  - Syntax: `%dst = make_struct <StructName><<op_type_args...>> { field: <op>, ... }`
  - Semantics: allocate a struct instance and write a reference into `%dst`.
    - `<op_type_args...>` are evaluated to `typerep` values and stored on the heap object
  - Operand evaluation order: field operands are evaluated **left-to-right in
    textual field order**.

- `make_array`:
  - Syntax: `%dst = make_array [ <op>, <op>, ... ]`
  - Semantics: allocate an array instance and write a reference into `%dst`.
  - Operand evaluation order: elements are evaluated **left-to-right**.

- `make_tuple`:
  - Syntax: `%dst = make_tuple ( <op>, <op>, ... )`
  - Semantics: allocate a tuple instance and write a reference into `%dst`.
  - Special case: a tuple with zero elements is `unit` and does not allocate.
  - Operand evaluation order: elements are evaluated **left-to-right**.

- `make_enum`:
  - Syntax: `%dst = make_enum <Enum><<op_type_args...>>::<Variant>(<op>, <op>, ...)`
  - Semantics: allocate an enum instance and write a reference into `%dst`.
    - `<op_type_args...>` are evaluated to `typerep` values and stored on the heap object
  - Operand evaluation order: fields are evaluated **left-to-right**.

- `get_field`:
  - Syntax: `%dst = get_field <op_obj> <field>`
  - Semantics:
    - for structs: read the named field
    - for tuples: `field` must be of the form `".<n>"` and reads element `n` (0-based)
  - Trap: non-struct/non-tuple, missing field / out-of-bounds.

- `set_field`:
  - Syntax: `set_field <op_obj> <field> <op_val>`
  - Semantics:
    - for structs: write the named field
    - for tuples: `field` must be of the form `".<n>"` and writes element `n` (0-based)
  - Trap: readonly reference, non-struct/non-tuple, missing field / out-of-bounds.

- `struct_get`:
  - Syntax: `%dst = struct_get <op_obj> <idx>`
  - Semantics: read struct field by a lowering-time resolved 0-based index.
  - Trap: non-struct, missing field / out-of-bounds.

- `struct_set`:
  - Syntax: `struct_set <op_obj> <idx> <op_val>`
  - Semantics: write struct field by a lowering-time resolved 0-based index.
  - Trap: readonly reference, non-struct, missing field / out-of-bounds.

- `tuple_get`:
  - Syntax: `%dst = tuple_get <op_tup> <idx>`
  - Semantics: read tuple element `idx` (0-based).
  - Trap: non-tuple, out-of-bounds.

- `tuple_set`:
  - Syntax: `tuple_set <op_tup> <idx> <op_val>`
  - Semantics: write tuple element `idx` (0-based).
  - Trap: readonly reference, non-tuple, out-of-bounds.

- `index_get`:
  - Syntax: `%dst = index_get <op_arr> <op_idx>`
  - Semantics:
    - If `<op_arr>` is an array reference, reads element `idx` (0-based).
    - If `<op_arr>` is a `bytes` value, reads byte `idx` (0-based) and yields a `byte` value.
  - Trap:
    - non-array and non-bytes base,
    - non-int index,
    - out-of-bounds.

- `index_set`:
  - Syntax: `index_set <op_arr> <op_idx> <op_val>`
  - Trap: readonly reference, non-array, non-int index, out-of-bounds.

- `len`:
  - Syntax: `%dst = len <op_arr>`
  - Trap: non-array.

### 7.3 Primitive Ops

The following opcodes exist to make primitive arithmetic and comparisons cheap on hot paths.
They are operationally equivalent to calling the corresponding `core::intrinsics::*` host functions,
but avoid call/dispatch overhead.

- `int_add`:
  - Syntax: `%dst = int_add <op_a> <op_b>`
  - Semantics: evaluate operands, require `int`, write `a + b` to `%dst`.

- `int_sub`:
  - Syntax: `%dst = int_sub <op_a> <op_b>`
  - Semantics: evaluate operands, require `int`, write `a - b` to `%dst`.

- `int_mul`:
  - Syntax: `%dst = int_mul <op_a> <op_b>`
  - Semantics: evaluate operands, require `int`, write `a * b` to `%dst`.

- `int_div`:
  - Syntax: `%dst = int_div <op_a> <op_b>`
  - Semantics: evaluate operands, require `int`, write `a / b` (truncating division) to `%dst`.
  - Trap: division by zero.

- `int_mod`:
  - Syntax: `%dst = int_mod <op_a> <op_b>`
  - Semantics: evaluate operands, require `int`, write `a % b` to `%dst`.
  - Trap: modulo by zero.

- `int_lt` / `int_le` / `int_gt` / `int_ge`:
  - Syntax: `%dst = int_lt <op_a> <op_b>` (and similarly)
  - Semantics: evaluate operands, require `int`, write the comparison result (`bool`) to `%dst`.

- `int_eq` / `int_ne`:
  - Syntax: `%dst = int_eq <op_a> <op_b>` (and similarly)
  - Semantics: evaluate operands, require `int`, write the comparison result (`bool`) to `%dst`.

- `bool_not`:
  - Syntax: `%dst = bool_not <op_v>`
  - Semantics: evaluate operand, require `bool`, write `!v` to `%dst`.

- `bool_eq` / `bool_ne`:
  - Syntax: `%dst = bool_eq <op_a> <op_b>` (and similarly)
  - Semantics: evaluate operands, require `bool`, write the comparison result (`bool`) to `%dst`.

### 7.4 Calls

- `call` (direct):
  - Syntax: `%dst = call <fn_name> (<op_args...>)`
  - Semantics: call a MIR function or a host function by name.
  - Convention: `%dst` may be written as `_` to indicate the return value is
    ignored.

- `call_id` (direct, resolved):
  - Syntax: `%dst = call_id <target> (<op_args...>)`
  - Semantics: same as `call`, but the callee has already been resolved to a
    `CallTarget`:
    - `mir(FunctionId)` for MIR calls
    - `host(HostImportId)` for declared host imports
  - This avoids repeated string-key lookups on hot paths (e.g. arithmetic lowered to host calls).

- `call_id_multi` (direct, resolved, multi-return; compiler-internal):
  - Syntax: `(%dst0, %dst1, ...) = call_id_multi <target> (<op_args...>)`
  - Semantics: like `call_id`, but the callee returns **multiple values** via `return_multi`.
    Each returned value is assigned to the corresponding destination local.
  - This is currently used by compiler optimizations (e.g. unboxed `Option<T>` returns) and is
    not part of the public language surface.

- `icall` (indirect):
  - Syntax: `%dst = icall <op_fnptr> (<op_args...>)`
  - Trap: operand is not a function reference.

- `vcall` (virtual):
  - Syntax: `%dst = vcall <op_obj> <method> <method_type_args...> (<op_args...>)`
  - Semantics:
    - Evaluate `<op_obj>` to a reference value and determine its dynamic type name.
    - Read the object’s stored instantiated type arguments (a list of runtime `typerep` values).
    - Resolve `<method>` via module/host-defined method metadata, typically:
      `(dynamic_type_name, <method>) -> <fn_name>`.
    - Invoke `<fn_name>` with arguments:
      1. the receiver’s dynamic type arguments (as `typerep` values),
      2. `<method_type_args...>` (as `typerep` values),
      3. the receiver value,
      4. then `<op_args...>`.
  - Trap: missing method resolution.

For interface dynamic dispatch in v0.4, the compiler uses a canonical method-id string:

- `<origin_interface_fqn>::<method_name>`

Argument passing respects Rusk’s value/reference semantics:
- primitives and other value types are cloned
- reference values are passed by reference (cloned reference)
- readonly parameters create readonly views in the callee

Notes for the Rusk front-end (source-level methods):

- An instance method is lowered to a normal MIR function where the **first value parameter** is the receiver.
- A `readonly fn` method is represented by giving the receiver parameter a readonly view type, so the corresponding MIR parameter is marked `readonly` and runtime writes through it trap.
- A `static fn` method is lowered as a normal function with **no receiver parameter** (direct `call`).
- Default interface methods may be compiled by generating:
  - a private function for the default body, and
  - a per-`impl` wrapper used in the method-resolution table when the impl omits that method.

---

## 8. Terminators (Control Flow)

Every basic block ends with exactly one terminator.

- `br`:
  - Syntax: `br <block>(<op_args...>)`
  - Semantics: evaluate args and enter target block, assigning block params.
  - Trap: invalid MIR where arg count != target block param count.

- `cond_br`:
  - Syntax: `cond_br <op_cond> <then_block>(<args...>) <else_block>(<args...>)`
  - Trap: condition is not `bool`.
  - Trap: invalid MIR where arg count != target block param count.

- `switch`:
  - Syntax:
    ```text
    switch <op_value> [
      <pattern> -> <block>,
      ...
    ] <default_block>
    ```
  - Semantics:
    - evaluate the scrutinee
    - try cases in order
    - on first match, enter the target block, passing pattern bindings as block args
  - Trap: invalid MIR where target block param count != binding count.

- `return`:
  - Syntax: `return <op_value>`

- `return_multi` (multi-return; compiler-internal):
  - Syntax: `return_multi (<op_values...>)`
  - Semantics: end the current function, returning multiple values to a `call_id_multi` caller.
  - Trap: invalid MIR where the caller destination count does not match the returned arity.

- `trap`:
  - Syntax: `trap <message>`
  - Semantics: terminate execution with a runtime error.

---

## 9. Effect System in MIR

### 9.1 Handler Stack

MIR maintains an implicit **handler stack** during execution.

- `push_handler`:
  - Syntax:
    ```text
    push_handler <handler_id> {
      <Interface<op_iface_type_args...>.method>(<pat_args...>) -> <block>,
      ...
    }
    ```
  - Semantics: push a handler entry owned by the current frame.
  - Trap: invalid MIR where a handler target block does not have
    `binding_count + 1` parameters (the final parameter receives the
    continuation token).
  - Note: `<op_iface_type_args...>` are evaluated to `typerep` values when the handler is pushed.
    Effect identity includes these instantiated interface arguments.

- `pop_handler`:
  - Syntax: `pop_handler`
  - Semantics: pop the most-recent handler.
  - Trap: mismatched pop (top handler not owned by current frame).

### 9.2 `perform`

- Syntax:
  ```text
  %dst = perform <Interface<op_iface_type_args...>.method>(<op_args...>)
  ```

Semantics:
1. Evaluate the interface type-argument operands (`<op_iface_type_args...>`) to `typerep` values,
   then evaluate the ordinary effect arguments.
2. Walk the handler stack from top to bottom.
3. For each handler entry, try its clauses in order:
   - clause matches if the interface/method matches, the instantiated interface type arguments
     match, and all patterns match args.
4. On the first match:
   - capture the current execution state as a **one-shot continuation token** `k`
     - the captured computation is **delimited** by the selected handler’s owning frame:
       frames below that owning frame are not part of the captured stack
   - unwind to the owning frame of the selected handler
   - transfer control to the handler block with:
     - the clause’s bound values (in binding order)
     - `k` as the final block argument
5. If no handler matches, trap `"unhandled effect"`.

### 9.3 Continuations and `resume`

- Syntax:
  ```text
  %dst = resume <op_k> <op_value>
  ```

Semantics:
- `resume` consumes the continuation token `k` (one-shot).
- `resume` switches execution to the captured execution stack, injecting
  `<op_value>` as the result of the suspended `perform`.
- Execution proceeds until that captured computation terminates by returning from
  its bottom-most frame.
- The final returned value becomes the result of `resume` and is written to `%dst`.

Traps:
- resuming an already-resumed token
- resuming a non-continuation value

### 9.4 Abandoning a Continuation

If a captured continuation token `k` is **never resumed** and becomes
unreachable (dropped), then the captured computation will never run.

Not resuming immediately is allowed: a handler may store or return `k` and
resume it later (first-class continuations). Returning from a handler without
resuming is therefore the MIR-level mechanism behind both:
- “abort” behaviors (drop `k`), and
- “delayed continuation” behaviors (resume `k` later).

---

## 10. Calling Convention

The v0.2 interpreter model uses a normal call stack:

- `call` pushes a new frame and starts executing at the callee’s entry block.
- `return` pops the current frame and returns to the caller.
- tail-call optimization is not required by this spec, but MIR is structured so
  it is straightforward to add.

---

## 11. Errors and Traps

MIR uses `trap <message>` and implicit traps for runtime errors such as:

- unhandled effect
- invalid `resume` (non-continuation / already resumed)
- illegal write through a readonly reference
- out-of-bounds array index
- missing field / wrong type for an operation
- reading an uninitialized local

Implementations may map traps to language-level errors.

---

## 12. Lowering Guidelines (Rusk → MIR)

### 12.1 `match` with Effect Branches

Rusk:

```rust
match expr {
  () => a,
  @Logger.log(msg) => { print(msg); resume(()) }
}
```

MIR sketch (illustrative):

```text
block0:
  push_handler H0 { Logger.log(%msg) -> block_log }
  %v = <eval expr>
  pop_handler
  switch %v [unit -> block_done] block_default

block_log(%msg, %k):
  _ = call print(%msg)
  %r = resume %k unit
  return %r
```

Notes:
- The handler is active only during evaluation of `expr` (between push/pop).
- Effect branches do not participate in the final value `switch` of `match`.
- To make `resume` return at a useful boundary, lowering may wrap the handled
  region into a helper function and `call` it from the surrounding code.

### 12.2 Loops

`while cond { body }` lowers to `cond_br` with a backedge.

---

## 13. Versioning

MIR is intentionally minimal in v0.2. Future extensions may add:
- explicit heap allocation instructions and GC metadata
- optimized decision trees for `switch`
- richer value categories and typed references
- multi-shot continuations
