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
- **Interface method**: `<Interface>.<method>` (e.g. `Logger.log`)

An implementation may intern these names.

---

## 3. Program Structure

### 3.1 Module

A module contains:
- functions (required)
- optional interface/type metadata (optional, for tooling/validation/optimization)
- optional method-resolution metadata (optional, for `vcall`)

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
- `string` (UTF-8)
- `bytes` (opaque byte vector)
- reference values:
  - `struct` instances
  - `enum` instances
  - `array` instances
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

- primitives: `int`, `float`, `bool`, `string`, `bytes`, `unit`
- composite: `struct`, `enum`, `array`
- callable: `fn`
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
- struct literal: `Type { field: <lit>, ... }`
- enum literal: `Enum::Variant(<lit>, ...)`

Composite literals allocate fresh reference values when evaluated.

### 6.3 Patterns (for `switch` and handler clauses)

Patterns are a restricted subset of Rusk patterns.

Supported forms:
- `_` (wildcard)
- `bind` (a binding site; textual form uses a local, e.g. `%msg`)
- literals: `unit`, `true`, `123`, `"s"`, `b\"...\"`
- enum destructuring: `Enum::Variant(<pat>, ...)`
- struct destructuring: `Type { field: <pat>, ... }`
- array prefix: `[<pat>, <pat>, ..]`

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

### 7.2 Data and Fields

- `make_struct`:
  - Syntax: `%dst = make_struct <Type> { field: <op>, ... }`
  - Semantics: allocate a struct instance and write a reference into `%dst`.
  - Operand evaluation order: field operands are evaluated **left-to-right in
    textual field order**.

- `make_array`:
  - Syntax: `%dst = make_array [ <op>, <op>, ... ]`
  - Semantics: allocate an array instance and write a reference into `%dst`.
  - Operand evaluation order: elements are evaluated **left-to-right**.

- `make_enum`:
  - Syntax: `%dst = make_enum <Enum>::<Variant>(<op>, <op>, ...)`
  - Semantics: allocate an enum instance and write a reference into `%dst`.
  - Operand evaluation order: fields are evaluated **left-to-right**.

- `get_field`:
  - Syntax: `%dst = get_field <op_obj> <field>`
  - Trap: non-struct, missing field.

- `set_field`:
  - Syntax: `set_field <op_obj> <field> <op_val>`
  - Trap: readonly reference, non-struct, missing field.

- `index_get`:
  - Syntax: `%dst = index_get <op_arr> <op_idx>`
  - Trap: non-array, non-int index, out-of-bounds.

- `index_set`:
  - Syntax: `index_set <op_arr> <op_idx> <op_val>`
  - Trap: readonly reference, non-array, non-int index, out-of-bounds.

- `len`:
  - Syntax: `%dst = len <op_arr>`
  - Trap: non-array.

### 7.3 Calls

- `call` (direct):
  - Syntax: `%dst = call <fn_name> (<op_args...>)`
  - Semantics: call a MIR function or a host function by name.
  - Convention: `%dst` may be written as `_` to indicate the return value is
    ignored.

- `icall` (indirect):
  - Syntax: `%dst = icall <op_fnptr> (<op_args...>)`
  - Trap: operand is not a function reference.

- `vcall` (virtual):
  - Syntax: `%dst = vcall <op_obj> <method> (<op_args...>)`
  - Semantics: method resolution is module/host-defined.
  - Trap: missing method resolution.

Argument passing respects Rusk’s value/reference semantics:
- primitives and other value types are cloned
- reference values are passed by reference (cloned reference)
- readonly parameters create readonly views in the callee

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
      <Interface.method>(<pat_args...>) -> <block>,
      ...
    }
    ```
  - Semantics: push a handler entry owned by the current frame.
  - Trap: invalid MIR where a handler target block does not have
    `binding_count + 1` parameters (the final parameter receives the
    continuation token).

- `pop_handler`:
  - Syntax: `pop_handler`
  - Semantics: pop the most-recent handler.
  - Trap: mismatched pop (top handler not owned by current frame).

### 9.2 `perform`

- Syntax:
  ```text
  %dst = perform <Interface.method>(<op_args...>)
  ```

Semantics:
1. Evaluate arguments.
2. Walk the handler stack from top to bottom.
3. For each handler entry, try its clauses in order:
   - clause matches if the interface/method matches and all patterns match args.
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
