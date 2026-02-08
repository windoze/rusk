# Rusk MIR Specification (v0.1 Draft)

This document defines a mid-level intermediate representation (MIR) for Rusk.
MIR is a control-flow graph (CFG) with explicit effect handling and tail-call
friendly structure. It is designed to be easy to interpret or compile while
preserving high-level language constructs (match, effects, containers).

---

## 1. Goals

- **Structured CFG**: explicit basic blocks and terminators for fast execution.
- **Effect-aware**: handlers and perform/resume are first-class IR operations.
- **Low-overhead loops**: no per-iteration closure allocations required.
- **Language-aligned**: direct lowering from Rusk syntax.

---

## 2. Program Structure

### 2.1 Module

A module contains:
- type declarations (optional)
- function bodies
- interface metadata (optional)

### 2.2 Function

Each function is a CFG of basic blocks:

```
fn <name>(<params>) -> <ret> {
  block0:
    ...
    <terminator>
  block1:
    ...
}
```

### 2.3 Basic Block

A basic block is a sequence of instructions ending in one terminator:
- `br <block>`
- `cond_br <value> <then_block> <else_block>`
- `switch <value> [<pat> -> <block>]* <default_block>`
- `return <value>`
- `trap <message>`

---

## 3. Values and Locals

MIR uses local slots (SSA-like without phi) and explicit moves/copies.

- Locals: `%0`, `%1`, ...
- Each instruction writes to a destination local.
- A local is either **moved** or **copied**.

### 3.1 Copy vs Move

- **Copy**: for primitive/value types (int/float/bool/string/bytes/unit).
- **Move**: for reference types (struct/enum/array/object).

Implementations may track moves dynamically or statically.

---

## 4. Types (Optional)

MIR may carry types for optimization/debugging:
- `int`, `float`, `bool`, `string`, `bytes`, `unit`
- `struct`, `enum`, `array`, `fn`, `interface`

Type checking is optional; MIR can be interpreted dynamically.

### 4.1 Host-registered Primitive Types (FFI)

The host may register additional **primitive/value types** (e.g., `Point`) via FFI.
These types behave like built-in primitives:
- **Copy semantics**: `copy` is allowed and does not change identity.
- **By-value passing**: arguments are passed by value in `call/vcall`.
- **No implicit heap identity**: values are treated as plain data.

Construction and literals for such types are host-defined (e.g., via FFI functions).

---

## 5. Instructions

### 5.1 Constants and Binding

- `const <dst> <literal>`
- `copy <dst> <src>`
- `move <dst> <src>`

### 5.2 Data and Fields

- `make_struct <dst> <Type> { field: <val>, ... }`
- `get_field <dst> <obj> <field>`
- `set_field <obj> <field> <val>`
- `index_get <dst> <arr> <idx>`
- `index_set <arr> <idx> <val>`
- `len <dst> <arr>`

### 5.3 Calls

- `call <dst> <fn> (<args...>)`
- `vcall <dst> <obj> <method> (<args...>)`
- `icall <dst> <fnptr> (<args...>)`

### 5.4 Control Helpers

- `phi` is **not** used; explicit temporaries and block arguments should be used.
- Block arguments are allowed:
  `block1(%x, %y): ...`

---

## 6. Effect System in MIR

### 6.1 Handler Stack

MIR maintains an implicit handler stack during execution.

- `push_handler <handler_id> { <effect_sig> -> <block> }`
- `pop_handler`

`effect_sig` is `Interface.method(arg_types) -> ret_type`.

### 6.2 Perform

- `perform <dst> <Interface.method> (<args...>)`

Semantics:
1. Walk the handler stack from top to bottom.
2. Select the first handler that matches `Interface.method` and arguments.
3. Suspend at this perform site and jump to the handler block with:
   - effect arguments
   - an opaque **continuation token** `k` (first-class value)

If no handler matches, `trap "unhandled effect"`.

### 6.3 Resume

- `resume <dst> <k> <value>`

Semantics:
- `resume` continues the suspended computation at the original perform site.
- `resume` may be invoked **outside** the original handler (first-class continuation).
- Resuming switches to the captured continuation stack; control does not return to
  the caller until the continuation completes.
- `resume` returns the final value of that continuation into `<dst>`.
- `resume` is **one-shot**; multiple calls are runtime errors.

### 6.4 Handler Return

A handler block may:
- call `resume`, or
- return a value directly (treated as the perform result and **discarding** the
  captured continuation).

---

## 7. Lowering Rules (High-Level to MIR)

### 7.1 match with Effect Branches

Rusk:

```
match expr {
  () => a,
  @Logger.log(msg) => { print(msg); resume(()) }
}
```

MIR sketch:

```
block0:
  push_handler H0 { Logger.log -> block_log }
  %v = <eval expr>
  pop_handler
  switch %v [unit -> block_done] block_default

block_log(%msg, %k):
  call _ = print(%msg)
  resume %r %k unit
  return %r
```

Notes:
- The handler is active only during evaluation of `expr`.
- Effect branches do not participate in value matching.

### 7.2 Loop

`while cond { body }` lowers to `cond_br` + backedge.

---

## 8. Variables and Mutability

- `let` maps to a local slot.
- Assignment `x = expr` becomes `move`/`copy` into that slot.
- `readonly` restrictions are enforced at runtime (or checked earlier).

---

## 9. Calling Convention

Arguments are passed as:
- **by value** for primitives
- **by reference** for complex types

MIR is responsible for respecting this rule in `call/vcall`.

---

## 10. Errors and Traps

MIR uses `trap <message>` for:
- unhandled effect
- invalid `resume`
- illegal write to `readonly`
- out-of-bounds index

Implementations may map traps to language-level errors.

---

## 11. Example MIR (Complete)

Rusk:

```
fn main() {
  let data = [1, 2, 3];
  match process_data(data) {
    () => print("Done"),
    @Logger.log(msg) => { print(msg); resume(()) }
  }
}
```

MIR sketch:

```
fn main() -> unit {
block0:
  %0 = const [1, 2, 3]
  push_handler H0 { Logger.log -> block_log }
  %1 = call process_data(%0)
  pop_handler
  switch %1 [unit -> block_done] block_default

block_log(%msg, %k):
  call _ = print(%msg)
  resume %r %k unit
  return %r

block_done:
  call _ = print("Done")
  return unit

block_default:
  trap "non-exhaustive match"
}
```

---

## 12. Versioning

MIR is intentionally minimal in v0.1. Future extensions may add:
- explicit allocations
- optimized pattern matching tables
- typed references or borrow metadata
- multi-shot continuations
