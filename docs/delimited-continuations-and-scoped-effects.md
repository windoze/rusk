# Delimited Continuations and Scoped Effects in Rusk

This document provides a detailed explanation of how **delimited continuations** and **scoped effects** work in the Rusk language, with special attention to **escaped continuation** behavior.

## Table of Contents

1. [Core Concepts](#core-concepts)
2. [Scoped Effects](#scoped-effects)
3. [Delimited Continuations](#delimited-continuations)
4. [Escaped Continuation Behavior](#escaped-continuation-behavior)
5. [Implementation Details](#implementation-details)
6. [Detailed Examples](#detailed-examples)

---

## Core Concepts

### What are Algebraic Effects?

Algebraic effects are a control flow mechanism that allows programs to "perform effect operations" during execution, with external "effect handlers" deciding how to respond to these effects.

In Rusk, effects are defined through `interface` declarations and invoked using the `@Interface.method(...)` syntax.

```rusk
interface Logger {
    fn log(message: string);
}

// Perform an effect operation
@Logger.log("Hello, world!")
```

### What are Continuations?

A **continuation** represents "the rest of the program's computation." When an effect is handled, the handler receives a continuation representing all remaining computation from the effect call site to the handler boundary.

Calling this continuation "resumes" the suspended computation, providing a value as the return value of the effect call.

---

## Scoped Effects

### Handler Scoping Rules

Effect handlers in Rusk are **scoped**. This means:

1. **Handlers are only active within specific code regions**
2. **Handlers are installed via effect arms in `match` expressions**
3. **Handler lifetime is strictly limited to the evaluation of the scrutinee (matched expression)**

### Key Characteristics

```rusk
match compute() {
    @Logger.log(msg) => { print(msg); resume(()) }
    value => value
}
```

In this example:

- The `@Logger.log` handler is **only active while `compute()` is executing**
- Once `compute()` completes evaluation and returns a result, the handler becomes inactive
- Executing `@Logger.log` in a value arm (e.g., `value => value`) causes a runtime error

### Example: Effect Call Outside Scope Fails

```rusk
// fixtures/063_effect_handlers_scoped_to_scrutinee_runtime_error.rusk
interface E { fn boom() -> unit; }

fn main() -> unit {
    match 0 {
        @E.boom() => resume(())
        0 => { @E.boom() }  // ❌ Error! Handler not in scope
        _ => ()
    }
}
```

This program produces `runtime_error: unhandled effect` because:

1. The scrutinee of `match 0` is the constant `0`, which evaluates immediately
2. The effect handler `@E.boom()` is only active during scrutinee evaluation
3. When execution reaches the value arm `0 => { @E.boom() }`, the handler is out of scope
4. Therefore, `@E.boom()` cannot find a handler, causing a runtime error

### Why Scope Limitation?

Scoped design offers several important advantages:

1. **Local reasoning**: You can determine which code is affected by which handlers
2. **Prevent accidental capture**: Prevents handlers from inadvertently handling effects they shouldn't
3. **Composability**: Allows nested handlers without inner handlers affecting outer completed computations
4. **Resource management**: Ensures cleanup code runs at the correct time (see cleanup example below)

### Example: Scoped Cleanup Operations

```rusk
// fixtures/158_effects_cleanup_scoped_to_handler_ok.rusk
interface Subscribe<T> {
    fn subscribe(source: string) -> T;
}

interface Cleanup {
    fn on_cleanup(cleanup_fn: fn());
}

fn with_subscriptions<T>(component: fn() -> T) -> T {
    let cleanups: [fn()] = [];

    // Handler is active during component() execution
    let result = match component() {
        @Subscribe<int>.subscribe(source) => {
            resume(42)
        }
        @Cleanup.on_cleanup(cleanup_fn) => {
            array_push(cleanups, cleanup_fn);
            resume(())
        }
        v => v
    };

    // After component() completes, run all cleanup functions
    let i = 0;
    while i < array_len(cleanups) {
        cleanups[i]();
        i = i + 1;
    };

    result
}
```

This example demonstrates an important pattern:

1. **Effects are captured during `component()` execution** (`@Subscribe` and `@Cleanup`)
2. **Resume restores computation, potentially triggering more effects** (handled by the same handler)
3. **Handler collects all cleanup functions**
4. **When `component()` completes, cleanup code runs**

---

## Delimited Continuations

### What are Delimited Continuations?

**Delimited continuations** are "bounded continuations"—they don't capture the entire program's remaining computation, but only the computation up to a specific boundary.

In Rusk, this boundary is **the completion point of the `match` expression that installed the handler**.

### Delimited vs Undelimited

- **Undelimited continuation**: Captures all remaining computation to program end
- **Delimited continuation**: Only captures remaining computation to handler boundary

```rusk
fn outer() -> int {
    let x = inner();  // ← Call site
    x + 100
}

fn inner() -> int {
    match compute() {
        @Effect.op() => resume(42)  // continuation only to match end, excludes + 100 in outer
        v => v
    }
}

fn compute() -> int {
    @Effect.op() + 10  // ← Effect call site
}
```

When `@Effect.op()` executes:

1. **The captured continuation is**: `... + 10` then return to `match` value arm matching
2. **Does not include**: The `x + 100` in the outer function
3. Calling `resume(42)` will:
   - Use `42` as the return value of `@Effect.op()`
   - Continue executing `42 + 10`, getting `52`
   - Match value arm `v => v`, returning `52`
   - The `match` expression returns `52` to the `inner` function

### Implementation Mechanism

From the interpreter's perspective, a delimited continuation contains:

```rust
struct ContinuationState {
    stack: Vec<Frame>,           // Captured call stack frames
    handlers: Vec<HandlerEntry>, // Captured handler stack
    perform_dst: Option<Local>,  // Destination for effect call result
}
```

**Key points**:

1. **Stack is captured starting from the handler's owner frame**
   - Owner frame is the stack frame containing the `match` expression that installed the handler
   - Only captures frames from owner frame to current stack top

2. **Stack splicing on resume**
   - `resume` does not replace the entire interpreter state
   - Instead, it "splices" captured stack frames onto the current stack
   - This supports nested effect handlers

### Nested Handlers and Stack Splicing

```rusk
// fixtures/168_nested_effect_handlers_ok.rusk
fn with_state<T>(initial: T, component: fn() -> int) -> int {
    let state = initial;

    match component() {
        @State<T>.get() => resume(state)
        @State<T>.set(new_value) => {
            state = new_value;
            resume(())
        }
        result => result,
    }
}

fn main() -> int {
    with_state(UserId(0)) {
        with_state(PostId(42)) {
            counter() + counter()
        }
    }
}
```

This example has two nested handler levels:

1. Outer handles `State<UserId>`
2. Inner handles `State<PostId>`

When `@State<UserId>.get()` is called in `counter()`:

1. Inner continuation is captured (starting from inner `with_state` match)
2. Inner handler doesn't match (different type)
3. Searches outer handler, matches successfully
4. Outer handler resumes, **splicing** inner continuation back onto stack
5. Execution continues with inner handler still active

**What would happen with replacement instead of splicing**:

- Resume would replace the entire stack
- Inner handler would be lost
- Subsequent `@State<PostId>` effects couldn't be handled → Error!

Therefore, stack splicing is the key mechanism supporting nested handlers.

---

## Escaped Continuation Behavior

### One-Shot Semantics

Continuations in Rusk are **one-shot**:

- Each continuation can only be called once
- Calling the same continuation a second time causes `runtime_error: invalid resume`

### Why One-Shot?

1. **Performance**: No need to copy entire stack state
2. **Simplicity**: Avoids complex state management
3. **Safety**: Prevents accidental multiple resumes causing inconsistent state

### Example: Multiple Continuation Calls Fail

```rusk
// fixtures/067_continuation_call_one_shot_runtime_error.rusk
struct Cell<T> { v: T }
interface E { fn boom() -> int; }

fn main() -> unit {
    let cell = Cell { v: Option::None };

    // Capture continuation
    match @E.boom() {
        @E.boom() -> k => {
            cell.v = Option::Some(k);
            0
        }
        x => x
    };

    // Attempt to call twice
    match cell.v {
        Option::Some(k) => {
            k(1);  // ✓ First call succeeds
            k(2);  // ❌ Second call fails: invalid resume
            ()
        }
        Option::None => ()
    }
}
```

### Detecting Escaped Continuations

Continuation state is represented internally with `Option<ContinuationState>`:

```rust
pub struct ContinuationToken(Rc<RefCell<ContinuationInner>>);

struct ContinuationInner {
    state: Option<ContinuationState>,  // Becomes None after call
}

fn take_state(&self) -> Option<ContinuationState> {
    self.0.borrow_mut().state.take()  // Moves out state, can only succeed once
}
```

When resume is called:

```rust
let Some(mut cont) = token.take_state() else {
    return Err(RuntimeError::InvalidResume);  // state already taken
};
```

### Legal Continuation Storage and Delayed Call

While one-shot, continuations can be stored and called later:

```rusk
// fixtures/065_store_and_resume_continuation_later_default.rusk
struct Cell<T> { v: T }
interface E { fn boom() -> int; }

fn main() -> int {
    let cell = Cell { v: Option::None };

    // Store continuation instead of calling immediately
    match @E.boom() {
        @E.boom() => {
            cell.v = Option::Some(resume);  // Store resume
            0
        }
        x => x
    };

    // Call later
    match cell.v {
        Option::Some(k) => k(41)  // ✓ Success, returns 41
        Option::None => 0
    }
}
```

This example shows:

1. **Continuations can be stored** (in structs, arrays)
2. **Can be called later** (as long as not more than once)
3. **If never called, computation never completes** (abandoned)

---

## Implementation Details

### MIR-Level Implementation

#### Related Instructions

```rust
pub enum Instruction {
    // Install handler
    PushHandler {
        handler_id: String,
        clauses: Vec<HandlerClause>,
    },

    // Remove handler
    PopHandler,

    // Perform effect
    Perform {
        dst: Option<Local>,
        effect: EffectSpec,
        args: Vec<Operand>,
    },

    // Resume continuation
    Resume {
        dst: Option<Local>,
        k: Operand,
        value: Operand,
    },
}
```

#### Handler Entry Structure

```rust
struct HandlerEntry {
    owner_depth: usize,              // Stack frame depth owning this handler
    clauses: Vec<RuntimeHandlerClause>,
}

struct RuntimeHandlerClause {
    effect: RuntimeEffectId,         // Effect identity (interface + type args + method)
    arg_patterns: Vec<Pattern>,      // Argument patterns
    target: BlockId,                 // Handler code block
}
```

### Perform Execution Flow

When a `Perform` instruction executes (in `interpreter.rs:1484-1574`):

```rust
fn perform_effect(...) -> Result<(), RuntimeError> {
    // 1. Build effect identity
    let effect_id = RuntimeEffectId {
        interface: effect.interface,
        interface_args: evaluated_type_args,
        method: effect.method,
    };

    // 2. Find matching handler in handler stack
    let Some((handler_index, clause_index, binds)) =
        self.find_handler_for_effect(&effect_id, &args)? else {
        return Err(RuntimeError::UnhandledEffect { ... });
    };

    let handler_owner_depth = self.handlers[handler_index].owner_depth;

    // 3. Capture delimited continuation
    //    All frames from owner frame to current stack top
    let mut captured_stack = self.stack[handler_owner_depth..].to_vec();

    // 4. Capture related handlers (adjust owner_depth)
    let captured_handlers = self.handlers
        .iter()
        .filter_map(|entry| {
            let owner_depth = entry.owner_depth.checked_sub(handler_owner_depth)?;
            Some(HandlerEntry { owner_depth, clauses: entry.clauses.clone() })
        })
        .collect();

    // 5. Clear dst local in captured top frame (ensure uninitialized)
    if let Some(dst_local) = dst {
        captured_stack.last_mut()?.clear_local(dst_local)?;
    }

    // 6. Create continuation token
    let token = ContinuationToken::new(ContinuationState {
        stack: captured_stack,
        handlers: captured_handlers,
        perform_dst: dst,
    });

    // 7. Unwind to handler's owner frame
    self.stack.truncate(handler_owner_depth + 1);
    self.handlers.truncate(handler_index + 1);

    // 8. Jump to handler code block, passing arguments + continuation
    let mut block_args = binds;
    block_args.push(Value::Continuation(token));
    self.enter_block(handler_frame_index, clause.target, block_args)?;

    Ok(())
}
```

**Key steps**:

1. **Effect lookup**: Search from stack top down for first matching handler
2. **Capture stack**: Only capture from owner frame to stack top
3. **Capture handlers**: Also capture related handlers (for nested scenarios)
4. **Unwind**: Truncate stack and handler stack to handler position
5. **Transfer control**: Jump to handler code block

### Resume Execution Flow

When a `Resume` instruction executes (in `interpreter.rs:1392-1444`):

```rust
Instruction::Resume { dst, k, value } => {
    // 1. Check if k is a continuation
    let Value::Continuation(token) = k_value else {
        return Err(RuntimeError::InvalidResume);
    };

    // 2. Take continuation state (one-shot check)
    let Some(mut cont) = token.take_state() else {
        return Err(RuntimeError::InvalidResume);  // Already called
    };

    // 3. Write resume value to perform_dst in captured top frame
    if let Some(perform_dst) = cont.perform_dst {
        let top_frame = cont.stack.last_mut()?;
        top_frame.write_local(perform_dst, v)?;
    }

    // 4. Stack splicing instead of replacement (supports nested handlers)
    let base_depth = self.stack.len();

    // 5. Fix return destination of captured bottom frame
    let bottom = cont.stack.first_mut()?;
    bottom.return_dst = dst;

    // 6. Adjust owner_depth of captured handlers
    for handler in &mut cont.handlers {
        handler.owner_depth = handler.owner_depth.saturating_add(base_depth);
    }

    // 7. Splice: append captured stack and handlers to current stack
    self.stack.extend(cont.stack);
    self.handlers.extend(cont.handlers);
}
```

**Key points**:

1. **One-shot check**: `take_state()` can only succeed once
2. **Inject value**: Write resume argument to perform destination
3. **Stack splicing**: Not replacement, but append (`extend`)
4. **Depth adjustment**: All captured handlers' `owner_depth` needs base depth added
5. **Return target**: Fix bottom frame's return target to return to correct location

---

## Detailed Examples

### Example 1: Basic Effect Handling

```rusk
// fixtures/060_effects_resume_sum.rusk
interface Tick { fn tick(n: int) -> int; }

fn main() -> int {
    match @Tick.tick(1) + @Tick.tick(2) {
        @Tick.tick(n) => resume(n * 10)
        v => v
    }
}
```

**Execution flow**:

1. Install `@Tick.tick` handler (for scrutinee)
2. Start evaluating `@Tick.tick(1) + @Tick.tick(2)`
3. **First effect**: Execute `@Tick.tick(1)`
   - Find handler, matches (`n = 1`)
   - Capture continuation: `... + @Tick.tick(2)` then return to match value arms
   - Jump to handler: `resume(1 * 10)`
   - Call resume(10)
   - Resume computation: `10 + @Tick.tick(2)`
4. **Second effect**: Execute `@Tick.tick(2)` (handler still active!)
   - Find handler, matches (`n = 2`)
   - Capture continuation: `10 + ...` then return to match value arms
   - Jump to handler: `resume(2 * 10)`
   - Call resume(20)
   - Resume computation: `10 + 20 = 30`
5. Scrutinee evaluation completes with value `30`
6. Match value arm `v => v`, returns `30`

**Result**: `30`

**Key observations**:

- Handler remains active throughout scrutinee evaluation
- Even after first resume, handler can still handle second effect
- Continuation captures "remaining scrutinee computation"

### Example 2: Not Calling Resume

```rusk
// fixtures/061_effect_no_resume_result.rusk
interface Tick { fn tick(n: int) -> int; }

fn main() -> int {
    match @Tick.tick(1) {
        @Tick.tick(_) => 99
        v => v
    }
}
```

**Execution flow**:

1. Install handler
2. Execute `@Tick.tick(1)`
3. Handler matches, jumps to `=> 99`
4. **Don't call resume, directly return 99**
5. 99 becomes the result of the entire `match` expression

**Result**: `99`

**Key observations**:

- If continuation is not called, remaining computation is abandoned
- Handler's return value directly becomes match expression value
- Useful for "short-circuit" behavior (e.g., exception handling)

### Example 3: Nested Effect Handlers

```rusk
// fixtures/168_nested_effect_handlers_ok.rusk
interface State<T> {
    fn get() -> T;
    fn set(value: T);
}

fn with_state<T>(initial: T, component: fn() -> int) -> int {
    let state = initial;
    match component() {
        @State<T>.get() => resume(state)
        @State<T>.set(new_value) => {
            state = new_value;
            resume(())
        }
        result => result,
    }
}

fn counter() -> int {
    let u = get_state::<UserId>().0;
    set_state(UserId(u + 1));
    let p = get_state::<PostId>().0;
    set_state(PostId(p + 10));
    u + p
}

fn main() -> int {
    with_state(UserId(0)) {
        with_state(PostId(42)) {
            counter() + counter()
        }
    }
}
```

**Execution flow** (simplified):

1. Outer: Install `State<UserId>` handler
2. Inner: Install `State<PostId>` handler
3. First `counter()`:
   - `get_state::<UserId>()` → outer handles, resume(UserId(0)), returns 0
   - `set_state(UserId(1))` → outer handles, updates state, resume(())
   - `get_state::<PostId>()` → inner handles, resume(PostId(42)), returns 42
   - `set_state(PostId(52))` → inner handles, updates state, resume(())
   - Returns `0 + 42 = 42`
4. Second `counter()`:
   - `get_state::<UserId>()` → outer handles, resume(UserId(1)), returns 1
   - `set_state(UserId(2))` → outer handles, updates state, resume(())
   - `get_state::<PostId>()` → inner handles, resume(PostId(52)), returns 52
   - `set_state(PostId(62))` → inner handles, updates state, resume(())
   - Returns `1 + 52 = 53`
5. Returns `42 + 53 = 95`

**Result**: `95`

**Key observations**:

- Two state layers are completely independent (distinguished by type parameters)
- Outer resume restores inner computation, inner handler remains active
- This is thanks to **stack splicing** mechanism rather than stack replacement

---

## Summary

### Key Characteristics of Scoped Effects

1. ✅ **Handler scope limited to scrutinee evaluation period**
2. ✅ **Prevents handlers from accidentally capturing unrelated effects**
3. ✅ **Supports local reasoning and resource management**

### Key Characteristics of Delimited Continuations

1. ✅ **Only captures computation to handler boundary** (excludes outer functions)
2. ✅ **Supports nested handlers through stack splicing**
3. ✅ **Each continuation represents "remaining scrutinee computation"**

### Key Characteristics of Escaped Continuations

1. ✅ **One-shot semantics**: Each continuation callable only once
2. ✅ **Second call produces `invalid resume` error**
3. ✅ **Can be stored and called later** (as long as not more than once)
4. ✅ **Not calling means abandoning remaining computation**

### Design Trade-offs

| Feature | Rusk's Choice | Rationale |
|---------|---------------|-----------|
| Scope | Scoped | Local reasoning, safety |
| Boundary | Delimited | Composability, nesting support |
| Reentrancy | One-shot | Performance, simplicity |
| Stack restoration | Splicing | Nested handler support |

These design choices together form a powerful and predictable effect system suitable for exception handling, async programming, state management, dependency injection, and many other scenarios.

---

## References

- [RUSK_SPEC.md §7: Effects and Handlers](../RUSK_SPEC.md#7-effects-and-handlers)
- [MIR_SPEC.md](../MIR_SPEC.md)
- VM implementation: [crates/rusk-vm/src/lib.rs](../crates/rusk-vm/src/lib.rs)
- Test fixtures: [fixtures/](../fixtures/)
