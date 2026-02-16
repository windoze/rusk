# Rusk 中的 Delimited Continuation 与 Scoped Effect

本文档详细解释 Rusk 语言中 **delimited continuation（界定延续）** 和 **scoped effect（作用域效应）** 的工作原理，以及 **escaped continuation（逃逸延续）** 的行为。

## 目录

1. [核心概念](#核心概念)
2. [Scoped Effect（作用域效应）](#scoped-effect作用域效应)
3. [Delimited Continuation（界定延续）](#delimited-continuation界定延续)
4. [Escaped Continuation 行为](#escaped-continuation-行为)
5. [实现细节](#实现细节)
6. [示例详解](#示例详解)

---

## 核心概念

### 什么是 Algebraic Effect（代数效应）？

代数效应是一种控制流机制，允许程序在执行过程中"执行效应操作"（perform effect），然后由外部的"效应处理器"（effect handler）来决定如何响应这个效应。

在 Rusk 中，效应通过 `interface` 定义，通过 `@Interface.method(...)` 语法调用。

```rusk
interface Logger {
    fn log(message: string);
}

// 执行效应操作
@Logger.log("Hello, world!")
```

### 什么是 Continuation（延续）？

**Continuation** 代表"程序的剩余计算"。当一个效应被处理时，处理器会收到一个 continuation，它代表从效应调用点到处理器边界之间的所有剩余计算。

调用这个 continuation 就相当于"恢复"被暂停的计算，并提供一个值作为效应调用的返回值。

---

## Scoped Effect（作用域效应）

### 处理器作用域规则

Rusk 中的效应处理器是 **作用域限定的**（scoped）。这意味着：

1. **处理器仅在特定代码区域内有效**
2. **处理器通过 `match` 表达式的 effect arm 安装**
3. **处理器的生命周期严格限定在 scrutinee（被匹配表达式）的求值期间**

### 关键特性

```rusk
match compute() {
    @Logger.log(msg) => { print(msg); resume(()) }
    value => value
}
```

在上面的例子中：

- `@Logger.log` 处理器**只在 `compute()` 执行期间有效**
- 一旦 `compute()` 完成求值并返回结果，处理器就失效了
- 在 value arm（`value => value`）中执行 `@Logger.log` 会导致运行时错误

### 示例：作用域外的效应调用失败

```rusk
// fixtures/063_effect_handlers_scoped_to_scrutinee_runtime_error.rusk
interface E { fn boom() -> unit; }

fn main() -> unit {
    match 0 {
        @E.boom() => resume(())
        0 => { @E.boom() }  // ❌ 错误！处理器不在作用域内
        _ => ()
    }
}
```

这个程序会产生 `runtime_error: unhandled effect`，因为：

1. `match 0` 的 scrutinee 是常量 `0`，立即求值完成
2. 效应处理器 `@E.boom()` 只在 scrutinee 求值期间有效
3. 当执行到 value arm `0 => { @E.boom() }` 时，处理器已经不在作用域内了
4. 因此 `@E.boom()` 找不到处理器，导致运行时错误

### 为什么需要作用域限定？

作用域限定的设计有几个重要优势：

1. **局部推理**：你可以确定哪些代码受哪些处理器影响
2. **避免意外捕获**：防止处理器无意中处理了不应该处理的效应
3. **支持组合**：允许嵌套的处理器，内层处理器不会影响外层已完成的计算
4. **资源管理**：确保清理代码在正确的时机运行（见下面的 cleanup 示例）

### 示例：清理操作的作用域管理

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

    // 处理器在 component() 执行期间有效
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

    // component() 完成后，运行所有清理函数
    let i = 0;
    while i < array_len(cleanups) {
        cleanups[i]();
        i = i + 1;
    };

    result
}
```

这个示例展示了一个重要的模式：

1. **效应在 `component()` 执行期间被捕获**（`@Subscribe` 和 `@Cleanup`）
2. **resume 恢复计算，可能触发更多效应**（这些效应由同一个处理器继续处理）
3. **处理器收集所有清理函数**
4. **当 `component()` 完成后，运行清理代码**

---

## Delimited Continuation（界定延续）

### 什么是 Delimited Continuation？

**Delimited continuation** 是指"有界的延续"——它不是捕获整个程序的剩余计算，而是只捕获到某个特定边界为止的计算。

在 Rusk 中，这个边界就是**安装处理器的 `match` 表达式的求值完成点**。

### Delimited vs Undelimited

- **Undelimited continuation**（无界延续）：捕获到程序结束的所有剩余计算
- **Delimited continuation**（界定延续）：只捕获到处理器边界的剩余计算

```rusk
fn outer() -> int {
    let x = inner();  // ← 这里是调用点
    x + 100
}

fn inner() -> int {
    match compute() {
        @Effect.op() => resume(42)  // continuation 只到 match 结束，不包括 outer 中的 + 100
        v => v
    }
}

fn compute() -> int {
    @Effect.op() + 10  // ← 效应调用点
}
```

当 `@Effect.op()` 被执行时：

1. **捕获的 continuation 是**：`... + 10` 然后返回到 `match` 的 value arm 匹配
2. **不包括**：outer 函数中的 `x + 100`
3. 调用 `resume(42)` 会：
   - 将 `42` 作为 `@Effect.op()` 的返回值
   - 继续执行 `42 + 10`，得到 `52`
   - 匹配 value arm `v => v`，返回 `52`
   - `match` 表达式返回 `52` 给 `inner` 函数

### 实现机制

从解释器的角度看，delimited continuation 包含：

```rust
struct ContinuationState {
    stack: Vec<Frame>,           // 捕获的调用栈帧
    handlers: Vec<HandlerEntry>, // 捕获的处理器栈
    perform_dst: Option<Local>,  // 效应调用结果的目标位置
}
```

**关键点**：

1. **栈是从处理器的 owner frame 开始捕获的**
   - 处理器的 owner frame 是指安装处理器的 `match` 表达式所在的栈帧
   - 只捕获从 owner frame 到当前栈顶的所有帧

2. **恢复时的栈拼接**
   - `resume` 不会替换整个解释器状态
   - 而是将捕获的栈帧"拼接"到当前栈上
   - 这样可以支持嵌套的效应处理器

### 嵌套处理器与栈拼接

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

这个例子中有两层嵌套的处理器：

1. 外层处理 `State<UserId>`
2. 内层处理 `State<PostId>`

当 `counter()` 中的 `@State<UserId>.get()` 被调用时：

1. 内层的 continuation 被捕获（从内层 `with_state` 的 match 开始）
2. 内层处理器不匹配（因为类型不同）
3. 搜索外层处理器，匹配成功
4. 外层处理器 resume，将内层 continuation **拼接**回栈上
5. 继续执行，内层处理器仍然有效

**如果使用替换而非拼接**，会发生什么：

- resume 会替换整个栈
- 内层处理器会丢失
- 后续的 `@State<PostId>` 效应无法处理 → 错误！

因此，栈拼接是支持嵌套处理器的关键机制。

---

## Escaped Continuation 行为

### One-Shot 语义

Rusk 中的 continuation 是 **one-shot**（一次性的）：

- 每个 continuation 只能被调用一次
- 第二次调用同一个 continuation 会导致 `runtime_error: invalid resume`

### 为什么需要 One-Shot？

1. **性能**：不需要复制整个栈状态
2. **简单性**：避免复杂的状态管理
3. **安全性**：防止意外的多次恢复导致的不一致状态

### 示例：多次调用 Continuation 失败

```rusk
// fixtures/067_continuation_call_one_shot_runtime_error.rusk
struct Cell<T> { v: T }
interface E { fn boom() -> int; }

fn main() -> unit {
    let cell = Cell { v: Option::None };

    // 捕获 continuation
    match @E.boom() {
        @E.boom() -> k => {
            cell.v = Option::Some(k);
            0
        }
        x => x
    };

    // 尝试调用两次
    match cell.v {
        Option::Some(k) => {
            k(1);  // ✓ 第一次调用成功
            k(2);  // ❌ 第二次调用失败：invalid resume
            ()
        }
        Option::None => ()
    }
}
```

### 检测逃逸的 Continuation

Continuation 的状态在内部用 `Option<ContinuationState>` 表示：

```rust
pub struct ContinuationToken(Rc<RefCell<ContinuationInner>>);

struct ContinuationInner {
    state: Option<ContinuationState>,  // 调用后变为 None
}

fn take_state(&self) -> Option<ContinuationState> {
    self.0.borrow_mut().state.take()  // 移出状态，只能成功一次
}
```

当 resume 被调用时：

```rust
let Some(mut cont) = token.take_state() else {
    return Err(RuntimeError::InvalidResume);  // state 已经被取走
};
```

### 合法的 Continuation 存储和延迟调用

虽然是 one-shot，但 continuation 可以存储后稍后调用：

```rusk
// fixtures/065_store_and_resume_continuation_later_default.rusk
struct Cell<T> { v: T }
interface E { fn boom() -> int; }

fn main() -> int {
    let cell = Cell { v: Option::None };

    // 存储 continuation 而不是立即调用
    match @E.boom() {
        @E.boom() => {
            cell.v = Option::Some(resume);  // 存储 resume
            0
        }
        x => x
    };

    // 稍后调用
    match cell.v {
        Option::Some(k) => k(41)  // ✓ 成功，返回 41
        Option::None => 0
    }
}
```

这个示例展示了：

1. **Continuation 可以被存储**（在 struct、array 中）
2. **可以稍后调用**（只要不超过一次）
3. **如果从不调用，计算就永远不会完成**（被放弃）

---

## 实现细节

### MIR 层面的实现

#### 相关指令

```rust
pub enum Instruction {
    // 安装处理器
    PushHandler {
        handler_id: String,
        clauses: Vec<HandlerClause>,
    },

    // 移除处理器
    PopHandler,

    // 执行效应
    Perform {
        dst: Option<Local>,
        effect: EffectSpec,
        args: Vec<Operand>,
    },

    // 恢复 continuation
    Resume {
        dst: Option<Local>,
        k: Operand,
        value: Operand,
    },
}
```

#### Handler Entry 结构

```rust
struct HandlerEntry {
    owner_depth: usize,              // 拥有此处理器的栈帧深度
    clauses: Vec<RuntimeHandlerClause>,
}

struct RuntimeHandlerClause {
    effect: RuntimeEffectId,         // 效应标识（interface + 类型参数 + method）
    arg_patterns: Vec<Pattern>,      // 参数模式
    target: BlockId,                 // 处理器代码块
}
```

### Perform 的执行流程

当 `Perform` 指令执行时（在 `interpreter.rs:1484-1574`）：

```rust
fn perform_effect(...) -> Result<(), RuntimeError> {
    // 1. 构建效应标识
    let effect_id = RuntimeEffectId {
        interface: effect.interface,
        interface_args: evaluated_type_args,
        method: effect.method,
    };

    // 2. 在处理器栈中查找匹配的处理器
    let Some((handler_index, clause_index, binds)) =
        self.find_handler_for_effect(&effect_id, &args)? else {
        return Err(RuntimeError::UnhandledEffect { ... });
    };

    let handler_owner_depth = self.handlers[handler_index].owner_depth;

    // 3. 捕获 delimited continuation
    //    从 owner frame 到当前栈顶的所有帧
    let mut captured_stack = self.stack[handler_owner_depth..].to_vec();

    // 4. 捕获相关的处理器（调整 owner_depth）
    let captured_handlers = self.handlers
        .iter()
        .filter_map(|entry| {
            let owner_depth = entry.owner_depth.checked_sub(handler_owner_depth)?;
            Some(HandlerEntry { owner_depth, clauses: entry.clauses.clone() })
        })
        .collect();

    // 5. 清除捕获栈顶帧中的 dst local（确保未初始化）
    if let Some(dst_local) = dst {
        captured_stack.last_mut()?.clear_local(dst_local)?;
    }

    // 6. 创建 continuation token
    let token = ContinuationToken::new(ContinuationState {
        stack: captured_stack,
        handlers: captured_handlers,
        perform_dst: dst,
    });

    // 7. 展开到处理器的 owner frame
    self.stack.truncate(handler_owner_depth + 1);
    self.handlers.truncate(handler_index + 1);

    // 8. 跳转到处理器代码块，传入参数 + continuation
    let mut block_args = binds;
    block_args.push(Value::Continuation(token));
    self.enter_block(handler_frame_index, clause.target, block_args)?;

    Ok(())
}
```

**关键步骤**：

1. **效应查找**：从栈顶向下搜索第一个匹配的处理器
2. **捕获栈**：只捕获从 owner frame 到栈顶的部分
3. **捕获处理器**：同时捕获相关的处理器（用于嵌套场景）
4. **展开**：将栈和处理器栈截断到处理器位置
5. **转移控制**：跳转到处理器代码块

### Resume 的执行流程

当 `Resume` 指令执行时（在 `interpreter.rs:1392-1444`）：

```rust
Instruction::Resume { dst, k, value } => {
    // 1. 检查 k 是否是 continuation
    let Value::Continuation(token) = k_value else {
        return Err(RuntimeError::InvalidResume);
    };

    // 2. 取出 continuation state（one-shot 检查）
    let Some(mut cont) = token.take_state() else {
        return Err(RuntimeError::InvalidResume);  // 已经被调用过
    };

    // 3. 将 resume value 写入捕获栈顶帧的 perform_dst
    if let Some(perform_dst) = cont.perform_dst {
        let top_frame = cont.stack.last_mut()?;
        top_frame.write_local(perform_dst, v)?;
    }

    // 4. 栈拼接而非替换（支持嵌套处理器）
    let base_depth = self.stack.len();

    // 5. 修正捕获栈底帧的返回目标
    let bottom = cont.stack.first_mut()?;
    bottom.return_dst = dst;

    // 6. 调整捕获的处理器的 owner_depth
    for handler in &mut cont.handlers {
        handler.owner_depth = handler.owner_depth.saturating_add(base_depth);
    }

    // 7. 拼接：将捕获的栈和处理器追加到当前栈上
    self.stack.extend(cont.stack);
    self.handlers.extend(cont.handlers);
}
```

**关键点**：

1. **One-shot 检查**：`take_state()` 只能成功一次
2. **注入值**：将 resume 的参数写入 perform 的目标位置
3. **栈拼接**：不是替换，而是追加（`extend`）
4. **深度调整**：所有捕获的处理器的 `owner_depth` 需要加上当前栈深度
5. **返回目标**：修正底帧的返回目标，确保返回到正确的位置

---

## 示例详解

### 示例 1：基础效应处理

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

**执行流程**：

1. 安装 `@Tick.tick` 处理器（对应 scrutinee）
2. 开始求值 `@Tick.tick(1) + @Tick.tick(2)`
3. **第一次效应**：执行 `@Tick.tick(1)`
   - 查找处理器，匹配成功（`n = 1`）
   - 捕获 continuation：`... + @Tick.tick(2)` 然后返回到 match value arms
   - 跳转到处理器：`resume(1 * 10)`
   - 调用 resume(10)
   - 恢复计算：`10 + @Tick.tick(2)`
4. **第二次效应**：执行 `@Tick.tick(2)`（处理器仍有效！）
   - 查找处理器，匹配成功（`n = 2`）
   - 捕获 continuation：`10 + ...` 然后返回到 match value arms
   - 跳转到处理器：`resume(2 * 10)`
   - 调用 resume(20)
   - 恢复计算：`10 + 20 = 30`
5. scrutinee 求值完成，值为 `30`
6. 匹配 value arm `v => v`，返回 `30`

**结果**：`30`

**关键观察**：

- 处理器在整个 scrutinee 求值期间保持有效
- 即使在第一次 resume 后，处理器仍然可以处理第二次效应
- Continuation 捕获的是"剩余的 scrutinee 计算"

### 示例 2：不调用 Resume

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

**执行流程**：

1. 安装处理器
2. 执行 `@Tick.tick(1)`
3. 处理器匹配，跳转到 `=> 99`
4. **不调用 resume，直接返回 99**
5. 99 成为整个 `match` 表达式的结果

**结果**：`99`

**关键观察**：

- 如果不调用 continuation，剩余计算就被放弃
- 处理器的返回值直接成为 match 表达式的值
- 这对于"短路"行为很有用（例如异常处理）

### 示例 3：嵌套效应处理器

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

**执行流程**（简化）：

1. 外层：安装 `State<UserId>` 处理器
2. 内层：安装 `State<PostId>` 处理器
3. 第一次 `counter()`：
   - `get_state::<UserId>()` → 外层处理，resume(UserId(0))，返回 0
   - `set_state(UserId(1))` → 外层处理，更新状态，resume(())
   - `get_state::<PostId>()` → 内层处理，resume(PostId(42))，返回 42
   - `set_state(PostId(52))` → 内层处理，更新状态，resume(())
   - 返回 `0 + 42 = 42`
4. 第二次 `counter()`：
   - `get_state::<UserId>()` → 外层处理，resume(UserId(1))，返回 1
   - `set_state(UserId(2))` → 外层处理，更新状态，resume(())
   - `get_state::<PostId>()` → 内层处理，resume(PostId(52))，返回 52
   - `set_state(PostId(62))` → 内层处理，更新状态，resume(())
   - 返回 `1 + 52 = 53`
5. 返回 `42 + 53 = 95`

**结果**：`95`

**关键观察**：

- 两层状态完全独立（通过类型参数区分）
- 外层的 resume 会恢复内层的计算，内层处理器仍然有效
- 这得益于**栈拼接**机制而非栈替换

---

## 总结

### Scoped Effect 的关键特性

1. ✅ **处理器作用域限定在 scrutinee 求值期间**
2. ✅ **防止处理器意外捕获不相关的效应**
3. ✅ **支持局部推理和资源管理**

### Delimited Continuation 的关键特性

1. ✅ **只捕获到处理器边界的计算**（不包括外层函数）
2. ✅ **通过栈拼接支持嵌套处理器**
3. ✅ **每个 continuation 代表"剩余的 scrutinee 计算"**

### Escaped Continuation 的关键特性

1. ✅ **One-shot 语义**：每个 continuation 只能调用一次
2. ✅ **第二次调用会产生 `invalid resume` 错误**
3. ✅ **可以存储和延迟调用**（只要不超过一次）
4. ✅ **不调用就意味着放弃剩余计算**

### 设计权衡

| 特性 | Rusk 的选择 | 理由 |
|------|------------|------|
| 作用域 | Scoped（限定作用域） | 局部推理、安全性 |
| 边界 | Delimited（有界） | 可组合性、嵌套支持 |
| 可重入性 | One-shot（一次性） | 性能、简单性 |
| 栈恢复 | Splicing（拼接） | 嵌套处理器支持 |

这些设计选择共同构成了一个强大而可预测的效应系统，适用于异常处理、异步编程、状态管理、依赖注入等多种场景。

---

## 参考资料

- [RUSK_SPEC.md §7: Effects and Handlers](../RUSK_SPEC.md#7-effects-and-handlers)
- [MIR_SPEC.md](../MIR_SPEC.md)
- VM 实现：[crates/rusk-vm/src/lib.rs](../crates/rusk-vm/src/lib.rs)
- 测试用例：[fixtures/](../fixtures/)
