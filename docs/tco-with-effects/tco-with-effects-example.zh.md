# 使用 Effect 实现手动尾调用优化（TCO）

本文档演示如何使用 Rusk 的 effect 系统来实现手动尾调用优化（Tail Call Optimization, TCO），避免栈溢出。

## 什么是尾调用优化？

尾调用优化是指编译器将尾递归函数转换为循环的优化技术。在尾递归中，递归调用是函数的最后一个操作，因此不需要保留当前栈帧。

例如，计算阶乘的尾递归版本：

```javascript
// 尾递归版本（伪代码）
function factorial(n, acc) {
  if (n <= 1) return acc;
  return factorial(n - 1, n * acc);  // 尾调用
}
```

在有 TCO 的语言中，这个函数会被优化为循环，不会导致栈溢出。

## 使用 Effect 实现 TCO

在 Rusk 中，我们可以使用 effect 系统来手动实现类似 trampoline 的 TCO 机制：

1. 定义一个 `TailCall` effect，表示需要进行尾递归调用
2. 在递归函数中，不直接调用自身，而是执行 `@TailCall.call(args)`
3. 外部用一个循环处理器，不断处理 `TailCall` effect，直到得到最终结果

## 示例：阶乘函数

```rusk
// 定义尾调用 effect
interface TailCall<R> {
    fn call(n: int, acc: int) -> R;
}

// 尾递归风格的阶乘实现
// 注意：这里不直接递归调用，而是通过 effect 实现
fn factorial_tail(n: int, acc: int) -> int {
    if n <= 1 {
        acc
    } else {
        // 不是直接调用 factorial_tail(n - 1, n * acc)
        // 而是执行一个 effect
        @TailCall<int>.call(n - 1, n * acc)
    }
}

// TCO 循环处理器：将递归转换为循环
fn with_tco<R>(initial_n: int, initial_acc: int, f: fn(int, int) -> R) -> R {
    let n = initial_n;
    let acc = initial_acc;

    // 用循环不断处理尾调用 effect
    loop {
        match f(n, acc) {
            // 当函数执行 @TailCall.call 时，捕获参数
            @TailCall<R>.call(next_n, next_acc) => {
                // 更新参数，继续循环
                n = next_n;
                acc = next_acc;
                // 不调用 resume，让循环重新执行
                continue;
            }
            // 当函数返回最终结果时（不执行 effect）
            result => {
                // 返回最终结果，跳出循环
                break result;
            }
        }
    }
}

// 使用 TCO 计算阶乘
fn factorial(n: int) -> int {
    with_tco(n, 1, factorial_tail)
}

fn main() -> int {
    // 即使 n 很大，也不会栈溢出
    factorial(10)  // 3628800
}
```

## 工作原理

让我们跟踪 `factorial(5)` 的执行流程：

1. **初始化**：`with_tco(5, 1, factorial_tail)` 被调用
   - 设置 `n = 5, acc = 1`
   - 进入循环

2. **第一次迭代**：调用 `f(5, 1)` → `factorial_tail(5, 1)`
   - 条件 `5 <= 1` 为假
   - 执行 `@TailCall<int>.call(4, 5)`
   - 处理器捕获：`next_n = 4, next_acc = 5`
   - 更新：`n = 4, acc = 5`
   - `continue` 继续循环

3. **第二次迭代**：调用 `f(4, 5)` → `factorial_tail(4, 5)`
   - 条件 `4 <= 1` 为假
   - 执行 `@TailCall<int>.call(3, 20)`
   - 更新：`n = 3, acc = 20`
   - `continue` 继续循环

4. **第三次迭代**：调用 `f(3, 20)` → `factorial_tail(3, 20)`
   - 条件 `3 <= 1` 为假
   - 执行 `@TailCall<int>.call(2, 60)`
   - 更新：`n = 2, acc = 60`
   - `continue` 继续循环

5. **第四次迭代**：调用 `f(2, 60)` → `factorial_tail(2, 60)`
   - 条件 `2 <= 1` 为假
   - 执行 `@TailCall<int>.call(1, 120)`
   - 更新：`n = 1, acc = 120`
   - `continue` 继续循环

6. **第五次迭代**：调用 `f(1, 120)` → `factorial_tail(1, 120)`
   - 条件 `1 <= 1` 为真
   - 返回 `acc = 120`（不执行 effect）
   - 匹配 `result => break result`
   - 返回 `120`

## 关键观察

### 1. 无栈增长

整个过程中，解释器的调用栈深度保持恒定：

```
with_tco (循环)
  └─ factorial_tail (在每次迭代中被调用)
```

每次执行 `@TailCall.call` 时：
- 捕获 continuation（从 effect 调用点到 match 边界）
- 但我们**不调用 `resume`**，而是选择 `continue` 循环
- 因此 continuation 被丢弃，栈不会增长

### 2. 对比直接递归

如果我们写直接递归：

```rusk
fn factorial_direct(n: int) -> int {
    if n <= 1 {
        1
    } else {
        n * factorial_direct(n - 1)  // 不是尾调用！
    }
}
```

调用栈会是：

```
factorial_direct(5)
  └─ factorial_direct(4)
      └─ factorial_direct(3)
          └─ factorial_direct(2)
              └─ factorial_direct(1)
```

栈深度随 n 线性增长，最终可能栈溢出。

### 3. Effect 作为"可控的递归"

通过 effect 系统：
- **递归函数不直接调用自身**，而是"请求"递归（通过 effect）
- **外部处理器决定如何处理递归请求**（循环、栈展开、异步等）
- 这提供了极大的灵活性

## 扩展：通用 TCO 模式

我们可以将这个模式泛化为更通用的形式：

```rusk
interface TailCall<Args, R> {
    fn call(args: Args) -> R;
}

fn trampoline<Args, R>(
    initial: Args,
    f: fn(Args) -> R
) -> R {
    let args = initial;

    loop {
        match f(args) {
            @TailCall<Args, R>.call(next_args) => {
                args = next_args;
                continue;
            }
            result => break result
        }
    }
}
```

这样可以支持任意参数的尾递归函数。

## 实际应用场景

1. **深度递归计算**：树遍历、图搜索等
2. **状态机**：游戏循环、协议处理等
3. **CPS 转换**：将任意递归转换为尾递归
4. **异步编程**：配合其他 effect 实现 async/await

## 与其他语言的对比

| 语言 | TCO 支持方式 |
|------|------------|
| Scheme/Racket | 语言规范要求 TCO |
| JavaScript | ES6 提出了 TCO，但多数引擎未实现 |
| Rust | 无自动 TCO，需手动用循环 |
| OCaml/Haskell | 编译器自动优化尾调用 |
| **Rusk (本例)** | 通过 effect 手动实现，灵活可控 |

## 更多示例

### 示例 2：斐波那契数列

斐波那契数列是另一个经典的递归问题，使用 TCO 可以高效计算：

```rusk
// 参见：docs/tco_fibonacci_example.rusk

interface FibTailCall {
    fn call(n: int, a: int, b: int) -> int;
}

fn fib_tail(n: int, a: int, b: int) -> int {
    if n <= 0 {
        a
    } else {
        @FibTailCall.call(n - 1, b, a + b)
    }
}

fn fibonacci(target: int) -> int {
    let n = target;
    let a = 0;  // F(0) = 0
    let b = 1;  // F(1) = 1
    let result = 0;
    let done = false;

    while !done {
        let _ = match fib_tail(n, a, b) {
            @FibTailCall.call(next_n, next_a, next_b) => {
                n = next_n;
                a = next_a;
                b = next_b;
                0
            }
            value => {
                result = value;
                done = true;
                0
            }
        };
    };

    result
}

fn main() -> int {
    fibonacci(10)  // 返回 55
}
```

这个例子展示了：
- 多参数的尾递归（n, a, b 三个状态变量）
- 状态转换：`(n, a, b) -> (n-1, b, a+b)`
- 同样的 TCO 模式适用于更复杂的递归结构

### 示例 3：带日志的 TCO

Effect 系统的强大之处在于可以组合多个 effect。我们可以在 TCO 中加入日志：

```rusk
interface TailCall {
    fn call(n: int, acc: int) -> int;
}

interface Logger {
    fn log(msg: string);
}

fn factorial_tail_with_log(n: int, acc: int) -> int {
    @Logger.log(f"factorial_tail({n}, {acc})");

    if n <= 1 {
        acc
    } else {
        @TailCall.call(n - 1, n * acc)
    }
}

fn factorial_with_logging(n: int) -> int {
    // 外层处理日志
    match factorial_inner(n) {
        @Logger.log(msg) => {
            print(msg);
            resume(())
        }
        result => result
    }
}

fn factorial_inner(initial_n: int) -> int {
    let n = initial_n;
    let acc = 1;
    let result = 0;
    let done = false;

    while !done {
        let _ = match factorial_tail_with_log(n, acc) {
            @TailCall.call(next_n, next_acc) => {
                n = next_n;
                acc = next_acc;
                0
            }
            value => {
                result = value;
                done = true;
                0
            }
        };
    };

    result
}
```

这展示了 effect 的可组合性：
- 内层循环处理 `TailCall` effect
- 外层 match 处理 `Logger` effect
- 两个 effect 协同工作，互不干扰

## 实际代码文件

本仓库包含以下可运行的示例：

1. **[tco_factorial_simple.rusk](tco_factorial_simple.rusk)** - 阶乘的简化版实现
2. **[tco_fibonacci_example.rusk](tco_fibonacci_example.rusk)** - 斐波那契数列实现
3. **[tco_factorial_example.rusk](tco_factorial_example.rusk)** - 带详细注释的阶乘实现

## 总结

使用 effect 系统实现 TCO 的优势：

1. ✅ **防止栈溢出**：递归深度不受限制
2. ✅ **保持函数式风格**：代码看起来像递归，实际上是循环
3. ✅ **灵活可控**：处理器可以自定义行为（日志、调试、异步等）
4. ✅ **类型安全**：所有递归调用都被类型系统检查
5. ✅ **可组合**：可以与其他 effect 组合使用

这展示了 Rusk effect 系统的强大表达能力：不仅可以处理异常、状态等传统 effect，还能实现控制流优化！

## 性能考虑

虽然这种手动 TCO 看起来增加了代码复杂度，但它具有以下优势：

1. **恒定栈空间**：无论递归深度，栈使用量恒定
2. **显式控制**：可以在循环中插入调试、监控等逻辑
3. **教育价值**：清楚展示了 TCO 的工作原理
4. **通用性**：可以应用于任何尾递归函数

在实际应用中，如果编译器支持自动 TCO，那当然更好。但在不支持的情况下，这种 effect-based 的手动 TCO 提供了一个优雅的解决方案。
