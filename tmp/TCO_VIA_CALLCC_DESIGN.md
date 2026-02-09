# 使用 Call/CC 实现 TCO 的设计方案

## 核心思路

Rusk 已经通过 effect system 实现了 **delimited continuations**（`perform` / `resume`），这本质上是一种受限的 call/cc。我们可以利用这个机制实现 TCO，将尾调用转换为 effect perform，由外层的 trampoline handler 循环处理。

---

## 🎯 方案概述

### 传统 TCO vs Call/CC TCO

**传统 TCO（编译期循环变换）**：
```rust
fn factorial(n: int, acc: int) -> int {
    if n <= 1 { acc } else { factorial(n-1, n*acc) }
}

// 编译器变换为：
fn factorial(n: int, acc: int) -> int {
    loop {
        if n <= 1 { return acc }
        let (n', acc') = (n-1, n*acc)
        n = n'
        acc = acc'
        // continue loop
    }
}
```

**Call/CC TCO（使用 effect）**：
```rust
// 用户代码不变
fn factorial(n: int, acc: int) -> int {
    if n <= 1 { acc } else { factorial(n-1, n*acc) }
}

// 编译器变换为：
fn factorial(n: int, acc: int) -> int {
    if n <= 1 {
        acc
    } else {
        @TailCall.invoke(factorial, n-1, n*acc)  // 尾调用 → effect
    }
}

// 用户在调用点安装 handler
fn main() {
    let result = with_tco(|| factorial(10000, 1))
    print(result)
}
```

---

## 📋 详细设计

### 1. 定义 TailCall Effect Interface

```rust
// 标准库提供的 effect interface
interface TailCall {
    fn invoke<T>(func: fn(...) -> T, args: ...) -> T;
}
```

**注意**：实际实现中，由于 Rusk 的类型系统限制，可能需要为不同 arity 的函数定义多个方法：

```rust
interface TailCall {
    fn invoke0<T>(func: fn() -> T) -> T;
    fn invoke1<T, A>(func: fn(A) -> T, a: A) -> T;
    fn invoke2<T, A, B>(func: fn(A, B) -> T, a: A, b: B) -> T;
    fn invoke3<T, A, B, C>(func: fn(A, B, C) -> T, a: A, b: B, c: C) -> T;
    // ... 根据需要扩展
}
```

### 2. 编译期变换（在 MIR 生成时）

**编译器在 `compiler.rs` 中识别尾调用并转换**：

```rust
// 原始 AST
fn foo(x: int) -> int {
    bar(x + 1)  // 尾位置的调用
}

// 编译为 MIR（未优化）
fn foo(%x) -> int {
entry:
  %x1 = call int_add(%x, 1)
  %result = call bar(%x1)        // 普通 call
  return %result                  // 立即返回
}

// 编译为 MIR（TCO 优化）
fn foo(%x) -> int {
entry:
  %x1 = call int_add(%x, 1)
  %bar_fn = const bar              // 函数值
  %result = perform TailCall.invoke1(%bar_fn, %x1)  // effect call
  return %result
}
```

**检测规则**：
1. 遍历函数的所有基本块
2. 查找模式：`%tmp = call f(...); return %tmp`
3. 或者：terminator 是 `return call f(...)`
4. 将其转换为 `perform TailCall.invoke(...)`

### 3. Trampoline 运行器（标准库提供）

**用户调用 TCO 函数时，需要包裹在 trampoline 中**：

```rust
// stdlib: 标准库提供的 trampoline 函数
fn with_tco<T>(f: fn() -> T) -> T {
    match f() {
        @TailCall.invoke0(next_func) => {
            // 循环处理尾调用链
            let mut current = next_func
            loop {
                match current() {
                    @TailCall.invoke0(next) => {
                        current = next
                        // continue
                    }
                    final_value => {
                        return final_value
                    }
                }
            }
        }
        @TailCall.invoke1(next_func, arg1) => {
            let mut current = next_func
            let mut a = arg1
            loop {
                match current(a) {
                    @TailCall.invoke1(next, next_a) => {
                        current = next
                        a = next_a
                    }
                    final_value => return final_value
                }
            }
        }
        // ... 其他 arity
        direct_result => direct_result
    }
}

// 两参数版本
fn with_tco2<T, A, B>(f: fn(A, B) -> T, a: A, b: B) -> T {
    match f(a, b) {
        @TailCall.invoke2(next_func, next_a, next_b) => {
            let mut current = next_func
            let mut arg1 = next_a
            let mut arg2 = next_b
            loop {
                match current(arg1, arg2) {
                    @TailCall.invoke2(next, a, b) => {
                        current = next
                        arg1 = a
                        arg2 = b
                    }
                    final_value => return final_value
                }
            }
        }
        direct_result => direct_result
    }
}
```

### 4. 用户代码示例

```rust
fn factorial(n: int, acc: int) -> int {
    if n <= 1 {
        acc
    } else {
        factorial(n - 1, n * acc)  // 编译器自动转换为 @TailCall.invoke2
    }
}

fn main() -> unit {
    // 用户需要显式使用 with_tco 包裹
    let result = with_tco2(factorial, 10000, 1)
    print(result)
}
```

---

## ✅ 优势分析

### 1. **覆盖所有尾调用场景** ⭐⭐⭐⭐⭐

| 场景 | 传统编译期 TCO | Call/CC TCO |
|------|---------------|-------------|
| 直接尾递归 | ✅ | ✅ |
| 相互递归 | ⚠️ 复杂 | ✅ |
| 间接尾调用（函数指针） | ❌ | ✅ |
| 跨模块尾调用 | ❌ | ✅ |
| 条件分支的尾调用 | ✅ | ✅ |

**示例：间接尾调用**
```rust
fn apply_twice(f: fn(int) -> int, x: int) -> int {
    f(f(x))  // 第二个 f 是尾调用，传统方法无法优化
}

// 使用 call/cc TCO，编译器转换为：
fn apply_twice(f: fn(int) -> int, x: int) -> int {
    let tmp = f(x)
    @TailCall.invoke1(f, tmp)  // 可以优化！
}

// 运行时 trampoline 自动处理
let result = with_tco1(apply_twice, some_func, 42)
```

### 2. **实现简单** ⭐⭐⭐⭐

**编译器端**（~200 行代码）：
- 识别尾调用位置（已有的控制流分析）
- 转换 `call` → `perform TailCall.invoke`

**运行时端**（0 行新代码）：
- 完全复用现有的 effect system
- Trampoline 用纯 Rusk 代码实现（在 stdlib 中）

### 3. **与现有架构完全兼容** ⭐⭐⭐⭐⭐

- ✅ 不需要修改解释器
- ✅ 不需要修改 GC
- ✅ 不需要修改 effect system
- ✅ 不影响 continuation 捕获语义

### 4. **渐进式采用** ⭐⭐⭐⭐

用户可以选择性地优化：
```rust
// 不需要 TCO 的场景
fn small_recursion(n: int) -> int {
    if n <= 0 { 0 } else { small_recursion(n - 1) + 1 }
}
small_recursion(10)  // 直接调用，栈深度只有 10

// 需要 TCO 的场景
fn deep_recursion(n: int) -> int {
    if n <= 0 { 0 } else { deep_recursion(n - 1) + 1 }
}
with_tco1(deep_recursion, 100000)  // 使用 trampoline，栈深度恒定
```

### 5. **支持动态尾调用链** ⭐⭐⭐⭐⭐

```rust
fn bounce(n: int, which: bool) -> int {
    if n <= 0 {
        0
    } else if which {
        bounce(n - 1, false)  // 调用自己
    } else {
        helper(n - 1)         // 调用其他函数
    }
}

fn helper(n: int) -> int {
    bounce(n, true)           // 又调回去
}

// trampoline 自动处理整个调用链
let result = with_tco1(bounce, 10000, true)
```

---

## ⚠️ 劣势和挑战

### 1. **性能开销** ⭐⭐⭐

每次尾调用需要：
- Perform effect（查找 handler 栈）
- 匹配 handler clause
- Resume continuation

**估计开销**：每次尾调用 ~50-200 CPU cycles

**对比**：
- 编译期循环变换：~5-10 cycles（几乎零开销）
- 普通函数调用：~20-50 cycles

**缓解措施**：
- 对于静态已知的直接递归，仍然用编译期变换
- Call/CC TCO 只用于无法静态优化的场景

### 2. **类型系统复杂性** ⭐⭐⭐⭐

**问题**：
```rust
interface TailCall {
    fn invoke<T, Args...>(func: fn(Args...) -> T, args: Args...) -> T;
}
```

Rusk 的类型系统可能不支持 variadic generics。

**解决方案**：
- 为每个 arity 定义单独的方法（`invoke0`, `invoke1`, ...）
- 或引入 tuple types 和 tuple unpacking

### 3. **用户需要显式包裹** ⭐⭐

```rust
// 用户必须记得使用 with_tco
let result = with_tco2(factorial, n, 1)

// 而不能直接：
let result = factorial(n, 1)  // 仍然会栈溢出！
```

**缓解措施**：
- 编译器可以检测深度递归函数，发出警告
- 或者提供编译选项：`--auto-tco`，自动为所有递归函数注入 trampoline

### 4. **调试体验** ⭐⭐

```rust
// 栈跟踪会显示 trampoline 的痕迹
Traceback:
  at with_tco2 (stdlib.rusk:42)
  at factorial (user.rusk:10)
  at with_tco2 (stdlib.rusk:48)  // 循环中
  at factorial (user.rusk:10)
  at with_tco2 (stdlib.rusk:48)
  ...
```

**缓解措施**：
- 调试器可以过滤掉 trampoline 帧
- 或提供 `@inline` 注解优化 trampoline

---

## 🔬 与编译期 TCO 的对比

| 维度 | 编译期循环变换 | Call/CC Trampoline |
|------|---------------|-------------------|
| **覆盖率** | 70% | 100% |
| **性能** | ⭐⭐⭐⭐⭐ (0开销) | ⭐⭐⭐ (轻微开销) |
| **实现复杂度** | 中 (~300行) | 低 (~200行) |
| **需要修改解释器** | ❌ | ❌ |
| **支持间接调用** | ❌ | ✅ |
| **支持相互递归** | ⚠️ | ✅ |
| **用户透明度** | ✅ 完全透明 | ⚠️ 需要包裹 |
| **调试友好** | ✅ | ⚠️ |
| **嵌入式适用** | ✅ | ✅ |

---

## 💡 混合策略：两者结合

**最佳方案**：同时实现两种优化！

### 编译器决策树：

```
检测到尾调用 →
  ├─ 是直接自递归？
  │   └─ YES → 使用编译期循环变换（零开销）
  │
  ├─ 是简单相互递归（2-3个函数）？
  │   └─ YES → 使用编译期状态机变换
  │
  └─ 否则（间接调用/复杂调用图）
      └─ 转换为 @TailCall.invoke（trampoline）
```

### 示例：

```rust
// 情况 1：直接递归 → 编译期优化
fn factorial(n: int, acc: int) -> int {
    if n <= 1 { acc } else { factorial(n-1, n*acc) }
}
// 编译器自动变换为循环，用户直接调用：
factorial(10000, 1)  // ✅ 零开销

// 情况 2：间接调用 → call/cc trampoline
fn apply_fn(f: fn(int) -> int, x: int) -> int {
    if x <= 0 { 0 } else { f(x) }
}
// 编译器转换为 effect，用户需要包裹：
with_tco2(apply_fn, some_func, 10000)  // ✅ 轻微开销但可用

// 情况 3：相互递归 → 视复杂度选择
fn is_even(n: int) -> bool {
    if n == 0 { true } else { is_odd(n-1) }
}
fn is_odd(n: int) -> bool {
    if n == 0 { false } else { is_even(n-1) }
}
// 简单情况 → 编译期合并
// 复杂情况 → call/cc trampoline
```

---

## 🛠️ 实现路线图

### Phase 1：基础 Call/CC TCO（~2-3天）

1. **定义 TailCall interface**（stdlib）
   ```rust
   interface TailCall {
       fn invoke1<T, A>(func: fn(A) -> T, a: A) -> T;
       fn invoke2<T, A, B>(func: fn(A, B) -> T, a: A, b: B) -> T;
       // ...
   }
   ```

2. **实现 Trampoline 运行器**（stdlib）
   ```rust
   fn with_tco1<T, A>(f: fn(A) -> T, a: A) -> T { ... }
   fn with_tco2<T, A, B>(f: fn(A, B) -> T, a: A, b: B) -> T { ... }
   ```

3. **编译器尾调用检测**（compiler.rs）
   - 在 `FunctionLowerer::finish()` 中添加 `detect_tail_calls()`
   - 识别 `return call(...)` 模式

4. **编译器尾调用变换**（compiler.rs）
   - 将 `call` instruction 替换为 `perform TailCall.invoke`
   - 添加编译选项：`--enable-callcc-tco`

**交付物**：
- ✅ 用户可以手动使用 `with_tco` 包裹递归函数
- ✅ 覆盖 100% 尾调用场景
- ✅ 性能可接受（比无优化好 100x，比编译期优化慢 5-10x）

### Phase 2：混合优化（~1-2周）

1. **实现编译期循环变换**（针对直接递归）
   - 覆盖 70% 场景，零开销

2. **智能决策逻辑**
   - 编译器自动选择最优策略

3. **自动注入 trampoline**（可选）
   - `--auto-tco` 编译选项
   - 为所有递归函数自动包裹

**交付物**：
- ✅ 最常见场景（直接递归）零开销
- ✅ 复杂场景（间接调用）自动降级到 trampoline
- ✅ 用户透明（可选）

### Phase 3：性能优化（~1周）

1. **Inline trampoline**
   - 减少 effect perform 开销

2. **专门化**
   - 为常见 arity 生成优化代码

3. **编译时常量传播**
   - 识别编译时已知的函数指针

---

## 📊 性能预测

### 微基准测试：Factorial(10000)

| 方案 | 执行时间 | 内存占用 | 栈深度 |
|------|---------|---------|--------|
| 无优化（会崩溃） | - | - | 10000 帧 |
| 编译期循环 | 1.0x | 8 KB | 1 帧 |
| Call/CC Trampoline | 1.5-3.0x | 8 KB | 2-3 帧 |
| 手写循环 | 1.0x | 8 KB | 1 帧 |

### 宏基准测试：相互递归 is_even/is_odd(10000)

| 方案 | 执行时间 | 内存占用 | 栈深度 |
|------|---------|---------|--------|
| 无优化（会崩溃） | - | - | 10000 帧 |
| 编译期状态机 | 1.0x | 8 KB | 1 帧 |
| Call/CC Trampoline | 2.0-4.0x | 16 KB | 2-3 帧 |

**结论**：Call/CC TCO 在嵌入式场景下完全可用，性能开销可接受。

---

## 🎯 最终建议

### 对于 Rusk 项目：

1. **立即实现 Phase 1**（Call/CC TCO）
   - 投入：2-3 天
   - 收益：覆盖 100% 尾调用场景
   - 风险：低（复用现有 effect system）

2. **中期实现 Phase 2**（混合优化）
   - 投入：1-2 周
   - 收益：最常见场景零开销
   - 风险：中（需要编译器分析）

3. **性能优化按需进行**（Phase 3）
   - 如果 trampoline 开销可接受，Phase 2 就够了

### 关键洞察：

> **Rusk 已经有了实现 TCO 所需的所有机制！**
> 只需要将尾调用视为一种特殊的 effect，由 trampoline handler 循环处理。

这是一个优雅的方案，充分利用了语言的核心特性（effect system），而不是添加特殊的运行时支持。

---

## 附录：完整示例代码

```rust
// ======================
// stdlib/tco.rusk
// ======================

interface TailCall {
    fn invoke2<T, A, B>(func: fn(A, B) -> T, a: A, b: B) -> T;
}

fn with_tco2<T, A, B>(f: fn(A, B) -> T, init_a: A, init_b: B) -> T {
    match f(init_a, init_b) {
        @TailCall.invoke2(next_func, next_a, next_b) => {
            let mut current_func = next_func
            let mut arg_a = next_a
            let mut arg_b = next_b

            loop {
                match current_func(arg_a, arg_b) {
                    @TailCall.invoke2(f, a, b) => {
                        current_func = f
                        arg_a = a
                        arg_b = b
                        continue
                    }
                    final_result => {
                        return final_result
                    }
                }
            }
        }
        direct_result => direct_result
    }
}

// ======================
// user_code.rusk
// ======================

fn factorial(n: int, acc: int) -> int {
    if n <= 1 {
        acc
    } else {
        // 编译器自动转换为：@TailCall.invoke2(factorial, n-1, n*acc)
        factorial(n - 1, n * acc)
    }
}

fn main() -> unit {
    let result = with_tco2(factorial, 100000, 1)
    print(f"factorial(100000) = {result}")
}
```

**编译后的 MIR**（自动生成）：

```text
fn factorial(%n, %acc) -> int {
entry:
  %cond = call int_le(%n, 1)
  cond_br %cond then_block else_block

then_block:
  return %acc

else_block:
  %n1 = call int_sub(%n, 1)
  %nacc = call int_mul(%n, %acc)
  %factorial_fn = const factorial
  %result = perform TailCall.invoke2(%factorial_fn, %n1, %nacc)
  return %result
}
```

这样，栈深度永远不会超过 trampoline 的 3-5 帧！
