# Rusk 概念与语法（Concepts & Syntax）

本指南提供一个“从上往下”的语言概览：先给心智模型，再按功能块介绍语法与核心概念。更权威的细节以 `RUSK_SPEC.md` 为准。

---

## 0. 心智模型：Rusk 是什么样的语言？

你可以把 Rusk 理解为：

- **表达式为中心**：`if` / `match` 等控制流会产生值。
- **静态类型 + 名义类型（nominal）**：`struct`/`enum` 是名义类型；不会像 TypeScript 那样默认走结构类型系统。
- **接口（interface）像 Rust trait**：通过 `impl` 实现；可以做静态分发（泛型约束）或显式转成“接口值”走动态分发。
- **effects 是一等公民**：`@I.m(...)` 是 effect 调用；`match` 可以安装处理器并用 `resume(...)` 恢复 continuation。
- **可嵌入**：宿主可以通过“宿主函数（host imports）”与“外部化 effects（StepResult::Request）”提供平台能力。

---

## 1. 代码组织：文件、模块与 `use`

Rusk 使用文件/目录模块（类似 Rust 的 `mod`）：

```rusk
mod math;   // 加载 math.rusk 或 math/mod.rusk
mod util;   // 加载 util/mod.rusk

use math::fib as fibonacci;
use util::strings::greet;
```

要点：

- `mod name;` 负责把模块“编译进来”。
- `use path::to::item` 负责把名字引入当前作用域，可用 `as` 起别名。
- `crate::...` 表示从当前 crate 根开始的路径。

示例可参考 `examples/11-modules/`。

---

## 2. 绑定与可变性：`let` / `const` / `readonly`

### `let`：可重新赋值的绑定

```rusk
let x = 1;
x = x + 1;
```

### `const`：不可重新赋值

```rusk
const n = 42;
// n = 0; // 类型错误
```

### `readonly`：只读视图（禁止通过该视图进行可变操作）

`readonly` 既禁止重新赋值，也会把值“视图化”为只读：

```rusk
readonly xs = [1, 2, 3];
// xs[0] = 9; // 类型错误：不能通过 readonly 视图写入
```

> 直觉：`readonly` 更像 Rust 的 `&T` / `&[T]` 视图，而不是 JS 的“深冻结（deep freeze）”。

---

## 3. 基本类型与复合类型

### 3.1 基本类型（常用）

- `unit`：只有一个值 `()`，常用于“没有有意义的返回值”。
- `bool`
- `int`：64-bit 有符号整数（语义上当作语言的“默认整数”）。
- `float`：`f64`
- `string`：UTF-8 字符串（不可索引；`slice` 按 codepoint 下标切片；`byte_slice` 按字节偏移切片并返回 `bytes`；可按字符迭代）。
- `bytes`：字节序列（可索引；可切片；可与 `[byte]` 转换）。
- `byte`：无符号 8-bit
- `char`：Unicode scalar value

关于 `string` 切片要点（非常重要）：

- `s.slice(from, to)` 的 `from/to` 是 **Unicode scalar 下标**（codepoints），边界计算是 O(n)
- `s.byte_slice(from, to)` 的 `from/to` 是 **UTF-8 字节偏移**，返回 `bytes`（因此可以索引）

一个最小对比示例：

```rusk
fn demo() -> unit {
    let s = "é"; // 1 个 codepoint，但 UTF-8 是 2 个字节

    // 按字符切片（codepoint index）
    let s1 = s.slice(0, Option::Some(1)); // "é"

    // 按字节切片（byte offset），得到 bytes
    let b0 = s.byte_slice(0, Option::Some(1));
    let first = b0[0].to_int();

    std::println(f"s1={s1}, first_byte={first}");
    ()
}
```

### 3.2 数组 `[T]` 与只读数组 `readonly [T]`

数组是堆上动态数组（更像 Rust 的 `Vec<T>`，不是固定长度数组）：

```rusk
let xs = [1, 2, 3];
xs.push(4);
match xs.pop() {
    Option::Some(v) => { /* ... */ }
    Option::None => { /* 空数组 */ }
}
```

数组支持内建方法（语法糖），底层会降低到 `core::intrinsics::array_*`。

### 3.3 元组与 newtype

元组：`(A, B, ...)`，可用 `.0/.1/...` 访问。

newtype：`struct UserId(int);`，也是名义类型，可用 `.0` 访问单字段。

### 3.4 结构体 `struct`

```rusk
struct Point { x: int, y: int }
```

结构体字段固定；不能像动态语言那样运行时加字段。

### 3.5 枚举 `enum`

```rusk
enum Color {
    Rgb(int, int, int),
    Named(string),
}
```

枚举是“带 tag 的联合类型”（代数数据类型的一种常见形式）。

---

## 4. 函数、闭包与函数类型

函数定义：

```rusk
fn add(a: int, b: int) -> int { a + b }
```

函数类型（作为值）：

- `fn(int) -> int` 表示“可调用值”的类型之一
- 命名函数与不捕获闭包都可以作为 `fn(...) -> ...` 传递

闭包示例：

```rusk
fn apply(f: fn(int) -> int, x: int) -> int { f(x) }

fn main() -> int {
    let add1 = |n| { n + 1 };
    apply(add1, 41)
}
```

---

## 5. 控制流：表达式化的 `if` / `match` / 循环

### 5.1 `if` 是表达式

```rusk
let abs = if n < 0 { 0 - n } else { n };
```

### 5.2 `match` 是表达式（模式匹配）

```rusk
let label = match n {
    0 => "zero"
    _ => "other"
};
```

### 5.3 循环

- `loop { ... }`：无限循环，直到 `break;`
- `while cond { ... }`
- `for x in iterable { ... }`

`break` / `continue` 控制最内层循环。

`for` 的语义：

- 如果被迭代的类型实现了 `core::iter::Iterator`，就直接调用 `next()`
- 否则对于内建容器（数组 / bytes / string）使用编译器内建迭代逻辑

---

## 6. 模式与解构（Destructuring）

模式可以出现在 `let/const/readonly` 绑定里：

```rusk
let (a, b) = (1, 2);
let [head, ..rest] = [1, 2, 3, 4];
let Point { x, y } = Point { x: 1, y: 2 };
```

也可以出现在 `match` 分支里，用于拆解结构体/枚举/newtype/数组等。

---

## 7. 方法：固有方法（inherent）与接口方法（trait-like）

### 7.1 固有方法：`impl Type { ... }`

```rusk
struct Counter { n: int }

impl Counter {
    static fn new(n: int) -> Counter { Counter { n: n } }
    fn inc() -> int { self.n = self.n + 1; self.n }
}
```

要点：

- `static fn`：静态方法，用 `Type::m(...)` 调用
- `fn`：实例方法，有隐式接收者 `self`
- `readonly fn`：接收者视图为只读；可在 `readonly T` 上调用

### 7.2 接口：`interface` + `impl`

```rusk
interface Shape { fn area() -> int; }

struct Rect { w: int, h: int }
impl Shape for Rect { fn area() -> int { self.w * self.h } }
```

动态分发需要显式把具体值转成“接口值”：

```rusk
let s: Shape = Rect { w: 3, h: 4 } as Shape;
s.area()
```

---

## 8. 泛型、约束与关联类型

### 8.1 泛型结构体/函数

```rusk
struct Pair<A, B> { first: A, second: B }
fn swap<A, B>(p: Pair<A, B>) -> Pair<B, A> { ... }
```

### 8.2 泛型约束（bounds）

```rusk
fn f<T: core::hash::Hash + core::ops::Eq>(x: T) -> int { x.hash() }
```

### 8.3 关联类型（associated types）

典型例子是 `core::iter::Iterator`：

```rusk
interface Iterator {
    type Item;
    fn next() -> Option<Self::Item>;
}
```

关联类型让实现者决定 `Item` 是什么，而不必把接口本身写成 `Iterator<T>`。

---

## 9. 字符串与格式化：`f"..."` 与 `core::fmt::ToString`

`f"..."` 会把 `{expr}` 转成 `string` 再拼接，转换规则基于 `core::fmt::ToString`：

```rusk
use core::fmt::ToString;

struct Point { x: int, y: int }
impl ToString for Point {
    readonly fn to_string() -> string { f"({self.x}, {self.y})" }
}

fn main() {
    let p = Point { x: 1, y: 2 };
    std::println(f"p = {p}");
    ()
}
```

---

## 10. 核心库（core）常用组件：Option / Result / Map

### 10.1 `Option<T>`

表达“可能缺失”的值：`Some(v)` / `None`。常用方法：

- `map`
- `and_then`
- `unwrap_or` / `unwrap_or_else`

### 10.2 `Result<T, E>` 与 `try/throw/catch/finally`

本仓库的 `core::result` 提供了一套基于 effects 的错误处理抽象：

- `throw(e)`：抛出一个类型化错误（底层是 `Throw<E>` effect）
- `try { ... }.catch(|e| { ... }) -> Result<T, E>`
- `Result::finally { ... }`：收尾逻辑

示例见 `examples/24-result-try-throw.rusk`。

### 10.3 `core::map::Map<K, V>`

`Map` 是纯 Rusk 实现的 HashMap 风格容器，`K` 需要实现 `Hash + Eq`。示例见 `examples/16-core-map.rusk`。

---

## 11. effects：`@I.m(...)`、处理器与 `resume`

### 11.1 调用 effect

```rusk
interface Logger { fn log(msg: string); }
@Logger.log("hello")
```

### 11.2 用 `match` 安装处理器

```rusk
match compute() {
    @Logger.log(msg) => { std::println(msg); resume(()) }
    v => v
}
```

要点：

- 处理器的作用域是 `match` scrutinee 的求值过程（这是一个非常关键的语义点）。
- `resume(v)` 会恢复 continuation，并把 `v` 作为 effect 调用表达式的返回值。

更深入解释见：

- `docs/delimited-continuations-and-scoped-effects.zh.md`

---

## 12. 与宿主边界：宿主函数 vs 外部化 effect

嵌入时你通常会用两种机制提供平台能力：

1. **宿主函数（host imports）**：像 `std::println("...")` 这样直接调用宿主导入函数。
2. **外部化 effects**：让未处理的 `@I.m(...)` 不在 VM 内陷阱，而是“冒泡到宿主”，由宿主决定如何恢复（`StepResult::Request`）。

两者都受一个强约束：跨 VM/宿主边界的值必须是 ABI 安全的基本类型集合（`unit/bool/int/float/string/bytes`）。

具体做法见 `docs/guides/host_integration.md` 与更完整的 `docs/embedding-vm.zh.md`。
