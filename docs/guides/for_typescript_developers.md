# 面向 TypeScript 开发者的 Rusk 指南

本指南从 TypeScript 开发者的视角，介绍 Rusk 的“相似点、关键差异、以及常见写法/模式”。

如果你只想先跑起来：先看 `docs/guides/quick_start.md`。

---

## 1. 你会觉得熟悉的地方

### 1.1 `let` / `const`

Rusk 也有 `let` 与 `const`：

- `let`：可以重新赋值
- `const`：不可重新赋值

此外还有一个 TS 里没有（或语义不同）的：`readonly`（见下文）。

### 1.2 字符串插值

TypeScript：

```ts
const msg = `hello ${name}, n=${n}`;
```

Rusk：

```rusk
let msg = f"hello {name}, n={n}";
```

Rusk 的 `f"..."` 通过 `core::fmt::ToString` 进行转换；你可以为自定义类型实现 `ToString` 来控制输出格式。

### 1.3 `interface` 这个词

Rusk 也叫 `interface`，但语义更接近 **Rust trait**：

- 需要显式 `impl` 才算实现
- 可以有默认方法
- 可以用于泛型约束（静态分发）或转成“接口值”做动态分发

它不是 TypeScript 的结构类型系统接口（不会因为“形状长得像”就自动兼容）。

---

## 2. 关键差异一览（建议先读）

### 2.1 名义类型（nominal）而非结构类型（structural）

TypeScript 的类型兼容性主要按“结构”来；Rusk 的 `struct`/`enum` 是名义类型：

- 字段一样也不代表同一个类型
- “可选字段/鸭子类型”的习惯需要换成 `enum`/`Option`/`interface` + `impl`

### 2.2 没有 `null`/`undefined` 作为默认空值

TS 常用 `T | undefined` / `T | null` 表达可选值。

Rusk 推荐用 `Option<T>`：

- `Option::Some(v)`：有值
- `Option::None`：无值

### 2.3 `readonly` 不是 TS 的只读属性

TypeScript 的 `readonly` 多用于“不可重新赋值字段/属性”。

Rusk 的 `readonly` 更像“只读视图”：

- 不仅不能重新赋值绑定
- 还会阻止你通过该视图做可变操作（例如数组写入、调用可变方法）

> 直觉类比：更接近 Rust 的 `&T` / `&[T]`，而不是“深冻结对象”。

### 2.4 ADT（enum + match）比“对象 + if/else”更常见

在 TS 中你可能会用 discriminated union（判别联合）：

```ts
type Expr =
  | { kind: "lit"; value: number }
  | { kind: "add"; left: Expr; right: Expr };
```

在 Rusk 中通常用 `enum`：

```rusk
enum Expr {
    Lit(int),
    Add(Expr, Expr),
}

fn eval(e: Expr) -> int {
    match e {
        Expr::Lit(n) => n
        Expr::Add(a, b) => eval(a) + eval(b)
    }
}
```

### 2.5 异常不是默认控制流；`Result`/`Option` 更常用

你可以用 `core::result` 的 `try/throw/catch/finally`（底层基于 effects），但在“可预期失败”的路径上，`Option/Result` 往往更清晰。

---

## 3. 常见结构与模式（从 TS 到 Rusk）

### 3.1 `T | undefined` → `Option<T>`

TypeScript：

```ts
function findUser(id: string): User | undefined {
  return db.get(id);
}

const name = findUser(id)?.name ?? "guest";
```

Rusk（典型写法）：

```rusk
fn find_user(id: string) -> Option<User> { /* ... */ }

let name = find_user(id)
    .map(|u: User| { u.name })
    .unwrap_or("guest");
```

这种“链式组合子”在 Rusk 里很常见（见 `examples/18-option-methods.rusk`）。

### 3.2 判别联合（discriminated union）→ `enum + match`

TypeScript：

```ts
switch (node.kind) {
  case "ok": return node.value;
  case "err": return 0;
}
```

Rusk：

```rusk
match node {
    Ok(v) => v
    Err(_) => 0
}
```

### 3.3 class / prototype → `struct + impl`（再配合 interface）

Rusk 的“方法”不是挂在原型链上，而是通过 `impl` 为名义类型添加：

```rusk
struct Counter { n: int }

impl Counter {
    static fn new(n: int) -> Counter { Counter { n: n } }
    fn inc() -> int { self.n = self.n + 1; self.n }
}
```

如果你需要多态，通常写 `interface`：

```rusk
interface Shape { fn area() -> int; }
impl Shape for Rect { fn area() -> int { ... } }
```

### 3.4 “对象形状”复用 → 用接口 + 显式实现

在 TS 里你经常依赖结构兼容性复用工具函数；在 Rusk 里要么：

- 用泛型约束：`fn f<T: I>(x: T) -> ...`
- 要么把值转成接口值：`x as I`（动态分发）

### 3.5 Map/Record → `core::map::Map`

TypeScript：

```ts
const m = new Map<string, number>();
m.set("a", 1);
```

Rusk：

```rusk
use core::map::Map;

let m: Map<string, int> = Map::new();
m.insert("a", 1);
```

需要注意：`Map<K, V>` 的 `K` 需要实现 `Hash + Eq`。自定义 key 的写法见 `examples/16-core-map.rusk`。

---

## 4. effects：把“副作用能力”当成可组合的依赖

TypeScript 里你可能通过：

- 传入回调/依赖对象（DI）
- 使用 Promise/async
- try/catch

Rusk 有一套更“语言级”的工具：**代数效应**。

一个直觉类比：

- `@Console.log("...")` 像是在“请求一个能力”
- `match` 的 handler 像在“注入该能力的实现”

最小例子见 `examples/20-effects-basic.rusk`；更大的组合例子见 `examples/23-effects-state-management.rusk`（用 effects 表达状态、订阅与 cleanup）。

---

## 5. 推荐的学习路线（TS → Rusk）

1. 先把 `Option/Result` 写顺：`match` 与组合子（`map/and_then/unwrap_or`）。
2. 用 `enum + match` 建模业务状态（替代“对象 + kind 字段”）。
3. 用 `struct + impl` 写可维护的数据与方法，再用 `interface` 做抽象边界。
4. 需要平台能力时，优先考虑：
   - “纯函数 + 显式参数”够不够
   - 不够时再用 effects 或宿主函数/外部化 effects（见 `docs/guides/host_integration.md`）

---

## 6. 进一步阅读

- 语法/概念总览：`docs/guides/concepts_and_syntax.md`
- 示例目录（由易到难）：`examples/`
- 宿主集成：`docs/guides/host_integration.md`
