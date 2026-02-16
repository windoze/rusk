# Trait / Interface 分离提案（Draft）
Date: 2026-02-15

本提案引入一个新的顶层条目 `trait`，并将现有 `interface` 的语义收窄为**运行时类型 + 动态分发 + effect 命名空间**。
核心目标是建立一个清晰、可从源码直接识别的边界：

- `trait`：**静态语义**（不是类型、不能 `as`、不产生接口值、无运行时按类型的动态分发）。
- `interface`：**动态语义**（是类型、可 `as` 产生接口值、可动态分发、用于 effects）。
- `Self`：只允许出现在 `trait` 中（以及 `impl Trait for Type` 的方法签名/方法体中）；`interface` 中禁止出现 `Self`。

> 直觉：看到 `trait` 就知道“只能静态”；看到 `interface`（以及 `as I`、`@I.m(...)`）就知道“动态”。

---

## 背景 / 动机

当前语言把“像 Rust trait 一样的能力约束（如 `Hash`/`Show`）”与“像接口对象一样的动态分发（`as I` + vcall）”混在同一个 `interface` 机制里。
这会带来几类问题：

1) **动态/静态边界不清晰**
   - `T: I` 形式的约束目前会导致方法调用走动态分发（`vcall`），但从源码上不容易一眼看出这里发生了“运行时按动态类型选 impl”。

2) **`Self` 相关语义很难和动态分发兼容**
   - `Clone { fn clone() -> Self }` 这类定义对“接口值（interface-typed value）”天然不友好：当 `x: Clone` 时，`Self` 在静态上不可命名。
   - 用 object-safety 规则可以约束哪些方法能在接口值上被调用，但这种规则对读代码的人不直观；并且会牵连默认方法（default methods）的可用性与编译策略。

3) **默认方法（default method）现状与 `Self` 的期望相冲突**
   - 当前实现里，interface 的默认方法体会被编译为一个接收者类型为 `Interface` 的内部函数（语义上相当于“在接口值上运行的代码”）。
   - 如果我们希望 `Self` 表示实现者类型，那么默认方法体里的 `self` 就不应当是 `Interface`；这会直接与现有 interface 动态语义冲突。

本提案通过把 `trait` 与 `interface` 分离，让 `Self` 仅出现在静态域（trait）中，从根本上移除 object-safety 的复杂度。

---

## 目标（Goals）

1) **语义边界显式**：仅凭源码能区分“静态调用”与“动态调用”。
2) `trait` 支持 `Self`，并可用于表达 `Eq`/`Clone` 这类“值语义能力”。
3) `interface` 继续作为运行时类型用于动态分发与 effects，但其方法签名被限制为天然 object-safe（禁止 `Self`）。
4) 不引入隐式子类型；接口值依然通过显式 `as I` 产生。

---

## 非目标（Non-goals）

- 不实现 Rust 风格的 full specialization / overlap resolution。
- 不引入 associated types / GAT / HKT reification（可在未来单独提案）。
- 不在本提案中重做 effect 系统；effects 继续使用 `interface` 作为命名空间。

---

## 设计概览

### 一句话总结

- `trait`：约束 + 静态分发；**不是类型**；允许 `Self`；不能用于 `as`/`is`/`as?`。
- `interface`：运行时接口对象 + 动态分发 + effects；**是类型**；禁止 `Self`；可以 `as Interface`。

---

## 语法（Syntax）

### 1) 新增条目：`trait`

新增关键字：`trait`（与 `struct`/`enum`/`interface` 同级）。

示例：

```rusk
trait Clone {
    readonly fn clone() -> Self;
}

trait Eq {
    readonly fn eq(other: readonly Self) -> bool;
    readonly fn ne(other: readonly Self) -> bool { !self.eq(other) }
}
```

约束与约定：

- `trait` 成员方法与现有 method 模型一致：有**隐式接收者** `self`，不允许显式写 `self:` 参数。
- 保留 `readonly fn`：表示在方法体内 `self` 视为 readonly view（并允许在 `readonly T` 上调用）。
- `static fn`：本提案**暂不引入** trait 的 static/associated functions（避免“没有接收者时如何选择实现者类型”的额外规则）。

### 2) 新增类型关键字：`Self`（仅在 trait 域可用）

`Self` 是一个仅在以下位置可用的类型名：

- `trait` 的方法签名中
- `trait` 的默认方法体中（作为类型名使用）
- `impl Trait for Type { ... }` 的方法签名与方法体中

在其它上下文（普通函数、`interface`、`struct`/`enum` 定义等）出现 `Self` 视为错误。

> 这使得 `Self` 不再“难以检测”：只要你看见 `Self`，你就一定处在 `trait` 的静态语义域里。

### 3) `impl` 头部

新增：

```rusk
impl Clone for S { ... }
impl<T> Iterable<T> for Vec<T> { ... }
```

保留既有：

```rusk
impl I for S { ... }          // interface impl（动态域）
impl<T> I<T> for S<T> { ... } // interface 泛型 impl（仍受“无 specialization”限制）
```

---

## 类型与分发语义（Static vs Dynamic）

### 1) `trait`：静态语义（不产生接口值）

关键点：

- `trait` **不是类型**：不能写 `let x: Clone = ...`，不能写 `x as Clone`，不能作为 `is`/`as?` 目标。
- `trait` 的方法调用永远不依赖“运行时对象的动态类型”去查表；换言之，不使用现有 `vcall` 机制。
- `Self` 在 `trait` 中精确定义为：**当前 impl 的实现者类型**（即 `impl Trait for Type` 中的 `Type`，包含其类型参数实例化）。

### 2) `interface`：动态语义（可产生 interface-typed value）

关键点：

- `interface` 仍是类型，可用于参数、字段、局部变量等。
- `expr as I` 产生 interface 值，后续方法调用可动态分发（现有语义保持）。
- effects 继续使用 `@I.m(...)`，其中 `I` 必须是 `interface`。
- `interface` 方法签名**禁止出现 `Self`**，从而保证 interface 方法天然 object-safe。

---

## Bounds（泛型约束）

为了保持“动态/静态边界清晰”，本提案建议把“约束”语义归入 `trait`：

- `T: Trait1 + Trait2<...>`：静态约束（trait bound）。
- 不再允许 `T: Interface` 这种把 interface 当作 bound 的写法（或将其标记为 deprecated 并在实现阶段删除）。

动态需求通过显式类型来表达：

- 直接把参数写成 interface 类型：`fn f(x: I) -> ...`
- 或在调用点显式 `as I`：`let i = s as I; i.m(...)`

> 这样一来，只要你看到 `T: ...` 就知道是 trait（静态）；看到 `: I` 或 `as I` 才是动态。

---

## 方法调用与解析规则（Method Resolution）

### 1) 显式限定调用（始终推荐、语义最清晰）

- Trait 方法：`Trait::m(recv, args...)`
- Interface 方法：`Interface::m(recv, args...)`
- Effect 调用：`@Interface.m(args...)`

### 2) 方法糖（`recv.m(args...)`）的建议规则

为了保证“看到代码就能判断动态/静态”，建议调整糖的适用范围：

1. **Inherent methods**（`impl Type { ... }`）优先。
2. 若接收者的静态类型是具体名义类型或 trait-bound 的泛型，解析到 **trait methods**：
   - 若唯一候选：OK。
   - 若多个候选：报歧义，要求写 `Trait::m(recv, ...)`。
3. 仅当接收者静态类型本身就是 `interface` 类型时，允许解析到 **interface methods**（动态）：
   - 这使得 `.m(...)` 出现在 interface 值上时“动态”是显式可见的（因为变量类型就是 interface）。
4. 其它情况不对 interface 做“隐式补全”，从而避免 `T: I` 这类隐式动态语义。

---

## 默认方法（Default Methods）

### 1) `interface` 默认方法

保留现有语义：默认方法体在“interface 值”语境下运行，因此：

- `self` 的静态类型视为该 `interface` 类型（可带接口自身的类型参数）。
- 允许动态分发调用同一 interface 的其它方法（包括被实现者覆盖的方法）。
- 禁止使用 `Self`（因为 interface 不允许 `Self`）。

### 2) `trait` 默认方法

trait 的默认方法体属于静态语义，允许使用 `Self`：

- `self` 的静态类型为 `Self`（或 `readonly Self`，取决于 `readonly fn`）。
- 默认方法体内调用其它 trait 方法应当遵循“按当前 impl 选择实现”的规则（即尊重覆盖）。

这要求编译期为 trait 默认方法提供“可调用当前 impl 方法”的机制；见下节“实现草案”。

---

## 编译与运行时实现草案（Implementation Sketch）

### 1) interface（动态域）

保持现状：

- interface 值通过 `as I` 构造。
- 方法调用在需要时 lowering 为 `vcall`（运行时按动态类型查 `(type, origin_iface, method)` 表）。

### 2) trait（静态域）— 推荐用“字典传递（dictionary passing）”

为了在“泛型 + trait bound”场景仍保持静态语义，我们需要一种不依赖运行时动态类型查表的调用方式。
推荐方案是类似 typeclass 的 **字典传递**：

- 对每个 `trait T`，编译器内部生成一个“字典布局”（不暴露给用户），包含该 trait 的方法实现的函数引用（以及需要的 reified type args 入口）。
- 对每个 `impl T for S`，编译器生成一个该字典的值（概念上是常量/静态表）。
- 对每个带 trait bound 的泛型函数 `fn f<A: T>(x: A)`, 编译器在函数 ABI 中加入隐藏参数 `dict_T_for_A`。
- 当 `f` 内部调用 `x.m(...)`（解析为 trait 方法）时，lowering 为：
  1) 从 `dict_T_for_A` 中取出 `m` 的函数引用（类型为 `fn(A, ...) -> ...` 的函数值）
  2) 使用 MIR 的 `ICall` 进行间接调用

这种方式的关键性质：

- 调用的实现由**调用点**在编译期确定并传入（静态选择），而不是被调用方在运行时按对象类型查表（动态选择）。
- 语义上属于静态分发；实现上可能是 `ICall`（可在后续优化中内联/去虚拟化）。

### 3) trait 默认方法如何“尊重覆盖”

默认方法体内部如果调用同 trait 的其它方法，必须调用“当前 impl 的版本”（可能被覆盖）。
在字典传递模型下，默认方法体也接收一个字典参数 `dict_T_for_Self`，因此：

- 默认方法体内对其它方法的调用也通过字典间接调用
- 当某个 impl 没有提供该方法实现时，字典条目指向默认实现；提供了实现则指向 override

这样可以保证默认方法在所有实现者上行为一致且尊重覆盖。

---

## 一致性与冲突处理（Coherence / Name Conflicts）

### 1) 名称空间

建议：`trait` 与 `struct`/`enum`/`interface` 共享同一“名义类型名”命名空间。
因此同一模块路径下不允许同时存在 `trait Clone` 与 `interface Clone`（避免歧义）。

### 2) impl 冲突

本提案不引入 full specialization；因此：

- 对同一对 `(Trait, Type)`，最多允许一个 `impl`（与当前 interface impl 类似）。
- trait 若支持自身泛型参数，则初期可沿用当前“无 specialization”限制：impl 头部的 trait 类型参数必须是 impl 自己的类型参数按序填充（避免通过具体参数选择 impl）。
  - 例：允许 `impl<T> Trait<T> for S<T>`；拒绝 `impl Trait<int> for S`。

这条限制是否过严，是一个可讨论点（见“开放问题”）。

---

## 迁移建议（Migration）

1) 把“作为能力约束使用、无需动态接口值”的 `interface` 改为 `trait`
   - 典型：`Hash`、`Show`、`Eq`、`Clone` 等。
2) 保留 `interface` 给：
   - 需要 `as I` 产生接口值并动态分发的场景
   - effects 的命名空间（`@I.m(...)`）
3) 将原先依赖 `T: I` 进行动态分发的代码改为显式 interface 参数：
   - `fn f(x: I) { x.m() }` 或 `let i = x as I; i.m()`

---

## 开放问题（Open Questions）

1) trait 泛型 impl 的限制是否需要放宽？
   - 放宽会触及 overlap / 选择规则（可能走向 specialization/coherence 更复杂的一侧）。
2) trait 是否需要 `static fn` / associated functions？
   - 例如 `Default::default()` 这类模式是否要支持，以及如何选择实现者类型。
3) interface 默认方法是否要保留“在 interface 值语境下运行”的语义？
   - 若未来希望 interface 也支持某种 `Self`，则必须重新引入 object-safety 或更复杂的“dyn/impl”区分；本提案的立场是避免。

