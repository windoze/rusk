# 提案：逃逸分析（Escape Analysis）以避免短生命周期对象进入 GC Heap

Date: 2026-02-17  
Status: Draft

> 本提案提出在 Rusk 的编译优化阶段增加 **逃逸分析（escape analysis）**，让一部分可证明“不逃逸”的复合对象
>（struct / enum / tuple / array）不必进入 GC heap：要么被 **栈帧内分配**（frame-local arena），要么被 **标量替换**
>（SROA，scalar replacement of aggregates）。
>
> 重点用例：`Option<T>` 作为返回值在调用点被立即 `match`，且之后不再使用——此时 `Option` 仅用于控制流分支，
> 不需要真实分配一个带 identity 的堆对象。
>
> 本文档是**设计提案**，不包含具体实现改动。

---

## 1. 背景 / 动机

在当前的规范与实现里（见 `RUSK_SPEC.md` / `MIR_SPEC.md` / `BYTECODE_SPEC.md`）：

- struct / enum / tuple / array 在语义上是 **heap object with identity**。
- 编译到 MIR/bytecode 后，对应的是 `MakeStruct` / `MakeEnum` / `MakeTuple` / `MakeArray` 指令。
- VM 通过 `crates/rusk-gc` 的 GC heap 承载这些对象，并在一定 allocation 次数后触发 GC。

这带来一个常见的性能问题：

- 许多“只用于控制流/临时打包”的对象非常短命，但仍会进入 GC heap；
- GC 压力、写屏障（未来如果引入）、trace 成本都会被放大；
- 典型场景是 `Option<T>` / `Result<T, E>`：它们在用户代码里极常用于“要么继续要么提前分支”，但多数时候并不需要
  “可共享、可变、可逃逸”的对象语义。

### 1.1 目标示例：返回的 `Option` 立即 `match`

```rusk
fn maybe_inc(x: int) -> Option<int> {
  if x > 0 { Some(x + 1) } else { None }
}

fn use_it(x: int) -> int {
  match maybe_inc(x) {
    Some(v) => v,
    None => 0,
  }
}
```

在当前 lowering 下，`maybe_inc` 通常会构造一个 enum heap object 作为返回值；`use_it` 立刻对它做匹配，然后丢弃。
这属于非常“经典”的可优化模式：

- `Option` 的 identity 在语义上存在，但在该用法中 **不可观测**（没有任何别名存活到 match 之后）。
- 因此：我们希望 **0 次 GC heap 分配** 地完成这段逻辑。

---

## 2. 目标 / 非目标

### 2.1 目标（Goals）

1) **减少 GC heap 分配**：对可证明不逃逸的对象，尽可能避免 `Make*` 进入 GC heap。

2) **覆盖真实高频用例**：
   - `Option<T>` / `Result<T,E>` 的“立即 match”模式（尤其是函数返回值立即 match）。
   - 临时 tuple/struct 打包、立即解构的模式。

3) **保持语义等价**：
   - 不改变语言层可观测行为（尤其是：可变性、别名可见性、trap 的先后顺序、effect/handler 语义）。

4) **可分阶段落地**：先做保守高收益版本，再逐步扩展到更激进的跨函数优化。

### 2.2 非目标（Non-goals）

- 不做完整的全程序/跨模块逃逸分析（第一版以单模块、单函数为主）。
- 不要求一开始就覆盖所有 effect/continuation 场景（第一版可以保守地把“跨 `perform` 的存活”当作逃逸）。
- 不引入 borrow/lifetime 语义（仍然保持 Rusk 的“共享引用 + readonly view”模型）。

---

## 3. 术语与“逃逸”的定义

### 3.1 分配点（Allocation Site）

在 MIR/bytecode 层，以下指令产生新的复合对象分配点：

- `MakeStruct`
- `MakeEnum`
- `MakeTuple`
- `MakeArray`

本提案的“对象”主要指这些分配点产生的 reference-like 值。

### 3.2 逃逸（Escapes）

一个分配点产生的对象在某个作用域外仍可能被访问时，称其“逃逸”。为了让实现可落地，我们采用偏保守的定义：

对象被认为**逃逸**，如果它（或由它派生的别名）满足任一条件：

1) **函数边界逃逸**：作为 `Return` 返回值，或写入到返回值可达的结构里。
2) **进入堆（heap reachability）**：被写入到任意 GC heap object 的字段/元素中，并且该 heap object 在该时刻可在当前
   作用域之外存活（例如：它本身已逃逸，或无法证明它不逃逸）。
3) **被未知调用持有**：作为实参传给无法证明“不保存该引用”的 call target（包括 host call、间接调用、vcall、以及第一版中
   绝大多数 mir call）。
4) **跨越 continuation capture 点存活**（第一版建议保守处理）：
   - 如果某对象在 `perform` 可能捕获的 continuation 中仍然 live，则视为逃逸（除非我们明确支持“栈对象随 continuation 捕获一起搬移”）。

“不逃逸”的意思是：该对象的所有可达引用都只在一个可控的生命周期内使用，且不会进入 GC heap 或超出其宿主栈帧。

---

## 4. 总体设计概览（两条落地路线）

本提案建议将“避免进入 GC heap”的手段分为两类，分别对应不同复杂度与收益：

### 4.1 路线 A：栈帧内对象（Frame-local Arena / Stack Objects）

对“不逃逸但需要保持 identity/可变性”的对象：

- 不再用 GC heap 分配；
- 而是在 VM 的 `Frame` 内维护一个 **frame-local arena**（例如 `Vec<StackObject>` 或更紧凑的 bump arena）；
- 产生一种新的引用值：`StackRef`（仍是引用语义，有 identity，支持 alias 与 mutation），但其存储在栈帧生命周期内。

适用场景：

- 局部构造的 struct/array/tuple，存在字段写入（`SetField`/`IndexSet`）但不逃逸；
- 需要 alias 行为一致（同一对象的多个引用在局部可观察到同一份 mutation）。

### 4.2 路线 B：标量替换（SROA / Unboxed Aggregates）

对“不逃逸且不需要 identity”的对象，直接“让对象不存在”：

- 将对象拆成若干个 local/register（例如：enum = `discriminant + payloads`）；
- 将 `GetField` / `TupleGet` / `Switch` 等操作重写为对这些标量的访问；
- 消除对应的 `Make*` 指令（因此 0 次分配，甚至 0 次 StackObject 记录）。

适用场景：

- `Option<T>` / `Result<T,E>` 纯控制流用法（尤其是“立即 match”）；
- 只读的 tuple/struct 临时打包后立即解构；
- 没有任何字段写入/别名可观测性需求的对象。

**建议优先级**：先落地路线 B（编译器改动为主，不需要 VM 引入新引用类别），再视需求落地路线 A。

---

## 5. MIR 层逃逸分析（EA0，单函数）

### 5.1 输入假设

为了让分析简单可靠，建议在进入 EA 之前满足以下条件之一：

1) MIR lowering 保持“近 SSA”风格：每个 `Make*` 的结果 local 仅被定义一次（当前编译器多数路径已经倾向这样做），或
2) 在 EA 前增加一个轻量的 `rename_locals` pass，把多次赋值拆成多 local（可选）。

### 5.2 分析输出

对每个分配点（`Make*` 的 `dst` local），分类为：

- `Escapes`：必须进入 GC heap（或必须被 box）；
- `NoEscape`：可以进入路线 A/B；
- （可选更细）`NoEscapeButNeedsIdentity` vs `NoEscapeAndPure`，用于选择 A 还是 B。

### 5.3 逃逸判定（保守规则，第一版）

以 `alloc_dst` 为分配点产生的引用 local，若出现任一用法则标记为 `Escapes`：

- 出现在 `Terminator::Return { value: ... }` 的可达数据里（直接 return 或被包装后 return）。
- 作为 `Instruction::SetField/StructSet/TupleSet/IndexSet` 的 `value` 写入到某个已逃逸对象里。
- 作为 `Call/ICall/VCall/Perform` 的参数（第一版全部视为逃逸；后续可以对已知纯 intrinsic 做白名单放宽）。
- 被写入到数组/结构等对象中，而该宿主对象是否逃逸无法被证明为 `NoEscape`。
- 在 CFG 上可能跨越 `Perform` 的捕获边界仍 live（第一版可以直接判逃逸，避免处理 continuation 捕获/搬移）。

实现上可以用一个 worklist 求不动点：

1) 初始把明显逃逸点加入集合（return、传参、写入未知宿主等）。  
2) 如果对象 A 被写入到对象 B 的字段/元素中，且 B 已逃逸，则 A 也逃逸。  
3) 重复传播直到收敛。

> 注意：这里的“写入到对象”包括 `MakeStruct`/`MakeTuple`/`MakeEnum`/`MakeArray` 的 fields/items 里出现引用，
> 以及后续的 `SetField`/`IndexSet` 等 mutation 写入。

---

## 6. 路线 B：标量替换（优先落地）

### 6.1 核心思路

当一个对象满足：

- `NoEscape`，并且
- 在其生命周期内，没有任何需要 identity 的观测（典型：没有将同一引用复制到多处后再发生 mutation 让别名可观测），并且
- 其所有用法是可重写的（例如：仅用于 `Switch` 模式匹配、字段读取、以及构造时字段值的传播）

则把它替换为若干标量 local：

- tuple：`t = (a, b, c)` → 三个 locals：`t.0 = a, t.1 = b, t.2 = c`
- struct：按 layout 拆分字段 local
- enum：`discriminant + fields`（对于不同 variant 字段数不同的 enum，需要保守处理；第一版建议只做 `Option` / `Result`）

### 6.2 `Option<T>` 的重写（关键用例）

把 `Option<T>` 视为：

- `tag: bool`（或 `int`），`false = None`, `true = Some`
- `payload: T`（仅当 `tag=true` 时有效；可以保持为 uninitialized/任意值，但必须保证不会在 `tag=false` 路径读取）

例：`match maybe_inc(x) { Some(v) => A(v), None => B }` 的目标形态：

1) 让 call 直接产出 `tag` 与 `payload`（见 §7 的“返回值立即 match”）。
2) `match` lowering 变成 `JumpIf(tag)`，并在 `then` 分支把 `payload` 绑定到 `v`。

### 6.3 语义约束

- 必须保持 operand 左到右的求值顺序（尤其是：构造 fields/items 的顺序、call 参数的顺序）。
- 必须保持 trap 的先后顺序（例如：索引越界、类型错误等）。
- 对于“payload 在 None 分支不可读”的约束，建议通过：
  - CFG 重写保证不会读，且
  - （可选）在 debug 模式下保持 payload 为 uninitialized，让误读更早 trap。

---

## 7. 关键用例：返回的 `Option` 立即 `match`

仅做单函数 EA 无法消除“在 callee 内构造并 return 的 `Option` 分配”，因为它在 callee 视角下必然逃逸（return）。
因此需要一个 **调用点驱动** 的机制：

- caller 看见“返回值只用于立即 match、之后不再使用”，就可以要求 callee 以“非分配形式”返回。

本提案给出两个可选实现路线，建议优先实现 A。

### 7.1 方案 A（推荐）：为 `Option`/小 enum 引入“拆箱返回（unboxed return）”的内部 ABI

新增内部约定：对部分 enum（第一版仅 `Option<T>`），编译器可以生成一个“拆箱返回”的版本：

- `maybe_inc$unboxed(x, ...) -> (tag: bool, payload: T)`

然后 caller 在“立即 match”场景改为调用 `$unboxed` 版本，并用 `JumpIf(tag)` 实现 match。

落地到 MIR/bytecode 时，有两种实现方式：

1) **多返回寄存器（multi-reg return）**  
   - bytecode 增加 `CallMulti { dsts: [Reg; N], func, args }` 与 `ReturnMulti { srcs: [Reg; N] }`  
   - 对 `Option<T>`：`N = 2`（tag + payload）

2) **隐式 out 参数（sret / return slot）**（更接近传统 ABI，但需要“可写 location”概念）  
   - caller 分配一个“return slot”，传给 callee；callee 直接写入该 slot 的 tag/payload。
   - 这需要引入 `StackRef` 或“地址/slot”概念，工程量更大，但可扩展到 struct/tuple/array。

第一版建议走 1)，因为它只影响 VM 的 call/return 机制，不需要引入新的引用类别。

### 7.2 方案 B：在 caller 做“call + switch fusion”，并依赖 inlining

识别 `v = Call foo(...); Switch v { ... }` 模式后，尝试对 `foo` 做 inlining：

- inlining 后 `Option` 的构造点回到 caller；
- 再由 §6 的 SROA 消除分配。

优点：

- 不需要修改 VM 指令集/调用约定。

缺点：

- 必须实现较可靠的 inliner（以及 code size 控制）；
- 对非小函数不稳定；
- 会影响调试与错误定位，且与后续优化框架耦合更重。

因此不推荐作为第一版落地路径。

---

## 8. 放置位置（Pipeline Integration）

建议把该优化作为编译器的一个阶段，受 `CompileOptions.opt_level` 控制（例如：`O2` 开启）：

1) Rusk source → MIR（现有）  
2) **MIR Escape Analysis + SROA（新增）**  
3) MIR → bytecode lowering（现有）  
4) bytecode peephole/copy-prop/DCE（现有 `rusk_bytecode::OptLevel`）  

对“返回值立即 match”的优化（§7），建议在 MIR 层实现：

- 因为 MIR 仍然保留较清晰的结构信息（switch、call target、block args），更容易识别并重写；
- bytecode 层虽然也能做，但需要更复杂的 CFG 还原/重写与 PC relocation。

---

## 9. 验证 / 测试策略

### 9.1 语义回归

- `cargo test --all` 覆盖现有单测/集成测试。
- 增加一组新的集成测试：构造多种 `Option`/tuple/struct 的短生命周期场景，确保行为一致（尤其是：
  - mutation 可见性
  - `readonly` trap 行为
  - effect handler 语义未变）

### 9.2 “确实减少 GC 分配”的验证

建议增加两类测试信号（可二选一或都做）：

1) **编译产物断言**：对指定输入程序编译后，检查 bytecode 中不再出现某些 `MakeEnum`（或出现次数显著减少）。
2) **运行期指标**：为 VM 增加一个可选的 metrics 计数器（例如 `heap_alloc_instructions`），并在测试里断言其为 0/更小。

---

## 10. 风险与注意事项

1) **effect/continuation 交互**：如果对象跨 `perform` 存活，且 continuation 捕获/恢复需要携带它，
   需要非常小心对象的存储位置与生命周期。第一版建议保守：跨 `perform` 的对象直接判逃逸，不做该优化。

2) **语义可观测性**：
   - 若未来引入“引用相等/地址可观测”的功能（类似 `ptr_eq`），会显著收窄可优化的范围。
   - 目前 Rusk 的主要可观测性来自 mutation + alias；只要我们不破坏 alias 行为，SROA 需要更谨慎地判定“是否需要 identity”。

3) **调试体验**：
   - 优化可能改变 bytecode 结构，影响 debug dump 的可读性。
   - 建议提供 `--opt-level=O0` 或更细的开关以便定位问题。

---

## 11. 开放问题（Open Questions）

- 第一版 SROA 是否只做 `Option<T>`（以及可能的 `Result<T,E>`），还是也做固定 arity 的 tuple？
  - 如果可能建议覆盖 tuple，因为它也是非常常见的短生命周期打包用法；但需要评估实现复杂度与潜在风险。
  - 在保证“不影响以后引入更细粒度分析”原则的情况下，同时建议也覆盖其他“小型 enum”，可以自行设定一个合理的限制。
- “需要 identity”的判定边界如何设计才既安全又不太保守？
  - 是否以“出现任何字段写入/可能别名”为准？
  - 还是引入更精细的 alias analysis（成本较高）？
  - 最终方案以“不影响以后引入更细粒度分析”为原则，如果实现复杂度过高，可以先做一个相对简单的保守版本。
- 方案 A（multi-reg return）是否会引入新的 bytecode 格式版本与序列化兼容问题？
  - 兼容性在这个阶段不是问题，可以引入任何形式的breaking change，只需保证不兼容版本使用不同的版本号即可。
- 是否要为 `CheckedCast` 的 `Option` 产出单独做一个更轻量的 fast-path（它是非常纯的 `Option` 构造点）？
  - 如果实现复杂度不高，建议覆盖这个场景，因为它是非常典型的“构造后立即 match”用例。
