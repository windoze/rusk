# 提案：字节码 Peephole 优化（编译期）

Date: 2026-02-14  
Status: Draft

> 本提案讨论在 Rusk 的 **bytecode lowering 之后**（得到 `rusk_bytecode::ExecutableModule` 之后）增加一个
> **peephole optimization**（窥孔优化）阶段，用于减少 VM 执行的指令数、降低不必要的 `Copy`/临时寄存器开销，
> 并为后续更系统的优化（CFG 简化、liveness、DCE）打基础。

---

## 1. 背景

当前编译/执行路径（简化）：

1) Rusk 源码 → 前端解析/类型检查  
2) 前端产物 → MIR  
3) MIR → `crates/rusk-bytecode` lowering → `ExecutableModule`（每个函数 `Function.code: Vec<Instruction>`）  
4) `crates/rusk-vm` 逐条解释执行 `Instruction`

bytecode 设计有一个关键事实：

- `Jump/JumpIf/Switch` 以及 `PushHandler` 内的 `HandlerClause.target_pc` 都使用 **绝对 PC（u32 指向 code 下标）**。
- 这意味着：只要优化会 **插入/删除** 指令，就必须做 **PC 重映射（relocation）** 并修正所有跳转目标。

与此同时，bytecode lowering 本身会生成一些“机械性”的指令序列：

- 分支/跳转的边（edge）为了实现 block 参数传递，会生成一段 `Copy...; Jump target` 的序列；
  在“无参数变化”的情况下，这段 edge 可能退化成纯 `Jump`（trampoline）。
- 字面量（literal）会被 lowering 成 `Const tmp, ...` 再参与计算，常见模式是临时寄存器仅使用一次。

这些模式非常适合用 peephole 在 lowering 之后做清理/折叠。

---

## 2. 目标 / 非目标

### 2.1 目标（Goals）

- **降低运行时开销**：减少解释器 dispatch 次数；减少 `Copy` 导致的 `clone()`（尤其是 `String/Bytes`）。
- **保持实现简单可控**：以“局部规则 + 小规模 CFG 信息”为主，避免一次引入完整优化框架。
- **可渐进扩展**：先做保守规则（O1），后续可加更激进的优化（需要 liveness/数据流）。
- **可开关、可对比**：支持 opt-level（至少 O0/O1），便于调试与性能对比。

### 2.2 非目标（Non-goals）

- 不做全局寄存器分配/重命名（不试图减少 `Function.reg_count`）。
- 不引入 JIT 或复杂的全程序优化。
- 不改变 `.rbc` 的序列化格式（bytecode v0 不新增必须字段）；如需新增 `Nop` 等指令，单独提案。
- 不保证对“外部手写/恶意 `.rbc`”的 trap 文本/行为完全一致（详见 §6 语义约束）。

---

## 3. 放置位置（Pipeline Integration）

建议把优化实现为 **bytecode 层的独立 pass**：

- 输入：`&mut rusk_bytecode::ExecutableModule`
- 输出：在原地修改 `module.functions[*].code`（以及必要的跳转 PC）

调用位置（两种可选）：

1) **编译器侧调用（推荐作为第一步）**  
   `crates/rusk-compiler` 在 `lower_mir_module_with_options(...)` 返回后，根据 `CompileOptions` 决定是否执行优化。

2) **bytecode crate 内部调用**  
   `crates/rusk-bytecode` 在 lowering 完成后（patch PC 之后）直接对每个 `Function.code` 运行优化。

两者差异主要在“职责边界”，对实现影响不大。本提案更偏向 1)：让编译器显式控制优化级别，bytecode crate 保持更纯粹的 lowering 行为。

---

## 4. 关键挑战：绝对 PC 的重映射

### 4.1 为什么必须做 relocation

bytecode 指令包含多个“跳转到 PC”的字段：

- `Instruction::Jump { target_pc }`
- `Instruction::JumpIf { then_pc, else_pc }`
- `Instruction::Switch { cases[*].target_pc, default_pc }`
- `Instruction::PushHandler { clauses[*].target_pc }`

因此任何改变 `code.len()` 或改变指令下标的操作，都需要：

1) 生成 `old_pc -> new_pc` 映射（mapping）  
2) 用 mapping 重写上述所有 `*_pc` 字段  
3) 进行一致性校验：所有 target 必须落在 `0..new_code.len()`

### 4.2 约束简化：利用“编译器产物的结构”

对“编译器生成的 bytecode”（而非任意 `.rbc`），跳转目标通常只会指向 **基本块入口**。
bytecode lowering 内部是按 MIR block 顺序线性拼接并 patch 的，这使得我们可以：

- 通过扫描跳转目标集合 `label_pcs` 来重建基本块边界；
- 约束 peephole 的“删指令”仅发生在块内、且尽量不删除块入口；
  这样 mapping 的正确性更容易保证。

### 4.3 建议的 relocation 策略（实现草案）

对每个函数：

1) 扫描 `code` 收集 `label_pcs = {0} ∪ {all jump targets}`  
2) 以“基本块”为单位运行局部优化，生成 `new_code`  
3) 同时构造 `old_to_new: Vec<Option<u32>>`，对每个保留的旧指令位置记录新位置  
4) 第二遍遍历 `new_code`，将所有 `*_pc` 用 `old_to_new[target]` 改写  
5) Debug/断言：所有 `*_pc` 都可映射且合法

对于少数“允许删除块入口”的规则（如 trampoline block 仅含 `Jump`），需要支持：

- `old_to_new[target]` 可以指向“块入口被删除后，块内下一条保留指令”的新位置；
- 或者更简单：不直接删除块入口，而是将其改写为更短的等价形式（如果未来引入 `Nop` 才更方便）。

---

## 5. 优化规则（建议分阶段）

### 5.1 O1（保守 & 高收益）

这一档的目标是：**不引入数据流分析**，但能实打实减少指令数/跳转层级。

#### 规则 A：跳转线程化（Jump Threading / Trampoline Folding）

lowering 常见产生“跳板块”：

- 块内容只有 `Jump { target_pc = X }`（例如分支参数为空、并行拷贝为空时）

优化：

- 若 `T` 是 trampoline（仅一条 `Jump`，且无其他指令），将所有跳到 `T` 的跳转改为直接跳到 `T.target`。
- 进一步：可做传递闭包，直到落到非 trampoline 块。

收益：

- 减少 `JumpIf` 分支落到 edge 再 `Jump` 的二级跳转；
- 改善指令 cache/解释器分支预测。

注意点：

- trampoline 的判定必须确保块内没有 `PushHandler/PopHandler/Perform/...` 等语义点；
  初版建议只接受“块只有单条 Jump”的最简单形态。

#### 规则 B：删除明显冗余指令

在不改变语义（对编译器产物）前提下删除：

- `Copy { dst, src }` 且 `dst == src`
- `Jump { target_pc }` 且 `target_pc == next_pc`（跳到下一条）

这些删除会触发 relocation，但规则非常直接、可验证。

#### 规则 C：常量折叠（Constant Folding，纯局部）

仅对“操作数都来自常量”的情况折叠（不做代数化简 `x + 0` 这类需要类型证明的变换）：

- `Const a = Int(k1)` + `Const b = Int(k2)` + `IntAdd dst a b` → `Const dst = Int(k1+k2)`
- `BoolNot dst v` 且 `v` 为常量 → `Const dst = Bool(!v)`
- `BoolEq/BoolNe`、`IntEq/IntNe/IntLt/...` 同理

语义约束：

- `int_div/int_mod` 遇到除 0 不折叠（保持原指令，让 VM 维持现有 trap 行为）。
- `int_add/int_sub/int_mul` 若发生 i64 溢出：不折叠（避免引入“编译期溢出语义”争议）。

实现提示：

- 由于 bytecode 是寄存器机，没有 immediate 形式，常量折叠要拿到“常量值”，通常需要一个很小的
  局部值表（例如在单个基本块内跟踪最近一次 `Const` 写入）。
- 折叠如果会让前面的 `Const tmp` 变成死代码，初版可先不删（保守），后续再加块内 DCE。

### 5.2 O2（需要轻量数据流）

当 O1 稳定后，可以考虑引入轻量分析来提升收益：

#### 规则 D：块内 Copy Propagation + DCE（局部活跃性）

- 在单个基本块内做 use-count / last-use，删除“只为过渡而产生的临时寄存器”。
- 典型收益：消除 `lower_operand_to_reg` 为 literal 生成的 temp + 后续运算之间的中间 copy 链。

#### 规则 E：Copy → Move（需要全函数 liveness）

`Copy` 在 VM 里会 clone；`Move` 则 take（避免 clone）。
把 `Copy` 变成 `Move` 的前提是 `src` 在之后不再被使用（跨块也要考虑）。

- 初版不做（风险高，容易错）。
- 作为 O2/O3 目标：实现一次全函数 liveness，挑选可安全 move 的点。

---

## 6. 正确性与语义约束

### 6.1 语义基准

本优化 pass 的语义保持基准建议定义为：

- 对 **编译器前端产生的、类型正确的程序**，优化前后运行结果一致。

理由：

- VM 也支持直接运行外部 `.rbc`，但优化 pass 不会自动对“加载的 `.rbc`”运行（至少 v0）。
- 对“非法 bytecode”的 trap 形态/报错信息在早期阶段不必作为强约束，否则会限制优化空间。

### 6.2 必须避免的语义破坏

无论如何都必须保持：

- 控制流正确：所有跳转目标合法，`Switch/HandlerClause` 的 target 修正完整。
- 副作用顺序不变：不得跨越或删除可能产生副作用的指令（`Call/ICall/VCall/Perform/Resume/...`）。
- handler 栈语义不变：不得跨越 `PushHandler/PopHandler` 边界做 CFG 合并。

初版规则应刻意避开：

- 任何可能改变 trap 行为的“代数化简”（例如把 `JumpIf then==else` 变成 `Jump` 会跳过 `cond` 的类型检查）。

---

## 7. 测试计划

建议分层测试：

1) **单元测试（bytecode crate）**  
   - 构造小型 `Function { code: Vec<Instruction> }`，跑 optimizer 后断言：
     - target PC 全部有效
     - 指令序列满足期望（例如 trampoline folding 生效）

2) **端到端对拍（compiler + vm）**  
   - 对 fixtures/examples 中的 `.rusk`：
     - O0 编译执行结果 == O1 编译执行结果
   - 覆盖：分支、switch、handler（如果已有样例）、数组/结构体等。

3) **回归测试（最关键路径）**  
   - 特别针对 lowering 产生的 `CondBr` edge trampoline：确保优化后仍能正确传参/跳转。

---

## 8. 评估指标（Metrics）

短期可观察指标：

- 编译产物统计：每个函数/模块的 `code.len()` 下降比例
- trampoline 数量：优化前后 trampoline block 数量变化

中期（可选）：

- VM 指令计数（需要 VM 增加可选 metrics；目前 `rusk-measure` 的 bytecode backend 还未采集）：
  - executed_instructions
  - copy/move 次数（尤其是 `Copy`）

---

## 9. 实施步骤（建议）

1) 引入 `OptLevel`（O0/O1），默认 O0 或 O1（待讨论）  
2) 实现 per-function relocation 框架 + 基本一致性校验  
3) 落地 O1：trampoline folding + 冗余 Jump/Copy 删除 + 保守常量折叠  
4) 增加测试：optimizer 单测 + 编译执行对拍  
5) 视收益与稳定性再推进 O2（块内 DCE / liveness）

---

## 10. 备选方案

### 10.1 在 lowering 的 BlockId 阶段做 peephole

因为 `crates/rusk-bytecode` lowering 内部在 patch 前仍保留 `BlockId`（通过 `PcPatch`），理论上可以在
“块级结构”上做优化，最后再一次性线性化/patch。

优点：

- 不需要在绝对 PC 上做 relocation（更自然）

缺点：

- 需要更深度侵入 lowering 实现；对当前结构改动更大

本提案选择：先做“lowering 后优化”，把优化逻辑和 lowering 解耦；如果后续发现 relocation 成本太高，再考虑迁移。

### 10.2 新增 `Nop` 指令以简化 rewrite-only

如果 bytecode v0 新增 `Nop`：

- 许多 peephole 可以通过“把无用指令改写成 Nop”来避免 relocation

但这会触及：

- bytecode 格式/VM 指令集演进与兼容策略

因此不作为本提案的首选路径。

---

## 11. 开放问题

- 默认优化级别：CLI/测试默认走 O0 还是 O1？
    
    默认走 O1

- trap 行为的严格度：是否需要把“优化前后 trap message 一致”作为约束？

    不需要把“trap message 一致”作为约束

- handler/continuation 相关代码形态：实际样例中 trampoline folding 是否会跨越 handler 边界？需要哪些 guard？

    尚需确认，可以考虑延后
