# 在 JVM 上实现 Rusk Bytecode VM（Java 版本，使用 JVM 原生 GC）

Date: 2026-02-15  
Status: Draft

本提案建议为 Rusk 的 `.rbc` 字节码格式实现一个 **纯 Java** 的参考 VM，使其可以在 JVM 生态中
（服务端、桌面、Android、插件系统等）直接加载并执行 Rusk 字节码，并依赖 **JVM 原生 GC** 管理
运行时堆对象（struct/enum/array/tuple 等），而不是在 Java 侧重复实现一套专用 GC。

同时，本提案明确 Java 版本 VM 如何实现并暴露：

- **Host Functions（Host Imports）**：同步、不可 yield 的宿主函数调用边界。
- **Externalized Effects（外部化效果）**：当 `perform` 没有被语言内 handler 捕获时，将请求外部化给宿主，
  由宿主异步/事件循环处理并通过 `resume/drop` 恢复/取消执行。

> 规范对齐：本提案以 `BYTECODE_SPEC.md`（v0.1 draft）作为行为基线，并参考已实现的 Rust 参考 VM
>（`crates/rusk-vm`）的 `step/resume/drop` 交互模型与错误语义。

---

## 1. 背景与动机

Rusk 当前的执行层已经有明确的“可移植 VM 边界”：

- 编译器 `ruskc` 输出 `.rbc`（字节码模块）。
- VM 通过 `step()` API 被宿主驱动执行。
- 与平台集成通过两条路径完成：
  - **Host Imports**：同步调用，返回 ABI 值；
  - **Externalized Effects**：通过 `StepResult::Request` 把“可能阻塞/需要等待的操作”外包给宿主。

在 JVM 上实现一套 Java VM 的价值：

1) **天然 GC**：Rusk 运行时对象是高频创建/传递的数据结构；在 Java 里用 JVM GC 管理对象生命周期，
   能显著降低实现复杂度（尤其是对象移动、写屏障、slot/handle、Drop 语义等）。

2) **深度嵌入 JVM 生态**：可以把 Rusk 当成 JVM 应用的脚本/插件语言：
   - 服务器端：结合现有线程池、`CompletableFuture`、NIO/Netty；
   - Android：在应用内运行脚本逻辑；
   - IDE/构建工具：复用 JVM 的分发能力。

3) **对齐“可移植 VM”愿景**：`.rbc` 的规范目标之一就是可在 C/JVM/CLR/JS 等环境重实现。
   Java VM 是一个重要落点，且可作为跨实现一致性测试的参照。

---

## 2. 目标与非目标

### 2.1 目标（Goals）

- **规范一致性**：实现 `.rbc` 解析 + 校验 + 执行，行为与 `BYTECODE_SPEC.md` 对齐。
- **Step 驱动**：提供与 Rust VM 等价的 `step(fuel)` / `resume(k, value)` /
  `dropContinuation(k)` API。
- **Host Imports（同步）**：在 Java 中可注册/安装宿主函数，实现 ABI 类型校验与不可重入约束。
- **Externalized Effects（外部化）**：在 Java 中把未被 handler 捕获的 `perform` 外包给宿主，
  并支持宿主异步恢复。
- **JVM GC 托管堆对象**：Rusk heap objects 直接表示为 Java 对象，由 JVM GC 回收；
  VM 不维护“对象表/移动 GC”作为必需组件（允许后续增加可选的统计/调试追踪）。
- **可测试性**：提供跨实现对拍路径（同一 `.rbc` 在 Rust VM 与 Java VM 下得到相同结果）。

### 2.2 非目标（Non-goals）

- **JIT / AOT**：不在本提案范围内（先把解释器与边界做对）。
- **多线程 VM**：先保持单 VM 实例单线程驱动（宿主可多 VM 并发）。
- **把 VM 内部引用对象暴露到宿主**：VM ↔ 宿主 ABI 仍限定为 primitive（见 `AbiType`）。
- **实现 Java 侧“标准库”**：本提案只定义宿主函数/外部化效果机制；具体 host set 可分阶段提供。

---

## 3. Java VM 总体架构（建议）

建议在仓库内新增一个 JVM 子工程（可选方案）：

- `jvm/`（Gradle/Maven）或 `java/`：
  - `rusk-bytecode-java`：`.rbc` 解析/数据模型/校验（对应 Rust `rusk-bytecode`）
  - `rusk-vm-java`：VM 执行器（对应 Rust `rusk-vm`）

也可以先作为独立 repo 孵化，但仍以本仓库的 `BYTECODE_SPEC.md` 为单一事实来源。

核心组件（概念层）：

- `ExecutableModule`（Java 版数据模型）
- `RbcDecoder`（严格按规范解析）
- `ModuleVerifier`（校验表完整性、索引范围、基础不变量）
- `Vm`：
  - `List<Frame>` 调用栈
  - `List<HandlerEntry>` handler 栈
  - `VmState`（Running / Suspended / Done / Trapped）
  - `HostImportRegistry`（host import 安装表）
  - `TypeRepInterner`（可选：TypeRep 结构共享/去重）

---

## 4. 值模型与 JVM GC（关键点）

### 4.1 `Value`（VM 内部值）

Java VM 内部需要表示 `BYTECODE_SPEC.md` 中的运行时值集合（至少）：

- primitive：`unit/bool/int(i64)/float(f64)/string/bytes`
- `typerep`
- 引用对象（heap objects）：`struct/enum/array/tuple`
- `FunctionId`（函数引用）
- continuation token（语言内 `perform/resume` 用的一次性 token）

建议用 Java 的 sealed types/records（Java 21+）表达（示意）：

```java
sealed interface Value permits
  Value.Unit, Value.Bool, Value.Int, Value.Float, Value.Str, Value.Bytes,
  Value.TypeRep, Value.Ref, Value.Fn, Value.Continuation { ... }
```

### 4.2 引用对象：直接用 Java 对象表示

Rusk 的 heap objects（struct/enum/array/tuple）在 Java VM 中建议直接用 Java 对象图表示：

- `StructObj { String typeName; Value[] fields; }`
- `EnumObj { String enumName; String variant; Value[] fields; }`
- `ArrayObj { ArrayList<Value> items; }`
- `TupleObj { Value[] items; }`

由 JVM GC 回收不可达对象，无需 VM 显式 trace/collect。

重要：**不要维护“全局对象列表”作为强引用**，否则 JVM GC 无法回收。
如果需要统计（例如 `heap_live_objects`），建议用 `WeakReference` + `ReferenceQueue`
做调试级别的近似统计，而不是将其作为语义的一部分。

### 4.3 Readonly reference views

字节码层的 readonly 是“引用视图限制”，不是深冻结（`BYTECODE_SPEC.md` 2.4）。

建议将引用值表示为：

- `Value.Ref { HeapObject obj; boolean readonly; }`

当执行写操作（例如 `set_field` / `array_set` / `array_push` 等）时：

- 若 `readonly == true`，直接 trap。
- 否则允许写入（即便同一对象同时被其他 readonly 引用别名指向）。

### 4.4 `bytes` 的可变性与边界复制

Rusk 的 `bytes` 语义是“值”，但 Java 的 `byte[]` 是可变引用类型。
为了避免宿主/VM 之间共享同一个 `byte[]` 导致“外部修改影响 VM 内语义”，建议：

- VM 内部 `Value.Bytes` 持有 `byte[]` 但当从 ABI 边界进入/离开时都进行 defensively copy；
  或统一使用不可变封装（例如 `ByteString` 风格结构）。

这对 **determinism** 与 **可调试性** 很重要。

---

## 5. Host Functions（Host Imports）在 Java 中的设计

### 5.1 与字节码的对应关系

`.rbc` 模块包含 `host_imports: Vec<HostImport>`：

- `name: String`（建议视为全限定名，例如 `std::println`）
- `sig: HostFnSig { params: Vec<AbiType>, ret: AbiType }`

VM 执行 `CallTarget::Host(HostImportId)` 时：

- 从调用寄存器读取参数；
- 将参数值转换为 `AbiValue` 并做 **arity + 类型** 校验；
- 调用已安装的 Java host function；
- 校验返回值 `AbiType` 并写回寄存器（如有 `dst`）。

### 5.2 Java 侧 API 草案

建议 Java VM 暴露“按 ID 安装”为主、按 name 安装为辅：

```java
@FunctionalInterface
interface HostFn {
  AbiValue call(List<AbiValue> args) throws HostError;
}

final class HostImportRegistry {
  void register(HostImportId id, HostFn fn);
  void registerByName(String name, HostFn fn); // 可选：内部映射到 id
}
```

并在 VM 层实现下列约束（与 `BYTECODE_SPEC.md` 3.2 对齐）：

- **不可重入（No VM reentry during host calls）**：host function 执行期间禁止调用
  `vm.step / vm.resume / vm.dropContinuation`，否则返回 Trap/抛出 `VmError`。
- host imports 是 **同步** 且 **不可 yield** 的：host function 不能触发外部化效果；
  需要等待的事情应当用 `perform` + externalized effects 表达。

### 5.3 错误语义

建议约定：

- host function 抛出异常或返回错误 → VM trap（错误信息做适度封装，避免泄露宿主内部细节）。
- arity/type mismatch → VM trap（错误信息包含 host import name + 期望/实际类型）。

另外建议提供“预检（preflight）”：

- VM 启动时或首次 `step` 前校验：模块声明的每个 host import 都有实现，否则直接报错（更早失败）。

---

## 6. Externalized Effects（外部化效果）在 Java 中的设计

### 6.1 与字节码的对应关系

外部化效果来自 `perform`：

- VM 先尝试语言内 handler 匹配；
- 若没有匹配：
  - 若该 effect 在 `module.external_effects` 中声明（且是可外部化的单态 effect），则 VM **挂起**
    并返回 `StepResult::Request`；
  - 否则 trap（“unhandled effect”）。

`.rbc` 的 `external_effects: Vec<ExternalEffectDecl>` 定义了可外部化 effect：

- `interface: String`
- `method: String`
- `sig: HostFnSig`（ABI-safe）

返回给宿主的请求包含：

- `effect_id: EffectId`（索引）
- `args: List<AbiValue>`（按 `sig.params` 校验）
- `k: ContinuationHandle`（用于 `resume/drop` 的一次性 token）

### 6.2 Java 侧 `StepResult` 与 continuation handle

建议 Java VM 暴露与 Rust VM 结构一致的结果类型（示意）：

```java
sealed interface StepResult permits Done, Trap, Request, Yield { ... }
record Request(EffectId effectId, List<AbiValue> args, ContinuationHandle k) implements StepResult {}
record ContinuationHandle(int index, int generation) {}
```

实现策略建议与 Rust VM 一致：**单 VM 同时最多只有一个挂起请求**。
因此 `ContinuationHandle.index` 可以固定为 `0`，仅用 `generation` 防止重复 resume/drop。

### 6.3 宿主如何处理 `Request`

外部化效果的核心价值，是让宿主用 JVM 生态的异步/事件循环模型管理等待：

- 宿主拿到 `Request` 后，依据 `(interface, method)` 或 `effectId` 分派处理；
- 宿主在处理完成时调用 `vm.resume(k, value)`；
- 或在取消/超时/错误时调用 `vm.dropContinuation(k)`（VM 进入 trapped 状态，message 为 `cancelled`）。

建议提供两个宿主侧分派层级：

1) `EffectId -> handler`（快路径；直接索引）
2) `(interface, method) -> handler`（更易读；启动时解析 module 表构建映射）

### 6.4 重要约束（与规范对齐）

- **ABI 限制**：externalized effect 的参数与 resume 值只能是 `AbiValue`
  （`unit/bool/int/float/string/bytes`）。
- **不可重入**：宿主在处理 request 的回调中调用 `resume/drop` 时，应确保不发生 VM 重入；
  VM 侧仍需要守卫。
- **挂起状态机**：当 VM Suspended 时：
  - 再次调用 `step()` 应返回 Trap（或抛出明确的状态错误），提示必须先 `resume/drop`；
  - 调用 `resume/drop` 后，VM 回到 Running 或进入 Trapped（cancelled）。

---

## 7. Java VM 对外 API（建议最小集）

面向 embedding 的最小 API（示意）：

```java
final class Vm {
  Vm(ExecutableModule module, HostImportRegistry hostImports);

  StepResult step(Long fuel /* null = unlimited */);

  void resume(ContinuationHandle k, AbiValue value) throws VmError;
  void dropContinuation(ContinuationHandle k) throws VmError;
}
```

额外（可选）：

- `VmMetrics`（指令计数、trap 原因分类）用于基准/回归。
- `VmConfig`（最大栈深、最大寄存器数、最大数组长度等）用于“运行不可信 `.rbc`”时的资源限制。

---

## 8. 测试与一致性验证计划

建议采用“同一套 `.rbc` fixtures + 结果对拍”的策略：

1) 用现有 `ruskc` 编译 `fixtures/*.rusk` 得到 `.rbc`；
2) 在 Rust VM（`crates/rusk-vm`）与 Java VM 上分别执行；
3) 对比：
   - `StepResult::Done` 的 `AbiValue`；
   - `StepResult::Trap` 的 trap 类别（message 可允许不完全一致，但建议至少提供稳定错误码/前缀）；
   - `StepResult::Request` 的 `(effectId, args)` 与 `resume` 后的可恢复性。

在 Java 侧建议按层测试：

- `.rbc` 解析/编码（若实现编码器）：`encode(decode(bytes))` 稳定性
- verifier：拒绝越界索引、无效 tag、结构不一致
- 指令语义：为每类指令建立小模块“单测模块”
- host imports：arity/type mismatch、异常封装、不可重入守卫
- externalized effects：挂起、resume 注入、drop 取消、generation 防重放

---

## 9. 分阶段里程碑（建议）

1) **MVP：可执行 `.rbc`**
   - `.rbc` 解码 + 基础 verifier
   - `Vm.step` 跑通：常量、算术、跳转、call/return
2) **Host Imports**
   - 注册表 + ABI 校验 + 不可重入守卫 + 预检
3) **Effects**
   - handler 栈 + `perform/resume`（语言内）
   - externalized effects：`StepResult::Request` + `resume/drop`
4) **完整指令集与优化**
   - pattern matching、vcall、intrinsics 完整覆盖
   - 性能优化（减少装箱、结构共享、热点路径特化）

---

## 10. 风险与开放问题

1) **性能/装箱成本**
   - 直接用 `Value` 对象会产生大量分配；可后续引入：
     - tagged union（例如 `long` + tag）表示部分 primitive，
     - 常量池对象共享（Unit/Bool）、
     - `Value[]` + `null` 表示 uninitialized。

2) **错误信息稳定性**
   - Rust VM 当前用字符串 message；Java VM 若要对拍，建议引入结构化错误码（可选）。

3) **Java 版本选择**
   - sealed classes/records 需要较新版本；若要兼容 Android/旧 JVM，需要降级设计。

4) **`bytes` 可变别名**
   - 必须在 ABI 边界复制或使用不可变封装，否则宿主可“篡改”VM 内数据，破坏确定性与语义。

