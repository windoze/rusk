# 提案：用 C 实现 Rusk 字节码 VM（含 GC / Host Functions / Externalized Effects）

Date: 2026-02-15  
Status: Draft

> 本提案描述一个用 **C11** 实现的 Rusk Bytecode VM（下称 **C VM**），目标是与现有 `BYTECODE_SPEC.md`
>（v0.1 Draft）以及 Rust 参考实现 `crates/rusk-vm` 的 **step-driven embedding API** 在语义上对齐。
>
> 本提案重点覆盖三块：
>
> 1) **GC（C 版）**：STW、单线程、handle-based（可选 Immix + 移动/压缩）。  
> 2) **Host functions（C 版）**：`HostImport` 的回调注册/调用与 ABI 校验。  
> 3) **Externalized effects（C 版）**：`StepResult::Request` / `vm_resume` / `vm_drop_continuation`。

---

## 1. 背景与动机

当前 Rusk 字节码层的参考实现由 Rust 完成（`crates/rusk-vm`），这在研发阶段非常合适：语义清晰、易于迭代。
但在下列场景下，一个 **C 语言 VM** 具有明显优势：

- **嵌入与分发**：游戏引擎/插件系统/移动端/嵌入式等环境更偏好 C ABI（或仅允许 C ABI）。
- **跨语言集成**：C API 天然适配 C++/Swift/Java JNI/C# P/Invoke 等。
- **更可控的运行时**：对内存分配、GC 策略、栈布局等进行更底层的优化与裁剪。
- **实现多样性**：Rust VM 继续作为“规范化参考”，C VM 可作为“生产嵌入实现”，两者互相校验。

本提案不要求立即替换 Rust VM，而是新增一个并行实现：**以 `BYTECODE_SPEC.md` 为语义基准**，通过一致性测试维持对齐。

---

## 2. 目标 / 非目标

### 2.1 目标（Goals）

- **语义对齐**：执行模型、trap 行为、fuel 规则与 `BYTECODE_SPEC.md` 对齐。
- **Step API**：提供与 Rust VM 等价的四类返回：
  - `Done { value: AbiValue }`
  - `Trap { message }`
  - `Request { effect_id, args: [AbiValue...], k: ContinuationHandle }`
  - `Yield { remaining_fuel }`
- **Host imports（host functions）**：
  - 按 `HostImportId` 注册 C 回调；
  - VM 调用时做 ABI 类型校验（`AbiType`）。
  - 严格禁止“host call 期间 VM reentry”（同 `BYTECODE_SPEC.md` §3.2）。
- **Externalized effects**：
  - 实现 `BYTECODE_SPEC.md` §6.4：当 `perform` 未命中语言内 handler 且满足外部化条件时，返回 `Request` 并进入 Suspended；
  - 支持 `vm_resume` 与 `vm_drop_continuation`，并用 `(index, generation)` 使句柄可失效。
- **GC（C 实现）**：
  - 管理 heap objects（struct/enum/array/tuple 等引用对象）；
  - STW、单线程；
  - **稳定句柄（handle）** + slot table，支持对象移动时仅更新 slot 指针；
  - 实现路径允许分阶段：先非移动 mark-sweep，再演进到 Immix + 选择性搬迁。
- **可移植性**：C11（避免编译器扩展依赖），可在 Clang/GCC/MSVC 下构建（MSVC 需要对应 C 标准支持策略）。

### 2.2 非目标（Non-goals）

- JIT / AOT 本地代码生成。
- 并发、增量、分代 GC（需要写屏障等复杂机制；可未来单独提案）。
- 多线程 VM（先以单线程语义对齐为主）。
- 让 host 回调在执行期间再次调用 `vm_step` / `vm_resume` / `vm_drop_continuation`（明确禁止）。
- 立即做到与 Rust VM “每个 trap message 字符串”完全一致（但会尽量保持关键错误类型一致，测试以行为为主）。

---

## 3. 对齐基准（Normative References）

本提案以仓库内下列文档/实现作为对齐基准：

- `BYTECODE_SPEC.md`：
  - §3 VM ↔ host ABI（`AbiType`、host import 合约）
  - §5.2/§5.3 step API 与 fuel
  - §6 effects / continuations（尤其 §6.4 externalized effects）
- Rust 参考实现（用于对照行为，但不是稳定 ABI）：
  - `crates/rusk-vm/src/lib.rs` 中的 `AbiValue` / `StepResult` / `ContinuationHandle` 语义

---

## 4. C API 草案（嵌入接口）

> 本节给出“建议的 C API 形状”。最终命名、错误码、内存策略可以在实现阶段微调，但应保持核心语义不变。

### 4.1 核心数据类型（示意）

```c
// 不透明类型（opaque）。
typedef struct rusk_vm rusk_vm_t;
typedef struct rusk_module rusk_module_t;

typedef enum {
  RUSK_ABI_UNIT,
  RUSK_ABI_BOOL,
  RUSK_ABI_INT,
  RUSK_ABI_FLOAT,
  RUSK_ABI_STRING,
  RUSK_ABI_BYTES,
} rusk_abi_type_t;

typedef struct {
  const char* ptr;
  size_t len;
} rusk_str_view_t;

typedef struct {
  const uint8_t* ptr;
  size_t len;
} rusk_bytes_view_t;

// ABI 值：用于 host imports / external effects / Done 返回值。
// 约定：String/Bytes 在 C API 侧为“拥有型（owned）”，需要显式释放。
typedef struct {
  rusk_abi_type_t tag;
  union {
    bool b;
    int64_t i;
    double f;
    struct { char* ptr; size_t len; } str;
    struct { uint8_t* ptr; size_t len; } bytes;
  } as;
} rusk_abi_value_t;

typedef struct {
  uint32_t index;
  uint32_t generation;
} rusk_continuation_handle_t;

typedef enum {
  RUSK_STEP_DONE,
  RUSK_STEP_TRAP,
  RUSK_STEP_REQUEST,
  RUSK_STEP_YIELD,
} rusk_step_tag_t;

typedef struct {
  rusk_step_tag_t tag;
  union {
    struct { rusk_abi_value_t value; } done;
    struct { char* message; } trap;
    struct {
      uint32_t effect_id;
      rusk_abi_value_t* args;
      size_t args_len;
      rusk_continuation_handle_t k;
    } request;
    struct { uint64_t remaining_fuel; } yield;
  } as;
} rusk_step_result_t;
```

### 4.2 内存与所有权规则（关键）

为了让未来 **moving GC** 成为可能，C VM **不应把 GC heap 内部指针直接暴露给 host**。因此建议：

- `rusk_step_result_t` 中：
  - `Trap.message`、`Done.value`、`Request.args[*]` 内的 `string/bytes` 都是 **owned**；
  - host 使用完必须调用 `rusk_step_result_drop(vm, &res)` 或逐个 `rusk_abi_value_drop(...)` 释放。
- host 传入 `vm_resume(..., value)` 的 `rusk_abi_value_t`：
  - 视为输入参数；VM 会对 `string/bytes` 做 **拷贝**（host 可在调用返回后自由释放/复用缓冲）。
- VM 内部的 `Value::string/bytes` 可以：
  - 继续用“值语义”（`copy` 时深拷贝），但底层存储由 GC 管理或由专用 arena 管理；
  - 对外一律转成 `rusk_abi_value_t` 的拷贝，以避免暴露可移动地址。

### 4.3 Host functions（host imports）回调（示意）

```c
typedef enum {
  RUSK_OK = 0,
  RUSK_ERR = 1,
} rusk_status_t;

typedef rusk_status_t (*rusk_host_fn_t)(
  void* ctx,
  const rusk_abi_value_t* args,
  size_t args_len,
  rusk_abi_value_t* out_ret,
  char** out_err_message  // owned by VM allocator or host allocator：实现阶段统一
);
```

VM 提供注册函数：

```c
rusk_status_t rusk_vm_register_host_import(
  rusk_vm_t* vm,
  uint32_t host_import_id,
  rusk_host_fn_t fn,
  void* ctx
);
```

### 4.4 Step / Resume / Drop（示意）

```c
rusk_status_t rusk_vm_step(
  rusk_vm_t* vm,
  bool has_fuel,
  uint64_t fuel,
  rusk_step_result_t* out_result
);

rusk_status_t rusk_vm_resume(
  rusk_vm_t* vm,
  rusk_continuation_handle_t k,
  const rusk_abi_value_t* value
);

rusk_status_t rusk_vm_drop_continuation(
  rusk_vm_t* vm,
  rusk_continuation_handle_t k
);

void rusk_step_result_drop(rusk_vm_t* vm, rusk_step_result_t* res);
void rusk_abi_value_drop(rusk_vm_t* vm, rusk_abi_value_t* v);
```

### 4.5 典型嵌入流程（示意）

```c
for (;;) {
  rusk_step_result_t r;
  rusk_vm_step(vm, /*has_fuel=*/true, /*fuel=*/1000, &r);

  if (r.tag == RUSK_STEP_DONE) { /* use r.as.done.value */ break; }
  if (r.tag == RUSK_STEP_TRAP) { /* report r.as.trap.message */ break; }
  if (r.tag == RUSK_STEP_YIELD) { /* scheduler */ }
  if (r.tag == RUSK_STEP_REQUEST) {
    // host handles externalized effect (by effect_id + args), then resumes:
    rusk_abi_value_t ret = /* build ABI value */;
    rusk_vm_resume(vm, r.as.request.k, &ret);
  }

  rusk_step_result_drop(vm, &r);
}
```

---

## 5. Host Functions（Host Imports）设计

### 5.1 与字节码/模块的关系

字节码模块的 `host_imports: Vec<HostImport>` 为每个 import 提供：

- `name: String`
- `sig: HostFnSig { params: [AbiType...], ret: AbiType }`

在 C VM 中，建议直接沿用“按 `HostImportId`（`u32` 索引）注册”的方式：

- 好处：VM 调用路径不需要字符串哈希；与 `.rbc` 中的索引表一致。
- host 若想按名字注册，可通过模块查询 API 将 `name -> id` 映射在 host 侧建立。

### 5.2 调用时校验与错误传播

调用 `CallTarget::Host` 时，C VM 必须执行（与 Rust VM 同类）校验：

1) 是否已注册该 `host_import_id` 的实现；否则 trap：`missing host import implementation`。
2) 从寄存器读取参数：
   - 读到未初始化寄存器：trap。
   - 值类型无法转为 ABI（例如把 `ref` 传给 ABI `int`）：trap。
3) 校验参数 ABI 类型与 `sig.params` 一致；否则 trap。
4) 进入 host call：
   - 设置 `vm->in_host_call = true`，禁止 reentry（若 reentry，返回 trap）。
5) host 回调返回：
   - 若回调返回错误：trap，message 包含 host error 文本；
   - 校验返回 ABI 类型与 `sig.ret` 一致；否则 trap。
6) 将返回值写入 `dst`（若有），并执行必要的 ABI → VM 内部值转换。

### 5.3 ABI 值拷贝策略

为了语义明确与内存安全：

- VM → host：
  - 将 VM 内部值转换为 `rusk_abi_value_t`，其中 `string/bytes` 以拷贝形式输出（owned）。
- host → VM：
  - `string/bytes` 输入一律拷贝进入 VM（GC 管理或 VM arena 管理）。

这与 Rust 参考实现中 `Value::to_abi()` / `Value::from_abi()` 的思路一致：**ABI 边界是“值拷贝边界”**。

---

## 6. Externalized Effects（对外化 effect）设计

### 6.1 触发条件（对齐 `BYTECODE_SPEC.md` §6.4）

当执行 `perform`：

1) VM 从 handler 栈顶向下匹配语言内 handler clause；
2) 若无匹配 handler：
   - 若 effect 是 **monomorphic**（接口无运行时 type args）且在 `module.external_effects` 中声明，
     则 **externalize**；
   - 否则 trap：`unhandled effect: Interface.method`。

### 6.2 `Request` 的返回内容

若 externalize：

- `effect_id`：对应 `module.external_effects[effect_id]` 的索引。
- `args`：将 `perform` 的参数值转换为 ABI 值数组（逐个校验 `AbiType`）。
- `k`：`ContinuationHandle { index, generation }`，并使 VM 进入 **Suspended** 状态。

重要约束（建议保持与 Rust VM 一致）：

- **同一时间最多一个 Suspended**（单一 outstanding request）。
- `k.index` 可固定为 `0`，`generation` 来自 VM 内部计数器：
  - `vm_resume` 或 `vm_drop_continuation` 都会使 `generation++`，从而使旧句柄失效。

### 6.3 `vm_resume` / `vm_drop_continuation`

- `vm_resume(vm, k, value)`：
  - 校验 VM 当前确实 suspended 且句柄匹配；
  - 将 `value`（ABI）写回到被挂起的 `perform` 的目的寄存器（若该 `perform` 有 `dst`）；
  - VM 回到 Running。
- `vm_drop_continuation(vm, k)`：
  - 校验句柄匹配；
  - 取消执行并将 VM 置为 Trapped（message：`cancelled`），同时清空栈/handler。

### 6.4 host 侧如何识别 effect

host 通常需要从 `effect_id` 得到 `(interface, method, sig)`：

- C VM 建议提供查询 API（只读）：
  - `rusk_module_external_effect_decl(module, effect_id, out_interface, out_method, out_sig)`。
  - 或者一次性导出 `external_effects` 表供 host 建立映射。

---

## 7. GC（C 版）设计

> 本节给出一个“可实现、可演进”的 GC 方案：从简单到复杂分阶段落地，但核心始终是 **handle-based**。

### 7.1 引用模型：稳定句柄（Handle）+ Slot Table

GC 管理的对象（struct/enum/array/tuple 等）不直接以裸指针暴露给 VM 值层，而使用稳定句柄：

```c
typedef struct { uint32_t index; uint32_t generation; } rusk_gc_ref_t;
```

- VM 寄存器/对象字段内保存 `rusk_gc_ref_t`（而不是指针）。
- GC 维护 `slots[index] = { generation, ptr, ... }`：
  - 访问对象：`ptr = slots[index].ptr`，并校验 `generation`。
  - 对象移动：只需更新 `slots[index].ptr`，无需扫描/修补对象内部引用（因为内部引用也是 handle）。

这与已有的 Rust 方向提案 `proposals/vm-gc.md` 的核心思想一致，只是实现语言改为 C。

### 7.2 对象头（Header）建议

为了让 GC 在扫描/搬迁时能从对象地址反查 slot，建议对象头至少包含：

- `slot_index: u32`
- `kind: u16`（对象种类：Struct/Enum/Array/Tuple/String/Bytes/...）
- `flags: u16`（如 pinned/readonly?）
- `size_bytes: u32`（包含 header 的总大小，或 payload 大小）

其中 `slot_index` 很关键：在搬迁/回收时可直接定位并更新 `slots[slot_index]`。

### 7.3 追踪接口：按对象种类枚举子引用

GC 的 mark 阶段需要“知道对象里有哪些 `rusk_gc_ref_t`”。
建议在 C VM 中为每种对象种类提供一个 `trace_children(obj, visitor)`：

- `Array/Tuple`：遍历元素值，若元素为引用类型则 mark。
- `Struct/Enum`：按字段顺序遍历。
- `String/Bytes`：
  - 若实现为 GC 对象且不包含子引用，则无需递归。

### 7.4 Root Set（根集合）

GC roots 至少包括：

- 当前 VM 执行栈 `frames[*].regs[*]` 中的所有值；
- 当前 pending 的语言内 continuation token（如果 continuation 捕获的 frames/regs 被放在 GC 外部内存，也必须扫描）；
- VM 暂停态（Suspended）下的寄存器/栈仍然在 `frames` 中，因此自然是 root；
- 任何 VM 内部“全局表”中存储的值（如常量池缓存、intern 表等，若其中存放 GC handles）。

### 7.5 分阶段实现路线

#### Phase 1：Mark-Sweep（非移动，尽快跑通语义）

第一阶段建议以最小风险实现 GC：

- 分配：`malloc`/arena 分配对象，`slots[index].ptr` 指向稳定地址；
- GC：标记-清扫：
  - mark：从 roots 出发把 reachable slot 标记；
  - sweep：释放未标记对象，回收 slot（`generation++` 并放入 free list）。

优点：实现快，便于快速构建一致性测试体系；缺点：碎片化与内存峰值可能较差。

#### Phase 2：Immix（块/线元数据 + hole-filling）

在 Phase 1 语义稳定后，再切换到 Immix 风格的空间管理：

- heap 由固定大小 **blocks**（例如 32KiB）组成；
- 每个 block 切分为固定大小 **lines**（例如 128B）；
- mark 阶段同时标记对象与其覆盖的 lines；
- sweep 阶段根据 line mark 发现 holes，构建 hole-filling 分配结构。

仍可先保持“非移动”，即只做 hole-filling，不做 evacuation。

#### Phase 3：选择性搬迁（Evacuation / Moving）

在 handle-based 基础上实现 moving 成本相对可控：

- 选择碎片严重的 blocks 作为候选；
- 对其中的 live objects 重新分配并 `memcpy` 到新位置；
- 通过对象头 `slot_index` 更新 `slots[slot_index].ptr = new_ptr`；
- 旧 block 释放/复用。

由于对象内部引用为 handle，不需要全堆指针修补（与传统 moving GC 相比大幅简化）。

### 7.6 GC 触发与安全点

建议：

- 使用“分配计数阈值触发”（例如每 N 次/每 M 字节分配触发一次 collect）。
- **安全点**：只在“指令边界”触发 GC（step 循环每执行完一条指令检查阈值）。
- **host call 期间禁止触发 GC**：
  - host call 期间 VM 不应运行任何可能分配/GC 的逻辑；
  - 这也与“禁止 VM reentry”相互配合，避免 host 侧持有任何临时借用导致不一致。

---

## 8. 实施计划（建议里程碑）

为了降低风险，本提案建议按以下节奏实现：

1) **M0：骨架与最小执行闭环**
   - C 版 `rusk_vm_step` 框架、frame/regs/pc、基础指令子集（const/copy/move/jump/return/trap）。
2) **M1：Host imports + ABI 边界跑通**
   - 按 `HostImportId` 注册/调用回调；
   - 先把 `string/bytes` 做成“总是拷贝”的 ABI 策略。
3) **M2：Effects + externalized effects**
   - handler 栈、perform/resume（语言内 continuation token）；
   - `StepResult::Request` + `vm_resume`/`vm_drop_continuation`。
4) **M3：GC Phase 1（Mark-Sweep）**
   - 引入 handle/slot；
   - 验证引用对象语义（readonly、aliasing、写入 trap）。
5) **M4：GC Phase 2/3（Immix + 可选 moving）**
   - 在一致性测试覆盖足够后替换 allocator/collector。

---

## 9. 测试与一致性策略

建议引入“对照测试（differential testing）”：

- 同一份 `.rbc` 输入：
  - Rust VM 与 C VM 都用相同 fuel 策略运行；
  - 比较每次 `vm_step` 的 `StepResult` 序列（Done/Trap/Request/Yield 类型与关键字段）。
- 对 Request：
  - host 用“纯函数式”处理（确定性），然后 `resume`；
  - 比较两边最终结果一致。
- 在 `fixtures/` 中收集覆盖：
  - 复杂值（struct/enum/array/tuple）与 readonly；
  - 多层 handler 与 continuation；
  - externalized effect 的 resume/drop 分支；
  - host import 参数/返回类型错误的 trap 行为。

---

## 10. 风险与开放问题

1) **模块加载边界**：C VM 是否必须独立解码 `.rbc`？
   - 选项 A：实现 C 版 `.rbc` decoder/verifier（更独立，但工作量大）。
   - 选项 B：先由 Rust 侧解码/验证并导出“扁平 C 结构”（更快落地，但依赖 Rust 工具链）。
2) **trap message 精确性**：是否把“错误类型”作为一致性基准，而不是精确字符串？
3) **ABI 内存策略**：`string/bytes` 的 owned/borrrowed 约定最终如何落地（以及 allocator 归属）？
4) **`string/bytes` 的内部表示**：是否也放入 GC（作为无子引用对象）以便统一管理？
5) **性能基准**：先追求语义一致性，再对 GC/dispatch 做系统优化；优化方向是否要与 Rust VM 对齐？

