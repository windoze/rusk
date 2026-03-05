# 嵌入 Rusk 字节码虚拟机 (`rusk-vm`)

本文档介绍了 `rusk-vm` loaf 暴露的**字节码虚拟机嵌入 API**：

- 如何将 `.rusk` 编译为字节码 `rusk_bytecode::ExecutableModule`，
- 如何从宿主应用程序中**嵌入**和**驱动**虚拟机实例，
- 如何安装**宿主函数**（导入），
- 如何声明和处理**外部化效果**（"冒泡"到宿主的效果），以及
- 如何声明**宿主导入** / **外部化 effects** 以便编译/类型检查以及运行时执行都能成功。

字节码虚拟机被设计为一个小型的、步进驱动的运行时，适合嵌入到 CLI、编辑器、服务器或其他运行时中。

---

## 目录

1. [Loaves 和层次](#loaves-和层次)
2. [ABI 边界：`AbiType` / `AbiValue`](#abi-边界abitype--abivalue)
3. [编译时声明（"存在什么"）](#编译时声明存在什么)
   - [宿主导入（`extern fn`）](#宿主导入extern-fn)
   - [外部效果声明](#外部效果声明)
4. [生成字节码模块](#生成字节码模块)
   - [从源代码编译](#从源代码编译)
   - [加载/保存 `.rbc`](#加载保存-rbc)
5. [嵌入虚拟机](#嵌入虚拟机)
   - [创建虚拟机](#创建虚拟机)
   - [安装宿主导入](#安装宿主导入)
   - [使用 `vm_step` 驱动执行](#使用-vm_step-驱动执行)
6. [外部化效果：`StepResult::Request`](#外部化效果stepresultrequest)
   - [宿主端效果分发](#宿主端效果分发)
   - [恢复 vs 取消](#恢复-vs-取消)
   - [限制](#限制)
7. [固定延续：宿主可存储的延续句柄](#固定延续宿主可存储的延续句柄)
   - [带延续签名的宿主函数](#带延续签名的宿主函数)
   - [延续值如何跨越 ABI 边界](#延续值如何跨越-abi-边界)
   - [宿主驱动的尾恢复](#宿主驱动的尾恢复)
   - [删除固定延续](#删除固定延续)
8. [泛型特化（高级）](#泛型特化高级)
9. [故障排除](#故障排除)

---

## Loaves 和层次

从高层次来看，处理流程是：

`Rusk 源代码 (.rusk)` → `MIR` → `字节码模块 (.rbc)` → `虚拟机执行`

在本仓库中，这对应于：

- `rusk-compiler`（Rust 代码中的 `rusk_compiler`）：解析/类型检查 Rusk 并降级到 MIR，也可以通过调用降级器来生成字节码。
- `rusk-bytecode`（`rusk_bytecode`）：字节码模块数据模型（`ExecutableModule`）、验证器和 `.rbc` 编码/解码。
- `rusk-vm`（`rusk_vm`）：一个带有**步进 API** 和严格的虚拟机/宿主 ABI 的小型字节码解释器。
- `rusk-host`（`rusk_host`）：可选的辅助工具，用于安装匹配的运行时宿主导入实现（例如：`std::print`、`std::println`）。

本文档重点关注嵌入 `rusk-vm`。

---

## ABI 边界：`AbiType` / `AbiValue`

`rusk-vm` 有意为跨越虚拟机/宿主边界暴露了一个小型的 ABI 表面。

在宿主端，你需要处理：

- `rusk_vm::AbiValue`：可以跨越边界的运行时值。
- `rusk_bytecode::AbiType`：这些值的类型级视图，存储在字节码签名中。

### ABI 值集合（v1）

当前字节码 VM 的 ABI 支持：

- 基础类型（leaf primitives）：
  - `unit`
  - `bool`
  - `int`（有符号 64 位）
  - `float`（IEEE-754 `f64`）
  - `byte`（无符号 8 位）
  - `char`（Unicode 标量值）
  - `string`（UTF-8）
  - `bytes`（`Vec<u8>`）
  - `continuation`（不透明的延续句柄；参见[固定延续：宿主可存储的延续句柄](#固定延续宿主可存储的延续句柄)）
- 复合值（以不透明 VM 引用/句柄的形式跨边界）：
  - `array(T)` / `[T]`
  - `tuple(T0, T1, ...)` / `(T0, T1, ...)`
  - Rusk 定义的 `struct` / `enum`

在 Rust 中：

```rust
use rusk_vm::AbiValue;

let v: AbiValue = 123_i64.into();
assert_eq!(v.ty(), rusk_bytecode::AbiType::Int);
```

### 复合 ABI 值：`HostContext`

复合值跨 VM/宿主边界时是 **VM 本地句柄/引用**，不会被深拷贝成 Rust 结构体。宿主如果要读取或构造数组/元组/结构体/枚举，需要使用 `HostContext`（它在一次宿主调用或 effect 分发期间借用 VM 的堆与模块元数据）。

### 这对嵌入意味着什么

- **宿主导入** 与 **外部化 effects** 都可以接受/返回上面的 ABI 类型集合，但签名是严格的：不匹配会 trap。
- 复合 ABI 值是 **VM 本地句柄**：它们只在产生它们的 `Vm` 实例里有意义，并且必须通过 `HostContext` 访问。
- v1 限制：
  - 名义 ABI 类型在边界上必须是单态（不能跨边界传递带类型实参的泛型 struct/enum 值）。
  - 宿主定义名义类型不在范围内。

---

## 编译时声明（"存在什么"）

Rusk 被设计为可嵌入的：平台集成由宿主定义的接口提供。

有**两个不同的阶段**：

1. **编译时声明**（名称 + 签名）：用于让编译/类型检查知道“存在什么”。
   - **宿主导入**：在 Rusk 源码里用 `extern fn` 声明。
   - **外部化 effects**：通过 `CompileOptions` 注册（同时在源码里声明 `interface`）。
2. **运行时实现**：用于让执行真的能成功。
   - 宿主导入需要在 VM 里安装实现。
   - 外部化 effects 需要在宿主侧处理 `StepResult::Request`。

如果你跳过编译时声明，编译失败。如果你跳过运行时实现，虚拟机在运行时调用缺失的导入时会陷阱。

### 宿主导入（`extern fn`）

宿主导入在 Rusk 源码里通过 `extern fn` 声明，并放在普通模块中。

示例：声明一个宿主导入并调用它：

```rusk
mod host {
    pub extern fn println(s: string) -> unit;
}

fn main() -> unit {
    host::println("hello from rusk");
    ()
}
```

规则（v1）：

- `extern fn` 必须以 `;` 结尾，并且**必须**显式写返回类型（可以 `-> unit`）。
- v1 不支持泛型 `extern fn`。
- 参数/返回值类型必须是字节码 ABI 安全的；否则编译为字节码会失败。

#### 字节码的 ABI 安全性

对于你打算在 `rusk-vm` 上运行的程序，宿主导入签名必须使用 ABI 安全的类型集合：

- 基础类型：`unit` / `bool` / `int` / `float` / `byte` / `char` / `string` / `bytes`
- `continuation`（`cont(P) -> R`，在 ABI 上是句柄）
- 复合类型：数组/元组/struct/enum（名义类型必须是单态，且由模块/sysroot 定义）

如果你声明了不 ABI 安全的宿主导入，编译为字节码时会出现类似错误：

> `host import '<name>' is not ABI-safe for bytecode v0`

#### 推荐：声明放到 sysroot，运行时用 `rusk-host` 安装实现

本仓库的 sysroot `std` 模块提供了 `extern fn` 声明；运行时可以通过 `rusk-host` 安装实现：

```rust
use rusk_host::std_io;

std_io::install_vm(&module, &mut vm);
```

---

### 外部效果声明

效果在 Rusk 中声明为 `interface`，并使用 `@Interface.method(...)` *执行*。

通常，如果执行了一个效果且**没有找到语言内处理器**，执行会陷入"未处理效果"运行时错误。

要让**宿主**处理效果的一个子集，你可以在编译为字节码时将它们声明为**外部化效果**。然后，未处理的 `perform` 会变成 `StepResult::Request` 而不是陷阱。

#### 1) 在 Rusk 源代码中声明接口

```rusk
interface TestFfi {
    fn add(a: int, b: int) -> int;
}

fn main() -> int {
    @TestFfi.add(1, 2)
}
```

#### 2) 在编译为字节码时注册外部化效果

```rust
use rusk_compiler::CompileOptions;

let mut options = CompileOptions::default();
options.register_external_effect_typed::<(i64, i64), i64>("TestFfi", "add")?;
```

#### ABI 安全性和签名匹配

重要注意事项：

- 外部效果签名也必须是 ABI 安全的（与宿主导入相同的 ABI 类型集合）。编译器在编译为字节码时会拒绝非 ABI 安全的签名。
- 编译器当前**不会**类型检查你注册的外部效果签名是否与程序中的接口方法签名匹配。保持它们对齐（理想情况下在你的嵌入代码库中的一个地方定义它们）。

---

## 生成字节码模块

### 从源代码编译

最简单的嵌入方法是"编译然后运行"：

```rust
use rusk_compiler::{CompileOptions, compile_file_to_bytecode_with_options};
use std::path::Path;

let options = CompileOptions::default();

let module = compile_file_to_bytecode_with_options(Path::new("path/to/program.rusk"), &options)?;
```

如果你已经在内存中有源文本：

```rust
use rusk_compiler::{CompileOptions, compile_to_bytecode_with_options};

let module = compile_to_bytecode_with_options(source_text, &options)?;
```

### 加载/保存 `.rbc`

对于"编译一次，运行多次"的嵌入，将字节码序列化到 `.rbc` 并稍后加载：

```rust
use rusk_bytecode::{from_bytes, to_bytes};

let bytes: Vec<u8> = to_bytes(&module)?;
let loaded = from_bytes(&bytes)?; // 包括验证
```

如果你在内存中构造或修改 `ExecutableModule`，你也可以显式验证它：

```rust
use rusk_bytecode::verify_module;

verify_module(&module)?;
```

---

## 嵌入虚拟机

### 创建虚拟机

要运行字节码，从 `ExecutableModule` 创建一个 `Vm`：

```rust
use rusk_vm::Vm;

let mut vm = Vm::new(module.clone())?;
```

注意事项：

- `Vm::new` 从 `module.entry`（通常是 `main`）开始执行。
- `Vm::new` 要求入口函数参数个数为 0（对应 `fn main()`）。

如果你的程序定义了 `fn main(argv: [string])`，则需要在创建虚拟机时传入 argv：

```rust
use rusk_vm::Vm;

// 由宿主提供的命令行参数。
// 约定：argv[0] 是被执行文件的完整路径；如果没有对应文件则为 ""。
let argv = vec![String::new(), "arg1".to_string(), "arg2".to_string()];

let mut vm = Vm::new_with_argv(module.clone(), argv)?;
```

`argv` 中的所有字符串都必须是有效的 UTF-8。宿主在读取平台/操作系统参数时需要确保这一点，
必要时应进行有损转换（lossy conversion）。

### 安装宿主导入

宿主导入在字节码模块内部声明，并由稳定的 `rusk_bytecode::HostImportId` 标识。

在运行时，你必须通过 `Vm::register_host_import`（底层）或 `Vm::register_host_import_typed`
（推荐）提供实现。

`register_host_import_typed` 对 ABI 基础类型与 continuation 最方便；如果你要处理复合 ABI 值
（数组/元组/结构体/枚举），请使用底层的 `register_host_import`：宿主函数会收到一个
`&mut HostContext`，用于安全地读取/构造这些值。

#### 按名称安装

符合人体工程学的模式是：

1. 使用模块按名称查找 `HostImportId`。
2. 注册一个闭包（或任何 `HostFn` 实现）。

```rust
use rusk_vm::{HostError, Vm};

if let Some(id) = module.host_import_id("std::println") {
    vm.register_host_import_typed(id, |(s,): (String,)| -> Result<(), HostError> {
        println!("{s}");
        Ok(())
    })?;
}
```

#### 安装整个宿主集

尽可能优先重用 `rusk-host`。例如：

```rust
use rusk_host::std_io;

std_io::install_vm(&module, &mut vm);
```

#### 运行时验证行为

虚拟机在运行时验证宿主调用：

- 参数个数必须与模块中的签名匹配。
- 每个参数必须是 ABI 安全的并匹配预期的 `AbiType`。
- 返回值必须匹配声明的返回 `AbiType`。

如果任何这些检查失败，虚拟机会陷入并显示描述性错误。

#### 可重入规则

当宿主导入正在执行时，虚拟机被认为处于"宿主调用中"。

- 回调到虚拟机（例如从宿主函数内部调用 `vm_step`）将导致陷阱："vm re-entered during host call"。

设计意图：宿主导入是同步的叶操作，而不是递归的虚拟机入口点。

---

### 使用 `vm_step` 驱动执行

执行通过重复调用来驱动：

```rust
use rusk_vm::{StepResult, vm_step};

let step: StepResult = vm_step(&mut vm, /* fuel */ None);
```

`vm_step` 是一个小型状态机：

- `StepResult::Done { value }`：程序成功完成。
- `StepResult::Trap { message }`：不可恢复的运行时错误。
- `StepResult::Request { effect_id, args, k }`：虚拟机在外部化效果上挂起。
- `StepResult::Yield { remaining_fuel }`：虚拟机用完了其燃料预算（协作调度）。

#### 燃料（协作调度 / 时间切片）

`vm_step(vm, fuel)` 接受：

- `None` 运行直到 `Done` / `Trap` / `Request`（实际上是"无限制"），
- `Some(n)` 最多运行 `n` 条指令步骤，然后返回 `Yield`。

这对以下情况很有用：

- 避免在单个宿主 tick 中无限制执行，
- 将多个虚拟机集成到调度器/事件循环中，
- 实现超时。

---

## 外部化效果：`StepResult::Request`

当执行效果且没有语言内处理器匹配时，虚拟机要么：

- 陷入（`unhandled effect`），或
- 如果执行的效果在模块的外部效果表中声明，则挂起并返回 `Request`。

在 `Request` 时，宿主接收：

- `effect_id: rusk_bytecode::EffectId`（稳定的表索引），
- `args: Vec<AbiValue>`（ABI 安全的有效载荷），
- `k: rusk_vm::ContinuationHandle`（表示"计算的其余部分"的句柄）。

### 宿主端效果分发

推荐做法：启动时建立 `EffectId -> handler` 的稠密表，然后按表索引分发：

```rust
use rusk_vm::{EffectDispatchTable, StepResult, vm_resume, vm_step};

let mut effects = EffectDispatchTable::new(&module);
effects.register_typed::<(i64, i64), i64>(&module, "TestFfi", "add", |(a, b)| Ok(a + b))?;

loop {
    match vm_step(&mut vm, None) {
        StepResult::Done { value } => break value,
        StepResult::Trap { message } => panic!("vm trapped: {message}"),
        StepResult::Yield { .. } => continue,
        StepResult::Request { effect_id, args, k } => {
            let resume_value = vm
                .with_host_context(|cx| effects.dispatch(cx, effect_id, &args))
                .expect("dispatch");
            vm_resume(&mut vm, k, resume_value).expect("resume");
        }
    }
}
```

### 恢复 vs 取消

一旦你收到 `Request`，虚拟机就**挂起**了：

- 在没有恢复/取消的情况下再次调用 `vm_step` 会产生陷阱："vm is suspended; call resume/drop first"。

你有两个选择：

1. **恢复**并带有一个值：
   - `vm_resume(&mut vm, k, value)`
   - 该值成为 Rusk 程序中 `@Interface.method(...)` 表达式的结果。
2. **取消**延续：
   - `vm_drop_continuation(&mut vm, k)`
   - 虚拟机转换到陷入的"已取消"状态（用于中止）。

两个操作都会验证延续句柄；使用过时/错误的句柄恢复会返回 `VmError::InvalidContinuation`。

### 限制

当前的外部效果行为有意的 v0 限制：

- **仅限非泛型效果。** 虚拟机当前仅外部化执行的接口**没有运行时类型参数**的效果（即还没有 `interface Foo<T> { ... }` 外部化）。
- **仅限 ABI 安全的参数/返回。** 外部化 effects 受 VM/宿主 ABI 类型集合限制；复合值以不透明 VM 引用跨边界。若你需要传递“宿主自定义结构化数据”，建议编码为 `bytes`/`string`。
- **每个虚拟机一个未完成的请求。** 在挂起时，虚拟机在宿主恢复或取消之前无法取得进展。

有关 Rusk 中效果和延续如何工作的更深入背景，请参见：
`docs/delimited-continuations-and-scoped-effects.md`。

---

## 固定延续：宿主可存储的延续句柄

Rusk 支持**一等、一次性的限定延续**作为语言级别的值（类型 `cont(<param>) -> <ret>`）。这些可以在效果处理器中捕获、存储在变量中，并稍后恢复。

从"宿主尾恢复"功能（2026-02-20）开始，**延续值可以作为不透明句柄跨越 VM/宿主 ABI 边界**。这使得以下嵌入场景成为可能：

- Rusk 程序捕获一个延续并将其传递给宿主（通过宿主导入调用或外部化效果结果）。
- 宿主将延续句柄存储在宿主拥有的状态中（例如调度器、事件循环或自定义注册表）。
- 宿主稍后可以**恢复**延续（要么将句柄传回 Rusk 代码，要么使用宿主驱动的尾恢复 API）。
- 宿主可以显式**删除**未使用的延续以释放资源。

这对于需要将 Rusk 延续与原生事件循环、异步运行时或其他宿主端调度机制集成的嵌入特别有用。

### 带延续签名的宿主函数

要声明接受或返回延续的宿主函数，在 Rusk 源码中把它们声明为宿主导入，并使用 `cont(P) -> R` 类型：

```rusk
mod host {
    pub extern fn store_cont(k: cont(int) -> int) -> unit;
    pub extern fn take_cont() -> cont(int) -> int;
}
```

在 Rusk 代码中，这些宿主函数可以自然地调用：

```rusk
mod host {
    pub extern fn store_cont(k: cont(int) -> int) -> unit;
    pub extern fn take_cont() -> cont(int) -> int;
}

interface E { fn boom() -> int; }

fn main() -> int {
    // 捕获一个延续并将其传递给宿主进行存储
    match @E.boom() {
        @E.boom() -> k => { host::store_cont(k); 0 }
        x => x
    };

    // 之后：从宿主检索延续并恢复它
    let k = host::take_cont();
    k(42)
}
```

### 延续值如何跨越 ABI 边界

当 `Value::Continuation` 跨越 VM/宿主边界（作为参数或返回值）时，VM 自动在内部 GC 根表中**固定**延续，并将其转换为：

- `AbiValue::Continuation(ContinuationHandle)`

其中 `ContinuationHandle` 是一个不透明的、经过代数检查的句柄：

```rust
use rusk_vm::{Cont, HostError};

// 在宿主导入实现中：
if let Some(id) = module.host_import_id("host::store_cont") {
    vm.register_host_import_typed(id, |(k,): (Cont<i64, i64>,)| -> Result<(), HostError> {
        // 将句柄存储在宿主状态中（例如 Vec 或 HashMap）
        store_continuation_somewhere(k.into_handle());
        Ok(())
    })?;
}
```

当 `AbiValue::Continuation(handle)` 流回 VM（例如从宿主导入返回）时，VM 查找固定的延续并将其转换回 `Value::Continuation`。

**重要属性：**

- **固定延续是 GC 根**：当宿主持有句柄时，捕获的 Rusk 帧/值保持活动状态（不会被回收）。
- **句柄是不透明的且特定于 VM**：它们在发出它们的 VM 之外没有意义。
- **保留一次性语义**：恢复延续（在语言中或从宿主）消耗底层状态；进一步的恢复尝试将失败并返回 `InvalidContinuation`。
- **代数检查防止释放后使用**：如果句柄的槽被重用，陈旧的句柄将被拒绝。

### 宿主驱动的尾恢复

宿主可以使用以下方法直接恢复固定的延续：

```rust
use rusk_vm::vm_resume_pinned_continuation_tail;

vm_resume_pinned_continuation_tail(&mut vm, handle, 42_i64.into())?;
```

这是一个**尾恢复**操作：

- 它消耗一次性延续状态。
- 它将捕获的延续段拼接到当前 VM 栈*之上*（就像从一个在 VM 中未表示的宿主处理器帧进行尾恢复）。
- 延续的最终返回值被**丢弃**（没有"第二半部分"宿主处理器帧来接收它）。如果 VM 栈为空，恢复的延续成为新的入口计算，其返回值照常成为程序结果。
- 该操作是**仅调度的**：它不会步进 VM。宿主必须随后调用 `vm_step` 来驱动执行。

宿主端恢复循环示例：

```rust
use rusk_vm::{StepResult, vm_step, vm_resume_pinned_continuation_tail};

// 在从宿主导入调用存储延续句柄之后：
vm_resume_pinned_continuation_tail(&mut vm, stored_handle, 42_i64.into())?;

loop {
    match vm_step(&mut vm, None) {
        StepResult::Done { value } => break value,
        StepResult::Trap { message } => panic!("vm trapped: {message}"),
        StepResult::Yield { .. } => continue,
        StepResult::Request { .. } => { /* 处理外部化效果 */ }
    }
}
```

**为什么只支持尾恢复？**

非尾宿主恢复需要宿主为宿主效果处理器的"第二半部分"保持状态，这超出了当前嵌入模型的范围。尾恢复语义通过在不返回控制到宿主处理器帧的情况下消耗延续来避免这种情况。

### 删除固定延续

如果宿主决定不恢复延续（例如取消请求或在关闭时清理），它必须显式删除句柄以释放固定的资源：

```rust
use rusk_vm::vm_drop_pinned_continuation;

vm_drop_pinned_continuation(&mut vm, handle)?;
```

删除后，句柄变为无效，捕获的延续状态有资格进行垃圾回收（除非仍可从运行中的 VM 访问）。

**重要：**

- 删除延续**不是**错误；这是一个正常的清理操作（相当于在 Rusk 代码中放弃延续）。
- 尝试两次删除同一句柄，或删除已消耗的句柄，会返回错误。

---

## 泛型特化（高级）

大多数嵌入可以忽略本节。

字节码虚拟机通过向泛型函数传递隐式运行时 `TypeRep` 值来支持 Rusk 泛型。对于某些嵌入，在宿主中实现泛型函数的**特定实例化**可能很有用（例如快速路径）。

`rusk-vm` 通过以下方式暴露这一点：

- `Vm::intern_type_rep` 为类型构造器构建虚拟机内部的 `TypeRepId`，以及
- `Vm::register_generic_specialization` 在存在特定类型参数集时将调用路由到宿主导入。

高级思想：

1. 你的程序调用泛型字节码函数 `F<T>(...)`。
2. 在运行时，虚拟机可以观察该调用的具体 `TypeRep` 参数。
3. 如果宿主注册了 `(F, [type_args...]) -> host_import_id`，虚拟机会调用该宿主导入而不是解释函数体。

虚拟机强制执行的重要约束：

- 目标函数必须实际上是泛型的（`generic_params > 0`）。
- `type_args.len()` 必须与函数的泛型元数匹配。
- 宿主导入签名必须与*值*参数匹配（类型表示参数被排除）。

如果你需要这个，请阅读 `crates/rusk-vm/src/lib.rs` 中的实现，并期待 API 演变（这是一个 v0 运行时）。

---

## 故障排除

### 编译时错误

- **未知宿主函数**
  - 症状：编译在名称解析/类型检查期间失败。
  - 修复：确保该函数在程序可见范围内以 `extern fn` 声明（可以写在程序/库里，也可以来自已加载的 sysroot 模块，例如 `std`）。对 `std::*`，确保 `CompileOptions.load_std = true`。

- **"host import … is not ABI-safe for bytecode v0"**
  - 症状：编译为字节码时失败。
  - 修复：对于打算在 `rusk-vm` 上运行的代码，将宿主导入签名限制为 ABI 安全的类型集合（基础类型 + continuation + 复合类型），或将值编码为 `bytes`/`string`。

- **"external effect … has non-ABI-safe signature for bytecode v0"**
  - 症状：`register_external_effect(...)` / `register_external_effect_typed(...)` 可以工作，但编译到字节码失败。
  - 修复：将外部化 effect 的签名限制为 ABI 安全的类型集合，或将值编码为 `bytes`/`string`。

### 运行时陷阱

- **"missing host import implementation: `<name>`"**
  - 你编译了一个导入宿主函数的模块，但没有通过 `Vm::register_host_import` 或
    `Vm::register_host_import_typed` 注册它。

- **"vm is suspended; call resume/drop first"**
  - 你收到了 `StepResult::Request` 并在没有恢复/取消的情况下再次调用了 `vm_step`。

- **"vm re-entered during host call"**
  - 宿主导入尝试回调到虚拟机（直接或间接）。宿主导入必须是同步的叶操作。

- **"unhandled effect: Interface.method"**
  - 程序执行了一个没有语言内处理器的效果，并且它没有在模块中声明为外部化效果。

- **"invalid continuation handle"**（使用 `vm_resume_pinned_continuation_tail` 或 `vm_drop_pinned_continuation` 时）
  - 延续句柄已过期（已被消耗或删除）、代数错误或属于不同的 VM。
  - 确保句柄仅使用一次（一次性），并且在先前的消耗操作之后不要尝试恢复/删除。
