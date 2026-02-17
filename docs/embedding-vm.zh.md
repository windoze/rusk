# 嵌入 Rusk 字节码虚拟机 (`rusk-vm`)

本文档介绍了 `rusk-vm` crate 暴露的**字节码虚拟机嵌入 API**：

- 如何将 `.rusk` 编译为字节码 `rusk_bytecode::ExecutableModule`，
- 如何从宿主应用程序中**嵌入**和**驱动**虚拟机实例，
- 如何安装**宿主函数**（导入），
- 如何声明和处理**外部化效果**（"冒泡"到宿主的效果），以及
- 如何设置**编译器可见的原型**以便编译/类型检查能够成功。

字节码虚拟机被设计为一个小型的、步进驱动的运行时，适合嵌入到 CLI、编辑器、服务器或其他运行时中。

---

## 目录

1. [Crates 和层次](#crates-和层次)
2. [ABI 边界：`AbiType` / `AbiValue`](#abi-边界abitype--abivalue)
3. [编译时原型（"存在什么"）](#编译时原型存在什么)
   - [宿主函数原型](#宿主函数原型)
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
7. [泛型特化（高级）](#泛型特化高级)
8. [故障排除](#故障排除)

---

## Crates 和层次

从高层次来看，处理流程是：

`Rusk 源代码 (.rusk)` → `MIR` → `字节码模块 (.rbc)` → `虚拟机执行`

在本仓库中，这对应于：

- `rusk-compiler`（Rust 代码中的 `rusk_compiler`）：解析/类型检查 Rusk 并降级到 MIR，也可以通过调用降级器来生成字节码。
- `rusk-bytecode`（`rusk_bytecode`）：字节码模块数据模型（`ExecutableModule`）、验证器和 `.rbc` 编码/解码。
- `rusk-vm`（`rusk_vm`）：一个带有**步进 API** 和严格的虚拟机/宿主 ABI 的小型字节码解释器。
- `rusk-host`（`rusk_host`）：可选的辅助工具，用于定义宿主模块原型 + 安装匹配的运行时实现（例如：`std::print`、`std::println`）。

本文档重点关注嵌入 `rusk-vm`。

---

## ABI 边界：`AbiType` / `AbiValue`

`rusk-vm` 有意为跨越虚拟机/宿主边界暴露了一个小型的 ABI 表面。

在宿主端，你需要处理：

- `rusk_vm::AbiValue`：可以跨越边界的运行时值。
- `rusk_bytecode::AbiType`：这些值的类型级视图，存储在字节码签名中。

### ABI 安全的基本类型（v0）

当前的字节码虚拟机 ABI 支持：

- `unit`
- `bool`
- `int`（有符号 64 位）
- `float`（IEEE-754 `f64`）
- `string`（UTF-8）
- `bytes`（`Vec<u8>`）

在 Rust 中：

```rust
use rusk_vm::AbiValue;

let v = AbiValue::Int(123);
assert_eq!(v.ty(), rusk_bytecode::AbiType::Int);
```

### 这对嵌入意味着什么

- **宿主导入**（通过 `call` 调用的宿主函数）只能接受/返回 ABI 安全的基本类型。
- **外部化效果**（通过 `StepResult::Request` 处理）只能接受/返回 ABI 安全的基本类型。
- 如果你需要更丰富的有效载荷，当前的模式是将它们编码为 `bytes`（或 `string`）。

这是 v0 的一个刻意约束：它使嵌入保持简单，并使 `.rbc` 模块具有可移植性。

---

## 编译时原型（"存在什么"）

Rusk 被设计为可嵌入的：平台集成由宿主定义的接口提供。

有**两个不同的"注册"阶段**：

1. **编译器可见的原型**（名称 + 签名）：*编译*需要解析和类型检查调用。
2. **运行时实现**：*执行*需要成功。

如果你跳过（1），编译失败。如果你跳过（2），虚拟机在运行时调用缺失的导入时会陷阱。

### 宿主函数原型

宿主函数原型被分组到**宿主模块**中，并通过 `rusk_compiler::CompileOptions` 传递给编译。

示例：声明一个带有 `print` 和 `println` 的 `std` 模块：

```rust
use rusk_compiler::{
    CompileOptions, HostFnSig, HostFunctionDecl, HostModuleDecl, HostType, HostVisibility,
};

let mut options = CompileOptions::default();

options.register_host_module(
    "std",
    HostModuleDecl {
        visibility: HostVisibility::Public,
        functions: vec![
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "println".to_string(),
                sig: HostFnSig { params: vec![HostType::String], ret: HostType::Unit },
            },
        ],
    },
)?;
```

在 Rusk 程序中，这些像普通函数一样被调用：

```rusk
fn main() -> unit {
    std::println("hello from rusk");
}
```

#### 字节码的 ABI 安全性

对于你打算在 `rusk-vm` 上运行的程序，**宿主原型必须是 ABI 安全的**：

- 允许的：`unit`、`bool`、`int`、`float`、`string`、`bytes`
- 当前对于字节码 v0 不是 ABI 安全的：`any`、`typerep`、数组、元组和其他复合类型

如果程序调用的宿主导入不是 ABI 安全的，`compile_*_to_bytecode_with_options(...)` 会失败并显示类似以下的消息：

> `host import '<name>' is not ABI-safe for bytecode v0`

#### 推荐：使用 `rusk-host` 保持原型和实现对齐

本仓库提供 `rusk-host` 辅助工具，以避免"编译器认为存在什么"和"运行时实际提供什么"之间的差异。

例如，`rusk_host::std_io` 包含：

- `std_io::register_host_module(&mut CompileOptions)`（编译器原型）
- `std_io::install_vm(&ExecutableModule, &mut Vm)`（`rusk-vm` 的运行时实现）

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
use rusk_compiler::{CompileOptions, HostFnSig, HostType};

let mut options = CompileOptions::default();
options.register_external_effect(
    "TestFfi",
    "add",
    HostFnSig { params: vec![HostType::Int, HostType::Int], ret: HostType::Int },
)?;
```

#### ABI 安全性和签名匹配

重要注意事项：

- 对于字节码 v0，外部效果签名也必须是 ABI 安全的（与宿主导入相同的基本类型集）。编译器在编译为字节码时会拒绝非 ABI 安全的签名。
- 编译器当前**不会**类型检查你注册的外部效果签名是否与程序中的接口方法签名匹配。保持它们对齐（理想情况下在你的嵌入代码库中的一个地方定义它们）。

---

## 生成字节码模块

### 从源代码编译

最简单的嵌入方法是"编译然后运行"：

```rust
use rusk_compiler::{CompileOptions, compile_file_to_bytecode_with_options};
use rusk_host::std_io;
use std::path::Path;

let mut options = CompileOptions::default();
std_io::register_host_module(&mut options);

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

在运行时，你必须通过 `Vm::register_host_import` 提供实现。

#### 按名称安装

符合人体工程学的模式是：

1. 使用模块按名称查找 `HostImportId`。
2. 注册一个闭包（或任何 `HostFn` 实现）。

```rust
use rusk_vm::{AbiValue, HostError, Vm};

if let Some(id) = module.host_import_id("std::println") {
    vm.register_host_import(id, |args: &[AbiValue]| match args {
        [AbiValue::String(s)] => {
            println!("{s}");
            Ok(AbiValue::Unit)
        }
        other => Err(HostError { message: format!("std::println: bad args: {other:?}") }),
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

常见模式是从模块解析效果名称 + 签名，然后分发：

```rust
use rusk_vm::{AbiValue, StepResult, vm_drop_continuation, vm_resume, vm_step};

loop {
    match vm_step(&mut vm, None) {
        StepResult::Done { value } => break value,
        StepResult::Trap { message } => panic!("vm trapped: {message}"),
        StepResult::Yield { .. } => continue,
        StepResult::Request { effect_id, args, k } => {
            let decl = module
                .external_effect(effect_id)
                .unwrap_or_else(|| panic!("unknown external effect id {}", effect_id.0));

            let resume_value = match (decl.interface.as_str(), decl.method.as_str(), args.as_slice()) {
                ("TestFfi", "add", [AbiValue::Int(a), AbiValue::Int(b)]) => AbiValue::Int(a + b),
                other => {
                    let _ = vm_drop_continuation(&mut vm, k);
                    panic!("unhandled external effect request: {other:?}");
                }
            };

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
- **仅限 ABI 安全的参数/返回。** 使用 `bytes` 或 `string` 来编码复杂值。
- **每个虚拟机一个未完成的请求。** 在挂起时，虚拟机在宿主恢复或取消之前无法取得进展。

有关 Rusk 中效果和延续如何工作的更深入背景，请参见：
`docs/delimited-continuations-and-scoped-effects.md`。

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
  - 修复：通过 `register_host_module(...)` 在 `CompileOptions` 中注册函数的原型。

- **"host import … is not ABI-safe for bytecode v0"**
  - 症状：编译为字节码时失败。
  - 修复：对于打算在 `rusk-vm` 上运行的代码，将宿主函数签名限制为 ABI 安全的基本类型，或扩展 VM/ABI 以支持你需要的类型。

- **"external effect … has non-ABI-safe signature for bytecode v0"**
  - 症状：`register_external_effect(...)` 可以工作，但编译到字节码失败。
  - 修复：将外部效果签名限制为 ABI 安全的基本类型。

### 运行时陷阱

- **"missing host import implementation: `<name>`"**
  - 你编译了一个导入宿主函数的模块，但没有通过 `Vm::register_host_import` 注册它。

- **"vm is suspended; call resume/drop first"**
  - 你收到了 `StepResult::Request` 并在没有恢复/取消的情况下再次调用了 `vm_step`。

- **"vm re-entered during host call"**
  - 宿主导入尝试回调到虚拟机（直接或间接）。宿主导入必须是同步的叶操作。

- **"unhandled effect: Interface.method"**
  - 程序执行了一个没有语言内处理器的效果，并且它没有在模块中声明为外部化效果。
