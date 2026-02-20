# 宿主集成：把 Rusk 当作嵌入语言（Host Integration）

本指南面向“把 Rusk 嵌入到自己应用里”的读者：你可能在做 CLI、编辑器插件、服务端脚本、游戏逻辑、规则引擎等，希望让 Rusk 代码在你的进程里运行，并调用你提供的 API。

本仓库已经有一份更完整的嵌入 API 文档：`docs/embedding-vm.zh.md`。本指南提供一个更“落地/最小可用”的路线图，并解释最关键的概念差异。

---

## 1. 术语与组件（你需要知道的 4 个 crate）

从流程上看：

`Rusk 源码 (.rusk)` → `字节码模块 ExecutableModule` → `VM 执行`

对应到仓库 crate：

- `rusk-compiler`：解析/类型检查/降级到字节码（可直接编译文件到 `ExecutableModule`）
- `rusk-bytecode`：字节码模块数据结构、校验、`.rbc` 编解码
- `rusk-vm`：字节码虚拟机（可步进驱动）
- `rusk-host`：可选的“宿主集成辅助”模块（例如标准 I/O 的注册与安装）

---

## 2. 两种“把能力给脚本”的方式：宿主函数 vs 外部化 effect

Rusk 有两条能力注入路径，适用场景不同：

### 2.1 宿主函数（host imports）

像普通函数一样调用，例如：

```rusk
fn main() {
    std::println("hello");
    ()
}
```

这里的 `std::println` 在本仓库里是 **宿主函数导入**。要让它可用，你需要做两件事：

1. **编译期**：把模块与签名注册给编译器（让类型检查知道“存在什么”）
2. **运行期**：把同名/同签名的实现安装到 VM（让调用真的能执行）

推荐做法：复用 `rusk-host` 提供的配套工具（见下文）。

### 2.2 外部化 effect（外部效果）

Rusk 的 effect 调用是 `@Interface.method(...)`。如果 VM 运行时遇到 effect 且语言内没有 handler，默认会陷阱。

如果你希望 **由宿主来处理某些 effect**，可以在编译时把它们注册为“外部化 effect”。这样 VM 在运行时会把请求“冒泡”到宿主，表现为：

```rust
StepResult::Request { effect_id, args, ... }
```

宿主拿到请求后，可以：

- 计算一个返回值并 `resume`
- 或直接拒绝/取消（让脚本失败）

这种方式非常适合把“平台能力/特权操作”封装成 effect：例如文件系统、网络、数据库、UI、权限系统等。

---

## 3. ABI 边界：哪些值可以跨 VM/宿主边界？

当前字节码 v0 的 ABI 表面是刻意做小的：跨 VM/宿主边界只允许这些基础类型：

- `unit`
- `bool`
- `int`
- `float`
- `string`（UTF-8）
- `bytes`（字节序列）

这意味着：

- 宿主函数的参数/返回值必须是上述类型（否则编译为字节码会失败）
- 外部化 effect 的参数/返回值也必须是上述类型
- 如果你需要传更复杂的数据，一般做法是编码为 `bytes` 或 `string`（例如 JSON/msgpack/protobuf 等）

---

## 4. 最小嵌入流程（Rust 侧）

下面是一段“最小闭环”的宿主伪代码：编译 → 创建 VM → 安装宿主导入 → 步进执行。

```rust
use rusk_compiler::{CompileOptions, compile_file_to_bytecode_with_options};
use rusk_host::std_io;
use rusk_vm::{Vm, StepResult, vm_step};
use std::path::Path;

fn run_file(path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    // 1) 编译期：配置 sysroot / std / 宿主模块原型
    let mut options = CompileOptions::default();
    std_io::register_host_module(&mut options); // 提供 std::print/std::println 的“原型”

    let module = compile_file_to_bytecode_with_options(path, &options)?;

    // 2) 创建 VM（main() 或 main(argv: [string]) 都支持）
    let mut vm = Vm::new(module.clone());

    // 3) 运行期：安装宿主函数实现（与上面注册的原型要对齐）
    std_io::install_vm(&module, &mut vm);

    // 4) 步进驱动执行
    loop {
        match vm_step(&mut vm, None) {
            StepResult::Done { value } => {
                eprintln!("done: {value:?}");
                return Ok(());
            }
            StepResult::Trap { message } => {
                return Err(format!("runtime trap: {message}").into());
            }
            StepResult::Request { effect_id, args, .. } => {
                // 如果你注册了外部化 effects，这里会收到请求（见下一节）
                return Err(
                    format!("unhandled external effect: id={effect_id:?} args={args:?}").into(),
                );
            }
            StepResult::Yield { .. } => {
                return Err("unexpected yield".into());
            }
        }
    }
}
```

> 实际项目里你通常会：
> - 缓存编译结果（`.rbc`）避免每次都编译
> - 为每次执行创建新的 VM 或复用 VM（取决于你的隔离需求）
> - 对不可信脚本加上超时/步数限制/宿主能力白名单（见后文安全建议）

---

## 5. 如何提供宿主函数（host imports）

宿主函数需要“编译期原型 + 运行期实现”两步。

### 5.1 编译期：注册模块原型

你可以直接使用 `CompileOptions::register_host_module(...)` 注册模块与函数签名；更推荐用 `rusk-host` 保持声明与实现一致。

示例（更完整的写法见 `docs/embedding-vm.zh.md`）：

```rust
use rusk_compiler::{CompileOptions, HostFnSig, HostFunctionDecl, HostModuleDecl, HostType, HostVisibility};

options.register_host_module(
    "my_api",
    HostModuleDecl {
        visibility: HostVisibility::Public,
        functions: vec![
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "add".to_string(),
                sig: HostFnSig { params: vec![HostType::Int, HostType::Int], ret: HostType::Int },
            },
        ],
    },
)?;
```

然后 Rusk 侧就可以写：

```rusk
fn main() -> int {
    my_api::add(1, 2)
}
```

### 5.2 运行期：安装实现

运行时安装的方式取决于你选择的集成层（`rusk-host` 或自行实现）。核心思想是：

- 字节码模块里会记录“宿主导入列表”
- VM 在执行到 `call_host` 时，会通过导入 id 分发到宿主实现

最简单的做法是模仿 `rusk_host::std_io` 的实现模式：

- `*_::register_host_module(&mut CompileOptions)`：注册声明
- `*_::install_vm(&ExecutableModule, &mut Vm)`：把实现装到 VM 上

---

## 6. 如何把 effect 外部化到宿主（StepResult::Request）

### 6.1 Rusk 侧：声明一个 effect 接口并调用

```rusk
interface HostFs {
    fn read_file(path: string) -> bytes;
}

fn main() -> int {
    let data = @HostFs.read_file("hello.txt");
    data.len()
}
```

### 6.2 编译期：注册外部化 effect 的签名

```rust
use rusk_compiler::{CompileOptions, HostFnSig, HostType};

options.register_external_effect(
    "HostFs",
    "read_file",
    HostFnSig { params: vec![HostType::String], ret: HostType::Bytes },
)?;
```

### 6.3 运行期：处理 `StepResult::Request`

宿主在 `vm_step` 的返回值里拿到请求，并根据 `effect_id` 分发：

- 读取 args（都是 ABI 值：`AbiValue::String`/`AbiValue::Bytes`/等）
- 计算返回值
- 恢复 VM（“把值送回 continuation”）

具体的恢复 API 在 `docs/embedding-vm.zh.md` 有详细说明；核心概念是：宿主要么 **resume** 继续运行，要么让脚本失败（trap/取消）。

---

## 7. sysroot 打包与路径管理

嵌入时最常见的问题之一是：“VM 运行环境里 sysroot 在哪？”

你有几种选择：

1. **让宿主提供 sysroot 路径**（开发期简单）：通过 `CompileOptions.sysroot` 或环境变量 `RUSK_SYSROOT` 指定。
2. **把 sysroot 随应用打包**：例如放在应用资源目录中。
3. **更激进**：把 sysroot 当作内建资源（需要你在宿主侧提供文件加载逻辑，当前参考实现以目录为主）。

---

## 8. `.rbc`：缓存、分发与版本管理

`.rbc` 是稳定编码的字节码模块格式（带版本号）。典型用法：

- 开发期：`.rusk` → `.rbc`（缓存）
- 生产期：分发 `.rbc`，宿主直接加载运行（更快、也更可控）

要点：

- `.rbc` 格式有版本号；**旧版本 `.rbc` 可能无法被新 VM 加载**。
- 因此建议：
  - 在你的缓存 key 里带上版本信息（或 VM/编译器版本）
  - 版本不匹配时自动重新编译

编解码 API 位于 `rusk_bytecode::{to_bytes, from_bytes}`。

---

## 9. 运行不可信脚本的安全建议（非常重要）

Rusk 运行时的安全边界主要在“宿主给了什么能力”：

- **默认不要暴露** 文件系统/网络/进程/反射等高权限能力
- 对暴露的 API 做参数校验与权限控制
- 对执行加上限制：
  - 超时（wall time）
  - 指令步数/燃料（fuel）
  - 内存上限（如果你的宿主可控）
- 将错误（trap/request/类型不匹配）视为“脚本失败”，不要影响宿主稳定性

---

## 10. 进一步阅读（强烈推荐）

- 完整嵌入 API：`docs/embedding-vm.zh.md`
- effects/continuation 语义：`docs/delimited-continuations-and-scoped-effects.zh.md`
- examples（可运行）：`examples/`（尤其是 effects 与 host 相关示例）
