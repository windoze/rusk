# Rusk

![Rusk logo](docs/assets/Rusk-256x256.png)

Rusk 是一个实验性的编程语言和运行时，使用 Rust 实现。它的目标是结合：

- **类 Rust 语法**（块表达式、`match`、显式可变性控制），以及
- **类 TypeScript 人体工程学**（类型推断、泛型、接口驱动的抽象），还有
- **代数效应**作为一等机制用于控制流扩展（异常、async、生成器等）。

本仓库是一个参考实现：将 `.rusk` 源代码编译成由中级 IR（"MIR"）降级得到的紧凑字节码模块（`.rbc`），然后用一个小型 VM 执行该字节码。

Rusk 设计为可嵌入式（CLI、WASM、嵌入式设备），因此 I/O 和平台集成通过**宿主函数**提供：

- 宿主导入在 **Rusk 源码中**用 `extern fn` 声明（可以写在程序里，也可以写在 sysroot 的模块里，例如 `std`），以便名称解析与类型检查能够成功。
- **运行时**（VM）在运行时会获得具体的宿主实现；如果模块声明的宿主导入未安装，它会在执行时 trap。

在本仓库中，sysroot 的 `std` 模块用 `extern fn` 声明了 `std::print(string) -> unit` 与 `std::println(string) -> unit`，`rusk` CLI 会通过 `rusk_host::std_io::install_vm` 安装对应实现。

## 语言特性

源语言在 [`RUSK_SPEC.md`](RUSK_SPEC.md) 中规定（当前版本 v0.4）。MIR 在 [`MIR_SPEC.md`](MIR_SPEC.md) 中规定（当前版本 v0.2 草案）。字节码在 [`BYTECODE_SPEC.md`](BYTECODE_SPEC.md) 中规定（当前版本 v0.1 草案）。

一个简单的示例：

```rusk
fn main() -> int {
    let xs = [10, 20, 30];
    xs[1] = 99;
    xs[1]
}
```

## 项目结构

- `src/main.rs`：`rusk` CLI（`rusk <file.rusk|file.rbc>`）
- `src/bin/ruskc.rs`：`ruskc` 编译器 CLI（输出 `.rbc`）
- `crates/rusk-compiler/`：解析器/类型检查器 + 从 Rusk 到字节码的降级（内部会经过 MIR）
- `crates/rusk-mir/`：编译器内部的 MIR 数据结构（不是运行时后端）
- `crates/rusk-bytecode/`：字节码模块 + `.rbc` 序列化 + verifier
- `crates/rusk-vm/`：字节码 VM 运行时（step API、宿主导入、效应）
- `crates/rusk-host/`：可选的宿主集成辅助（主要用于在运行时安装宿主导入实现，例如 `std::print`）
- `crates/rusk-lsp/`：Language Server Protocol（LSP）语言服务器，用于编辑器集成
- `fixtures/` 和 `tests/`：可执行的测试夹具和回归测试

## 快速开始

```sh
cargo run --bin rusk -- fixtures/020_arrays_get_set.rusk
cargo run --bin ruskc -- fixtures/020_arrays_get_set.rusk
cargo run --bin rusk -- fixtures/020_arrays_get_set.rbc
cargo run --bin rusk -- fmt --check fixtures/020_arrays_get_set.rusk
cargo run --bin rusk -- lint fixtures/020_arrays_get_set.rusk
cargo test
```

## 文档

完整的指南和教程请参阅 **[docs/guides/](docs/guides/)** 目录：

- [快速上手指南](docs/guides/quick_start.md) — 快速上手，跑通示例与常用命令
- [概念与语法](docs/guides/concepts_and_syntax.md) — 语言概念与语法总览（表达式、类型、接口、effects、core 库）
- [TypeScript 开发者指南](docs/guides/for_typescript_developers.md) — 从 TypeScript 迁移到 Rusk
- [Python 开发者指南](docs/guides/for_python_developers.md) — 从 Python 迁移到 Rusk
- [宿主集成](docs/guides/host_integration.md) — 把 Rusk 嵌入到你的应用里（宿主函数、外部化 effects、ABI、sysroot）

更多资源：

- [嵌入 VM](docs/embedding-vm.zh.md) — VM 集成的详细指南
- [限定延续与作用域效应](docs/delimited-continuations-and-scoped-effects.zh.md) — 深入解释 Rusk 的效应系统
- [Rusk LSP](docs/rusk-lsp.zh.md) — 通过 LSP 进行编辑器集成
