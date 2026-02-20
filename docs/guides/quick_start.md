# Rusk 快速上手（Quick Start）

本指南面向第一次接触本仓库的读者，目标是 **10 分钟内跑通一个 Rusk 程序**，并了解日常开发最常用的命令与目录。

> 说明：本项目仍处于早期阶段，**不保证向后兼容**，规范与实现可能会快速演进。遇到差异时，以仓库中的实现与测试为准。

---

## 1. 构建仓库

你需要一套可用的 Rust 工具链（`cargo`）。

在仓库根目录：

```sh
cargo build
```

也可以快速检查：

```sh
cargo check
```

---

## 2. 运行第一个 Rusk 程序

运行一个单文件示例：

```sh
cargo run --bin rusk -- examples/01-hello-world.rusk
```

该示例大致长这样：

```rusk
fn main() {
    std::println("hello from rusk");
}
```

### 关于 `std::println`

在本仓库中，`std::print/std::println` 是由 `rusk` CLI 在启动 VM 时注册的 **宿主函数**（host functions），并不属于 `core`。

因此：

- 默认情况下直接可用（CLI 会加载 `sysroot/std` 并安装标准 I/O 宿主函数）。
- 如果你用 `--no-std` 禁用 `std`，这些函数就不可用。
- 如果你把 Rusk 嵌入到自己的应用里，需要自己注册对应的宿主函数（见 `docs/guides/host_integration.md`）。

---

## 3. 运行多文件模块示例

Rusk 支持 `mod` / `use` 的多文件模块结构。可以运行多文件示例：

```sh
cargo run --bin rusk -- examples/11-modules/main.rusk
```

---

## 4. 程序入口 `main` 与命令行参数

规范允许的入口形式是：

```rusk
fn main() -> unit { ... }
```

或者带参数的形式：

```rusk
fn main(argv: [string]) -> unit { ... }
```

其中 `argv` 是宿主传入的字符串数组：

- `argv[0]` 是正在执行的 `.rusk` 或 `.rbc` 的完整路径
- 后续元素是命令行参数（宿主需要保证它们是 UTF-8；必要时做 lossy 转换）

> 实现提示：当前 `rusk` CLI 也允许 `main` 返回非 `unit` 的值；如果返回值不是 `unit`，CLI 会把它打印出来，便于调试。

---

## 5. 编译为 `.rbc` 并运行

本仓库提供了一个简单的编译器前端二进制：`ruskc`，用于把 `.rusk` 编译为字节码模块 `.rbc`：

```sh
cargo run --bin ruskc -- examples/01-hello-world.rusk
```

然后直接运行生成的 `.rbc`：

```sh
cargo run --bin rusk -- examples/01-hello-world.rbc
```

### 注意：`.rbc` 版本兼容性

`.rbc` 格式带版本号；如果你拿到的 `.rbc` 是旧版本生成的，运行时可能会报类似：

> `unsupported rbc version ...`

这时用当前仓库的 `ruskc` 重新生成即可。

另外，仓库默认会忽略 `*.rbc`（它们通常是编译产物）。

---

## 6. sysroot / std 是什么？

Rusk 编译器会加载 `sysroot/` 目录作为“内建库”：

- `sysroot/core`：语言核心能力（`Option`、`Result`、`Iterator`、`Map`、`fmt::ToString` 等）
- `sysroot/std`：对宿主模块的源码包装层（在本仓库里提供 `std::print/std::println`）

默认 sysroot 搜索顺序大致是：

1. 环境变量 `RUSK_SYSROOT`
2. 当前工作目录下的 `./sysroot`
3. `rusk-compiler` crate 相对路径下的 `../../sysroot`

你也可以通过 CLI 显式指定：

```sh
cargo run --bin rusk -- --sysroot ./sysroot examples/01-hello-world.rusk
```

禁用 `std`：

```sh
cargo run --bin rusk -- --no-std examples/01-hello-world.rusk
```

---

## 7. 下一步看什么？

- 语言与语法总览：`docs/guides/concepts_and_syntax.md`
- TS/Python 迁移视角：
  - `docs/guides/for_typescript_developers.md`
  - `docs/guides/for_python_developers.md`
- 嵌入/宿主集成：`docs/guides/host_integration.md`
- 更深入的规范与设计：
  - `RUSK_SPEC.md`
  - `MIR_SPEC.md`
  - `BYTECODE_SPEC.md`
- 可运行示例：`examples/`（由浅入深）
