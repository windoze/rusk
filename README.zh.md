# Rusk

Rusk 是一个实验性的编程语言和运行时，使用 Rust 实现。它的目标是结合：

- **类 Rust 语法**（块表达式、`match`、显式可变性控制），以及
- **类 TypeScript 人体工程学**（类型推断、泛型、接口驱动的抽象），还有
- **代数效应**作为一等机制用于控制流扩展（异常、async、生成器等）。

本仓库是一个参考实现，将 `.rusk` 源代码编译成一个小型的中级 IR（"MIR"），然后使用解释器执行该 MIR。

Rusk 设计为可嵌入式（CLI、WASM、嵌入式设备），因此 I/O 和平台集成通过**宿主函数**提供：

- **编译器**在编译前会获得宿主*原型*（名称 + 签名），以便名称解析和类型检查能够成功。
- **解释器**在运行时会获得具体的宿主实现；如果 MIR 声明的宿主导入未安装，它将在执行前报错。

在本仓库中，`rusk` CLI 注册了一个最小的宿主定义的 `std` 模块，包含 `std::print(string) -> unit` 和 `std::println(string) -> unit`。

## 语言特性

源语言在 [`RUSK_SPEC.md`](RUSK_SPEC.md) 中规定（当前版本 v0.4）。MIR 在 [`MIR_SPEC.md`](MIR_SPEC.md) 中规定（当前版本 v0.2 草案）。

一个简单的示例：

```rusk
fn main() -> int {
    let xs = [10, 20, 30];
    xs[1] = 99;
    xs[1]
}
```

## 项目结构

- `src/main.rs`：`rusk` CLI（`rusk <file.rusk|file.mir>`）
- `crates/rusk-compiler/`：解析器/类型检查器 + 从 Rusk 到 MIR 的降级
- `crates/rusk-mir/`：MIR 数据结构（以及可选的序列化）
- `crates/rusk-interpreter/`：MIR 解释器 + GC + 核心运行时内建函数
- `crates/rusk-host/`：可重用的宿主模块声明 + 安装器（例如 `std::print`）
- `fixtures/` 和 `tests/`：可执行的测试夹具和回归测试

## 快速开始

```sh
cargo run --bin rusk -- fixtures/020_arrays_get_set.rusk
cargo test
```
