# Rusk 示例

本目录包含一组由浅入深的小程序，覆盖 Rusk 的主要语言特性与 `core`/`std` 核心库能力。

## 运行方式

从仓库根目录执行：

```sh
# 直接运行 .rusk 源码（会在运行前编译）
cargo run --bin rusk -- examples/01-hello-world.rusk

# 运行多文件模块示例
cargo run --bin rusk -- examples/11-modules/main.rusk

# 直接运行已编译的 .rbc 字节码模块
# 注意：仓库默认会忽略 `*.rbc`（它们通常是编译产物），如果你本地没有该文件可以先生成：
#   cargo run --bin ruskc -- examples/01-hello-world.rusk
cargo run --bin rusk -- examples/01-hello-world.rbc
```

注意事项：

- 大多数示例会使用 `std::print` / `std::println`。在本仓库中它们是由 `rusk` CLI 注册的**宿主函数**
  （不属于 `core`），并且 `std` 只是一个对宿主模块的源码包装层。
  - 默认情况下，`rusk` 会加载 `std` 并安装标准 I/O 宿主函数。
  - 使用 `--no-std` 运行时将不会加载 `std`，这些函数也就不可用。
- 语言规范见 `RUSK_SPEC.md`，MIR 规范见 `MIR_SPEC.md`，字节码规范见 `BYTECODE_SPEC.md`。
- `25-host-stored-continuations.rusk` 需要额外的宿主模块 `host`（默认 CLI 不提供），用于演示更底层的
  VM/宿主交互；其余示例默认都可以直接运行。

## 索引（由易到难）

- `01-hello-world.rusk`：最小程序、调用宿主 I/O（`std::print` / `std::println`）
- `01-hello-world.rbc`：与上面等价的字节码模块（展示 `.rbc` 运行入口；可用 `ruskc` 生成）
- `02-values-and-functions.rusk`：基本值、函数定义、返回值、类型标注
- `03-control-flow-if-match.rusk`：`if` / `match` 作为表达式（有值的控制流）
- `04-arrays-and-for-loops.rusk`：数组、索引、`for` / `while`、`break` / `continue`
- `05-strings-and-fmt.rusk`：字符串、`f"..."` 格式化、`core::fmt::ToString`
- `06-structs-and-methods.rusk`：结构体与固有方法（`impl Type { ... }`）
- `07-enums-and-patterns.rusk`：枚举、模式匹配、用 `Option` 表达“可能缺失”
- `08-destructuring-patterns.rusk`：`let`/`const`/`readonly` 解构、rest 模式
- `09-closures-and-functions-as-values.rusk`：闭包/函数值、高阶函数、捕获变量
- `10-generics.rusk`：泛型结构体 + 泛型函数（参数化多态）
- `11-modules/`：多文件程序：`mod` / `use` / 别名
- `12-interfaces-and-dynamic-dispatch.rusk`：接口（`interface`）、实现（`impl`）、动态分发（`as Interface`）
- `13-associated-types.rusk`：关联类型（`Self::Item` / 限定投影）、迭代器接口示例
- `14-type-tests-is-asq.rusk`：运行时类型测试：`is` 与安全向下转型 `as?`
- `15-newtype-structs.rusk`：newtype（名义包装器）与类型安全
- `16-core-map.rusk`：`core::map::Map`：插入/查找/删除、哈希与相等性约束
- `17-byte-char-slicing.rusk`：`byte`/`char`、`bytes`/`string` 的零拷贝切片
- `18-option-methods.rusk`：`Option` 的常用组合子（`map` / `and_then` / `unwrap_or` 等）
- `19-fixed-point-combinator.rusk`：不动点/“Y 组合子”思想：用高阶函数实现递归
- `20-effects-basic.rusk`：代数效应：`@Interface.method(...)` + `match` 处理器 + `resume`
- `21-effects-typed.rusk`：类型化（泛型）效应与显式类型实参
- `22-effects-generator.rusk`：用效应实现生成器：`yield` / `resume`
- `23-effects-state-management.rusk`：更大示例：用效应建模状态与清理（类 React）
- `24-result-try-throw.rusk`：`core::result`：基于效应的 `try`/`throw`/`catch`/`finally`
- `25-host-stored-continuations.rusk`：高级：把 continuation 句柄交给宿主保存与恢复
- `26-std-json-serde.rusk`：`std::json`：配合 `derive Serialize + Deserialize` 做 JSON 序列化/反序列化
