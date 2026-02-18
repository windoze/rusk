# Rusk 示例

本目录包含小型的、循序渐进的 Rusk 程序，从基础到高级排序。

## 运行方式

从仓库根目录：

```sh
cargo run --bin rusk -- examples/01-hello-world.rusk
cargo run --bin rusk -- examples/10-modules/main.rusk
```

注意事项：

- 许多示例使用 `std::print` / `std::println`。在本仓库中，这些函数是由 `rusk` CLI 二进制文件注册的**宿主函数**（不是 `core` 的一部分），因此它们在其他嵌入环境中可能不存在，除非你注册它们。
- 语言规范在 `RUSK_SPEC.md` 中，MIR 规范在 `MIR_SPEC.md` 中。

## 索引

- `01-hello-world.rusk`：通过 `std::print` / `std::println` 进行打印
- `02-values-and-functions.rusk`：值、函数定义、返回值
- `03-control-flow-if-match.rusk`：`if` 和 `match` 作为表达式
- `04-arrays-and-for-loops.rusk`：数组、索引、`for` 循环
- `05-strings-and-fmt.rusk`：字符串和 `f"..."` 格式化字符串
- `06-structs-and-methods.rusk`：结构体 + 固有方法（`impl Type { ... }`）
- `07-enums-and-patterns.rusk`：枚举和模式匹配
- `08-closures-and-functions-as-values.rusk`：lambda/闭包和 `fn(...) -> ...` 值
- `09-generics.rusk`：泛型结构体 + 泛型函数
- `10-modules/`：使用 `mod` 和 `use` 的多文件示例
- `11-interfaces-and-dynamic-dispatch.rusk`：接口、`as Interface`、动态分发
- `12-effects-basic.rusk`：代数效应 + `match` 效应处理器
- `13-effects-typed.rusk`：类型化（泛型）效应
- `14-type-tests-is-asq.rusk`：使用 `is` 和 `as?` 进行运行时类型测试
- `15-effects-generator.rusk`：使用代数效应实现生成器，演示 `yield` 和 `resume`
- `16-fixed-point-combinator.rusk`：Y-组合子和不动点递归，展示泛型和高阶函数的能力
- `17-effects-state-management.rusk`：类 React 状态管理，展示如何用 effects 克服 `useEffect` 的限制
- `18-newtype-structs.rusk`：新类型结构体（名义包装器）用于类型安全
- `19-destructuring-patterns.rusk`：在 `let`/`const`/`readonly` 语句中使用解构模式
- `20-associated-types.rusk`：接口中的关联类型，展示 `Self::Item` 和限定投影
- `21-byte-char-slicing.rusk`：`byte` 和 `char` 原始类型，零拷贝字符串/字节切片
- `22-option-methods.rusk`：`Option` 方法（`map`、`and_then`、`unwrap_or` 等）
