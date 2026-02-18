# Rusk Language Support

为 [Rusk](https://github.com/windoze/rusk) 编程语言提供 VSCode 语法高亮支持。

## 关于 Rusk

Rusk 是一个实验性编程语言和运行时，使用 Rust 实现。它结合了：

- **Rust 风格的语法**（块表达式、`match`、显式可变性控制）
- **TypeScript 风格的人体工程学**（类型推断、泛型、接口驱动的抽象）
- **代数效应**作为控制流扩展的一等机制（异常、async、生成器等）

## 功能特性

- 为 `.rusk` 文件提供语法高亮
- 支持 Rusk 语言的关键字、操作符和语法结构
- 自动识别 `.rusk` 文件扩展名

## 使用方法

安装此扩展后，打开任何 `.rusk` 文件即可自动获得语法高亮支持。

示例代码：

```rusk
fn main() -> int {
    let xs = [10, 20, 30];
    xs[1] = 99;
    xs[1]
}
```

## 相关链接

- [Rusk 项目主页](https://github.com/windoze/rusk)
- [Rusk 语言规范](https://github.com/windoze/rusk/blob/main/RUSK_SPEC.md)

## 许可证

与 Rusk 项目保持一致。
