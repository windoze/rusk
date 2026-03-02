# Rusk (LSP)

为 [Rusk](https://github.com/windoze/rusk) 编程语言提供 VSCode 支持：

- 语法高亮（TextMate grammar）
- 基于 `rusk-lsp` 的 Language Server Protocol（LSP）功能

## 关于 Rusk

Rusk 是一个实验性编程语言和运行时，使用 Rust 实现。它结合了：

- **Rust 风格的语法**（块表达式、`match`、显式可变性控制）
- **TypeScript 风格的人体工程学**（类型推断、泛型、接口驱动的抽象）
- **代数效应**作为控制流扩展的一等机制（异常、async、生成器等）

## 功能特性

- 语法高亮：`.rusk` 文件关键字/操作符/注释/字符串等
- 代码折叠：支持基于缩进/括号的折叠，并支持 `// region` / `// endregion` 折叠标记
- LSP（由 `rusk-lsp` 提供，具体能力取决于服务器实现）：
  - diagnostics（错误提示）
  - completion（补全；当前主要是关键字补全）
  - document symbols（文档符号）
  - goto definition（跳转到定义；best-effort，已支持顶层符号 + 参数/泛型 + struct 字段 + 方法 + enum 变体等）
  - 未来将支持 hover / rename / find references 等（见仓库 `proposals/rusk-lsp.md`）

## 使用方法

### 1) 安装 / 构建语言服务器

本扩展是 **LSP 客户端**，需要本机能运行 `rusk-lsp`。

在 rusk 仓库根目录：

```sh
cargo build -p rusk-lsp
```

或全局安装：

```sh
cargo install --path crates/rusk-lsp
```

### 2) 配置 `rusk-lsp` 路径（可选）

- 默认会自动查找：
  - `workspace/target/debug/rusk-lsp` / `workspace/target/release/rusk-lsp`
  - 或 PATH 中的 `rusk-lsp`
- 也可以在 VSCode 设置中手动指定：
  - `rusk.lsp.serverPath`

### 3) 打开 `.rusk` 文件

打开任何 `.rusk` 文件即可获得语法高亮，并启动 `rusk-lsp` 提供诊断等语言功能。

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
- [rusk-lsp 文档](../../docs/rusk-lsp.zh.md)

## 许可证

与 Rusk 项目保持一致。
