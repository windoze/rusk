# Rusk (LSP)

为 [Rusk](https://github.com/windoze/rusk) 编程语言提供 VSCode 支持：

- 语法高亮（TextMate grammar）
- 基于 `rusk-lsp` 的 Language Server Protocol（LSP）功能
- 基于 **tree-sitter-rusk** 的增量语法树（用于结构导航）

## 关于 Rusk

Rusk 是一个实验性编程语言和运行时，使用 Rust 实现。它结合了：

- **Rust 风格的语法**（块表达式、`match`、显式可变性控制）
- **TypeScript 风格的人体工程学**（类型推断、泛型、接口驱动的抽象）
- **代数效应**作为控制流扩展的一等机制（异常、async、生成器等）

## 功能特性

- 语法高亮：`.rusk` 文件关键字/操作符/注释/字符串等
- 代码折叠：支持基于缩进/括号的折叠，并支持 `// region` / `// endregion` 折叠标记
- 结构导航（tree-sitter，增量解析）：
  - Explorer 侧边栏提供 `Rusk Outline` 视图（类/枚举/接口/impl/方法/字段等）
  - `Rusk: Go to Symbol (Tree-sitter)` 命令（当前文件内符号快速跳转）
  - 当 `rusk-lsp` 未运行/启动失败时，tree-sitter 还会作为 fallback 为 VSCode 内置 Outline/Breadcrumbs 提供符号
- LSP（由 `rusk-lsp` 提供，具体能力取决于服务器实现）：
  - diagnostics（错误提示）
  - completion（补全；当前主要是关键字补全）
  - document symbols（文档符号）
  - goto definition（跳转到定义；best-effort，已支持顶层符号 + 参数/泛型 + struct 字段 + 方法 + enum 变体等）
  - hover（悬浮提示；best-effort：表达式类型 + 可解析到定义时的 doc comment）
  - 未来将支持 rename / find references 等（见仓库 `proposals/rusk-lsp.md`）

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

提示：
- 如果不配置 `rusk.lsp.entryFiles`，`rusk-lsp` 默认使用“当前打开/修改的文件”作为诊断入口。
  当你打开的是 sysroot 文件（`sysroot/core/*` / `sysroot/std/*`）时，服务器会避免把它当作入口，
  以免 std/core 被重复注入导致假阳性错误；此时会沿用最近一次的非 sysroot 文件作为入口。
- 对于多文件工程，建议显式配置 `rusk.lsp.entryFiles` 指向真正的入口文件（例如 `main.rusk`），
  这样打开任意模块文件都能得到更稳定的诊断结果。

示例代码：

```rusk
fn main() -> int {
    let xs = [10, 20, 30];
    xs[1] = 99;
    xs[1]
}
```

## 开发：更新 tree-sitter 解析器（可选）

本扩展内置了 `tree-sitter/rusk.wasm`（由仓库内 `misc/tree-sitter-rusk` 生成），用于增量解析与结构导航。

当你修改了 `misc/tree-sitter-rusk/grammar.js` 后，可以在本目录执行：

```sh
npm run build:tree-sitter-wasm
```

## 相关链接

- [Rusk 项目主页](https://github.com/windoze/rusk)
- [Rusk 语言规范](https://github.com/windoze/rusk/blob/main/RUSK_SPEC.md)
- [rusk-lsp 文档](../../docs/rusk-lsp.zh.md)

## 许可证

与 Rusk 项目保持一致。
