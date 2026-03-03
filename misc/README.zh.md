# Misc（杂项）

此目录包含 Rusk 编程语言的编辑器支持和辅助工具。

## 目录内容

- `rusk-lang`: Rusk 的 VSCode 扩展（语法高亮 + `rusk-lsp` LSP 客户端）
- `rusk.sublime-syntax`: Rusk 的 Sublime Text 语法定义
- `tree-sitter-rusk`: Rusk 的 Tree-sitter 语法（用于编辑器中的结构化导航）

## 构建与安装

### 前置要求

- **Rust 工具链**（用于构建 Rusk 二进制文件）
- **Node.js 和 npm**（用于构建 VSCode 扩展）

### 步骤 1：构建所需的二进制文件

在 **Rusk 仓库根目录**中，构建必要的可执行文件：

```sh
# 构建主要的 Rusk CLI（用于代码格式化）
cargo build --release --bin rusk

# 构建语言服务器协议（LSP）服务器
cargo build --release -p rusk-lsp
```

或者，你也可以全局安装：

```sh
# 全局安装
cargo install --path . --bin rusk
cargo install --path crates/rusk-lsp
```

构建完成后：
- `rusk` 二进制文件位于 `target/release/rusk`（如果全局安装则在系统 PATH 中）
- `rusk-lsp` 二进制文件位于 `target/release/rusk-lsp`（如果全局安装则在系统 PATH 中）

### 步骤 2：构建 VSCode 扩展

进入 VSCode 扩展目录：

```sh
cd misc/rusk-lang
```

安装依赖并编译 TypeScript：

```sh
# 安装 Node.js 依赖
npm install

# 编译 TypeScript 源代码
npm run compile
```

### 步骤 3：安装扩展

你有两种方式安装 VSCode 扩展：

#### 方式 A：打包并安装（推荐）

安装 `vsce`（Visual Studio Code 扩展管理器）：

```sh
npm install -g @vscode/vsce
```

打包扩展：

```sh
vsce package
```

这会创建一个 `.vsix` 文件。在 VSCode 中安装它：

```sh
code --install-extension rusk-lang-0.1.0.vsix
```

或者手动安装：打开 VSCode → 扩展 → "..." 菜单 → "从 VSIX 安装..." → 选择 `.vsix` 文件。

#### 方式 B：开发模式

用于开发和测试，你可以直接运行扩展：

1. 在 VSCode 中打开 `misc/rusk-lang` 文件夹
2. 按 `F5` 启动一个加载了该扩展的新 VSCode 窗口

### 步骤 4：配置 VSCode（可选）

扩展会自动在以下位置搜索 `rusk` 和 `rusk-lsp` 二进制文件：
- `workspace/target/debug/` 或 `workspace/target/release/`
- 系统 PATH

如有需要，你可以在 VSCode 设置中手动指定路径：

- `rusk.lsp.serverPath`: `rusk-lsp` 可执行文件路径
- `rusk.fmt.ruskPath`: `rusk` 可执行文件路径（用于格式化）

### 使用方法

安装完成后：
1. 打开任意 `.rusk` 文件即可激活语法高亮
2. LSP 服务器会自动启动，提供：
   - 诊断（错误检查）
   - 代码补全
   - 跳转到定义
   - 悬浮提示
   - 文档符号
3. 使用 **格式化文档**（或启用保存时格式化）来用 `rusk fmt` 格式化代码

## 故障排查

- **扩展未激活**：检查 VSCode 的输出面板 → "Rusk (LSP)" 查看错误消息
- **LSP 服务器未启动**：确认 `rusk-lsp` 已构建且可访问；检查设置中配置的路径
- **格式化不工作**：确认 `rusk` 二进制文件已构建且可访问

有关 VSCode 扩展功能和配置的更多详情，请参阅 [rusk-lang/README.md](rusk-lang/README.md)。
