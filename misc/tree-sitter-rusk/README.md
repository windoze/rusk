# tree-sitter-rusk

Rusk 编程语言的 tree-sitter 语法（parser + queries）。

本目录的目标：
- 提供一个可生成的 tree-sitter grammar（`grammar.js`）
- 提供基础的 highlight / fold queries（`queries/*.scm`）
- 提供 corpus 测试样例（`corpus/*.txt`），便于后续迭代语法

> 说明：本仓库目前以 **Rusk 编译器实现**（`crates/rusk-compiler`）为准；该 grammar 目标是
> “足够覆盖真实代码（sysroot / fixtures / examples）并适合编辑器高亮/折叠”，而非严格等价于
> 编译器的所有语义限制（例如某些上下文中的歧义会由编译器进一步拒绝）。

## 目录结构

- `grammar.js`：tree-sitter 语法定义
- `src/scanner.c`：外部 scanner（用于**可嵌套的** `/* ... */` 块注释）
- `queries/highlights.scm`：语法高亮捕获
- `queries/folds.scm`：代码折叠规则
- `corpus/`：tree-sitter 测试语料
- `tree-sitter.json`：tree-sitter 0.24+ 的 grammar 元数据配置（scope / file-types / queries）

## 生成 / 测试（本地）

需要安装 Node.js（建议 18+）。

```bash
cd misc/tree-sitter-rusk

# 安装 tree-sitter CLI（仅本目录）
npm install --no-audit --no-fund

# 生成 C parser（会写入 src/parser.c / src/node-types.json 等）
npm run generate
# 或：npx tree-sitter generate

# 运行 corpus 测试
npm test
# 或：npx tree-sitter test
```

## VSCode 后续集成（计划）

后续可以在 VSCode 扩展中使用 tree-sitter（或 WASM 版 parser）来增强：
- 更稳健的语法高亮（尤其是 f-string 插值 `{ ... }` 内的表达式）
- 基于语法树的代码折叠、结构导航等
