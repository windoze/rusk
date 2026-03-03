# rusk-lsp（Rusk 语言服务器）

`rusk-lsp` 是 Rusk 的官方 Language Server Protocol（LSP）实现。

它直接复用 `rusk-compiler` 的前端（模块加载、解析、名称解析、类型检查），通过 stdio
向编辑器提供语言功能。

## 状态 / 功能

当前 MVP 已支持：

- 在 `didOpen` / `didChange` / `didSave` 时发布诊断信息
  - 支持 `mod foo;` 的多文件模块树
  - 支持未保存的编辑器缓冲区（overlay source provider，无需落盘）
  - 将编译器内部的 UTF-8 字节 `Span` 转换为 LSP 需要的（0-based、UTF-16 code units）范围
- `textDocument/documentSymbol`（顶层符号）
  - 若符号上方存在 doc comment（`/// ...`），会把摘要写入 `DocumentSymbol.detail`
- `textDocument/completion`（关键字补全）
- `textDocument/hover`（best-effort：悬浮提示）
  - 对具备类型信息的表达式显示推断类型
  - 当悬浮的名字可解析到带 doc comment 的定义时，会显示对应注释（`/// ...`）
- `textDocument/definition`（best-effort：导航跳转）
  - 顶层 item（函数 / struct / enum / interface / module）
  - 函数参数与泛型参数
  - struct 字段（`s.a`、`S { a: ... }`）
  - inherent method（`s.m()`、`Type::m()`）
  - enum 变体（`E::V`、`E::V(...)`、match pattern）
  - `@I.m(...)` 中的 effect/interface 方法

后续计划（见 `proposals/rusk-lsp.md`）：

- 更准确的 go-to-definition（基于语义解析/作用域，处理 shadowing 等）
- 更好的错误累计（同一文件多个诊断）

## 运行方式

在仓库内直接运行：

```sh
cargo run -p rusk-lsp --bin rusk-lsp
```

本地安装：

```sh
cargo install --path crates/rusk-lsp
```

日志使用 `RUST_LOG` 控制（例如 `RUST_LOG=info`）。

## 配置

### CLI 参数

- `--entry <path>`：指定入口文件（可重复）
- `--sysroot <path>`：指定 sysroot 目录
- `--no-std`：禁用加载 `sysroot/std`

### `initializationOptions`

编辑器也可以通过 `initialize.initializationOptions` 传入配置（camelCase）：

```json
{
  "entryFiles": ["path/to/main.rusk"],
  "sysroot": "path/to/sysroot",
  "noStd": false
}
```

如果未提供 `entryFiles`（或为空），`rusk-lsp` 会把当前打开/修改的文档当作诊断入口。

注意：当当前文档位于 sysroot（例如 `sysroot/core/*.rusk` / `sysroot/std/*.rusk`）时，
`rusk-lsp` 不会把该文件当作诊断入口（否则会导致 sysroot 被注入两次，出现诸如
“module file ... loaded multiple times”、“`super` at crate root” 等假阳性错误）。
此时会沿用最近一次触发过诊断的“非 sysroot 文件”作为入口；若尚未存在该入口，则仅清空
sysroot 文件上的诊断并跳过本次重分析。

## 示例

`examples/lsp/` 提供了一个最小的多文件示例（`main.rusk` + `foo.rusk`）。
