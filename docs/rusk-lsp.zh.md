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
- `textDocument/completion`（关键字补全）

后续计划（见 `proposals/rusk-lsp.md`）：

- hover 类型信息
- go-to-definition
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

## 示例

`examples/lsp/` 提供了一个最小的多文件示例（`main.rusk` + `foo.rusk`）。

