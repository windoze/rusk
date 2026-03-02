# rusk-lsp (Rusk Language Server)

`rusk-lsp` is the official Language Server Protocol (LSP) server for Rusk.

It uses the real `rusk-compiler` front-end (module loader + parser + resolver + typechecker) to
provide editor features over stdio.

## Status / Features

Current MVP features:

- Diagnostics on `didOpen` / `didChange` / `didSave`
  - supports multi-file module trees via `mod foo;`
  - supports unsaved editor buffers via an overlay source provider (no need to save to disk)
  - converts compiler byte spans to correct LSP ranges (0-based UTF-16 columns)
- `textDocument/documentSymbol` (top-level symbols)
- `textDocument/completion` (keyword completion)

Planned follow-ups (see `proposals/rusk-lsp.md`):

- hover inferred type
- go-to-definition
- better error accumulation / multiple diagnostics per file

## Running

From the repo:

```sh
cargo run -p rusk-lsp --bin rusk-lsp
```

Install locally:

```sh
cargo install --path crates/rusk-lsp
```

Logging is controlled by `RUST_LOG` (e.g. `RUST_LOG=info`).

## Configuration

### CLI flags

- `--entry <path>`: entry file(s) to typecheck (repeatable)
- `--sysroot <path>`: override sysroot directory
- `--no-std`: disable loading `sysroot/std`

### `initializationOptions`

Editors can provide configuration via `initialize.initializationOptions` (camelCase):

```json
{
  "entryFiles": ["path/to/main.rusk"],
  "sysroot": "path/to/sysroot",
  "noStd": false
}
```

If `entryFiles` is omitted (or empty), `rusk-lsp` treats the currently opened/changed document as
the compilation root for diagnostics.

## Example

See `examples/lsp/` for a minimal multi-file program (`main.rusk` + `foo.rusk`).

