# Misc

This directory contains editor support and auxiliary tools for the Rusk programming language.

## Contents

- `rusk-lang`: VSCode extension for Rusk (syntax highlighting + LSP client for `rusk-lsp`).
- `rusk.sublime-syntax`: Sublime Text grammar for Rusk.
- `tree-sitter-rusk`: Tree-sitter grammar for Rusk (used for structural navigation in editors).

## Building and Installing

### Prerequisites

- **Rust toolchain** (for building Rusk binaries)
- **Node.js and npm** (for building the VSCode extension)

### Step 1: Build Required Binaries

From the **root of the Rusk repository**, build the necessary executables:

```sh
# Build the main Rusk CLI (used for formatting)
cargo build --release --bin rusk

# Build the Language Server Protocol (LSP) server
cargo build --release -p rusk-lsp
```

Alternatively, you can install them globally:

```sh
# Install globally
cargo install --path . --bin rusk
cargo install --path crates/rusk-lsp
```

After building:
- `rusk` binary will be at `target/release/rusk` (or in your PATH if installed globally)
- `rusk-lsp` binary will be at `target/release/rusk-lsp` (or in your PATH if installed globally)

### Step 2: Build the VSCode Extension

Navigate to the VSCode extension directory:

```sh
cd misc/rusk-lang
```

Install dependencies and compile TypeScript:

```sh
# Install Node.js dependencies
npm install

# Compile TypeScript sources
npm run compile
```

### Step 3: Install the Extension

You have two options to install the VSCode extension:

#### Option A: Package and Install (Recommended)

Install `vsce` (Visual Studio Code Extension manager):

```sh
npm install -g @vscode/vsce
```

Package the extension:

```sh
vsce package
```

This creates a `.vsix` file. Install it in VSCode:

```sh
code --install-extension rusk-lang-0.1.0.vsix
```

Or manually: Open VSCode → Extensions → "..." menu → "Install from VSIX..." → select the `.vsix` file.

#### Option B: Development Mode

For development and testing, you can run the extension directly:

1. Open the `misc/rusk-lang` folder in VSCode
2. Press `F5` to launch a new VSCode window with the extension loaded

### Step 4: Configure VSCode (Optional)

The extension will automatically search for `rusk` and `rusk-lsp` binaries in:
- `workspace/target/debug/` or `workspace/target/release/`
- Your system PATH

If needed, you can manually specify paths in VSCode settings:

- `rusk.lsp.serverPath`: Path to `rusk-lsp` executable
- `rusk.fmt.ruskPath`: Path to `rusk` executable (for formatting)

### Usage

Once installed:
1. Open any `.rusk` file to activate syntax highlighting
2. The LSP server will start automatically, providing:
   - Diagnostics (error checking)
   - Code completion
   - Go to definition
   - Hover information
   - Document symbols
3. Use **Format Document** (or enable Format on Save) to format code with `rusk fmt`

## Troubleshooting

- **Extension not activating**: Check VSCode's Output panel → "Rusk (LSP)" for error messages
- **LSP server not starting**: Verify `rusk-lsp` is built and accessible; check the configured path in settings
- **Formatting not working**: Verify `rusk` binary is built and accessible

For more details on the VSCode extension features and configuration, see [rusk-lang/README.md](rusk-lang/README.md).
