# Welcome to your VS Code Extension

## What's in the folder

* This folder contains all of the files necessary for your extension.
* `package.json` - extension manifest (language contributions + LSP client activation).
* `syntaxes/rusk.tmLanguage` - this is the Text mate grammar file that is used for tokenization.
* `language-configuration.json` - this is the language configuration, defining the tokens that are used for comments and brackets.
* `src/extension.ts` - VSCode extension entrypoint (starts `rusk-lsp` via stdio).
* `tsconfig.json` - TypeScript build configuration.

## Get up and running straight away

* Build/install the language server first (repo root):
  * `cargo build -p rusk-lsp`
* Install JS deps and compile the extension (this folder):
  * `npm install`
  * `npm run compile`
* Press `F5` to open a new window with your extension loaded (Extension Development Host).
* Create a new file with a file name suffix matching your language.
* Verify that syntax highlighting works, and that diagnostics/completions appear (LSP).

## Make changes

* You can relaunch the extension from the debug toolbar after making changes to the files listed above.
* You can also reload (`Ctrl+R` or `Cmd+R` on Mac) the VS Code window with your extension to load your changes.

## Add more language features

* Add server features in `crates/rusk-lsp/` (hover/definition/etc), then surface them automatically via the LSP client.
* VS Code language extensions overview: https://code.visualstudio.com/api/language-extensions/overview

## Install your extension

* To start using your extension with Visual Studio Code copy it into the `<user home>/.vscode/extensions` folder and restart Code.
* To share your extension with the world, read on https://code.visualstudio.com/api/working-with-extensions/publishing-extension about publishing an extension.
