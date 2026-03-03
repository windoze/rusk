import { spawn, spawnSync } from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  State,
} from "vscode-languageclient/node";
import { RuskOutlineView, registerRuskRevealCommand } from "./ruskOutline";
import { RuskTreeSitterDocumentSymbolProvider } from "./ruskDocumentSymbols";
import { RuskTreeSitter } from "./treeSitter";

let client: LanguageClient | undefined;
let lspRunning = false;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const output = vscode.window.createOutputChannel("Rusk");
  context.subscriptions.push(output);

  context.subscriptions.push(
    new vscode.Disposable(() => {
      void stopClient();
    }),
  );

  // tree-sitter：用于增量语法树 + 结构导航（Outline 视图、Go to Symbol）。
  const treeSitter = new RuskTreeSitter(context);
  const outline = new RuskOutlineView(treeSitter, context);
  registerRuskRevealCommand(context);
  context.subscriptions.push(treeSitter, outline);

  // 仅在 LSP 未运行时提供 DocumentSymbol（供 VSCode 内置 Outline/Breadcrumbs 使用），避免与 LSP 重复。
  context.subscriptions.push(
    vscode.languages.registerDocumentSymbolProvider(
      [{ language: "rusk" }],
      new RuskTreeSitterDocumentSymbolProvider(treeSitter, () => lspRunning),
    ),
  );

  // Formatter: `rusk fmt --stdin` as VSCode document formatter.
  context.subscriptions.push(
    vscode.languages.registerDocumentFormattingEditProvider(
      [{ language: "rusk" }],
      new RuskDocumentFormattingProvider(output),
    ),
  );

  const startOrRestart = async () => {
    await stopClient();

    const config = vscode.workspace.getConfiguration("rusk");
    const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;

    const serverCommand = resolveServerCommand(config, workspaceRoot);
    if (!serverCommand) {
      vscode.window.showErrorMessage(
        [
          "找不到 rusk-lsp 可执行文件。",
          "请先构建/安装语言服务器：",
          "  - 在 rusk 仓库内：cargo build -p rusk-lsp",
          "  - 或全局安装：cargo install --path crates/rusk-lsp",
          "然后在设置中指定 `rusk.lsp.serverPath`，或确保 PATH 中有 `rusk-lsp`。",
        ].join("\n"),
      );
      return;
    }

    const serverArgs = config.get<string[]>("lsp.serverArgs") ?? [];
    const serverOptions: ServerOptions = {
      command: serverCommand,
      args: serverArgs,
      options: {
        cwd: workspaceRoot,
      },
    };

    const initOptions = buildInitializationOptions(config, workspaceRoot);

    const clientOptions: LanguageClientOptions = {
      documentSelector: [{ scheme: "file", language: "rusk" }],
      synchronize: {
        configurationSection: "rusk",
      },
      initializationOptions: initOptions,
    };

    client = new LanguageClient(
      "rusk-lsp",
      "Rusk Language Server",
      serverOptions,
      clientOptions,
    );

    // 记录 LSP 状态，供 tree-sitter fallback 决策使用。
    lspRunning = client.state === State.Running;
    client.onDidChangeState((e) => {
      lspRunning = e.newState === State.Running;
    });

    try {
      await client.start();
    } catch (err) {
      lspRunning = false;
      throw err;
    }
  };

  context.subscriptions.push(
    vscode.commands.registerCommand("rusk.restartServer", async () => {
      await startOrRestart();
    }),
  );

  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration(async (e) => {
      if (
        e.affectsConfiguration("rusk.lsp.serverPath") ||
        e.affectsConfiguration("rusk.lsp.serverArgs") ||
        e.affectsConfiguration("rusk.lsp.entryFiles") ||
        e.affectsConfiguration("rusk.lsp.sysroot") ||
        e.affectsConfiguration("rusk.lsp.noStd")
      ) {
        await startOrRestart();
      }
    }),
  );

  await startOrRestart();
}

export async function deactivate(): Promise<void> {
  await stopClient();
}

async function stopClient(): Promise<void> {
  if (!client) {
    return;
  }
  const toStop = client;
  client = undefined;
  lspRunning = false;
  await toStop.stop();
}

function resolveServerCommand(
  config: vscode.WorkspaceConfiguration,
  workspaceRoot: string | undefined,
): string | undefined {
  const configured = (config.get<string>("lsp.serverPath") ?? "").trim();
  if (configured.length > 0) {
    const resolved = resolveMaybeRelativePath(configured, workspaceRoot);
    if (!fs.existsSync(resolved)) {
      return undefined;
    }
    return preflightServer(resolved);
  }

  if (workspaceRoot) {
    const candidates = [
      path.join(workspaceRoot, "target", "debug", exeName("rusk-lsp")),
      path.join(workspaceRoot, "target", "release", exeName("rusk-lsp")),
    ];
    for (const candidate of candidates) {
      if (fs.existsSync(candidate)) {
        return preflightServer(candidate);
      }
    }
  }

  // Fallback: rely on PATH lookup.
  return preflightServer("rusk-lsp");
}

function preflightServer(command: string): string | undefined {
  const res = spawnSync(command, ["--version"], {
    encoding: "utf8",
    windowsHide: true,
  });
  if (res.error && (res.error as NodeJS.ErrnoException).code === "ENOENT") {
    return undefined;
  }
  return command;
}

class RuskDocumentFormattingProvider
  implements vscode.DocumentFormattingEditProvider
{
  constructor(private readonly output: vscode.OutputChannel) {}

  async provideDocumentFormattingEdits(
    document: vscode.TextDocument,
    _options: vscode.FormattingOptions,
    token: vscode.CancellationToken,
  ): Promise<vscode.TextEdit[]> {
    const config = vscode.workspace.getConfiguration("rusk", document.uri);
    const workspaceRoot = vscode.workspace.getWorkspaceFolder(document.uri)?.uri.fsPath;

    const ruskCommand = resolveFormatterCommand(config, workspaceRoot);
    if (!ruskCommand) {
      vscode.window.showErrorMessage(
        [
          "找不到 rusk 可执行文件，无法格式化。",
          "请先构建/安装：",
          "  - 在 rusk 仓库内：cargo build --bin rusk",
          "  - 或全局安装：cargo install --path . --bin rusk",
          "然后在设置中指定 `rusk.fmt.ruskPath`，或确保 PATH 中有 `rusk`。",
        ].join("\n"),
      );
      return [];
    }

    const input = document.getText();
    let res: { stdout: string; stderr: string; exitCode: number | null };
    try {
      res = await runRuskFmt(ruskCommand, workspaceRoot, input, token);
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      this.output.appendLine(`rusk fmt 启动失败：${message}`);
      this.output.show(true);
      vscode.window.showErrorMessage("无法运行 rusk fmt，详情见 Output 面板中的 “Rusk”。");
      return [];
    }
    if (token.isCancellationRequested) {
      return [];
    }

    if (res.exitCode !== 0) {
      const header = `rusk fmt 失败（exit=${res.exitCode ?? "null"}）`;
      this.output.appendLine(header);
      if (res.stderr.trim().length > 0) {
        this.output.appendLine(res.stderr.trimEnd());
      }
      this.output.show(true);
      vscode.window.showErrorMessage("rusk fmt 失败，详情见 Output 面板中的 “Rusk”。");
      return [];
    }

    const formatted = res.stdout;
    if (formatted.length === 0) {
      this.output.appendLine("rusk fmt 返回空输出（stdout 为空），已跳过格式化。");
      this.output.show(true);
      return [];
    }

    if (formatted === input) {
      return [];
    }

    const fullRange = new vscode.Range(
      document.positionAt(0),
      document.positionAt(input.length),
    );
    return [vscode.TextEdit.replace(fullRange, formatted)];
  }
}

function resolveFormatterCommand(
  config: vscode.WorkspaceConfiguration,
  workspaceRoot: string | undefined,
): string | undefined {
  const configured = (config.get<string>("fmt.ruskPath") ?? "").trim();
  if (configured.length > 0) {
    const resolved = resolveMaybeRelativePath(configured, workspaceRoot);
    if (!fs.existsSync(resolved)) {
      return undefined;
    }
    return preflightRusk(resolved);
  }

  if (workspaceRoot) {
    const candidates = [
      path.join(workspaceRoot, "target", "debug", exeName("rusk")),
      path.join(workspaceRoot, "target", "release", exeName("rusk")),
    ];
    for (const candidate of candidates) {
      if (fs.existsSync(candidate)) {
        return preflightRusk(candidate);
      }
    }
  }

  // Fallback: rely on PATH lookup.
  return preflightRusk("rusk");
}

function preflightRusk(command: string): string | undefined {
  const res = spawnSync(command, ["--help"], {
    encoding: "utf8",
    windowsHide: true,
  });
  if (res.error && (res.error as NodeJS.ErrnoException).code === "ENOENT") {
    return undefined;
  }
  return command;
}

async function runRuskFmt(
  command: string,
  cwd: string | undefined,
  input: string,
  token: vscode.CancellationToken,
): Promise<{ stdout: string; stderr: string; exitCode: number | null }> {
  return await new Promise((resolve, reject) => {
    const child = spawn(command, ["fmt", "--stdin"], {
      cwd,
      windowsHide: true,
    });

    let stdout = "";
    let stderr = "";
    child.stdout.setEncoding("utf8");
    child.stderr.setEncoding("utf8");

    child.stdout.on("data", (chunk: string) => {
      stdout += chunk;
    });
    child.stderr.on("data", (chunk: string) => {
      stderr += chunk;
    });

    child.on("error", (err) => reject(err));
    child.on("close", (code) => {
      resolve({ stdout, stderr, exitCode: code });
    });

    token.onCancellationRequested(() => {
      try {
        child.kill();
      } catch {
        // ignore
      }
    });

    child.stdin.end(input, "utf8");
  });
}

function buildInitializationOptions(
  config: vscode.WorkspaceConfiguration,
  workspaceRoot: string | undefined,
): Record<string, unknown> {
  const entryFiles = config.get<string[]>("lsp.entryFiles") ?? [];
  const sysroot = (config.get<string>("lsp.sysroot") ?? "").trim();
  const noStd = config.get<boolean>("lsp.noStd") ?? false;

  const initOptions: Record<string, unknown> = {};

  if (entryFiles.length > 0) {
    initOptions.entryFiles = entryFiles.map((p) =>
      resolveMaybeRelativePath(p, workspaceRoot),
    );
  }

  if (sysroot.length > 0) {
    initOptions.sysroot = resolveMaybeRelativePath(sysroot, workspaceRoot);
  }

  if (noStd) {
    initOptions.noStd = true;
  }

  return initOptions;
}

function resolveMaybeRelativePath(
  p: string,
  workspaceRoot: string | undefined,
): string {
  if (path.isAbsolute(p)) {
    return p;
  }
  if (!workspaceRoot) {
    return p;
  }
  return path.join(workspaceRoot, p);
}

function exeName(base: string): string {
  return process.platform === "win32" ? `${base}.exe` : base;
}
