import { spawnSync } from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  context.subscriptions.push(
    new vscode.Disposable(() => {
      void stopClient();
    }),
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

    await client.start();
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
