import * as vscode from "vscode";
import { RuskTreeSitter } from "./treeSitter";
import { getRuskDocumentSymbols } from "./ruskSymbols";

type OutlineNode = {
  uri: vscode.Uri;
  symbol: vscode.DocumentSymbol;
};

export class RuskOutlineView implements vscode.Disposable, vscode.TreeDataProvider<OutlineNode> {
  private readonly disposables: vscode.Disposable[] = [];
  private readonly didChangeTreeDataEmitter = new vscode.EventEmitter<
    OutlineNode | undefined | null | void
  >();
  readonly onDidChangeTreeData = this.didChangeTreeDataEmitter.event;

  private readonly treeView: vscode.TreeView<OutlineNode>;

  constructor(
    private readonly treeSitter: RuskTreeSitter,
    context: vscode.ExtensionContext,
  ) {
    this.treeView = vscode.window.createTreeView("rusk.outline", {
      treeDataProvider: this,
      showCollapseAll: true,
    });

    this.disposables.push(
      this.treeView,
      vscode.window.onDidChangeActiveTextEditor(() => this.refresh()),
      vscode.workspace.onDidChangeTextDocument((e) => {
        const active = vscode.window.activeTextEditor?.document;
        if (active && e.document.uri.toString() === active.uri.toString()) {
          this.refresh();
        }
      }),
      vscode.commands.registerCommand("rusk.gotoSymbol", async () => {
        await this.gotoSymbol();
      }),
      vscode.commands.registerCommand("rusk.showOutline", async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor || editor.document.languageId !== "rusk") {
          void vscode.window.showInformationMessage("请先打开一个 .rusk 文件。");
          return;
        }

        const roots = await this.getChildren();
        if (roots.length === 0) {
          this.treeView.message = "未找到可导航的符号。";
          return;
        }

        await this.treeView.reveal(roots[0], {
          focus: true,
          select: true,
          expand: true,
        });
      }),
    );

    // 由上层（extension.ts）统一管理 dispose。
  }

  dispose(): void {
    this.didChangeTreeDataEmitter.dispose();
    for (const d of this.disposables) {
      d.dispose();
    }
    this.disposables.length = 0;
  }

  refresh(): void {
    this.didChangeTreeDataEmitter.fire();
  }

  async getChildren(element?: OutlineNode): Promise<OutlineNode[]> {
    const editor = vscode.window.activeTextEditor;
    if (!editor || editor.document.languageId !== "rusk") {
      return [];
    }

    if (element) {
      return element.symbol.children.map((s) => ({ uri: element.uri, symbol: s }));
    }

    const symbols = await getRuskDocumentSymbols(this.treeSitter, editor.document);
    return symbols.map((s) => ({ uri: editor.document.uri, symbol: s }));
  }

  getTreeItem(element: OutlineNode): vscode.TreeItem {
    const item = new vscode.TreeItem(
      element.symbol.name,
      element.symbol.children.length > 0
        ? vscode.TreeItemCollapsibleState.Collapsed
        : vscode.TreeItemCollapsibleState.None,
    );

    item.description = element.symbol.detail || undefined;
    item.contextValue = "ruskOutlineSymbol";
    item.iconPath = new vscode.ThemeIcon(symbolKindIconId(element.symbol.kind));

    item.command = {
      command: "rusk.revealSymbol",
      title: "Reveal Symbol",
      arguments: [element.uri, element.symbol.selectionRange],
    };
    return item;
  }

  private async gotoSymbol(): Promise<void> {
    const editor = vscode.window.activeTextEditor;
    if (!editor || editor.document.languageId !== "rusk") {
      void vscode.window.showInformationMessage("请先打开一个 .rusk 文件。");
      return;
    }

    const symbols = await getRuskDocumentSymbols(this.treeSitter, editor.document);
    const flat: Array<{
      label: string;
      description: string | undefined;
      range: vscode.Range;
    }> = [];

    const visit = (sym: vscode.DocumentSymbol, prefix: string) => {
      const label = prefix.length > 0 ? `${prefix}::${sym.name}` : sym.name;
      flat.push({ label, description: sym.detail || undefined, range: sym.selectionRange });
      for (const child of sym.children) {
        visit(child, label);
      }
    };
    for (const s of symbols) {
      visit(s, "");
    }

    if (flat.length === 0) {
      void vscode.window.showInformationMessage("当前文件未找到可导航的符号。");
      return;
    }

    const picked = await vscode.window.showQuickPick(
      flat.map((f) => ({
        label: f.label,
        description: f.description,
        range: f.range,
      })),
      {
        matchOnDescription: true,
        placeHolder: "选择一个符号跳转",
      },
    );

    if (!picked) {
      return;
    }

    await reveal(editor.document.uri, picked.range);
  }
}

export function registerRuskRevealCommand(context: vscode.ExtensionContext): void {
  context.subscriptions.push(
    vscode.commands.registerCommand(
      "rusk.revealSymbol",
      async (uri: vscode.Uri, range: vscode.Range) => {
        await reveal(uri, range);
      },
    ),
  );
}

async function reveal(uri: vscode.Uri, range: vscode.Range): Promise<void> {
  const doc = await vscode.workspace.openTextDocument(uri);
  const editor = await vscode.window.showTextDocument(doc, { preview: false });
  editor.revealRange(range, vscode.TextEditorRevealType.InCenter);
  editor.selection = new vscode.Selection(range.start, range.start);
}

function symbolKindIconId(kind: vscode.SymbolKind): string {
  switch (kind) {
    case vscode.SymbolKind.Module:
      return "symbol-module";
    case vscode.SymbolKind.Namespace:
      return "symbol-namespace";
    case vscode.SymbolKind.Class:
      return "symbol-class";
    case vscode.SymbolKind.Interface:
      return "symbol-interface";
    case vscode.SymbolKind.Enum:
      return "symbol-enum";
    case vscode.SymbolKind.Method:
      return "symbol-method";
    case vscode.SymbolKind.Function:
      return "symbol-function";
    case vscode.SymbolKind.Property:
      return "symbol-property";
    case vscode.SymbolKind.Field:
      return "symbol-field";
    case vscode.SymbolKind.EnumMember:
      return "symbol-enum-member";
    case vscode.SymbolKind.TypeParameter:
      return "symbol-type-parameter";
    default:
      return "symbol-misc";
  }
}
