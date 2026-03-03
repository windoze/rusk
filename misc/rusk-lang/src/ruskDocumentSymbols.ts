import * as vscode from "vscode";
import { getRuskDocumentSymbols } from "./ruskSymbols";
import { RuskTreeSitter } from "./treeSitter";

export class RuskTreeSitterDocumentSymbolProvider
  implements vscode.DocumentSymbolProvider
{
  constructor(
    private readonly treeSitter: RuskTreeSitter,
    private readonly isLspRunning: () => boolean,
  ) {}

  async provideDocumentSymbols(
    document: vscode.TextDocument,
    token: vscode.CancellationToken,
  ): Promise<vscode.DocumentSymbol[]> {
    // 当 LSP 正常运行时，优先使用 LSP 的符号结果，避免 Outline/Breadcrumbs 重复。
    if (this.isLspRunning()) {
      return [];
    }

    if (token.isCancellationRequested) {
      return [];
    }

    if (document.languageId !== "rusk") {
      return [];
    }

    const symbols = await getRuskDocumentSymbols(this.treeSitter, document);
    if (token.isCancellationRequested) {
      return [];
    }
    return symbols;
  }
}

