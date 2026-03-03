import * as path from "path";
import * as vscode from "vscode";
import {
  Edit,
  Language,
  Parser,
  Point,
  Tree,
} from "web-tree-sitter";

type ParsedDocument = {
  version: number;
  text: string;
  tree: Tree;
};

export class RuskTreeSitter implements vscode.Disposable {
  private readonly ready: Promise<void>;
  private readonly documents = new Map<string, ParsedDocument>();
  private readonly disposables: vscode.Disposable[] = [];

  private parser: Parser | undefined;

  constructor(private readonly context: vscode.ExtensionContext) {
    this.ready = this.init().catch((err) => {
      const message =
        err instanceof Error ? err.message : "未知错误";
      void vscode.window.showErrorMessage(
        `tree-sitter 初始化失败，结构导航功能不可用：${message}`,
      );
    });

    this.disposables.push(
      vscode.workspace.onDidOpenTextDocument((doc) => {
        void this.onDidOpenTextDocument(doc);
      }),
      vscode.workspace.onDidChangeTextDocument((e) => {
        void this.onDidChangeTextDocument(e);
      }),
      vscode.workspace.onDidCloseTextDocument((doc) => {
        this.onDidCloseTextDocument(doc);
      }),
    );

    for (const doc of vscode.workspace.textDocuments) {
      void this.onDidOpenTextDocument(doc);
    }
  }

  dispose(): void {
    for (const d of this.disposables) {
      d.dispose();
    }
    this.disposables.length = 0;
    this.documents.clear();
    this.parser?.delete();
    this.parser = undefined;
  }

  async getTree(document: vscode.TextDocument): Promise<Tree | undefined> {
    if (document.languageId !== "rusk") {
      return undefined;
    }

    await this.ready;

    if (!this.parser) {
      return undefined;
    }

    const key = document.uri.toString();
    const cached = this.documents.get(key);
    if (cached && cached.version === document.version) {
      return cached.tree;
    }

    await this.parseFull(document);
    return this.documents.get(key)?.tree;
  }

  private async init(): Promise<void> {
    await Parser.init({
      locateFile: (filename: string) =>
        this.context.asAbsolutePath(
          path.join("node_modules", "web-tree-sitter", filename),
        ),
    });

    const languageWasmPath = this.context.asAbsolutePath(
      path.join("tree-sitter", "rusk.wasm"),
    );

    this.parser = new Parser();
    try {
      const language = await Language.load(languageWasmPath);
      this.parser.setLanguage(language);
    } catch (err) {
      const message = err instanceof Error ? err.message : "未知错误";
      throw new Error(
        [
          "加载/设置 tree-sitter 语言模块失败。",
          "这通常意味着 rusk.wasm 的 ABI 版本与 web-tree-sitter 不兼容。",
          `详情：${message}`,
        ].join(" "),
      );
    }
  }

  private async onDidOpenTextDocument(document: vscode.TextDocument): Promise<void> {
    if (document.languageId !== "rusk") {
      return;
    }
    await this.parseFull(document);
  }

  private async onDidChangeTextDocument(
    event: vscode.TextDocumentChangeEvent,
  ): Promise<void> {
    const document = event.document;
    if (document.languageId !== "rusk") {
      return;
    }

    await this.ready;

    const key = document.uri.toString();
    const current = this.documents.get(key);
    if (!current) {
      await this.parseFull(document);
      return;
    }

    const parser = this.parser;
    if (!parser) {
      return;
    }

    const oldTree = current.tree;
    let text = current.text;

    // VSCode 给出的 rangeOffset/rangeLength 是基于“变更前”的文档，
    // 多个变更时可以按 offset 从大到小处理，避免做额外偏移修正。
    const changes = [...event.contentChanges].sort(
      (a, b) => b.rangeOffset - a.rangeOffset,
    );

    try {
      for (const change of changes) {
        const startOffsetUtf16 = change.rangeOffset;
        const oldEndOffsetUtf16 = change.rangeOffset + change.rangeLength;

        const startIndex = utf16OffsetToUtf8ByteOffset(text, startOffsetUtf16);
        const oldEndIndex = utf16OffsetToUtf8ByteOffset(text, oldEndOffsetUtf16);
        const newEndIndex = startIndex + utf8ByteLength(change.text);

        const startPosition = utf16OffsetToPoint(text, startOffsetUtf16);
        const oldEndPosition = utf16OffsetToPoint(text, oldEndOffsetUtf16);
        const newEndPosition = applyInsertedTextToPoint(startPosition, change.text);

        const edit = new Edit({
          startIndex,
          oldEndIndex,
          newEndIndex,
          startPosition,
          oldEndPosition,
          newEndPosition,
        });

        oldTree.edit(edit);

        text =
          text.slice(0, startOffsetUtf16) +
          change.text +
          text.slice(oldEndOffsetUtf16);
      }

      const parsed = parser.parse(text, oldTree);
      if (!parsed) {
        await this.parseFull(document);
        return;
      }

      if (parsed !== oldTree) {
        oldTree.delete();
      }

      this.documents.set(key, { version: document.version, text, tree: parsed });
    } catch (err) {
      // 任意一步失败就回退到全量解析，保证导航功能可用。
      await this.parseFull(document);
      void err;
    }
  }

  private onDidCloseTextDocument(document: vscode.TextDocument): void {
    const key = document.uri.toString();
    const cached = this.documents.get(key);
    if (cached) {
      cached.tree.delete();
    }
    this.documents.delete(key);
  }

  private async parseFull(document: vscode.TextDocument): Promise<void> {
    await this.ready;

    const parser = this.parser;
    if (!parser) {
      return;
    }

    const text = document.getText();
    const tree = parser.parse(text, null);
    if (!tree) {
      return;
    }

    const key = document.uri.toString();
    const prev = this.documents.get(key);
    if (prev) {
      prev.tree.delete();
    }

    this.documents.set(key, { version: document.version, text, tree });
  }
}

function utf16OffsetToUtf8ByteOffset(text: string, offsetUtf16: number): number {
  return utf8ByteLength(text.slice(0, offsetUtf16));
}

function utf16OffsetToPoint(text: string, offsetUtf16: number): Point {
  let row = 0;
  let columnBytes = 0;

  let i = 0;
  while (i < offsetUtf16) {
    const codeUnit = text.charCodeAt(i);
    if (codeUnit === 10 /* \\n */) {
      row += 1;
      columnBytes = 0;
      i += 1;
      continue;
    }

    const codePoint = text.codePointAt(i);
    if (codePoint === undefined) {
      break;
    }
    columnBytes += utf8ByteLengthOfCodePoint(codePoint);
    i += codePoint > 0xffff ? 2 : 1;
  }

  return { row, column: columnBytes };
}

function applyInsertedTextToPoint(start: Point, inserted: string): Point {
  if (inserted.length === 0) {
    return { row: start.row, column: start.column };
  }

  const lastNewline = inserted.lastIndexOf("\n");
  if (lastNewline === -1) {
    return { row: start.row, column: start.column + utf8ByteLength(inserted) };
  }

  const newlineCount = countNewlines(inserted);
  const tail = inserted.slice(lastNewline + 1);
  return { row: start.row + newlineCount, column: utf8ByteLength(tail) };
}

function countNewlines(text: string): number {
  let count = 0;
  for (let i = 0; i < text.length; i += 1) {
    if (text.charCodeAt(i) === 10) {
      count += 1;
    }
  }
  return count;
}

function utf8ByteLength(text: string): number {
  return Buffer.byteLength(text, "utf8");
}

function utf8ByteLengthOfCodePoint(codePoint: number): number {
  if (codePoint <= 0x7f) return 1;
  if (codePoint <= 0x7ff) return 2;
  if (codePoint <= 0xffff) return 3;
  return 4;
}
