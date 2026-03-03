import * as vscode from "vscode";
import { Node, Point } from "web-tree-sitter";
import { RuskTreeSitter } from "./treeSitter";

export async function getRuskDocumentSymbols(
  treeSitter: RuskTreeSitter,
  document: vscode.TextDocument,
): Promise<vscode.DocumentSymbol[]> {
  const tree = await treeSitter.getTree(document);
  if (!tree) {
    return [];
  }
  return collectItemSymbols(document, tree.rootNode);
}

function collectItemSymbols(
  document: vscode.TextDocument,
  node: Node,
): vscode.DocumentSymbol[] {
  const result: vscode.DocumentSymbol[] = [];

  for (const child of node.namedChildren) {
    const sym = symbolFromItem(document, child);
    if (sym) {
      result.push(sym);
    }
  }

  return result;
}

function symbolFromItem(
  document: vscode.TextDocument,
  node: Node,
): vscode.DocumentSymbol | undefined {
  switch (node.type) {
    case "function_item":
      return mkSymbolFromNamedItem(
        document,
        node,
        "name",
        vscode.SymbolKind.Function,
      );

    case "intrinsic_function_item":
      return mkSymbolFromNamedItem(
        document,
        node,
        "name",
        vscode.SymbolKind.Function,
        "intrinsic",
      );

    case "struct_item": {
      const sym = mkSymbolFromNamedItem(
        document,
        node,
        "name",
        vscode.SymbolKind.Class,
      );
      if (!sym) return undefined;
      const body = node.namedChildren.find(
        (c) => c.type === "named_struct_body" || c.type === "newtype_struct_body",
      );
      if (body?.type === "named_struct_body") {
        const fields = body.descendantsOfType("field_declaration");
        sym.children = fields
          .map((f) =>
            mkSymbolFromNamedItem(
              document,
              f,
              "name",
              vscode.SymbolKind.Field,
            ),
          )
          .filter((x): x is vscode.DocumentSymbol => x !== undefined);
      }
      return sym;
    }

    case "enum_item": {
      const sym = mkSymbolFromNamedItem(
        document,
        node,
        "name",
        vscode.SymbolKind.Enum,
      );
      if (!sym) return undefined;
      const variants = node.descendantsOfType("enum_variant");
      sym.children = variants
        .map((v) =>
          mkSymbolFromNamedItem(
            document,
            v,
            "name",
            vscode.SymbolKind.EnumMember,
          ),
        )
        .filter((x): x is vscode.DocumentSymbol => x !== undefined);
      return sym;
    }

    case "interface_item": {
      const sym = mkSymbolFromNamedItem(
        document,
        node,
        "name",
        vscode.SymbolKind.Interface,
      );
      if (!sym) return undefined;
      const members = node.descendantsOfType([
        "interface_method",
        "associated_type_declaration",
      ]);
      sym.children = members
        .map((m) => {
          if (m.type === "interface_method") {
            return mkSymbolFromNamedItem(
              document,
              m,
              "name",
              vscode.SymbolKind.Method,
            );
          }
          return mkSymbolFromNamedItem(
            document,
            m,
            "name",
            vscode.SymbolKind.TypeParameter,
          );
        })
        .filter((x): x is vscode.DocumentSymbol => x !== undefined);
      return sym;
    }

    case "impl_item": {
      const header = node.childForFieldName("header");
      const detail = header ? safeNodeText(document, header) : undefined;
      const name = detail ? `impl ${detail}` : "impl";

      const sym = new vscode.DocumentSymbol(
        name,
        "",
        vscode.SymbolKind.Class,
        nodeRange(document, node),
        header ? nodeRange(document, header) : nodeRange(document, node),
      );

      const members = node.descendantsOfType([
        "impl_method",
        "impl_associated_type",
      ]);
      sym.children = members
        .map((m) => {
          if (m.type === "impl_method") {
            return mkSymbolFromNamedItem(
              document,
              m,
              "name",
              vscode.SymbolKind.Method,
            );
          }
          return mkSymbolFromNamedItem(
            document,
            m,
            "name",
            vscode.SymbolKind.TypeParameter,
          );
        })
        .filter((x): x is vscode.DocumentSymbol => x !== undefined);

      return sym;
    }

    case "module_item": {
      const sym = mkSymbolFromNamedItem(
        document,
        node,
        "name",
        vscode.SymbolKind.Module,
      );
      if (!sym) return undefined;
      const body = node.childForFieldName("body");
      if (body && body.type === "module_body") {
        sym.children = collectItemSymbols(document, body);
      }
      return sym;
    }

    default:
      return undefined;
  }
}

function mkSymbolFromNamedItem(
  document: vscode.TextDocument,
  node: Node,
  fieldName: string,
  kind: vscode.SymbolKind,
  detail?: string,
): vscode.DocumentSymbol | undefined {
  const nameNode = node.childForFieldName(fieldName);
  if (!nameNode) {
    return undefined;
  }

  const name = safeNodeText(document, nameNode);
  if (!name) {
    return undefined;
  }

  return new vscode.DocumentSymbol(
    name,
    detail ?? "",
    kind,
    nodeRange(document, node),
    nodeRange(document, nameNode),
  );
}

function safeNodeText(document: vscode.TextDocument, node: Node): string {
  return document.getText(nodeRange(document, node));
}

function nodeRange(document: vscode.TextDocument, node: Node): vscode.Range {
  return new vscode.Range(
    pointToPosition(document, node.startPosition),
    pointToPosition(document, node.endPosition),
  );
}

function pointToPosition(document: vscode.TextDocument, point: Point): vscode.Position {
  const row = clamp(point.row, 0, Math.max(0, document.lineCount - 1));
  const line = document.lineAt(row);
  const character = clamp(
    utf8ColumnToUtf16Index(line.text, point.column),
    0,
    line.text.length,
  );
  return new vscode.Position(row, character);
}

function utf8ColumnToUtf16Index(lineText: string, columnBytes: number): number {
  if (columnBytes <= 0) {
    return 0;
  }

  let bytes = 0;
  let i = 0;
  while (i < lineText.length) {
    if (bytes >= columnBytes) break;

    const codePoint = lineText.codePointAt(i);
    if (codePoint === undefined) break;

    const b = utf8ByteLengthOfCodePoint(codePoint);
    if (bytes + b > columnBytes) {
      break;
    }

    bytes += b;
    i += codePoint > 0xffff ? 2 : 1;
  }

  return i;
}

function utf8ByteLengthOfCodePoint(codePoint: number): number {
  if (codePoint <= 0x7f) return 1;
  if (codePoint <= 0x7ff) return 2;
  if (codePoint <= 0xffff) return 3;
  return 4;
}

function clamp(value: number, min: number, max: number): number {
  return Math.min(max, Math.max(min, value));
}

