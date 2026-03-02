use crate::config::{RuskLspConfig, apply_init_options, resolve_path};
use crate::text::{byte_offset_to_position, byte_range_to_range, position_to_byte_offset};
use rusk_compiler::analysis as compiler_analysis;
use rusk_compiler::source_map::SourceName;
use rusk_compiler::vfs::SourceProvider;
use rusk_host::std_io;
use std::collections::{HashMap, HashSet};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use tower_lsp::jsonrpc;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    Diagnostic, DiagnosticSeverity, DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse,
    GotoDefinitionParams, GotoDefinitionResponse, InitializeParams, InitializeResult, Location,
    OneOf, Position, Range, ServerCapabilities, ServerInfo, SymbolKind,
    TextDocumentContentChangeEvent, TextDocumentItem, TextDocumentSyncCapability,
    TextDocumentSyncKind, Url, WorkDoneProgressOptions,
};
use tower_lsp::{Client, LanguageServer};
use tracing::info;

pub struct RuskLanguageServer {
    client: Client,
    state: Arc<ServerState>,
}

struct ServerState {
    overlay: RwLock<HashMap<PathBuf, Arc<str>>>,
    config: RwLock<RuskLspConfig>,
    workspace_root: RwLock<Option<PathBuf>>,
    last_published: RwLock<HashSet<Url>>,
    snapshots: RwLock<Vec<compiler_analysis::AnalysisSnapshot>>,
}

#[derive(Clone)]
struct OverlaySourceProvider {
    state: Arc<ServerState>,
}

impl OverlaySourceProvider {
    fn key_for_path(path: &Path) -> PathBuf {
        std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
    }
}

impl SourceProvider for OverlaySourceProvider {
    fn read_to_string(&self, path: &Path) -> io::Result<String> {
        let key = Self::key_for_path(path);
        if let Ok(overlay) = self.state.overlay.read()
            && let Some(src) = overlay.get(&key)
        {
            return Ok(src.to_string());
        }

        std::fs::read_to_string(path)
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        Ok(Self::key_for_path(path))
    }

    fn exists(&self, path: &Path) -> bool {
        let key = Self::key_for_path(path);
        if let Ok(overlay) = self.state.overlay.read()
            && overlay.contains_key(&key)
        {
            return true;
        }
        path.exists()
    }
}

impl RuskLanguageServer {
    pub fn new(client: Client, initial_config: RuskLspConfig) -> Self {
        Self {
            client,
            state: Arc::new(ServerState {
                overlay: RwLock::new(HashMap::new()),
                config: RwLock::new(initial_config),
                workspace_root: RwLock::new(None),
                last_published: RwLock::new(HashSet::new()),
                snapshots: RwLock::new(Vec::new()),
            }),
        }
    }

    fn uri_to_key_path(&self, uri: &Url) -> Option<PathBuf> {
        let path = uri.to_file_path().ok()?;
        Some(OverlaySourceProvider::key_for_path(&path))
    }

    fn set_overlay_text(&self, uri: &Url, text: Arc<str>) {
        let Some(path) = self.uri_to_key_path(uri) else {
            return;
        };
        if let Ok(mut overlay) = self.state.overlay.write() {
            overlay.insert(path, text);
        }
    }

    fn get_overlay_text(&self, uri: &Url) -> Option<Arc<str>> {
        let path = self.uri_to_key_path(uri)?;
        let overlay = self.state.overlay.read().ok()?;
        overlay.get(&path).map(Arc::clone)
    }

    fn remove_overlay_text(&self, uri: &Url) {
        let Some(path) = self.uri_to_key_path(uri) else {
            return;
        };
        if let Ok(mut overlay) = self.state.overlay.write() {
            overlay.remove(&path);
        }
    }

    fn create_compile_options(&self) -> rusk_compiler::CompileOptions {
        let config = self
            .state
            .config
            .read()
            .ok()
            .map(|g| g.clone())
            .unwrap_or_default();
        let mut options = rusk_compiler::CompileOptions {
            sysroot: config.sysroot,
            load_std: config.load_std,
            ..Default::default()
        };
        if options.load_std {
            std_io::register_host_module(&mut options);
        }
        options
    }

    fn entry_files_for_trigger(&self, trigger: Option<PathBuf>) -> Vec<PathBuf> {
        let config = self
            .state
            .config
            .read()
            .ok()
            .map(|g| g.clone())
            .unwrap_or_default();
        if !config.entry_files.is_empty() {
            return config.entry_files;
        }
        trigger.into_iter().collect()
    }

    async fn reanalyze_and_publish(&self, trigger: Option<PathBuf>) {
        let entry_files = self.entry_files_for_trigger(trigger);
        if entry_files.is_empty() {
            return;
        }

        let provider: Arc<dyn SourceProvider> = Arc::new(OverlaySourceProvider {
            state: Arc::clone(&self.state),
        });
        let options = self.create_compile_options();

        let mut next_published: HashMap<Url, Vec<Diagnostic>> = HashMap::new();
        let mut snapshots = Vec::new();

        for entry in &entry_files {
            let result =
                compiler_analysis::analyze_entry_file(entry, &options, Arc::clone(&provider));

            if let Some(snapshot) = result.snapshot {
                snapshots.push(snapshot);
            }

            for diag in &result.diagnostics {
                let Some((uri, lsp_diag)) = compiler_diag_to_lsp(diag, &result.source_map, entry)
                else {
                    continue;
                };
                next_published.entry(uri).or_default().push(lsp_diag);
            }
        }

        if let Ok(mut cache) = self.state.snapshots.write() {
            *cache = snapshots;
        }

        let next_keys: HashSet<Url> = next_published.keys().cloned().collect();
        let prev_keys: HashSet<Url> = self
            .state
            .last_published
            .read()
            .ok()
            .map(|s| s.clone())
            .unwrap_or_default();

        let mut to_clear = Vec::new();
        for uri in prev_keys.difference(&next_keys) {
            to_clear.push(uri.clone());
        }

        let mut to_publish: Vec<(Url, Vec<Diagnostic>)> = next_published.into_iter().collect();
        to_publish.sort_by(|(a, _), (b, _)| a.as_str().cmp(b.as_str()));

        for uri in to_clear {
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
        }
        for (uri, diags) in to_publish {
            self.client.publish_diagnostics(uri, diags, None).await;
        }

        if let Ok(mut last) = self.state.last_published.write() {
            *last = next_keys;
        }
    }

    fn apply_change_events(&self, uri: &Url, changes: Vec<TextDocumentContentChangeEvent>) {
        let mut text = self
            .get_overlay_text(uri)
            .unwrap_or_else(|| Arc::<str>::from(""))
            .to_string();

        for change in changes {
            match change.range {
                None => text = change.text,
                Some(range) => {
                    apply_incremental_change(&mut text, range, &change.text);
                }
            }
        }

        self.set_overlay_text(uri, Arc::from(text));
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for RuskLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        let root_path = params.root_uri.as_ref().and_then(|u| u.to_file_path().ok());

        if let Ok(mut root) = self.state.workspace_root.write() {
            *root = root_path.clone();
        }

        if let Ok(mut config) = self.state.config.write() {
            if let Some(root) = root_path.as_deref() {
                config.entry_files = config
                    .entry_files
                    .drain(..)
                    .map(|p| resolve_path(Some(root), p))
                    .collect();
                if let Some(sysroot) = config.sysroot.take() {
                    config.sysroot = Some(resolve_path(Some(root), sysroot));
                }
            }

            apply_init_options(
                &mut config,
                root_path.as_deref(),
                params.initialization_options.as_ref(),
            );
        }

        info!("rusk-lsp initialized (root={root_path:?})");

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: None,
                    all_commit_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                    completion_item: None,
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "rusk-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _params: tower_lsp::lsp_types::InitializedParams) {
        // No-op for now.
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: tower_lsp::lsp_types::DidOpenTextDocumentParams) {
        let TextDocumentItem { uri, text, .. } = params.text_document;
        self.set_overlay_text(&uri, Arc::from(text));

        let trigger = uri.to_file_path().ok();
        self.reanalyze_and_publish(trigger).await;
    }

    async fn did_change(&self, params: tower_lsp::lsp_types::DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        self.apply_change_events(&uri, params.content_changes);

        let trigger = uri.to_file_path().ok();
        self.reanalyze_and_publish(trigger).await;
    }

    async fn did_save(&self, params: tower_lsp::lsp_types::DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            self.set_overlay_text(&params.text_document.uri, Arc::from(text));
        }
        let trigger = params.text_document.uri.to_file_path().ok();
        self.reanalyze_and_publish(trigger).await;
    }

    async fn did_close(&self, params: tower_lsp::lsp_types::DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.remove_overlay_text(&uri);

        self.client
            .publish_diagnostics(uri.clone(), Vec::new(), None)
            .await;

        if let Ok(mut last) = self.state.last_published.write() {
            last.remove(&uri);
        }
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> jsonrpc::Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        let Some(path) = uri.to_file_path().ok() else {
            return Ok(Some(DocumentSymbolResponse::Nested(Vec::new())));
        };
        let key = OverlaySourceProvider::key_for_path(&path);
        let name = SourceName::Path(key);

        let snapshots = self
            .state
            .snapshots
            .read()
            .ok()
            .map(|g| g.clone())
            .unwrap_or_default();
        let Some(snapshot) = snapshots
            .iter()
            .find(|s| s.source_map().source_text(&name).is_some())
        else {
            return Ok(Some(DocumentSymbolResponse::Nested(Vec::new())));
        };

        let symbols = compiler_analysis::document_symbols(snapshot, &name);
        let Some(src) = snapshot.source_map().source_text(&name) else {
            return Ok(Some(DocumentSymbolResponse::Nested(Vec::new())));
        };

        let docs: Vec<DocumentSymbol> = symbols
            .into_iter()
            .filter_map(|s| symbol_to_document_symbol(snapshot.source_map(), &src, &name, s))
            .collect();

        Ok(Some(DocumentSymbolResponse::Nested(docs)))
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> jsonrpc::Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let Some(src) = self.get_overlay_text(&uri) else {
            return Ok(Some(CompletionResponse::Array(Vec::new())));
        };

        let prefix = extract_ident_prefix(&src, pos);
        let mut items: Vec<CompletionItem> = rusk_keywords()
            .into_iter()
            .filter(|kw| kw.starts_with(&prefix))
            .map(|kw| CompletionItem {
                label: kw.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            })
            .collect();
        items.sort_by(|a, b| a.label.cmp(&b.label));

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> jsonrpc::Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let Some(src) = self.get_overlay_text(&uri) else {
            return Ok(None);
        };

        let Some((ident, ident_start, ident_end)) = extract_ident_at(&src, pos) else {
            return Ok(None);
        };

        let Some(path) = uri.to_file_path().ok() else {
            return Ok(None);
        };
        let key = OverlaySourceProvider::key_for_path(&path);
        let doc_name = SourceName::Path(key);

        let snapshots = self
            .state
            .snapshots
            .read()
            .ok()
            .map(|g| g.clone())
            .unwrap_or_default();

        if let Some(snapshot) = snapshots
            .iter()
            .find(|s| s.source_map().source_text(&doc_name).is_some())
        {
            if let Some(def) = compiler_analysis::goto_definition(
                snapshot,
                &doc_name,
                (ident_start, ident_end),
                &ident,
            ) {
                let Some(src) = snapshot.source_map().source_text(&def.name) else {
                    // Best-effort: fall back to the older top-level lookup path.
                    // This can happen if the snapshot is missing source text for the returned
                    // location (e.g. stale/partial snapshots).
                    return Ok(find_definition_in_snapshot(snapshot, &ident, Some(&doc_name))
                        .map(GotoDefinitionResponse::Scalar));
                };
                let uri = match &def.name {
                    SourceName::Path(p) => Url::from_file_path(p).ok(),
                    SourceName::Virtual(_) => None,
                };
                if let Some(uri) = uri {
                    let range = byte_range_to_range(&src, def.start, def.end);
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location { uri, range })));
                }
            }

            if let Some(loc) = find_definition_in_snapshot(snapshot, &ident, Some(&doc_name)) {
                return Ok(Some(GotoDefinitionResponse::Scalar(loc)));
            }
        }

        for snapshot in &snapshots {
            if let Some(loc) = find_definition_in_snapshot(snapshot, &ident, None) {
                return Ok(Some(GotoDefinitionResponse::Scalar(loc)));
            }
        }

        Ok(None)
    }
}

fn compiler_diag_to_lsp(
    diag: &compiler_analysis::Diagnostic,
    source_map: &rusk_compiler::source_map::SourceMap,
    fallback_file: &Path,
) -> Option<(Url, Diagnostic)> {
    let source_name = diag
        .byte_range
        .as_ref()
        .map(|r| r.name.clone())
        .or_else(|| diag.source.clone())
        .unwrap_or_else(|| SourceName::Path(fallback_file.to_path_buf()));

    let path = match &source_name {
        SourceName::Path(p) => p.clone(),
        SourceName::Virtual(_) => fallback_file.to_path_buf(),
    };
    let uri = Url::from_file_path(&path).ok()?;

    let range = if let Some(br) = &diag.byte_range
        && br.name == source_name
        && let Some(src) = source_map.source_text(&br.name)
    {
        byte_range_to_range(&src, br.start, br.end)
    } else {
        Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        }
    };

    Some((
        uri,
        Diagnostic {
            range,
            severity: Some(match diag.severity {
                compiler_analysis::DiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
                compiler_analysis::DiagnosticSeverity::Warning => DiagnosticSeverity::WARNING,
            }),
            source: Some("rusk".to_string()),
            message: diag.message.clone(),
            ..Default::default()
        },
    ))
}

fn symbol_to_document_symbol(
    source_map: &rusk_compiler::source_map::SourceMap,
    src: &str,
    document: &SourceName,
    sym: compiler_analysis::Symbol,
) -> Option<DocumentSymbol> {
    let span = source_map.lookup_span_bytes(sym.span)?;
    if &span.name != document {
        return None;
    }
    let range = byte_range_to_range(src, span.start, span.end);

    let (selection_range, name_start) = source_map
        .lookup_span_bytes(sym.name_span)
        .filter(|s| &s.name == document)
        .map(|s| (byte_range_to_range(src, s.start, s.end), Some(s.start)))
        .unwrap_or((range, None));

    let children = sym
        .children
        .into_iter()
        .filter_map(|c| symbol_to_document_symbol(source_map, src, document, c))
        .collect();

    let detail = name_start
        .and_then(|start| doc_comment_for_symbol(src, start))
        .map(|docs| summarize_doc_comment(&docs));

    #[allow(deprecated)]
    Some(DocumentSymbol {
        name: sym.name,
        detail,
        kind: symbol_kind(sym.kind),
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children: Some(children),
    })
}

fn doc_comment_for_symbol(src: &str, symbol_name_start: usize) -> Option<String> {
    let line_idx = byte_offset_to_position(src, symbol_name_start).line as usize;
    let lines: Vec<&str> = src.lines().collect();
    if line_idx == 0 || line_idx > lines.len() {
        return None;
    }

    let mut docs = Vec::new();
    let mut idx = line_idx;
    while idx > 0 {
        idx = idx.saturating_sub(1);
        let line = lines.get(idx).copied().unwrap_or("");
        let trimmed = line.trim_start();

        if trimmed.is_empty() {
            break;
        }

        let Some(rest) = trimmed
            .strip_prefix("///")
            .or_else(|| trimmed.strip_prefix("//!"))
        else {
            break;
        };

        docs.push(rest.trim_start().to_string());
    }

    docs.reverse();
    if docs.is_empty() {
        None
    } else {
        Some(docs.join("\n"))
    }
}

fn summarize_doc_comment(docs: &str) -> String {
    let first = docs.lines().next().unwrap_or("").trim();
    if first.is_empty() {
        return docs.trim().to_string();
    }
    const MAX: usize = 120;
    if first.len() <= MAX {
        first.to_string()
    } else {
        let mut out = first.chars().take(MAX).collect::<String>();
        out.push_str("…");
        out
    }
}

fn symbol_kind(kind: compiler_analysis::SymbolKind) -> SymbolKind {
    match kind {
        compiler_analysis::SymbolKind::Function
        | compiler_analysis::SymbolKind::IntrinsicFunction => SymbolKind::FUNCTION,
        compiler_analysis::SymbolKind::Struct => SymbolKind::STRUCT,
        compiler_analysis::SymbolKind::Enum => SymbolKind::ENUM,
        compiler_analysis::SymbolKind::Interface => SymbolKind::INTERFACE,
        compiler_analysis::SymbolKind::Impl => SymbolKind::CLASS,
        compiler_analysis::SymbolKind::Module => SymbolKind::MODULE,
        compiler_analysis::SymbolKind::Use => SymbolKind::NAMESPACE,
    }
}

fn apply_incremental_change(text: &mut String, range: Range, replacement: &str) {
    let start = position_to_byte_offset(text, range.start);
    let end = position_to_byte_offset(text, range.end);

    let start = clamp_to_boundary(text, start);
    let end = clamp_to_boundary(text, end).max(start);

    if start <= end && end <= text.len() {
        text.replace_range(start..end, replacement);
    }
}

fn clamp_to_boundary(text: &str, mut offset: usize) -> usize {
    offset = offset.min(text.len());
    while offset > 0 && !text.is_char_boundary(offset) {
        offset = offset.saturating_sub(1);
    }
    offset
}

fn extract_ident_prefix(src: &str, pos: Position) -> String {
    let offset = position_to_byte_offset(src, pos);
    let offset = clamp_to_boundary(src, offset);

    let mut start = offset;
    while start > 0 {
        let prev = src[..start].chars().next_back().unwrap_or('\0');
        if prev == '_' || prev.is_ascii_alphanumeric() {
            start = src[..start]
                .char_indices()
                .next_back()
                .map(|(i, _)| i)
                .unwrap_or(0);
        } else {
            break;
        }
    }

    src[start..offset].to_string()
}

fn extract_ident_at(src: &str, pos: Position) -> Option<(String, usize, usize)> {
    let offset = position_to_byte_offset(src, pos);
    let offset = clamp_to_boundary(src, offset);

    let mut start = offset;
    while start > 0 {
        let Some((prev_idx, prev_ch)) = src[..start].char_indices().next_back() else {
            break;
        };
        if is_ident_continue(prev_ch) {
            start = prev_idx;
        } else {
            break;
        }
    }

    let mut end = offset;
    while end < src.len() {
        let Some(ch) = src[end..].chars().next() else {
            break;
        };
        if is_ident_continue(ch) {
            end = end.saturating_add(ch.len_utf8());
        } else {
            break;
        }
    }

    if start >= end {
        return None;
    }

    let ident = &src[start..end];
    let first = ident.chars().next()?;
    if !is_ident_start(first) {
        return None;
    }

    Some((ident.to_string(), start, end))
}

fn is_ident_start(ch: char) -> bool {
    ch == '_' || unicode_ident::is_xid_start(ch)
}

fn is_ident_continue(ch: char) -> bool {
    ch == '_' || unicode_ident::is_xid_continue(ch)
}

fn find_definition_in_snapshot(
    snapshot: &compiler_analysis::AnalysisSnapshot,
    name: &str,
    prefer_doc: Option<&SourceName>,
) -> Option<Location> {
    let source_map = snapshot.source_map();

    let mut docs = source_map.source_names();
    if let Some(prefer) = prefer_doc {
        if let Some(idx) = docs.iter().position(|d| d == prefer) {
            let preferred = docs.remove(idx);
            docs.insert(0, preferred);
        }
    }

    for doc in docs {
        let Some(src) = source_map.source_text(&doc) else {
            continue;
        };
        let symbols = compiler_analysis::document_symbols(snapshot, &doc);
        let Some(sym) = find_symbol_by_name(symbols, name) else {
            continue;
        };

        let Some(byte_range) = source_map.lookup_span_bytes(sym.name_span) else {
            continue;
        };
        if byte_range.name != doc {
            continue;
        }

        let uri = match &doc {
            SourceName::Path(p) => Url::from_file_path(p).ok()?,
            SourceName::Virtual(_) => continue,
        };

        let range = byte_range_to_range(&src, byte_range.start, byte_range.end);
        return Some(Location { uri, range });
    }

    None
}

fn find_symbol_by_name(
    symbols: Vec<compiler_analysis::Symbol>,
    name: &str,
) -> Option<compiler_analysis::Symbol> {
    for sym in symbols {
        if sym.name == name {
            return Some(sym);
        }
        if let Some(found) = find_symbol_by_name(sym.children, name) {
            return Some(found);
        }
    }
    None
}

fn rusk_keywords() -> Vec<&'static str> {
    vec![
        "as",
        "break",
        "const",
        "cont",
        "continue",
        "derive",
        "else",
        "enum",
        "fn",
        "for",
        "if",
        "impl",
        "in",
        "interface",
        "intrinsic",
        "let",
        "loop",
        "match",
        "mod",
        "pub",
        "readonly",
        "return",
        "static",
        "struct",
        "type",
        "use",
        "while",
        "is",
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_ident_prefix_works_for_ascii_ident() {
        let src = "fn main() { ret";
        let pos = Position {
            line: 0,
            character: src.len() as u32,
        };
        assert_eq!(extract_ident_prefix(src, pos), "ret");
    }

    #[test]
    fn extract_ident_at_handles_unicode_identifiers() {
        let src = "fn αβ() { () }\nfn main() { αβ(); }\n";
        let pos = Position {
            line: 1,
            character: 12,
        };
        assert_eq!(
            extract_ident_at(src, pos).as_ref().map(|(s, _, _)| s.as_str()),
            Some("αβ")
        );
    }
}
