use crate::ast::{ImplHeader, Item, ModKind, PathType, Program};
use crate::compiler::{CompileError, load_sysroot_items, reject_reserved_module_names};
use crate::source::Span;
use crate::source_map::{SourceByteRange, SourceMap, SourceName, SourceRange};
use crate::typeck::{ProgramEnv, TypeInfo};
use crate::vfs::SourceProvider;
use std::path::{Path, PathBuf};
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub message: String,
    pub severity: DiagnosticSeverity,
    pub span: Span,
    pub source: Option<SourceName>,
    pub range: Option<SourceRange>,
    pub byte_range: Option<SourceByteRange>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    IntrinsicFunction,
    Struct,
    Enum,
    Interface,
    Impl,
    Module,
    Use,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub span: Span,
    pub name_span: Span,
    pub children: Vec<Symbol>,
}

#[derive(Clone, Debug)]
pub struct AnalysisSnapshot {
    source_map: SourceMap,
    program: Program,
    #[allow(dead_code)]
    env: Option<ProgramEnv>,
    #[allow(dead_code)]
    types: Option<TypeInfo>,
}

impl AnalysisSnapshot {
    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }
}

#[derive(Clone, Debug)]
pub struct AnalysisResult {
    pub snapshot: Option<AnalysisSnapshot>,
    pub diagnostics: Vec<Diagnostic>,
    pub source_map: SourceMap,
}

pub fn analyze_entry_file(
    entry_path: &Path,
    options: &crate::host::CompileOptions,
    source_provider: Arc<dyn SourceProvider>,
) -> AnalysisResult {
    let mut loader = crate::modules::ModuleLoader::with_source_provider(source_provider);

    let sysroot_items = match load_sysroot_items(&mut loader, options) {
        Ok(items) => items,
        Err(err) => {
            let source_map = loader.source_map().clone();
            let diag = diagnostic_from_compile_error(err, &source_map);
            return AnalysisResult {
                snapshot: None,
                diagnostics: vec![diag],
                source_map,
            };
        }
    };

    let program = match loader.load_program_from_file(entry_path) {
        Ok(program) => program,
        Err(err) => {
            let err = CompileError::from(err);
            let mut source_map = loader.source_map().clone();

            // If the failing file was never added to the `SourceMap`, span 0 would otherwise map
            // to the first sysroot file. Keep error locations stable by creating a minimal map.
            if err.span == Span::new(0, 0) && err.message.starts_with("failed to read `") {
                source_map = fallback_source_map_for_unreadable_path(entry_path, &err.message);
            }

            let diag = diagnostic_from_compile_error(err, &source_map);
            return AnalysisResult {
                snapshot: None,
                diagnostics: vec![diag],
                source_map,
            };
        }
    };

    let mut items = sysroot_items;
    let mut program_items = program.items;
    items.append(&mut program_items);
    let program = Program { items };

    if let Err(err) = reject_reserved_module_names(&program.items) {
        let source_map = loader.source_map().clone();
        let diag = diagnostic_from_compile_error(err, &source_map);
        return AnalysisResult {
            snapshot: Some(AnalysisSnapshot {
                source_map: source_map.clone(),
                program,
                env: None,
                types: None,
            }),
            diagnostics: vec![diag],
            source_map,
        };
    }

    let (program, derive_error) = match crate::derive_expand::expand_derives(&program) {
        Ok(expanded) => (expanded, None),
        Err(err) => (program, Some(err)),
    };

    let mut diagnostics = Vec::new();
    let mut env = None;
    let mut types = None;

    let source_map = loader.source_map().clone();

    if let Some(err) = derive_error {
        diagnostics.push(diagnostic_from_compile_error(err, &source_map));
    } else {
        match crate::compiler::typecheck_program_with_entry_validation(&program, options) {
            Ok((e, t)) => {
                env = Some(e);
                types = Some(t);
            }
            Err(err) => diagnostics.push(diagnostic_from_compile_error(err, &source_map)),
        }
    }

    AnalysisResult {
        snapshot: Some(AnalysisSnapshot {
            source_map: source_map.clone(),
            program,
            env,
            types,
        }),
        diagnostics,
        source_map,
    }
}

pub fn document_symbols(snapshot: &AnalysisSnapshot, document: &SourceName) -> Vec<Symbol> {
    symbols_for_items(&snapshot.program.items, snapshot.source_map(), document)
}

fn diagnostic_from_compile_error(err: CompileError, source_map: &SourceMap) -> Diagnostic {
    let range = source_map.lookup_span(err.span);
    let byte_range = source_map.lookup_span_bytes(err.span);

    let source = range
        .as_ref()
        .map(|r| r.name.clone())
        .or_else(|| byte_range.as_ref().map(|r| r.name.clone()));

    Diagnostic {
        message: err.message,
        severity: DiagnosticSeverity::Error,
        span: err.span,
        source,
        range,
        byte_range,
    }
}

fn fallback_source_map_for_unreadable_path(entry_path: &Path, loader_message: &str) -> SourceMap {
    let path = loader_message
        .strip_prefix("failed to read `")
        .and_then(|rest| rest.split_once('`').map(|(p, _)| PathBuf::from(p)))
        .unwrap_or_else(|| entry_path.to_path_buf());

    let mut fallback = SourceMap::new();
    fallback.add_source(SourceName::Path(path), Arc::<str>::from(""), 0);
    fallback
}

fn symbols_for_items(items: &[Item], source_map: &SourceMap, document: &SourceName) -> Vec<Symbol> {
    let mut out = Vec::new();
    collect_symbols(items, source_map, document, &mut out);
    out
}

fn collect_symbols(
    items: &[Item],
    source_map: &SourceMap,
    document: &SourceName,
    out: &mut Vec<Symbol>,
) {
    for item in items {
        match item {
            Item::Function(f) => {
                if span_in_document(source_map, f.span, document) {
                    out.push(Symbol {
                        name: f.name.name.clone(),
                        kind: SymbolKind::Function,
                        span: f.span,
                        name_span: f.name.span,
                        children: Vec::new(),
                    });
                }
            }
            Item::IntrinsicFn(f) => {
                if span_in_document(source_map, f.span, document) {
                    out.push(Symbol {
                        name: f.name.name.clone(),
                        kind: SymbolKind::IntrinsicFunction,
                        span: f.span,
                        name_span: f.name.span,
                        children: Vec::new(),
                    });
                }
            }
            Item::Struct(s) => {
                if span_in_document(source_map, s.span, document) {
                    out.push(Symbol {
                        name: s.name.name.clone(),
                        kind: SymbolKind::Struct,
                        span: s.span,
                        name_span: s.name.span,
                        children: Vec::new(),
                    });
                }
            }
            Item::Enum(e) => {
                if span_in_document(source_map, e.span, document) {
                    out.push(Symbol {
                        name: e.name.name.clone(),
                        kind: SymbolKind::Enum,
                        span: e.span,
                        name_span: e.name.span,
                        children: Vec::new(),
                    });
                }
            }
            Item::Interface(i) => {
                if span_in_document(source_map, i.span, document) {
                    out.push(Symbol {
                        name: i.name.name.clone(),
                        kind: SymbolKind::Interface,
                        span: i.span,
                        name_span: i.name.span,
                        children: Vec::new(),
                    });
                }
            }
            Item::Impl(i) => {
                if span_in_document(source_map, i.span, document) {
                    let name = format_impl_header(&i.header);
                    let name_span = match &i.header {
                        ImplHeader::Inherent { span, .. }
                        | ImplHeader::InterfaceForType { span, .. } => *span,
                    };
                    out.push(Symbol {
                        name,
                        kind: SymbolKind::Impl,
                        span: i.span,
                        name_span,
                        children: Vec::new(),
                    });
                }
            }
            Item::Mod(m) => {
                if span_in_document(source_map, m.span, document) {
                    let mut sym = Symbol {
                        name: m.name.name.clone(),
                        kind: SymbolKind::Module,
                        span: m.span,
                        name_span: m.name.span,
                        children: Vec::new(),
                    };
                    if let ModKind::Inline { items: inner } = &m.kind {
                        sym.children = symbols_for_items(inner, source_map, document);
                    }
                    out.push(sym);
                } else if let ModKind::Inline { items: inner } = &m.kind {
                    // This module declaration is in a different file (e.g. `mod foo;`), but its
                    // inlined items may belong to `document`.
                    collect_symbols(inner, source_map, document, out);
                }
            }
            Item::Use(u) => {
                if span_in_document(source_map, u.span, document) {
                    out.push(use_symbol(u));
                }
            }
            Item::Derive(_) => {
                // Keep document symbols focused on user-declared items.
            }
        }
    }
}

fn span_in_document(source_map: &SourceMap, span: Span, document: &SourceName) -> bool {
    source_map
        .lookup_span_bytes(span)
        .is_some_and(|r| &r.name == document)
}

fn use_symbol(u: &crate::ast::UseItem) -> Symbol {
    let name = if let Some(alias) = &u.alias {
        alias.name.clone()
    } else {
        u.path
            .segments
            .iter()
            .map(|s| s.name.as_str())
            .collect::<Vec<_>>()
            .join("::")
    };
    let name_span = u
        .alias
        .as_ref()
        .map(|a| a.span)
        .unwrap_or_else(|| u.path.span);
    Symbol {
        name,
        kind: SymbolKind::Use,
        span: u.span,
        name_span,
        children: Vec::new(),
    }
}

fn format_impl_header(header: &ImplHeader) -> String {
    match header {
        ImplHeader::Inherent { ty, .. } => format!("impl {}", display_path_type_simple(ty)),
        ImplHeader::InterfaceForType { interface, ty, .. } => format!(
            "impl {} for {}",
            display_path_type_simple(interface),
            display_path_type_simple(ty)
        ),
    }
}

fn display_path_type_simple(ty: &PathType) -> String {
    ty.segments
        .iter()
        .map(|s| s.name.name.as_str())
        .collect::<Vec<_>>()
        .join("::")
}
