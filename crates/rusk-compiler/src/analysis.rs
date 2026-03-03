use crate::ast::{
    Block, Expr, FieldName, ImplHeader, ImplItem, ImplMember, InterfaceMember, Item, MatchPat,
    ModKind, Path as AstPath, PathType, Pattern, Program, Stmt, StructBody,
};
use crate::compiler::{CompileError, load_sysroot_items, reject_reserved_module_names};
use crate::modules::{DefKind, ModulePath};
use crate::source::Span;
use crate::source_map::{SourceByteRange, SourceMap, SourceName, SourceRange};
use crate::typeck::{FnTypeInfo, ProgramEnv, Ty, TyCon, TypeInfo};
use crate::vfs::SourceProvider;
use std::collections::BTreeSet;
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
    pub code: Option<String>,
    pub span: Span,
    pub label: Option<String>,
    pub help: Option<String>,
    pub notes: Vec<String>,
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

    // Only reject reserved module names in *user* sources. The sysroot always provides a `core`
    // module, so running this check after sysroot injection would spuriously fail every program.
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

    let mut items = sysroot_items;
    let mut program_items = program.items;
    items.append(&mut program_items);
    let program = Program { items };

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

/// Best-effort go-to-definition query for editor tooling.
///
/// This intentionally focuses on "small but useful" semantic jumps without committing to a full
/// rust-analyzer-style resolution map yet. The current implementation supports:
///
/// - function parameters (pattern binders in the signature)
/// - generic parameters (on functions/methods/structs/enums/interfaces/impls)
/// - struct fields (e.g. `s.a` or `S { a: ... }`)
/// - inherent methods (e.g. `s.m()` or `Type::m()`)
/// - enum variants (e.g. `E::V` / `E::V(...)` / `match ... { E::V(..) => ... }`)
/// - interface methods in effect calls (e.g. `@I.m(...)`)
///
/// The returned location is the file-relative byte range of the definition's *name*.
pub fn goto_definition(
    snapshot: &AnalysisSnapshot,
    document: &SourceName,
    ident_range: (usize, usize),
    ident: &str,
) -> Option<SourceByteRange> {
    if ident.is_empty() {
        return None;
    }

    let source_map = snapshot.source_map();
    let offset = ident_range.0;

    if let Some(def_span) =
        find_generic_param_def(&snapshot.program.items, source_map, document, offset, ident)
    {
        return source_map.lookup_span_bytes(def_span);
    }

    if let Some(def_span) =
        find_param_def(&snapshot.program.items, source_map, document, offset, ident)
    {
        return source_map.lookup_span_bytes(def_span);
    }

    let (Some(env), Some(types)) = (&snapshot.env, &snapshot.types) else {
        return None;
    };

    let def_span = find_member_or_method_def(
        &snapshot.program.items,
        source_map,
        env,
        types,
        document,
        ident_range,
        ident,
    )?;
    source_map.lookup_span_bytes(def_span)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HoverInfo {
    /// A short display string for hover popups (typically an inferred type).
    pub display: String,
    /// The file-relative byte range that the hover applies to (best-effort).
    pub range: Option<SourceByteRange>,
}

/// Best-effort hover query for editor tooling.
///
/// Currently returns the inferred type for the smallest expression containing `offset` within
/// `document`, when type information is available.
pub fn hover(
    snapshot: &AnalysisSnapshot,
    document: &SourceName,
    offset: usize,
) -> Option<HoverInfo> {
    let (Some(env), Some(types)) = (&snapshot.env, &snapshot.types) else {
        return None;
    };

    let source_map = snapshot.source_map();

    let (fn_info, body) = find_fn_type_info_at_offset(
        &snapshot.program.items,
        &ModulePath::root(),
        &snapshot.program.items,
        source_map,
        env,
        types,
        document,
        offset,
    )?;

    let (expr_span, ty) =
        find_smallest_typed_expr_in_block(source_map, document, offset, fn_info, body)?;
    let range = source_map
        .lookup_span_bytes(expr_span)
        .filter(|r| &r.name == document);

    Some(HoverInfo {
        display: ty.to_string(),
        range,
    })
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
        code: None,
        span: err.span,
        label: None,
        help: None,
        notes: Vec::new(),
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

fn find_generic_param_def(
    items: &[Item],
    source_map: &SourceMap,
    document: &SourceName,
    offset: usize,
    ident: &str,
) -> Option<Span> {
    let mut best: Option<(usize, Span)> = None;
    find_generic_param_def_in_items(items, source_map, document, offset, ident, &mut best);
    best.map(|(_, span)| span)
}

fn find_generic_param_def_in_items(
    items: &[Item],
    source_map: &SourceMap,
    document: &SourceName,
    offset: usize,
    ident: &str,
    best: &mut Option<(usize, Span)>,
) {
    for item in items {
        match item {
            Item::Function(f) => {
                if let Some(len) = container_len_if_contains(source_map, f.span, document, offset) {
                    for gp in &f.generics {
                        if gp.name.name == ident {
                            update_best(best, len, gp.name.span);
                        }
                    }
                }
            }
            Item::IntrinsicFn(f) => {
                if let Some(len) = container_len_if_contains(source_map, f.span, document, offset) {
                    for gp in &f.generics {
                        if gp.name.name == ident {
                            update_best(best, len, gp.name.span);
                        }
                    }
                }
            }
            Item::Struct(s) => {
                if let Some(len) = container_len_if_contains(source_map, s.span, document, offset) {
                    for gp in &s.generics {
                        if gp.name.name == ident {
                            update_best(best, len, gp.name.span);
                        }
                    }
                }
            }
            Item::Enum(e) => {
                if let Some(len) = container_len_if_contains(source_map, e.span, document, offset) {
                    for gp in &e.generics {
                        if gp.name.name == ident {
                            update_best(best, len, gp.name.span);
                        }
                    }
                }
            }
            Item::Interface(i) => {
                if let Some(len) = container_len_if_contains(source_map, i.span, document, offset) {
                    for gp in &i.generics {
                        if gp.name.name == ident {
                            update_best(best, len, gp.name.span);
                        }
                    }
                }
                for member in &i.members {
                    let InterfaceMember::Method(m) = member else {
                        continue;
                    };
                    if let Some(len) =
                        container_len_if_contains(source_map, m.span, document, offset)
                    {
                        for gp in &m.generics {
                            if gp.name.name == ident {
                                update_best(best, len, gp.name.span);
                            }
                        }
                    }
                }
            }
            Item::Impl(i) => {
                if let Some(len) = container_len_if_contains(source_map, i.span, document, offset) {
                    for gp in &i.generics {
                        if gp.name.name == ident {
                            update_best(best, len, gp.name.span);
                        }
                    }
                }
                for member in &i.members {
                    let ImplMember::Method(m) = member else {
                        continue;
                    };
                    if let Some(len) =
                        container_len_if_contains(source_map, m.span, document, offset)
                    {
                        for gp in &m.generics {
                            if gp.name.name == ident {
                                update_best(best, len, gp.name.span);
                            }
                        }
                    }
                }
            }
            Item::Mod(m) => {
                if let ModKind::Inline { items: inner } = &m.kind {
                    find_generic_param_def_in_items(
                        inner, source_map, document, offset, ident, best,
                    );
                }
            }
            Item::Use(_) | Item::Derive(_) => {}
        }
    }
}

fn find_param_def(
    items: &[Item],
    source_map: &SourceMap,
    document: &SourceName,
    offset: usize,
    ident: &str,
) -> Option<Span> {
    let mut best: Option<(usize, Span)> = None;
    find_param_def_in_items(items, source_map, document, offset, ident, &mut best);
    best.map(|(_, span)| span)
}

fn find_param_def_in_items(
    items: &[Item],
    source_map: &SourceMap,
    document: &SourceName,
    offset: usize,
    ident: &str,
    best: &mut Option<(usize, Span)>,
) {
    for item in items {
        match item {
            Item::Function(f) => {
                if let Some(len) = container_len_if_contains(source_map, f.span, document, offset)
                    && let Some(span) = find_binding_in_params(&f.params, ident)
                {
                    update_best(best, len, span);
                }
            }
            Item::IntrinsicFn(f) => {
                if let Some(len) = container_len_if_contains(source_map, f.span, document, offset)
                    && let Some(span) = find_binding_in_params(&f.params, ident)
                {
                    update_best(best, len, span);
                }
            }
            Item::Interface(i) => {
                for member in &i.members {
                    let InterfaceMember::Method(m) = member else {
                        continue;
                    };
                    if let Some(len) =
                        container_len_if_contains(source_map, m.span, document, offset)
                        && let Some(span) = find_binding_in_params(&m.params, ident)
                    {
                        update_best(best, len, span);
                    }
                }
            }
            Item::Impl(i) => {
                for member in &i.members {
                    let ImplMember::Method(m) = member else {
                        continue;
                    };
                    if let Some(len) =
                        container_len_if_contains(source_map, m.span, document, offset)
                        && let Some(span) = find_binding_in_params(&m.params, ident)
                    {
                        update_best(best, len, span);
                    }
                }
            }
            Item::Mod(m) => {
                if let ModKind::Inline { items: inner } = &m.kind {
                    find_param_def_in_items(inner, source_map, document, offset, ident, best);
                }
            }
            Item::Struct(_) | Item::Enum(_) | Item::Use(_) | Item::Derive(_) => {}
        }
    }
}

fn find_binding_in_params(params: &[crate::ast::Param], ident: &str) -> Option<Span> {
    for p in params {
        if let Some(span) = find_binding_in_pattern(&p.pat, ident) {
            return Some(span);
        }
    }
    None
}

fn find_binding_in_pattern(pat: &Pattern, ident: &str) -> Option<Span> {
    match pat {
        Pattern::Wildcard { .. } => None,
        Pattern::Bind { name, .. } => {
            if name.name == ident {
                Some(name.span)
            } else {
                None
            }
        }
        Pattern::Literal { .. } => None,
        Pattern::Tuple {
            prefix,
            rest,
            suffix,
            ..
        } => {
            for p in prefix {
                if let Some(span) = find_binding_in_pattern(p, ident) {
                    return Some(span);
                }
            }
            if let Some(rest) = rest
                && let Some(binding) = &rest.binding
                && binding.name == ident
            {
                return Some(binding.span);
            }
            for p in suffix {
                if let Some(span) = find_binding_in_pattern(p, ident) {
                    return Some(span);
                }
            }
            None
        }
        Pattern::Enum { fields, .. } => fields
            .iter()
            .find_map(|p| find_binding_in_pattern(p, ident)),
        Pattern::Ctor { args, .. } => args.iter().find_map(|p| find_binding_in_pattern(p, ident)),
        Pattern::Struct { fields, .. } => fields
            .iter()
            .find_map(|(_field, p)| find_binding_in_pattern(p, ident)),
        Pattern::Array {
            prefix,
            rest,
            suffix,
            ..
        } => {
            for p in prefix {
                if let Some(span) = find_binding_in_pattern(p, ident) {
                    return Some(span);
                }
            }
            if let Some(rest) = rest
                && let Some(binding) = &rest.binding
                && binding.name == ident
            {
                return Some(binding.span);
            }
            for p in suffix {
                if let Some(span) = find_binding_in_pattern(p, ident) {
                    return Some(span);
                }
            }
            None
        }
    }
}

fn find_member_or_method_def(
    items: &[Item],
    source_map: &SourceMap,
    env: &ProgramEnv,
    types: &TypeInfo,
    document: &SourceName,
    ident_range: (usize, usize),
    ident: &str,
) -> Option<Span> {
    find_member_or_method_def_in_items(
        items,
        &ModulePath::root(),
        items,
        source_map,
        env,
        types,
        document,
        ident_range,
        ident,
    )
}

#[allow(clippy::too_many_arguments)]
fn find_member_or_method_def_in_items(
    items: &[Item],
    module: &ModulePath,
    program_items: &[Item],
    source_map: &SourceMap,
    env: &ProgramEnv,
    types: &TypeInfo,
    document: &SourceName,
    ident_range: (usize, usize),
    ident: &str,
) -> Option<Span> {
    for item in items {
        match item {
            Item::Function(f) => {
                let fn_name = module.qualify(&f.name.name);
                let fn_info = types.for_fn(&fn_name);
                if let Some(span) = find_in_fn_body(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    ident,
                    fn_info,
                    &f.body,
                ) {
                    return Some(span);
                }
            }
            Item::Impl(imp) => {
                if let Some(span) = find_in_impl_item(
                    module,
                    imp,
                    program_items,
                    source_map,
                    env,
                    types,
                    document,
                    ident_range,
                    ident,
                ) {
                    return Some(span);
                }
            }
            Item::Mod(m) => {
                if let ModKind::Inline { items: inner } = &m.kind {
                    let child = module.child(&m.name.name);
                    if let Some(span) = find_member_or_method_def_in_items(
                        inner,
                        &child,
                        program_items,
                        source_map,
                        env,
                        types,
                        document,
                        ident_range,
                        ident,
                    ) {
                        return Some(span);
                    }
                }
            }
            Item::Interface(_)
            | Item::Struct(_)
            | Item::Enum(_)
            | Item::IntrinsicFn(_)
            | Item::Use(_)
            | Item::Derive(_) => {}
        }
    }
    None
}

#[allow(clippy::too_many_arguments)]
fn find_fn_type_info_at_offset<'a>(
    items: &'a [Item],
    module: &ModulePath,
    _program_items: &'a [Item],
    source_map: &SourceMap,
    env: &ProgramEnv,
    types: &'a TypeInfo,
    document: &SourceName,
    offset: usize,
) -> Option<(&'a FnTypeInfo, &'a Block)> {
    for item in items {
        match item {
            Item::Function(f) => {
                if container_len_if_contains(source_map, f.body.span, document, offset).is_some() {
                    let fn_name = module.qualify(&f.name.name);
                    if let Some(fn_info) = types.for_fn(&fn_name) {
                        return Some((fn_info, &f.body));
                    }
                }
            }
            Item::Impl(imp) => {
                let (iface_name, type_name) = resolve_impl_header_names(env, module, &imp.header);
                for member in &imp.members {
                    let ImplMember::Method(method) = member else {
                        continue;
                    };

                    if container_len_if_contains(source_map, method.body.span, document, offset)
                        .is_none()
                    {
                        continue;
                    }

                    let fn_name = match (&imp.header, iface_name.as_deref(), type_name.as_deref()) {
                        (ImplHeader::Inherent { .. }, _iface, Some(ty)) => {
                            format!("{ty}::{}", method.name.name)
                        }
                        (ImplHeader::InterfaceForType { .. }, Some(iface), Some(ty)) => {
                            format!("impl::{iface}::for::{ty}::{}", method.name.name)
                        }
                        _ => module.qualify(&method.name.name),
                    };

                    if let Some(fn_info) = types.for_fn(&fn_name) {
                        return Some((fn_info, &method.body));
                    }
                }
            }
            Item::Mod(m) => {
                if let ModKind::Inline { items: inner } = &m.kind {
                    let child = module.child(&m.name.name);
                    if let Some(found) = find_fn_type_info_at_offset(
                        inner,
                        &child,
                        _program_items,
                        source_map,
                        env,
                        types,
                        document,
                        offset,
                    ) {
                        return Some(found);
                    }
                }
            }
            Item::Interface(_)
            | Item::Struct(_)
            | Item::Enum(_)
            | Item::IntrinsicFn(_)
            | Item::Use(_)
            | Item::Derive(_) => {}
        }
    }
    None
}

#[allow(clippy::too_many_arguments)]
fn find_in_impl_item(
    module: &ModulePath,
    imp: &ImplItem,
    program_items: &[Item],
    source_map: &SourceMap,
    env: &ProgramEnv,
    types: &TypeInfo,
    document: &SourceName,
    ident_range: (usize, usize),
    ident: &str,
) -> Option<Span> {
    let (iface_name, type_name) = resolve_impl_header_names(env, module, &imp.header);

    for member in &imp.members {
        let ImplMember::Method(method) = member else {
            continue;
        };

        let fn_name = match (&imp.header, iface_name.as_deref(), type_name.as_deref()) {
            (ImplHeader::Inherent { .. }, _iface, Some(ty)) => {
                format!("{ty}::{}", method.name.name)
            }
            (ImplHeader::InterfaceForType { .. }, Some(iface), Some(ty)) => {
                format!("impl::{iface}::for::{ty}::{}", method.name.name)
            }
            _ => module.qualify(&method.name.name),
        };

        let fn_info = types.for_fn(&fn_name);
        if let Some(span) = find_in_fn_body(
            module,
            program_items,
            source_map,
            env,
            document,
            ident_range,
            ident,
            fn_info,
            &method.body,
        ) {
            return Some(span);
        }
    }

    None
}

#[derive(Clone, Debug)]
struct HoverTypeCandidate {
    len: usize,
    span: Span,
    ty: Ty,
}

fn update_hover_type_candidate(
    best: &mut Option<HoverTypeCandidate>,
    len: usize,
    span: Span,
    ty: &Ty,
) {
    match best {
        None => {
            *best = Some(HoverTypeCandidate {
                len,
                span,
                ty: ty.clone(),
            });
        }
        Some(existing) if len < existing.len => {
            *best = Some(HoverTypeCandidate {
                len,
                span,
                ty: ty.clone(),
            });
        }
        Some(_) => {}
    }
}

fn find_smallest_typed_expr_in_block(
    source_map: &SourceMap,
    document: &SourceName,
    offset: usize,
    fn_info: &FnTypeInfo,
    block: &Block,
) -> Option<(Span, Ty)> {
    let mut best: Option<HoverTypeCandidate> = None;
    find_smallest_typed_expr_in_block_acc(source_map, document, offset, fn_info, block, &mut best);
    best.map(|b| (b.span, b.ty))
}

fn find_smallest_typed_expr_in_block_acc(
    source_map: &SourceMap,
    document: &SourceName,
    offset: usize,
    fn_info: &FnTypeInfo,
    block: &Block,
    best: &mut Option<HoverTypeCandidate>,
) {
    for stmt in &block.stmts {
        find_smallest_typed_expr_in_stmt(source_map, document, offset, fn_info, stmt, best);
    }
    if let Some(tail) = block.tail.as_deref() {
        find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, tail, best);
    }
}

fn find_smallest_typed_expr_in_stmt(
    source_map: &SourceMap,
    document: &SourceName,
    offset: usize,
    fn_info: &FnTypeInfo,
    stmt: &Stmt,
    best: &mut Option<HoverTypeCandidate>,
) {
    match stmt {
        Stmt::Let { init, .. } => {
            if let Some(init) = init {
                find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, init, best);
            }
        }
        Stmt::Return { value, .. } => {
            if let Some(value) = value {
                find_smallest_typed_expr_in_expr(
                    source_map, document, offset, fn_info, value, best,
                );
            }
        }
        Stmt::Expr { expr, .. } => {
            find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, expr, best);
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
}

fn find_smallest_typed_expr_in_expr(
    source_map: &SourceMap,
    document: &SourceName,
    offset: usize,
    fn_info: &FnTypeInfo,
    expr: &Expr,
    best: &mut Option<HoverTypeCandidate>,
) {
    let Some(len) = container_len_if_contains(source_map, expr.span(), document, offset) else {
        return;
    };

    if let Some(ty) = fn_info.expr_types.get(&expr.span()) {
        update_hover_type_candidate(best, len, expr.span(), ty);
    }

    match expr {
        Expr::Call { callee, args, .. } => {
            find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, callee, best);
            for a in args {
                find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, a, best);
            }
        }
        Expr::Field { base, .. } => {
            find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, base, best);
        }
        Expr::StructLit { fields, .. } => {
            for (name, value) in fields {
                if let Some(name_len) =
                    container_len_if_contains(source_map, name.span, document, offset)
                    && let Some(ty) = fn_info.expr_types.get(&value.span())
                {
                    update_hover_type_candidate(best, name_len, name.span, ty);
                }
                find_smallest_typed_expr_in_expr(
                    source_map, document, offset, fn_info, value, best,
                );
            }
        }
        Expr::EffectCall { args, .. } => {
            for a in args {
                find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, a, best);
            }
        }
        Expr::If {
            cond,
            then_block,
            else_branch,
            ..
        } => {
            find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, cond, best);
            find_smallest_typed_expr_in_block_acc(
                source_map, document, offset, fn_info, then_block, best,
            );
            if let Some(else_expr) = else_branch.as_deref() {
                find_smallest_typed_expr_in_expr(
                    source_map, document, offset, fn_info, else_expr, best,
                );
            }
        }
        Expr::Match {
            scrutinee, arms, ..
        } => {
            find_smallest_typed_expr_in_expr(
                source_map, document, offset, fn_info, scrutinee, best,
            );
            for arm in arms {
                find_smallest_typed_expr_in_expr(
                    source_map, document, offset, fn_info, &arm.body, best,
                );
            }
        }
        Expr::Loop { body, .. } | Expr::Lambda { body, .. } => {
            find_smallest_typed_expr_in_block_acc(
                source_map, document, offset, fn_info, body, best,
            );
        }
        Expr::Block { block, .. } => {
            find_smallest_typed_expr_in_block_acc(
                source_map, document, offset, fn_info, block, best,
            );
        }
        Expr::While { cond, body, .. } => {
            find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, cond, best);
            find_smallest_typed_expr_in_block_acc(
                source_map, document, offset, fn_info, body, best,
            );
        }
        Expr::For { iter, body, .. } => {
            find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, iter, best);
            find_smallest_typed_expr_in_block_acc(
                source_map, document, offset, fn_info, body, best,
            );
        }
        Expr::Array { items, .. } | Expr::Tuple { items, .. } => {
            for item in items {
                find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, item, best);
            }
        }
        Expr::Index { base, index, .. } => {
            find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, base, best);
            find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, index, best);
        }
        Expr::Unary { expr, .. }
        | Expr::As { expr, .. }
        | Expr::AsQuestion { expr, .. }
        | Expr::Is { expr, .. } => {
            find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, expr, best);
        }
        Expr::Binary { left, right, .. } => {
            find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, left, best);
            find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, right, best);
        }
        Expr::Assign { target, value, .. } => {
            find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, target, best);
            find_smallest_typed_expr_in_expr(source_map, document, offset, fn_info, value, best);
        }
        Expr::Path { .. }
        | Expr::Unit { .. }
        | Expr::Bool { .. }
        | Expr::Int { .. }
        | Expr::Float { .. }
        | Expr::Char { .. }
        | Expr::String { .. }
        | Expr::Bytes { .. } => {}
    }
}

#[allow(clippy::too_many_arguments)]
fn find_in_fn_body(
    module: &ModulePath,
    program_items: &[Item],
    source_map: &SourceMap,
    env: &ProgramEnv,
    document: &SourceName,
    ident_range: (usize, usize),
    ident: &str,
    fn_info: Option<&FnTypeInfo>,
    body: &Block,
) -> Option<Span> {
    find_in_block(
        module,
        program_items,
        source_map,
        env,
        document,
        ident_range,
        ident,
        fn_info,
        body,
    )
}

#[allow(clippy::too_many_arguments)]
fn find_in_block(
    module: &ModulePath,
    program_items: &[Item],
    source_map: &SourceMap,
    env: &ProgramEnv,
    document: &SourceName,
    ident_range: (usize, usize),
    ident: &str,
    fn_info: Option<&FnTypeInfo>,
    block: &Block,
) -> Option<Span> {
    for stmt in &block.stmts {
        if let Some(span) = find_in_stmt(
            module,
            program_items,
            source_map,
            env,
            document,
            ident_range,
            ident,
            fn_info,
            stmt,
        ) {
            return Some(span);
        }
    }
    if let Some(tail) = &block.tail {
        return find_in_expr(
            module,
            program_items,
            source_map,
            env,
            document,
            ident_range,
            ident,
            fn_info,
            tail,
        );
    }
    None
}

#[allow(clippy::too_many_arguments)]
fn find_in_stmt(
    module: &ModulePath,
    program_items: &[Item],
    source_map: &SourceMap,
    env: &ProgramEnv,
    document: &SourceName,
    ident_range: (usize, usize),
    ident: &str,
    fn_info: Option<&FnTypeInfo>,
    stmt: &Stmt,
) -> Option<Span> {
    match stmt {
        Stmt::Let { pat, init, .. } => {
            if let Some(span) = find_in_pattern(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                pat,
            ) {
                return Some(span);
            }
            if let Some(init) = init {
                return find_in_expr(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    ident,
                    fn_info,
                    init,
                );
            }
            None
        }
        Stmt::Return { value, .. } => value.as_ref().and_then(|v| {
            find_in_expr(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                v,
            )
        }),
        Stmt::Expr { expr, .. } => find_in_expr(
            module,
            program_items,
            source_map,
            env,
            document,
            ident_range,
            ident,
            fn_info,
            expr,
        ),
        Stmt::Break { .. } | Stmt::Continue { .. } => None,
    }
}

#[allow(clippy::too_many_arguments)]
fn find_in_expr(
    module: &ModulePath,
    program_items: &[Item],
    source_map: &SourceMap,
    env: &ProgramEnv,
    document: &SourceName,
    ident_range: (usize, usize),
    ident: &str,
    fn_info: Option<&FnTypeInfo>,
    expr: &Expr,
) -> Option<Span> {
    match expr {
        Expr::Call { callee, args, .. } => {
            if let Expr::Field {
                base,
                name: FieldName::Named(field),
                ..
            } = callee.as_ref()
                && span_matches_range(source_map, document, field.span, ident_range)
                && let Some(fn_info) = fn_info
            {
                let recv_ty = fn_info.expr_types.get(&base.span()).cloned()?;
                return resolve_method_call(
                    program_items,
                    source_map,
                    env,
                    module,
                    &recv_ty,
                    &field.name,
                );
            }

            if let Expr::Path { path, .. } = callee.as_ref()
                && let Some(last) = path.segments.last()
                && span_matches_range(source_map, document, last.span, ident_range)
                && let Some(span) = resolve_path_last_segment(program_items, env, module, path)
            {
                return Some(span);
            }

            if let Some(span) = find_in_expr(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                callee,
            ) {
                return Some(span);
            }
            for a in args {
                if let Some(span) = find_in_expr(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    ident,
                    fn_info,
                    a,
                ) {
                    return Some(span);
                }
            }
            None
        }
        Expr::Field { base, name, .. } => {
            if let FieldName::Named(field) = name
                && span_matches_range(source_map, document, field.span, ident_range)
                && let Some(fn_info) = fn_info
            {
                let recv_ty = fn_info.expr_types.get(&base.span()).cloned()?;
                return resolve_field_access(program_items, env, module, &recv_ty, &field.name);
            }
            find_in_expr(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                base,
            )
        }
        Expr::StructLit {
            type_path, fields, ..
        } => {
            if let Some(last) = type_path.segments.last()
                && span_matches_range(source_map, document, last.span, ident_range)
                && let Some(span) =
                    resolve_nominal_type_name_span(program_items, env, module, type_path)
            {
                return Some(span);
            }
            for (name, value) in fields {
                if span_matches_range(source_map, document, name.span, ident_range) {
                    return resolve_struct_field_by_path(
                        program_items,
                        env,
                        module,
                        type_path,
                        &name.name,
                    );
                }
                if let Some(span) = find_in_expr(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    ident,
                    fn_info,
                    value,
                ) {
                    return Some(span);
                }
            }
            None
        }
        Expr::EffectCall {
            interface,
            method,
            args,
            ..
        } => {
            if span_matches_range(source_map, document, method.span, ident_range) {
                return resolve_effect_method(program_items, env, module, interface, &method.name);
            }
            for a in args {
                if let Some(span) = find_in_expr(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    ident,
                    fn_info,
                    a,
                ) {
                    return Some(span);
                }
            }
            None
        }
        Expr::Path { path, .. } => {
            if path.segments.len() >= 2 {
                // Support go-to-definition on the qualifier segment of a qualified call, e.g.
                // `MyHash::hash(x)` when the cursor is on `MyHash`.
                for idx in 0..path.segments.len().saturating_sub(1) {
                    let seg = path.segments.get(idx)?;
                    if !span_matches_range(source_map, document, seg.span, ident_range) {
                        continue;
                    }

                    let prefix_names: Vec<String> = path.segments[..=idx]
                        .iter()
                        .map(|s| s.name.clone())
                        .collect();
                    let Some((kind, fqn)) = env
                        .modules
                        .try_resolve_type_fqn(module, &prefix_names, path.span)
                        .ok()
                        .flatten()
                    else {
                        continue;
                    };

                    let span = match kind {
                        DefKind::Struct => {
                            find_struct_name_span(program_items, &ModulePath::root(), &fqn)
                        }
                        DefKind::Enum => {
                            find_enum_name_span(program_items, &ModulePath::root(), &fqn)
                        }
                        DefKind::Interface => {
                            find_interface_name_span(program_items, &ModulePath::root(), &fqn)
                        }
                    };
                    if let Some(span) = span {
                        return Some(span);
                    }
                }
            }

            if let Some(last) = path.segments.last()
                && span_matches_range(source_map, document, last.span, ident_range)
            {
                return resolve_path_last_segment(program_items, env, module, path);
            }
            None
        }
        Expr::Match {
            scrutinee, arms, ..
        } => {
            if let Some(span) = find_in_expr(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                scrutinee,
            ) {
                return Some(span);
            }
            for arm in arms {
                match &arm.pat {
                    MatchPat::Value(pat) => {
                        if let Some(span) = find_in_pattern(
                            module,
                            program_items,
                            source_map,
                            env,
                            document,
                            ident_range,
                            ident,
                            pat,
                        ) {
                            return Some(span);
                        }
                    }
                    MatchPat::Effect(ep) => {
                        if span_matches_range(source_map, document, ep.method.span, ident_range) {
                            return resolve_effect_method(
                                program_items,
                                env,
                                module,
                                &ep.interface,
                                &ep.method.name,
                            );
                        }
                    }
                }
                if let Some(span) = find_in_expr(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    ident,
                    fn_info,
                    &arm.body,
                ) {
                    return Some(span);
                }
            }
            None
        }
        Expr::If {
            cond,
            then_block,
            else_branch,
            ..
        } => {
            if let Some(span) = find_in_expr(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                cond,
            ) {
                return Some(span);
            }
            if let Some(span) = find_in_block(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                then_block,
            ) {
                return Some(span);
            }
            if let Some(else_expr) = else_branch.as_deref() {
                return find_in_expr(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    ident,
                    fn_info,
                    else_expr,
                );
            }
            None
        }
        Expr::Loop { body, .. } => find_in_block(
            module,
            program_items,
            source_map,
            env,
            document,
            ident_range,
            ident,
            fn_info,
            body,
        ),
        Expr::While { cond, body, .. } => {
            if let Some(span) = find_in_expr(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                cond,
            ) {
                return Some(span);
            }
            find_in_block(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                body,
            )
        }
        Expr::For {
            pat, iter, body, ..
        } => {
            if let Some(span) = find_in_pattern(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                pat,
            ) {
                return Some(span);
            }
            if let Some(span) = find_in_expr(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                iter,
            ) {
                return Some(span);
            }
            find_in_block(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                body,
            )
        }
        Expr::Block { block, .. } => find_in_block(
            module,
            program_items,
            source_map,
            env,
            document,
            ident_range,
            ident,
            fn_info,
            block,
        ),
        Expr::Array { items, .. } | Expr::Tuple { items, .. } => {
            for item in items {
                if let Some(span) = find_in_expr(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    ident,
                    fn_info,
                    item,
                ) {
                    return Some(span);
                }
            }
            None
        }
        Expr::Index { base, index, .. } => find_in_expr(
            module,
            program_items,
            source_map,
            env,
            document,
            ident_range,
            ident,
            fn_info,
            base,
        )
        .or_else(|| {
            find_in_expr(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                index,
            )
        }),
        Expr::Unary { expr, .. } => find_in_expr(
            module,
            program_items,
            source_map,
            env,
            document,
            ident_range,
            ident,
            fn_info,
            expr,
        ),
        Expr::Binary { left, right, .. } => find_in_expr(
            module,
            program_items,
            source_map,
            env,
            document,
            ident_range,
            ident,
            fn_info,
            left,
        )
        .or_else(|| {
            find_in_expr(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                right,
            )
        }),
        Expr::Assign { target, value, .. } => find_in_expr(
            module,
            program_items,
            source_map,
            env,
            document,
            ident_range,
            ident,
            fn_info,
            target,
        )
        .or_else(|| {
            find_in_expr(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                value,
            )
        }),
        Expr::As { expr, .. } | Expr::AsQuestion { expr, .. } | Expr::Is { expr, .. } => {
            find_in_expr(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                ident,
                fn_info,
                expr,
            )
        }
        Expr::Lambda { body, .. } => find_in_block(
            module,
            program_items,
            source_map,
            env,
            document,
            ident_range,
            ident,
            fn_info,
            body,
        ),
        Expr::Unit { .. }
        | Expr::Bool { .. }
        | Expr::Int { .. }
        | Expr::Float { .. }
        | Expr::Char { .. }
        | Expr::String { .. }
        | Expr::Bytes { .. } => None,
    }
}

#[allow(clippy::too_many_arguments)]
fn find_in_pattern(
    module: &ModulePath,
    program_items: &[Item],
    source_map: &SourceMap,
    env: &ProgramEnv,
    document: &SourceName,
    ident_range: (usize, usize),
    _ident: &str,
    pat: &Pattern,
) -> Option<Span> {
    match pat {
        Pattern::Struct {
            type_path, fields, ..
        } => {
            if let Some(last) = type_path.segments.last()
                && span_matches_range(source_map, document, last.span, ident_range)
                && let Some(span) =
                    resolve_nominal_type_name_span(program_items, env, module, type_path)
            {
                return Some(span);
            }
            for (field, subpat) in fields {
                if span_matches_range(source_map, document, field.span, ident_range) {
                    return resolve_struct_field_by_path(
                        program_items,
                        env,
                        module,
                        type_path,
                        &field.name,
                    );
                }
                if let Some(span) = find_in_pattern(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    _ident,
                    subpat,
                ) {
                    return Some(span);
                }
            }
            None
        }
        Pattern::Enum {
            enum_path,
            variant,
            fields,
            ..
        } => {
            if let Some(last) = enum_path.segments.last()
                && span_matches_range(source_map, document, last.span, ident_range)
                && let Some(span) =
                    resolve_nominal_type_name_span(program_items, env, module, enum_path)
            {
                return Some(span);
            }
            if span_matches_range(source_map, document, variant.span, ident_range) {
                return resolve_enum_variant_by_path(
                    program_items,
                    env,
                    module,
                    enum_path,
                    &variant.name,
                );
            }
            for p in fields {
                if let Some(span) = find_in_pattern(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    _ident,
                    p,
                ) {
                    return Some(span);
                }
            }
            None
        }
        Pattern::Tuple {
            prefix,
            rest,
            suffix,
            ..
        } => {
            for p in prefix {
                if let Some(span) = find_in_pattern(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    _ident,
                    p,
                ) {
                    return Some(span);
                }
            }
            if let Some(rest) = rest
                && let Some(binding) = &rest.binding
                && span_matches_range(source_map, document, binding.span, ident_range)
            {
                // Rest patterns bind a local name, not a member definition.
                return Some(binding.span);
            }
            for p in suffix {
                if let Some(span) = find_in_pattern(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    _ident,
                    p,
                ) {
                    return Some(span);
                }
            }
            None
        }
        Pattern::Array {
            prefix,
            rest,
            suffix,
            ..
        } => {
            for p in prefix {
                if let Some(span) = find_in_pattern(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    _ident,
                    p,
                ) {
                    return Some(span);
                }
            }
            if let Some(rest) = rest
                && let Some(binding) = &rest.binding
                && span_matches_range(source_map, document, binding.span, ident_range)
            {
                return Some(binding.span);
            }
            for p in suffix {
                if let Some(span) = find_in_pattern(
                    module,
                    program_items,
                    source_map,
                    env,
                    document,
                    ident_range,
                    _ident,
                    p,
                ) {
                    return Some(span);
                }
            }
            None
        }
        Pattern::Ctor { args, .. } => args.iter().find_map(|p| {
            find_in_pattern(
                module,
                program_items,
                source_map,
                env,
                document,
                ident_range,
                _ident,
                p,
            )
        }),
        Pattern::Bind { name, .. } => {
            if span_matches_range(source_map, document, name.span, ident_range) {
                Some(name.span)
            } else {
                None
            }
        }
        Pattern::Wildcard { .. } | Pattern::Literal { .. } => None,
    }
}

fn resolve_impl_header_names(
    env: &ProgramEnv,
    module: &ModulePath,
    header: &ImplHeader,
) -> (Option<String>, Option<String>) {
    match header {
        ImplHeader::Inherent { ty, .. } => (None, resolve_path_type_name(env, module, ty)),
        ImplHeader::InterfaceForType { interface, ty, .. } => (
            resolve_path_type_name(env, module, interface),
            resolve_path_type_name(env, module, ty),
        ),
    }
}

fn resolve_path_type_name(
    env: &ProgramEnv,
    module: &ModulePath,
    path: &PathType,
) -> Option<String> {
    let segments: Vec<String> = path.segments.iter().map(|s| s.name.name.clone()).collect();
    env.modules
        .resolve_type_fqn(module, &segments, path.span)
        .ok()
        .map(|(_, fqn)| fqn)
}

fn resolve_struct_field_by_path(
    program_items: &[Item],
    env: &ProgramEnv,
    module: &ModulePath,
    type_path: &AstPath,
    field: &str,
) -> Option<Span> {
    let segments: Vec<String> = type_path.segments.iter().map(|s| s.name.clone()).collect();
    let (kind, fqn) = env
        .modules
        .resolve_type_fqn(module, &segments, type_path.span)
        .ok()?;
    if kind != DefKind::Struct {
        return None;
    }
    find_struct_field_name_span(program_items, &ModulePath::root(), env, &fqn, field)
}

fn resolve_nominal_type_name_span(
    program_items: &[Item],
    env: &ProgramEnv,
    module: &ModulePath,
    type_path: &AstPath,
) -> Option<Span> {
    let segments: Vec<String> = type_path.segments.iter().map(|s| s.name.clone()).collect();
    let (kind, fqn) = env
        .modules
        .resolve_type_fqn(module, &segments, type_path.span)
        .ok()?;
    match kind {
        DefKind::Struct => find_struct_name_span(program_items, &ModulePath::root(), &fqn),
        DefKind::Enum => find_enum_name_span(program_items, &ModulePath::root(), &fqn),
        DefKind::Interface => find_interface_name_span(program_items, &ModulePath::root(), &fqn),
    }
}

fn resolve_enum_variant_by_path(
    program_items: &[Item],
    env: &ProgramEnv,
    module: &ModulePath,
    enum_path: &AstPath,
    variant: &str,
) -> Option<Span> {
    let segments: Vec<String> = enum_path.segments.iter().map(|s| s.name.clone()).collect();
    let (kind, fqn) = env
        .modules
        .resolve_type_fqn(module, &segments, enum_path.span)
        .ok()?;
    if kind != DefKind::Enum {
        return None;
    }
    find_enum_variant_name_span(program_items, &ModulePath::root(), &fqn, variant)
}

fn resolve_effect_method(
    program_items: &[Item],
    env: &ProgramEnv,
    module: &ModulePath,
    interface: &PathType,
    method: &str,
) -> Option<Span> {
    let segments: Vec<String> = interface
        .segments
        .iter()
        .map(|s| s.name.name.clone())
        .collect();
    let (kind, iface_name) = env
        .modules
        .resolve_type_fqn(module, &segments, interface.span)
        .ok()?;
    if kind != DefKind::Interface {
        return None;
    }
    let iface_def = env.interfaces.get(&iface_name)?;
    let method_info = iface_def.all_methods.get(method)?;
    find_interface_method_name_span(
        program_items,
        &ModulePath::root(),
        &method_info.origin,
        method,
    )
}

fn resolve_path_last_segment(
    program_items: &[Item],
    env: &ProgramEnv,
    module: &ModulePath,
    path: &AstPath,
) -> Option<Span> {
    if path.segments.len() < 2 {
        return None;
    }
    let (prefix, last) = path.segments.split_at(path.segments.len() - 1);
    let last = last.first()?;
    let prefix_names: Vec<String> = prefix.iter().map(|s| s.name.clone()).collect();

    let (kind, fqn) = env
        .modules
        .try_resolve_type_fqn(module, &prefix_names, path.span)
        .ok()
        .flatten()?;

    match kind {
        DefKind::Enum => {
            if let Some(span) =
                find_enum_variant_name_span(program_items, &ModulePath::root(), &fqn, &last.name)
            {
                return Some(span);
            }
            find_inherent_method_name_span(
                program_items,
                &ModulePath::root(),
                env,
                &fqn,
                &last.name,
            )
        }
        DefKind::Struct => find_inherent_method_name_span(
            program_items,
            &ModulePath::root(),
            env,
            &fqn,
            &last.name,
        ),
        DefKind::Interface => {
            find_interface_method_name_span(program_items, &ModulePath::root(), &fqn, &last.name)
        }
    }
}

fn resolve_field_access(
    program_items: &[Item],
    env: &ProgramEnv,
    _module: &ModulePath,
    recv_ty: &Ty,
    field: &str,
) -> Option<Span> {
    let ty_name = nominal_type_name(recv_ty)?;
    find_struct_field_name_span(program_items, &ModulePath::root(), env, &ty_name, field)
}

fn resolve_method_call(
    program_items: &[Item],
    _source_map: &SourceMap,
    env: &ProgramEnv,
    module: &ModulePath,
    recv_ty: &Ty,
    method: &str,
) -> Option<Span> {
    match recv_ty {
        Ty::Readonly(inner) => resolve_method_call(
            program_items,
            _source_map,
            env,
            module,
            inner.as_ref(),
            method,
        ),
        Ty::Iface { iface, .. } => {
            let iface_def = env.interfaces.get(iface)?;
            let method_info = iface_def.all_methods.get(method)?;
            find_interface_method_name_span(
                program_items,
                &ModulePath::root(),
                &method_info.origin,
                method,
            )
        }
        Ty::App(TyCon::Named(type_name), _args) => {
            if let Some(span) = find_inherent_method_name_span(
                program_items,
                &ModulePath::root(),
                env,
                type_name,
                method,
            ) {
                return Some(span);
            }

            let origin_iface =
                select_unique_accessible_origin_iface(env, module, type_name, method)?;
            let fn_name = env
                .interface_methods
                .get(&(type_name.clone(), origin_iface.clone(), method.to_string()))?
                .clone();

            if let Some((impl_iface, impl_ty, impl_method)) = parse_interface_impl_fn_name(&fn_name)
                && impl_method == method
                && let Some(span) = find_interface_impl_method_name_span(
                    program_items,
                    &ModulePath::root(),
                    env,
                    &impl_iface,
                    &impl_ty,
                    method,
                )
            {
                return Some(span);
            }

            // Fallback: default methods have no explicit impl method item.
            find_interface_method_name_span(
                program_items,
                &ModulePath::root(),
                &origin_iface,
                method,
            )
        }
        _ => None,
    }
}

fn nominal_type_name(ty: &Ty) -> Option<String> {
    match ty {
        Ty::Readonly(inner) => nominal_type_name(inner.as_ref()),
        Ty::App(TyCon::Named(name), _args) => Some(name.clone()),
        _ => None,
    }
}

fn select_unique_accessible_origin_iface(
    env: &ProgramEnv,
    from_module: &ModulePath,
    type_name: &str,
    method: &str,
) -> Option<String> {
    let mut candidates = BTreeSet::<String>::new();
    for (dyn_ty, origin_iface, mname) in env.interface_methods.keys() {
        if dyn_ty != type_name || mname != method {
            continue;
        }
        if let Some(def) = env.modules.def(origin_iface)
            && !def.vis.is_public()
            && !from_module.is_descendant_of(&def.defining_module)
        {
            continue;
        }
        candidates.insert(origin_iface.clone());
    }
    if candidates.len() == 1 {
        candidates.into_iter().next()
    } else {
        None
    }
}

fn parse_interface_impl_fn_name(name: &str) -> Option<(String, String, String)> {
    let rest = name.strip_prefix("impl::")?;
    let (iface, rest) = rest.split_once("::for::")?;
    let parts: Vec<&str> = rest.split("::").collect();
    if parts.len() < 2 {
        return None;
    }
    let method = parts.last()?.to_string();
    let ty = parts[..parts.len().saturating_sub(1)].join("::");
    Some((iface.to_string(), ty, method))
}

fn find_struct_field_name_span(
    items: &[Item],
    module: &ModulePath,
    env: &ProgramEnv,
    struct_fqn: &str,
    field: &str,
) -> Option<Span> {
    for item in items {
        match item {
            Item::Struct(s) => {
                let fqn = module.qualify(&s.name.name);
                if fqn != struct_fqn {
                    continue;
                }
                let StructBody::Named { fields } = &s.body else {
                    return None;
                };
                for f in fields {
                    if f.name.name == field {
                        return Some(f.name.span);
                    }
                }
                return None;
            }
            Item::Mod(m) => {
                if let ModKind::Inline { items: inner } = &m.kind {
                    let child = module.child(&m.name.name);
                    if let Some(span) =
                        find_struct_field_name_span(inner, &child, env, struct_fqn, field)
                    {
                        return Some(span);
                    }
                }
            }
            _ => {}
        }
    }
    let _ = env; // keep signature stable if we later need resolver context.
    None
}

fn find_struct_name_span(items: &[Item], module: &ModulePath, struct_fqn: &str) -> Option<Span> {
    for item in items {
        match item {
            Item::Struct(s) => {
                let fqn = module.qualify(&s.name.name);
                if fqn == struct_fqn {
                    return Some(s.name.span);
                }
            }
            Item::Mod(m) => {
                if let ModKind::Inline { items: inner } = &m.kind {
                    let child = module.child(&m.name.name);
                    if let Some(span) = find_struct_name_span(inner, &child, struct_fqn) {
                        return Some(span);
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn find_enum_variant_name_span(
    items: &[Item],
    module: &ModulePath,
    enum_fqn: &str,
    variant: &str,
) -> Option<Span> {
    for item in items {
        match item {
            Item::Enum(e) => {
                let fqn = module.qualify(&e.name.name);
                if fqn != enum_fqn {
                    continue;
                }
                for v in &e.variants {
                    if v.name.name == variant {
                        return Some(v.name.span);
                    }
                }
                return None;
            }
            Item::Mod(m) => {
                if let ModKind::Inline { items: inner } = &m.kind {
                    let child = module.child(&m.name.name);
                    if let Some(span) =
                        find_enum_variant_name_span(inner, &child, enum_fqn, variant)
                    {
                        return Some(span);
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn find_enum_name_span(items: &[Item], module: &ModulePath, enum_fqn: &str) -> Option<Span> {
    for item in items {
        match item {
            Item::Enum(e) => {
                let fqn = module.qualify(&e.name.name);
                if fqn == enum_fqn {
                    return Some(e.name.span);
                }
            }
            Item::Mod(m) => {
                if let ModKind::Inline { items: inner } = &m.kind {
                    let child = module.child(&m.name.name);
                    if let Some(span) = find_enum_name_span(inner, &child, enum_fqn) {
                        return Some(span);
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn find_interface_method_name_span(
    items: &[Item],
    module: &ModulePath,
    iface_fqn: &str,
    method: &str,
) -> Option<Span> {
    for item in items {
        match item {
            Item::Interface(i) => {
                let fqn = module.qualify(&i.name.name);
                if fqn != iface_fqn {
                    continue;
                }
                for member in &i.members {
                    let InterfaceMember::Method(m) = member else {
                        continue;
                    };
                    if m.name.name == method {
                        return Some(m.name.span);
                    }
                }
                return None;
            }
            Item::Mod(m) => {
                if let ModKind::Inline { items: inner } = &m.kind {
                    let child = module.child(&m.name.name);
                    if let Some(span) =
                        find_interface_method_name_span(inner, &child, iface_fqn, method)
                    {
                        return Some(span);
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn find_interface_name_span(items: &[Item], module: &ModulePath, iface_fqn: &str) -> Option<Span> {
    for item in items {
        match item {
            Item::Interface(i) => {
                let fqn = module.qualify(&i.name.name);
                if fqn == iface_fqn {
                    return Some(i.name.span);
                }
            }
            Item::Mod(m) => {
                if let ModKind::Inline { items: inner } = &m.kind {
                    let child = module.child(&m.name.name);
                    if let Some(span) = find_interface_name_span(inner, &child, iface_fqn) {
                        return Some(span);
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn find_inherent_method_name_span(
    items: &[Item],
    module: &ModulePath,
    env: &ProgramEnv,
    type_fqn: &str,
    method: &str,
) -> Option<Span> {
    for item in items {
        match item {
            Item::Impl(imp) => {
                let ImplHeader::Inherent { ty, .. } = &imp.header else {
                    continue;
                };
                let Some(impl_ty) = resolve_path_type_name(env, module, ty) else {
                    continue;
                };
                if impl_ty != type_fqn {
                    continue;
                }
                for member in &imp.members {
                    let ImplMember::Method(m) = member else {
                        continue;
                    };
                    if m.name.name == method {
                        return Some(m.name.span);
                    }
                }
            }
            Item::Mod(m) => {
                if let ModKind::Inline { items: inner } = &m.kind {
                    let child = module.child(&m.name.name);
                    if let Some(span) =
                        find_inherent_method_name_span(inner, &child, env, type_fqn, method)
                    {
                        return Some(span);
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn find_interface_impl_method_name_span(
    items: &[Item],
    module: &ModulePath,
    env: &ProgramEnv,
    iface_fqn: &str,
    type_fqn: &str,
    method: &str,
) -> Option<Span> {
    for item in items {
        match item {
            Item::Impl(imp) => {
                let ImplHeader::InterfaceForType { interface, ty, .. } = &imp.header else {
                    continue;
                };
                let Some(impl_iface) = resolve_path_type_name(env, module, interface) else {
                    continue;
                };
                let Some(impl_ty) = resolve_path_type_name(env, module, ty) else {
                    continue;
                };
                if impl_iface != iface_fqn || impl_ty != type_fqn {
                    continue;
                }
                for member in &imp.members {
                    let ImplMember::Method(m) = member else {
                        continue;
                    };
                    if m.name.name == method {
                        return Some(m.name.span);
                    }
                }
            }
            Item::Mod(m) => {
                if let ModKind::Inline { items: inner } = &m.kind {
                    let child = module.child(&m.name.name);
                    if let Some(span) = find_interface_impl_method_name_span(
                        inner, &child, env, iface_fqn, type_fqn, method,
                    ) {
                        return Some(span);
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn container_len_if_contains(
    source_map: &SourceMap,
    span: Span,
    document: &SourceName,
    offset: usize,
) -> Option<usize> {
    let br = source_map.lookup_span_bytes(span)?;
    if &br.name != document {
        return None;
    }
    if offset < br.start || offset > br.end {
        return None;
    }
    Some(br.end.saturating_sub(br.start))
}

fn update_best(best: &mut Option<(usize, Span)>, container_len: usize, def_span: Span) {
    if best.is_none() || best.is_some_and(|(len, _)| container_len < len) {
        *best = Some((container_len, def_span));
    }
}

fn span_matches_range(
    source_map: &SourceMap,
    document: &SourceName,
    span: Span,
    ident_range: (usize, usize),
) -> bool {
    source_map
        .lookup_span_bytes(span)
        .is_some_and(|r| &r.name == document && r.start == ident_range.0 && r.end == ident_range.1)
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
