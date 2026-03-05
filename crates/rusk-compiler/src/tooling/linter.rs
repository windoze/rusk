use crate::analysis::{Diagnostic, DiagnosticSeverity};
use crate::ast::{Block, Expr, Item, MatchPat, Pattern, Program, Stmt};
use crate::parser::{ParseError, Parser};
use crate::source::Span;
use crate::source_map::{SourceMap, SourceName};
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

#[derive(Clone, Copy, Debug, Default)]
pub struct LintOptions {
    pub deny_warnings: bool,
}

#[derive(Clone, Debug)]
pub struct LintOutput {
    pub diagnostics: Vec<Diagnostic>,
    pub source_map: SourceMap,
}

pub fn lint_source(source_name: SourceName, src: &str, options: &LintOptions) -> LintOutput {
    let source = Arc::<str>::from(src);
    let mut source_map = SourceMap::new();
    source_map.add_source(source_name.clone(), Arc::clone(&source), 0);

    let program = match parse_program(src) {
        Ok(p) => p,
        Err(err) => {
            let diag = diagnostic_from_parse_error(err, &source_map, Some(source_name));
            return LintOutput {
                diagnostics: vec![diag],
                source_map,
            };
        }
    };

    let mut diags = Vec::new();
    lint_program(&program, &mut diags, &source_map);

    if options.deny_warnings {
        for diag in &mut diags {
            if diag.severity == DiagnosticSeverity::Warning {
                diag.severity = DiagnosticSeverity::Error;
            }
        }
    }

    LintOutput {
        diagnostics: diags,
        source_map,
    }
}

pub fn lint_file(path: &Path, options: &LintOptions) -> LintOutput {
    let src = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(err) => {
            let mut source_map = SourceMap::new();
            let name = SourceName::Path(path.to_path_buf());
            source_map.add_source(name.clone(), Arc::<str>::from(""), 0);
            let diag = Diagnostic {
                message: format!("failed to read file: {err}"),
                severity: DiagnosticSeverity::Error,
                code: Some("io_error".to_string()),
                span: Span::new(0, 0),
                label: None,
                help: None,
                notes: Vec::new(),
                source: Some(name.clone()),
                range: None,
                byte_range: None,
            };
            return LintOutput {
                diagnostics: vec![diag],
                source_map,
            };
        }
    };
    lint_source(SourceName::Path(path.to_path_buf()), &src, options)
}

fn parse_program(src: &str) -> Result<Program, ParseError> {
    let mut parser = Parser::with_base_offset(src, 0)?;
    parser.parse_program()
}

fn diagnostic_from_parse_error(
    err: ParseError,
    source_map: &SourceMap,
    source_name: Option<SourceName>,
) -> Diagnostic {
    let span = err.span;
    Diagnostic {
        message: err.message,
        severity: DiagnosticSeverity::Error,
        code: Some("parse_error".to_string()),
        span,
        label: None,
        help: None,
        notes: Vec::new(),
        source: source_name,
        range: source_map.lookup_span(span),
        byte_range: source_map.lookup_span_bytes(span),
    }
}

fn lint_program(program: &Program, diags: &mut Vec<Diagnostic>, source_map: &SourceMap) {
    for item in &program.items {
        lint_item(item, diags, source_map);
    }
}

fn lint_item(item: &Item, diags: &mut Vec<Diagnostic>, source_map: &SourceMap) {
    match item {
        Item::Function(f) => {
            let mut ctx = VarLintCtx::new(diags, source_map);
            ctx.enter_scope();
            for param in &f.params {
                ctx.declare_pattern(&param.pat);
            }
            ctx.lint_block(&f.body);
            ctx.exit_scope();

            lint_unreachable_in_block(&f.body, diags, source_map);
            lint_needless_bool_in_block(&f.body, diags, source_map);
            lint_suspicious_comparisons_in_block(&f.body, diags, source_map);
            lint_redundant_else_in_block(&f.body, diags, source_map);
        }
        Item::ExternFn(_) => {}
        Item::IntrinsicFn(_) => {}
        Item::Struct(_) => {}
        Item::Enum(_) => {}
        Item::Interface(_) => {}
        Item::Impl(imp) => {
            for member in &imp.members {
                match member {
                    crate::ast::ImplMember::Method(m) => {
                        let mut ctx = VarLintCtx::new(diags, source_map);
                        ctx.enter_scope();
                        if let crate::ast::FnItemKind::Method { receiver } = m.kind
                            && matches!(receiver, crate::ast::MethodReceiverKind::Instance { .. })
                        {
                            // Implicit `self` is in scope for methods.
                            ctx.declare_name("self".to_string(), m.name.span);
                        }
                        for param in &m.params {
                            ctx.declare_pattern(&param.pat);
                        }
                        ctx.lint_block(&m.body);
                        ctx.exit_scope();

                        lint_unreachable_in_block(&m.body, diags, source_map);
                        lint_needless_bool_in_block(&m.body, diags, source_map);
                        lint_suspicious_comparisons_in_block(&m.body, diags, source_map);
                        lint_redundant_else_in_block(&m.body, diags, source_map);
                    }
                    crate::ast::ImplMember::AssocType(_) => {}
                }
            }
        }
        Item::Mod(_) => {}
        Item::Use(_) => {}
        Item::Derive(_) => {}
    }
}

fn make_lint_diag(
    code: &str,
    message: impl Into<String>,
    span: Span,
    label: Option<String>,
    help: Option<String>,
    source_map: &SourceMap,
) -> Diagnostic {
    Diagnostic {
        message: message.into(),
        severity: DiagnosticSeverity::Warning,
        code: Some(code.to_string()),
        span,
        label,
        help,
        notes: Vec::new(),
        source: source_map.lookup_span(span).map(|r| r.name),
        range: source_map.lookup_span(span),
        byte_range: source_map.lookup_span_bytes(span),
    }
}

struct VarInfo {
    span: Span,
    used: bool,
    ignore: bool,
}

struct VarLintCtx<'a> {
    scopes: Vec<HashMap<String, VarInfo>>,
    diags: &'a mut Vec<Diagnostic>,
    source_map: &'a SourceMap,
}

impl<'a> VarLintCtx<'a> {
    fn new(diags: &'a mut Vec<Diagnostic>, source_map: &'a SourceMap) -> Self {
        Self {
            scopes: Vec::new(),
            diags,
            source_map,
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        let Some(scope) = self.scopes.pop() else {
            return;
        };
        for (name, info) in scope {
            if info.ignore {
                continue;
            }
            if !info.used {
                self.diags.push(make_lint_diag(
                    "unused_variable",
                    format!("unused variable `{name}`"),
                    info.span,
                    Some("remove it, or prefix with `_` to silence this lint".to_string()),
                    None,
                    self.source_map,
                ));
            }
        }
    }

    fn declare_name(&mut self, name: String, span: Span) {
        let ignore = name.starts_with('_');
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name,
                VarInfo {
                    span,
                    used: false,
                    ignore,
                },
            );
        }
    }

    fn declare_pattern(&mut self, pat: &Pattern) {
        let mut binds = Vec::new();
        collect_pattern_binds(pat, &mut binds);
        for (name, span) in binds {
            self.declare_name(name, span);
        }
    }

    fn mark_used(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) {
                info.used = true;
                return;
            }
        }
    }

    fn lint_block(&mut self, block: &Block) {
        self.enter_scope();
        for stmt in &block.stmts {
            self.lint_stmt(stmt);
        }
        if let Some(tail) = block.tail.as_deref() {
            self.lint_expr(tail);
        }
        self.exit_scope();
    }

    fn lint_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let { pat, init, .. } => {
                if let Some(init) = init {
                    self.lint_expr(init);
                }
                self.declare_pattern(pat);
            }
            Stmt::Return { value, .. } => {
                if let Some(value) = value {
                    self.lint_expr(value);
                }
            }
            Stmt::Break { .. } | Stmt::Continue { .. } => {}
            Stmt::Expr { expr, .. } => self.lint_expr(expr),
        }
    }

    fn lint_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Unit { .. }
            | Expr::Bool { .. }
            | Expr::Int { .. }
            | Expr::Float { .. }
            | Expr::Char { .. }
            | Expr::String { .. }
            | Expr::Bytes { .. } => {}

            Expr::Path { path, .. } => {
                if path.segments.len() == 1 {
                    self.mark_used(&path.segments[0].name);
                }
            }
            Expr::Array { items, .. } | Expr::Tuple { items, .. } => {
                for item in items {
                    self.lint_expr(item);
                }
            }
            Expr::StructLit { fields, .. } => {
                for (_, value) in fields {
                    self.lint_expr(value);
                }
            }
            Expr::EffectCall { args, .. } => {
                for arg in args {
                    self.lint_expr(arg);
                }
            }
            Expr::Lambda { params, body, .. } => {
                self.enter_scope();
                for p in params {
                    self.declare_name(p.name.name.clone(), p.name.span);
                }
                self.lint_block(body);
                self.exit_scope();
            }
            Expr::If {
                cond,
                then_block,
                else_branch,
                ..
            } => {
                self.lint_expr(cond);
                self.lint_block(then_block);
                if let Some(e) = else_branch.as_deref() {
                    self.lint_expr(e);
                }
            }
            Expr::Match {
                scrutinee, arms, ..
            } => {
                self.lint_expr(scrutinee);
                for arm in arms {
                    self.enter_scope();
                    self.declare_match_pat(&arm.pat);
                    self.lint_expr(&arm.body);
                    self.exit_scope();
                }
            }
            Expr::Loop { body, .. } => self.lint_block(body),
            Expr::While { cond, body, .. } => {
                self.lint_expr(cond);
                self.lint_block(body);
            }
            Expr::For {
                pat, iter, body, ..
            } => {
                self.lint_expr(iter);
                self.enter_scope();
                self.declare_pattern(pat);
                self.lint_block(body);
                self.exit_scope();
            }
            Expr::Block { block, .. } => self.lint_block(block),

            Expr::Call { callee, args, .. } => {
                self.lint_expr(callee);
                for arg in args {
                    self.lint_expr(arg);
                }
            }
            Expr::Field { base, .. } => self.lint_expr(base),
            Expr::Index { base, index, .. } => {
                self.lint_expr(base);
                self.lint_expr(index);
            }
            Expr::Unary { expr, .. } => self.lint_expr(expr),
            Expr::Binary { left, right, .. } => {
                self.lint_expr(left);
                self.lint_expr(right);
            }
            Expr::Assign { target, value, .. } => {
                self.lint_expr(target);
                self.lint_expr(value);
            }
            Expr::As { expr, .. } | Expr::AsQuestion { expr, .. } | Expr::Is { expr, .. } => {
                self.lint_expr(expr);
            }
        }
    }

    fn declare_match_pat(&mut self, pat: &MatchPat) {
        match pat {
            MatchPat::Value(p) => self.declare_pattern(p),
            MatchPat::Effect(effect) => {
                for p in &effect.args {
                    self.declare_pattern(p);
                }
                if let Some(cont) = &effect.cont {
                    self.declare_name(cont.name.clone(), cont.span);
                } else {
                    self.declare_name("resume".to_string(), effect.span);
                }
            }
        }
    }
}

fn collect_pattern_binds(pat: &Pattern, out: &mut Vec<(String, Span)>) {
    match pat {
        Pattern::Wildcard { .. } | Pattern::Literal { .. } => {}
        Pattern::Bind { name, .. } => out.push((name.name.clone(), name.span)),
        Pattern::Tuple {
            prefix,
            rest,
            suffix,
            ..
        } => {
            for p in prefix {
                collect_pattern_binds(p, out);
            }
            if let Some(rest) = rest
                && let Some(binding) = &rest.binding
            {
                out.push((binding.name.clone(), binding.span));
            }
            for p in suffix {
                collect_pattern_binds(p, out);
            }
        }
        Pattern::Enum { fields, .. } | Pattern::Ctor { args: fields, .. } => {
            for p in fields {
                collect_pattern_binds(p, out);
            }
        }
        Pattern::Struct { fields, .. } => {
            for (_, p) in fields {
                collect_pattern_binds(p, out);
            }
        }
        Pattern::Array {
            prefix,
            rest,
            suffix,
            ..
        } => {
            for p in prefix {
                collect_pattern_binds(p, out);
            }
            if let Some(rest) = rest
                && let Some(binding) = &rest.binding
            {
                out.push((binding.name.clone(), binding.span));
            }
            for p in suffix {
                collect_pattern_binds(p, out);
            }
        }
    }
}

fn lint_unreachable_in_block(block: &Block, diags: &mut Vec<Diagnostic>, source_map: &SourceMap) {
    let mut terminated = false;
    for stmt in &block.stmts {
        if terminated {
            diags.push(make_lint_diag(
                "unreachable_code",
                "unreachable code",
                stmt_span(stmt),
                Some("this statement will never be executed".to_string()),
                None,
                source_map,
            ));
        }

        match stmt {
            Stmt::Return { .. } | Stmt::Break { .. } | Stmt::Continue { .. } => {
                terminated = true;
            }
            Stmt::Let { init, .. } => {
                if let Some(init) = init {
                    lint_unreachable_in_expr(init, diags, source_map);
                }
            }
            Stmt::Expr { expr, .. } => lint_unreachable_in_expr(expr, diags, source_map),
        }
    }

    if terminated {
        if let Some(tail) = block.tail.as_deref() {
            diags.push(make_lint_diag(
                "unreachable_code",
                "unreachable code",
                tail.span(),
                Some("this expression will never be evaluated".to_string()),
                None,
                source_map,
            ));
        }
    } else if let Some(tail) = block.tail.as_deref() {
        lint_unreachable_in_expr(tail, diags, source_map);
    }
}

fn lint_unreachable_in_expr(expr: &Expr, diags: &mut Vec<Diagnostic>, source_map: &SourceMap) {
    match expr {
        Expr::Block { block, .. }
        | Expr::Lambda { body: block, .. }
        | Expr::Loop { body: block, .. } => lint_unreachable_in_block(block, diags, source_map),
        Expr::If {
            then_block,
            else_branch,
            ..
        } => {
            lint_unreachable_in_block(then_block, diags, source_map);
            if let Some(e) = else_branch.as_deref() {
                lint_unreachable_in_expr(e, diags, source_map);
            }
        }
        Expr::While { cond, body, .. } => {
            lint_unreachable_in_expr(cond, diags, source_map);
            lint_unreachable_in_block(body, diags, source_map);
        }
        Expr::For { iter, body, .. } => {
            lint_unreachable_in_expr(iter, diags, source_map);
            lint_unreachable_in_block(body, diags, source_map);
        }
        Expr::Match {
            scrutinee, arms, ..
        } => {
            lint_unreachable_in_expr(scrutinee, diags, source_map);
            for arm in arms {
                lint_unreachable_in_expr(&arm.body, diags, source_map);
            }
        }
        Expr::Array { items, .. } | Expr::Tuple { items, .. } => {
            for item in items {
                lint_unreachable_in_expr(item, diags, source_map);
            }
        }
        Expr::StructLit { fields, .. } => {
            for (_, v) in fields {
                lint_unreachable_in_expr(v, diags, source_map);
            }
        }
        Expr::EffectCall { args, .. } => {
            for arg in args {
                lint_unreachable_in_expr(arg, diags, source_map);
            }
        }
        Expr::Call { callee, args, .. } => {
            lint_unreachable_in_expr(callee, diags, source_map);
            for arg in args {
                lint_unreachable_in_expr(arg, diags, source_map);
            }
        }
        Expr::Field { base, .. } => lint_unreachable_in_expr(base, diags, source_map),
        Expr::Index { base, index, .. } => {
            lint_unreachable_in_expr(base, diags, source_map);
            lint_unreachable_in_expr(index, diags, source_map);
        }
        Expr::Unary { expr, .. } => lint_unreachable_in_expr(expr, diags, source_map),
        Expr::Binary { left, right, .. } => {
            lint_unreachable_in_expr(left, diags, source_map);
            lint_unreachable_in_expr(right, diags, source_map);
        }
        Expr::Assign { target, value, .. } => {
            lint_unreachable_in_expr(target, diags, source_map);
            lint_unreachable_in_expr(value, diags, source_map);
        }
        Expr::As { expr, .. } | Expr::AsQuestion { expr, .. } | Expr::Is { expr, .. } => {
            lint_unreachable_in_expr(expr, diags, source_map);
        }
        Expr::Unit { .. }
        | Expr::Bool { .. }
        | Expr::Int { .. }
        | Expr::Float { .. }
        | Expr::Char { .. }
        | Expr::String { .. }
        | Expr::Bytes { .. }
        | Expr::Path { .. } => {}
    }
}

fn lint_needless_bool_in_block(block: &Block, diags: &mut Vec<Diagnostic>, source_map: &SourceMap) {
    for stmt in &block.stmts {
        if let Stmt::Expr { expr, .. } = stmt {
            lint_needless_bool_in_expr(expr, diags, source_map);
        } else if let Stmt::Let { init, .. } = stmt
            && let Some(init) = init
        {
            lint_needless_bool_in_expr(init, diags, source_map);
        } else if let Stmt::Return { value, .. } = stmt
            && let Some(value) = value
        {
            lint_needless_bool_in_expr(value, diags, source_map);
        }
    }
    if let Some(tail) = block.tail.as_deref() {
        lint_needless_bool_in_expr(tail, diags, source_map);
    }
}

fn lint_needless_bool_in_expr(expr: &Expr, diags: &mut Vec<Diagnostic>, source_map: &SourceMap) {
    match expr {
        Expr::If {
            cond,
            then_block,
            else_branch,
            span,
        } => {
            if then_block.stmts.is_empty()
                && let Some(t_then) = then_block.tail.as_deref()
                && let Some(e) = else_branch.as_deref()
                && let (Some(then_val), Some(else_val)) = (bool_lit(t_then), bool_lit_expr(e))
            {
                if then_val && !else_val {
                    diags.push(make_lint_diag(
                        "needless_bool",
                        "this `if` can be simplified to its condition",
                        *span,
                        Some("replace with the condition expression".to_string()),
                        None,
                        source_map,
                    ));
                } else if !then_val && else_val {
                    diags.push(make_lint_diag(
                        "needless_bool",
                        "this `if` can be simplified to `!cond`",
                        *span,
                        Some("replace with `!cond`".to_string()),
                        None,
                        source_map,
                    ));
                }
            }

            lint_needless_bool_in_expr(cond, diags, source_map);
            lint_needless_bool_in_block(then_block, diags, source_map);
            if let Some(e) = else_branch.as_deref() {
                lint_needless_bool_in_expr(e, diags, source_map);
            }
        }
        Expr::Block { block, .. } => lint_needless_bool_in_block(block, diags, source_map),
        Expr::Match {
            scrutinee, arms, ..
        } => {
            lint_needless_bool_in_expr(scrutinee, diags, source_map);
            for arm in arms {
                lint_needless_bool_in_expr(&arm.body, diags, source_map);
            }
        }
        Expr::Loop { body, .. } | Expr::Lambda { body, .. } => {
            lint_needless_bool_in_block(body, diags, source_map);
        }
        Expr::While { cond, body, .. } => {
            lint_needless_bool_in_expr(cond, diags, source_map);
            lint_needless_bool_in_block(body, diags, source_map);
        }
        Expr::For { iter, body, .. } => {
            lint_needless_bool_in_expr(iter, diags, source_map);
            lint_needless_bool_in_block(body, diags, source_map);
        }
        Expr::Array { items, .. } | Expr::Tuple { items, .. } => {
            for item in items {
                lint_needless_bool_in_expr(item, diags, source_map);
            }
        }
        Expr::StructLit { fields, .. } => {
            for (_, v) in fields {
                lint_needless_bool_in_expr(v, diags, source_map);
            }
        }
        Expr::EffectCall { args, .. } => {
            for arg in args {
                lint_needless_bool_in_expr(arg, diags, source_map);
            }
        }
        Expr::Call { callee, args, .. } => {
            lint_needless_bool_in_expr(callee, diags, source_map);
            for arg in args {
                lint_needless_bool_in_expr(arg, diags, source_map);
            }
        }
        Expr::Field { base, .. } => lint_needless_bool_in_expr(base, diags, source_map),
        Expr::Index { base, index, .. } => {
            lint_needless_bool_in_expr(base, diags, source_map);
            lint_needless_bool_in_expr(index, diags, source_map);
        }
        Expr::Unary { expr, .. } => lint_needless_bool_in_expr(expr, diags, source_map),
        Expr::Binary { left, right, .. } => {
            lint_needless_bool_in_expr(left, diags, source_map);
            lint_needless_bool_in_expr(right, diags, source_map);
        }
        Expr::Assign { target, value, .. } => {
            lint_needless_bool_in_expr(target, diags, source_map);
            lint_needless_bool_in_expr(value, diags, source_map);
        }
        Expr::As { expr, .. } | Expr::AsQuestion { expr, .. } | Expr::Is { expr, .. } => {
            lint_needless_bool_in_expr(expr, diags, source_map);
        }
        Expr::Unit { .. }
        | Expr::Bool { .. }
        | Expr::Int { .. }
        | Expr::Float { .. }
        | Expr::Char { .. }
        | Expr::String { .. }
        | Expr::Bytes { .. }
        | Expr::Path { .. } => {}
    }
}

fn bool_lit(expr: &Expr) -> Option<bool> {
    match expr {
        Expr::Bool { value, .. } => Some(*value),
        _ => None,
    }
}

fn bool_lit_expr(expr: &Expr) -> Option<bool> {
    match expr {
        Expr::Bool { value, .. } => Some(*value),
        Expr::Block { block, .. } if block.stmts.is_empty() => {
            block.tail.as_deref().and_then(bool_lit)
        }
        _ => None,
    }
}

fn lint_suspicious_comparisons_in_block(
    block: &Block,
    diags: &mut Vec<Diagnostic>,
    source_map: &SourceMap,
) {
    for stmt in &block.stmts {
        lint_suspicious_comparisons_in_stmt(stmt, diags, source_map);
    }
    if let Some(tail) = block.tail.as_deref() {
        lint_suspicious_comparisons_in_expr(tail, diags, source_map);
    }
}

fn lint_suspicious_comparisons_in_stmt(
    stmt: &Stmt,
    diags: &mut Vec<Diagnostic>,
    source_map: &SourceMap,
) {
    match stmt {
        Stmt::Let { init, .. } => {
            if let Some(init) = init {
                lint_suspicious_comparisons_in_expr(init, diags, source_map);
            }
        }
        Stmt::Return { value, .. } => {
            if let Some(value) = value {
                lint_suspicious_comparisons_in_expr(value, diags, source_map);
            }
        }
        Stmt::Expr { expr, .. } => lint_suspicious_comparisons_in_expr(expr, diags, source_map),
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
}

fn lint_suspicious_comparisons_in_expr(
    expr: &Expr,
    diags: &mut Vec<Diagnostic>,
    source_map: &SourceMap,
) {
    match expr {
        Expr::Binary {
            op,
            left,
            right,
            span,
        } => {
            if matches!(
                op,
                crate::ast::BinaryOp::Eq
                    | crate::ast::BinaryOp::Ne
                    | crate::ast::BinaryOp::Lt
                    | crate::ast::BinaryOp::Le
                    | crate::ast::BinaryOp::Gt
                    | crate::ast::BinaryOp::Ge
            ) && let (Some(a), Some(b)) = (single_ident(left), single_ident(right))
                && a == b
            {
                diags.push(make_lint_diag(
                    "suspicious_comparison",
                    format!("suspicious comparison: `{a}` compared to itself"),
                    *span,
                    Some("this is often a mistake (did you mean another value?)".to_string()),
                    None,
                    source_map,
                ));
            }
            lint_suspicious_comparisons_in_expr(left, diags, source_map);
            lint_suspicious_comparisons_in_expr(right, diags, source_map);
        }
        Expr::Block { block, .. } => lint_suspicious_comparisons_in_block(block, diags, source_map),
        Expr::If {
            cond,
            then_block,
            else_branch,
            ..
        } => {
            lint_suspicious_comparisons_in_expr(cond, diags, source_map);
            lint_suspicious_comparisons_in_block(then_block, diags, source_map);
            if let Some(e) = else_branch.as_deref() {
                lint_suspicious_comparisons_in_expr(e, diags, source_map);
            }
        }
        Expr::Match {
            scrutinee, arms, ..
        } => {
            lint_suspicious_comparisons_in_expr(scrutinee, diags, source_map);
            for arm in arms {
                lint_suspicious_comparisons_in_expr(&arm.body, diags, source_map);
            }
        }
        Expr::Loop { body, .. } | Expr::Lambda { body, .. } => {
            lint_suspicious_comparisons_in_block(body, diags, source_map);
        }
        Expr::While { cond, body, .. } => {
            lint_suspicious_comparisons_in_expr(cond, diags, source_map);
            lint_suspicious_comparisons_in_block(body, diags, source_map);
        }
        Expr::For { iter, body, .. } => {
            lint_suspicious_comparisons_in_expr(iter, diags, source_map);
            lint_suspicious_comparisons_in_block(body, diags, source_map);
        }
        Expr::Array { items, .. } | Expr::Tuple { items, .. } => {
            for item in items {
                lint_suspicious_comparisons_in_expr(item, diags, source_map);
            }
        }
        Expr::StructLit { fields, .. } => {
            for (_, v) in fields {
                lint_suspicious_comparisons_in_expr(v, diags, source_map);
            }
        }
        Expr::EffectCall { args, .. } => {
            for arg in args {
                lint_suspicious_comparisons_in_expr(arg, diags, source_map);
            }
        }
        Expr::Call { callee, args, .. } => {
            lint_suspicious_comparisons_in_expr(callee, diags, source_map);
            for arg in args {
                lint_suspicious_comparisons_in_expr(arg, diags, source_map);
            }
        }
        Expr::Field { base, .. } => lint_suspicious_comparisons_in_expr(base, diags, source_map),
        Expr::Index { base, index, .. } => {
            lint_suspicious_comparisons_in_expr(base, diags, source_map);
            lint_suspicious_comparisons_in_expr(index, diags, source_map);
        }
        Expr::Unary { expr, .. } => lint_suspicious_comparisons_in_expr(expr, diags, source_map),
        Expr::Assign { target, value, .. } => {
            lint_suspicious_comparisons_in_expr(target, diags, source_map);
            lint_suspicious_comparisons_in_expr(value, diags, source_map);
        }
        Expr::As { expr, .. } | Expr::AsQuestion { expr, .. } | Expr::Is { expr, .. } => {
            lint_suspicious_comparisons_in_expr(expr, diags, source_map);
        }
        Expr::Unit { .. }
        | Expr::Bool { .. }
        | Expr::Int { .. }
        | Expr::Float { .. }
        | Expr::Char { .. }
        | Expr::String { .. }
        | Expr::Bytes { .. }
        | Expr::Path { .. } => {}
    }
}

fn single_ident(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Path { path, .. } if path.segments.len() == 1 => Some(path.segments[0].name.as_str()),
        _ => None,
    }
}

fn lint_redundant_else_in_block(
    block: &Block,
    diags: &mut Vec<Diagnostic>,
    source_map: &SourceMap,
) {
    for stmt in &block.stmts {
        if let Stmt::Expr { expr, .. } = stmt
            && let Expr::If {
                then_block,
                else_branch: Some(_),
                span,
                ..
            } = expr
            && block_ends_with_terminator(then_block)
        {
            diags.push(make_lint_diag(
                "redundant_else",
                "redundant `else` after a terminating `then` branch",
                *span,
                Some("the `else` branch can be unindented".to_string()),
                None,
                source_map,
            ));
        }
        // Recurse.
        match stmt {
            Stmt::Let { init, .. } => {
                if let Some(init) = init {
                    lint_redundant_else_in_expr(init, diags, source_map);
                }
            }
            Stmt::Return { value, .. } => {
                if let Some(value) = value {
                    lint_redundant_else_in_expr(value, diags, source_map);
                }
            }
            Stmt::Expr { expr, .. } => lint_redundant_else_in_expr(expr, diags, source_map),
            Stmt::Break { .. } | Stmt::Continue { .. } => {}
        }
    }
    if let Some(tail) = block.tail.as_deref() {
        lint_redundant_else_in_expr(tail, diags, source_map);
    }
}

fn lint_redundant_else_in_expr(expr: &Expr, diags: &mut Vec<Diagnostic>, source_map: &SourceMap) {
    match expr {
        Expr::If {
            cond,
            then_block,
            else_branch,
            ..
        } => {
            lint_redundant_else_in_expr(cond, diags, source_map);
            lint_redundant_else_in_block(then_block, diags, source_map);
            if let Some(e) = else_branch.as_deref() {
                lint_redundant_else_in_expr(e, diags, source_map);
            }
        }
        Expr::Block { block, .. } => lint_redundant_else_in_block(block, diags, source_map),
        Expr::Match {
            scrutinee, arms, ..
        } => {
            lint_redundant_else_in_expr(scrutinee, diags, source_map);
            for arm in arms {
                lint_redundant_else_in_expr(&arm.body, diags, source_map);
            }
        }
        Expr::Loop { body, .. } | Expr::Lambda { body, .. } => {
            lint_redundant_else_in_block(body, diags, source_map);
        }
        Expr::While { cond, body, .. } => {
            lint_redundant_else_in_expr(cond, diags, source_map);
            lint_redundant_else_in_block(body, diags, source_map);
        }
        Expr::For { iter, body, .. } => {
            lint_redundant_else_in_expr(iter, diags, source_map);
            lint_redundant_else_in_block(body, diags, source_map);
        }
        Expr::Array { items, .. } | Expr::Tuple { items, .. } => {
            for item in items {
                lint_redundant_else_in_expr(item, diags, source_map);
            }
        }
        Expr::StructLit { fields, .. } => {
            for (_, v) in fields {
                lint_redundant_else_in_expr(v, diags, source_map);
            }
        }
        Expr::EffectCall { args, .. } => {
            for arg in args {
                lint_redundant_else_in_expr(arg, diags, source_map);
            }
        }
        Expr::Call { callee, args, .. } => {
            lint_redundant_else_in_expr(callee, diags, source_map);
            for arg in args {
                lint_redundant_else_in_expr(arg, diags, source_map);
            }
        }
        Expr::Field { base, .. } => lint_redundant_else_in_expr(base, diags, source_map),
        Expr::Index { base, index, .. } => {
            lint_redundant_else_in_expr(base, diags, source_map);
            lint_redundant_else_in_expr(index, diags, source_map);
        }
        Expr::Unary { expr, .. } => lint_redundant_else_in_expr(expr, diags, source_map),
        Expr::Binary { left, right, .. } => {
            lint_redundant_else_in_expr(left, diags, source_map);
            lint_redundant_else_in_expr(right, diags, source_map);
        }
        Expr::Assign { target, value, .. } => {
            lint_redundant_else_in_expr(target, diags, source_map);
            lint_redundant_else_in_expr(value, diags, source_map);
        }
        Expr::As { expr, .. } | Expr::AsQuestion { expr, .. } | Expr::Is { expr, .. } => {
            lint_redundant_else_in_expr(expr, diags, source_map);
        }
        Expr::Unit { .. }
        | Expr::Bool { .. }
        | Expr::Int { .. }
        | Expr::Float { .. }
        | Expr::Char { .. }
        | Expr::String { .. }
        | Expr::Bytes { .. }
        | Expr::Path { .. } => {}
    }
}

fn block_ends_with_terminator(block: &Block) -> bool {
    matches!(
        block.stmts.last(),
        Some(Stmt::Return { .. } | Stmt::Break { .. } | Stmt::Continue { .. })
    )
}

fn stmt_span(stmt: &Stmt) -> Span {
    match stmt {
        Stmt::Let { span, .. }
        | Stmt::Return { span, .. }
        | Stmt::Break { span, .. }
        | Stmt::Continue { span, .. }
        | Stmt::Expr { span, .. } => *span,
    }
}
