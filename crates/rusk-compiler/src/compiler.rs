use crate::ast::{
    BinaryOp, BindingKind, Block, Expr, FnItem, FnItemKind, Ident, ImplHeader, ImplItem,
    ImplMember, Item, MatchArm, MatchPat, MethodReceiverKind, ModItem, ModKind, PatLiteral,
    Pattern as AstPattern, Program, Stmt, UnaryOp, Visibility,
};
use crate::host::CompileOptions;
use crate::modules::{DefKind, ModuleLoader, ModulePath};
use crate::parser::ParseError;
use crate::source::Span;
use crate::source_map::{SourceMap, SourceName};
use crate::typeck::{self, ProgramEnv, Ty, TypeError as TypeckError, TypeInfo};
use rusk_mir::{
    BasicBlock, BlockId, CallTarget, ConstValue, EffectSpec, Function, HandlerClause, HostImport,
    Instruction, Local, Module, Mutability, Operand, Param, Pattern, SwitchCase, Terminator, Type,
    TypeRepLit,
};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant};

mod lower;
use lower::Compiler;

const INTERNAL_CELL_STRUCT: &str = "$Cell";
const CELL_FIELD_VALUE: &str = "v";

const INTERNAL_CLOSURE_STRUCT: &str = "$Closure";
const CLOSURE_FIELD_FUNC: &str = "func";
const CLOSURE_FIELD_ENV: &str = "env";

const INTERNAL_CONTROL_ENUM: &str = "$Control";
const CONTROL_VARIANT_VALUE: &str = "Value";
const CONTROL_VARIANT_RETURN: &str = "Return";
const CONTROL_VARIANT_BREAK: &str = "Break";
const CONTROL_VARIANT_CONTINUE: &str = "Continue";

// Internal capture key prefix used to smuggle runtime `TypeRep` values for in-scope generic type
// parameters into lambdas and extracted helpers.
const CAPTURE_TYPE_REP_PREFIX: &str = "$typerep::";

fn parse_type_rep_capture_name(name: &str) -> Option<usize> {
    name.strip_prefix(CAPTURE_TYPE_REP_PREFIX)?
        .parse::<usize>()
        .ok()
}

fn free_value_vars_in_pattern(pat: &AstPattern, bound: &mut BTreeSet<String>) {
    match pat {
        AstPattern::Wildcard { .. } | AstPattern::Literal { .. } => {}
        AstPattern::Bind { name, .. } => {
            bound.insert(name.name.clone());
        }
        AstPattern::Tuple {
            prefix,
            rest,
            suffix,
            ..
        } => {
            for p in prefix {
                free_value_vars_in_pattern(p, bound);
            }
            if let Some(rest) = rest
                && let Some(binding) = &rest.binding
            {
                bound.insert(binding.name.clone());
            }
            for p in suffix {
                free_value_vars_in_pattern(p, bound);
            }
        }
        AstPattern::Enum { fields, .. } | AstPattern::Ctor { args: fields, .. } => {
            for p in fields {
                free_value_vars_in_pattern(p, bound);
            }
        }
        AstPattern::Struct { fields, .. } => {
            for (_name, p) in fields {
                free_value_vars_in_pattern(p, bound);
            }
        }
        AstPattern::Array {
            prefix,
            rest,
            suffix,
            ..
        } => {
            for p in prefix {
                free_value_vars_in_pattern(p, bound);
            }
            if let Some(rest) = rest
                && let Some(binding) = &rest.binding
            {
                bound.insert(binding.name.clone());
            }
            for p in suffix {
                free_value_vars_in_pattern(p, bound);
            }
        }
    }
}

fn free_value_vars_in_match_pat(pat: &MatchPat, bound: &mut BTreeSet<String>) {
    match pat {
        MatchPat::Value(p) => free_value_vars_in_pattern(p, bound),
        MatchPat::Effect(effect) => {
            for p in &effect.args {
                free_value_vars_in_pattern(p, bound);
            }
            let cont_name = effect
                .cont
                .as_ref()
                .map(|c| c.name.clone())
                .unwrap_or_else(|| "resume".to_string());
            bound.insert(cont_name);
        }
    }
}

fn free_value_vars_in_block(block: &Block, bound: &mut BTreeSet<String>) -> BTreeSet<String> {
    let mut free = BTreeSet::new();
    for stmt in &block.stmts {
        match stmt {
            Stmt::Let { pat, init, .. } => {
                if let Some(init) = init {
                    free_value_vars_in_expr(init, bound, &mut free);
                }
                free_value_vars_in_pattern(pat, bound);
            }
            Stmt::Return { value, .. } => {
                if let Some(value) = value {
                    free_value_vars_in_expr(value, bound, &mut free);
                }
            }
            Stmt::Break { .. } | Stmt::Continue { .. } => {}
            Stmt::Expr { expr, .. } => {
                free_value_vars_in_expr(expr, bound, &mut free);
            }
        }
    }
    if let Some(tail) = block.tail.as_deref() {
        free_value_vars_in_expr(tail, bound, &mut free);
    }
    free
}

fn free_value_vars_in_expr(expr: &Expr, bound: &BTreeSet<String>, out: &mut BTreeSet<String>) {
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
                let name = path.segments[0].name.as_str();
                if !bound.contains(name) {
                    out.insert(name.to_string());
                }
            }
        }
        Expr::Array { items, .. } | Expr::Tuple { items, .. } => {
            for item in items {
                free_value_vars_in_expr(item, bound, out);
            }
        }
        Expr::StructLit { fields, .. } => {
            for (_name, value) in fields {
                free_value_vars_in_expr(value, bound, out);
            }
        }
        Expr::EffectCall { args, .. } => {
            for arg in args {
                free_value_vars_in_expr(arg, bound, out);
            }
        }

        Expr::Lambda { params, body, .. } => {
            let mut lambda_bound = BTreeSet::new();
            for p in params {
                lambda_bound.insert(p.name.name.clone());
            }
            out.extend(free_value_vars_in_block(body, &mut lambda_bound));
        }
        Expr::If {
            cond,
            then_block,
            else_branch,
            ..
        } => {
            free_value_vars_in_expr(cond, bound, out);
            out.extend(free_value_vars_in_block(then_block, &mut bound.clone()));
            if let Some(e) = else_branch.as_deref() {
                free_value_vars_in_expr(e, &bound.clone(), out);
            }
        }
        Expr::Match {
            scrutinee, arms, ..
        } => {
            free_value_vars_in_expr(scrutinee, bound, out);
            for arm in arms {
                let mut arm_bound = bound.clone();
                free_value_vars_in_match_pat(&arm.pat, &mut arm_bound);
                free_value_vars_in_expr(&arm.body, &arm_bound, out);
            }
        }
        Expr::Loop { body, .. } => {
            out.extend(free_value_vars_in_block(body, &mut bound.clone()));
        }
        Expr::While { cond, body, .. } => {
            free_value_vars_in_expr(cond, bound, out);
            out.extend(free_value_vars_in_block(body, &mut bound.clone()));
        }
        Expr::For {
            pat, iter, body, ..
        } => {
            free_value_vars_in_expr(iter, bound, out);
            let mut inner_bound = bound.clone();
            free_value_vars_in_pattern(pat, &mut inner_bound);
            out.extend(free_value_vars_in_block(body, &mut inner_bound));
        }
        Expr::Block { block, .. } => {
            out.extend(free_value_vars_in_block(block, &mut bound.clone()));
        }

        Expr::Call { callee, args, .. } => {
            free_value_vars_in_expr(callee, bound, out);
            for arg in args {
                free_value_vars_in_expr(arg, bound, out);
            }
        }
        Expr::Field { base, .. } => {
            free_value_vars_in_expr(base, bound, out);
        }
        Expr::Index { base, index, .. } => {
            free_value_vars_in_expr(base, bound, out);
            free_value_vars_in_expr(index, bound, out);
        }
        Expr::Unary { expr, .. } => {
            free_value_vars_in_expr(expr, bound, out);
        }
        Expr::Binary { left, right, .. } => {
            free_value_vars_in_expr(left, bound, out);
            free_value_vars_in_expr(right, bound, out);
        }
        Expr::Assign { target, value, .. } => {
            free_value_vars_in_expr(target, bound, out);
            free_value_vars_in_expr(value, bound, out);
        }
        Expr::As { expr, .. } | Expr::AsQuestion { expr, .. } | Expr::Is { expr, .. } => {
            free_value_vars_in_expr(expr, bound, out);
        }
    }
}

fn collect_captured_vars_in_match_expr(
    scrutinee: &Expr,
    arms: &[MatchArm],
    bound: &BTreeSet<String>,
    captured: &mut BTreeSet<String>,
) {
    let has_effect_arms = arms.iter().any(|a| matches!(a.pat, MatchPat::Effect(_)));
    if has_effect_arms {
        let mut free = BTreeSet::new();
        let scratch_bound = BTreeSet::new();
        free_value_vars_in_expr(scrutinee, &scratch_bound, &mut free);
        for arm in arms {
            let mut arm_bound = scratch_bound.clone();
            free_value_vars_in_match_pat(&arm.pat, &mut arm_bound);
            free_value_vars_in_expr(&arm.body, &arm_bound, &mut free);
        }
        for name in free {
            if bound.contains(&name) {
                captured.insert(name);
            }
        }
    }

    collect_captured_vars_in_expr(scrutinee, bound, captured);
    for arm in arms {
        let mut arm_bound = bound.clone();
        free_value_vars_in_match_pat(&arm.pat, &mut arm_bound);
        collect_captured_vars_in_expr(&arm.body, &arm_bound, captured);
    }
}

fn collect_captured_vars_in_block(
    block: &Block,
    bound: &mut BTreeSet<String>,
    captured: &mut BTreeSet<String>,
) {
    for stmt in &block.stmts {
        collect_captured_vars_in_stmt(stmt, bound, captured);
    }
    if let Some(tail) = block.tail.as_deref() {
        collect_captured_vars_in_expr(tail, bound, captured);
    }
}

fn collect_captured_vars_in_stmt(
    stmt: &Stmt,
    bound: &mut BTreeSet<String>,
    captured: &mut BTreeSet<String>,
) {
    match stmt {
        Stmt::Let { pat, init, .. } => {
            if let Some(init) = init {
                collect_captured_vars_in_expr(init, bound, captured);
            }
            free_value_vars_in_pattern(pat, bound);
        }
        Stmt::Return { value, .. } => {
            if let Some(value) = value {
                collect_captured_vars_in_expr(value, bound, captured);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
        Stmt::Expr { expr, .. } => collect_captured_vars_in_expr(expr, bound, captured),
    }
}

fn collect_captured_vars_in_expr(
    expr: &Expr,
    bound: &BTreeSet<String>,
    captured: &mut BTreeSet<String>,
) {
    match expr {
        Expr::Unit { .. }
        | Expr::Bool { .. }
        | Expr::Int { .. }
        | Expr::Float { .. }
        | Expr::Char { .. }
        | Expr::String { .. }
        | Expr::Bytes { .. } => {}

        Expr::Path { .. } => {}

        Expr::Array { items, .. } | Expr::Tuple { items, .. } => {
            for item in items {
                collect_captured_vars_in_expr(item, bound, captured);
            }
        }
        Expr::StructLit { fields, .. } => {
            for (_name, value) in fields {
                collect_captured_vars_in_expr(value, bound, captured);
            }
        }
        Expr::EffectCall { args, .. } => {
            for arg in args {
                collect_captured_vars_in_expr(arg, bound, captured);
            }
        }

        Expr::Lambda { params, body, .. } => {
            let mut lambda_bound = BTreeSet::new();
            for p in params {
                lambda_bound.insert(p.name.name.clone());
            }
            let free = free_value_vars_in_block(body, &mut lambda_bound);
            for name in free {
                if bound.contains(&name) {
                    captured.insert(name);
                }
            }

            let mut inner_bound = bound.clone();
            for p in params {
                inner_bound.insert(p.name.name.clone());
            }
            collect_captured_vars_in_block(body, &mut inner_bound, captured);
        }
        Expr::If {
            cond,
            then_block,
            else_branch,
            ..
        } => {
            collect_captured_vars_in_expr(cond, bound, captured);
            collect_captured_vars_in_block(then_block, &mut bound.clone(), captured);
            if let Some(e) = else_branch.as_deref() {
                collect_captured_vars_in_expr(e, bound, captured);
            }
        }
        Expr::Match {
            scrutinee, arms, ..
        } => collect_captured_vars_in_match_expr(scrutinee, arms, bound, captured),
        Expr::Loop { body, .. } => {
            collect_captured_vars_in_block(body, &mut bound.clone(), captured)
        }
        Expr::While { cond, body, .. } => {
            collect_captured_vars_in_expr(cond, bound, captured);
            collect_captured_vars_in_block(body, &mut bound.clone(), captured);
        }
        Expr::For {
            pat, iter, body, ..
        } => {
            collect_captured_vars_in_expr(iter, bound, captured);
            let mut inner_bound = bound.clone();
            free_value_vars_in_pattern(pat, &mut inner_bound);
            collect_captured_vars_in_block(body, &mut inner_bound, captured);
        }
        Expr::Block { block, .. } => {
            collect_captured_vars_in_block(block, &mut bound.clone(), captured)
        }

        Expr::Call { callee, args, .. } => {
            collect_captured_vars_in_expr(callee, bound, captured);
            for arg in args {
                collect_captured_vars_in_expr(arg, bound, captured);
            }
        }
        Expr::Field { base, .. } => collect_captured_vars_in_expr(base, bound, captured),
        Expr::Index { base, index, .. } => {
            collect_captured_vars_in_expr(base, bound, captured);
            collect_captured_vars_in_expr(index, bound, captured);
        }
        Expr::Unary { expr, .. } => collect_captured_vars_in_expr(expr, bound, captured),
        Expr::Binary { left, right, .. } => {
            collect_captured_vars_in_expr(left, bound, captured);
            collect_captured_vars_in_expr(right, bound, captured);
        }
        Expr::Assign { target, value, .. } => {
            collect_captured_vars_in_expr(target, bound, captured);
            collect_captured_vars_in_expr(value, bound, captured);
        }
        Expr::As { expr, .. } | Expr::AsQuestion { expr, .. } | Expr::Is { expr, .. } => {
            collect_captured_vars_in_expr(expr, bound, captured);
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// A compiler error with an associated source [`Span`].
///
/// The error may optionally include a rendered filename/line/column location when a [`SourceMap`]
/// is available (for example when compiling from files).
pub struct CompileError {
    /// A human-readable error message.
    pub message: String,
    /// The best-effort source span associated with this error.
    pub span: Span,
    /// Cached rendered location for display (`Some("<file>: <line:col> - <line:col>")`).
    pub rendered_location: Option<String>,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(location) = &self.rendered_location {
            write!(f, "{location}: {}", self.message)
        } else {
            write!(
                f,
                "{} at {}..{}",
                self.message, self.span.start, self.span.end
            )
        }
    }
}

impl std::error::Error for CompileError {}

impl CompileError {
    fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            rendered_location: None,
        }
    }

    fn with_source_map(mut self, source_map: &SourceMap) -> Self {
        self.rendered_location = source_map.render_span_location(self.span);
        self
    }
}

impl From<ParseError> for CompileError {
    fn from(err: ParseError) -> Self {
        Self {
            message: err.message,
            span: err.span,
            rendered_location: None,
        }
    }
}

impl From<TypeckError> for CompileError {
    fn from(err: TypeckError) -> Self {
        Self {
            message: err.message,
            span: err.span,
            rendered_location: None,
        }
    }
}

impl From<crate::modules::LoadError> for CompileError {
    fn from(err: crate::modules::LoadError) -> Self {
        Self {
            message: err.message,
            span: err.span,
            rendered_location: None,
        }
    }
}

/// Compile-time measurements for the compiler pipeline.
///
/// These are intended for local benchmarking/profiling. Values are best-effort and may vary
/// across platforms (timer resolution, scheduling, etc.).
#[derive(Clone, Debug, Default)]
pub struct CompileMetrics {
    /// Time spent loading module files from disk (only for `*_file_*` entrypoints).
    pub load_time: Duration,
    /// Number of `.rusk` files read by the module loader.
    pub files_read: usize,
    /// Total bytes read across all loaded `.rusk` files.
    pub bytes_read: usize,
    /// Time spent reading module files from disk.
    pub read_time: Duration,
    /// Time spent parsing source text into an AST.
    pub parse_time: Duration,
    /// Time spent building the environment + typechecking.
    pub typecheck_time: Duration,
    /// Time spent lowering typed AST into MIR.
    pub lower_time: Duration,
    /// Total wall time of the compile entrypoint.
    pub total_time: Duration,
}

/// Compiles a single Rusk source file into MIR.
///
/// This exists only for compiler-internal tests; MIR is an internal compiler IR and is not part
/// of the public API.
#[cfg(test)]
pub fn compile_to_mir(source: &str) -> Result<Module, CompileError> {
    compile_to_mir_with_options(source, &CompileOptions::default())
}

/// Compiles a single Rusk source string into bytecode.
pub fn compile_to_bytecode(source: &str) -> Result<rusk_bytecode::ExecutableModule, CompileError> {
    compile_to_bytecode_with_options(source, &CompileOptions::default())
}

const SYSROOT_ENV: &str = "RUSK_SYSROOT";
const STD_HOST_MODULE: &str = "_std_host";

fn resolve_sysroot_dir(options: &CompileOptions) -> Result<PathBuf, CompileError> {
    if let Some(path) = &options.sysroot {
        return Ok(path.clone());
    }

    if let Some(val) = std::env::var_os(SYSROOT_ENV)
        && !val.is_empty()
    {
        return Ok(PathBuf::from(val));
    }

    let cwd_sysroot = PathBuf::from("sysroot");
    if cwd_sysroot.is_dir() {
        return Ok(cwd_sysroot);
    }

    let crate_sysroot = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../sysroot");
    if crate_sysroot.is_dir() {
        return Ok(crate_sysroot);
    }

    Err(CompileError::new(
        "sysroot not found; pass `--sysroot <path>` or set `RUSK_SYSROOT`",
        Span::new(0, 0),
    ))
}

fn synthesize_sysroot_module(name: &str, items: Vec<Item>) -> Item {
    let span0 = Span::new(0, 0);
    Item::Mod(ModItem {
        vis: Visibility::Public { span: span0 },
        name: Ident {
            name: name.to_string(),
            span: span0,
        },
        kind: ModKind::Inline { items },
        span: span0,
    })
}

fn load_sysroot_items(
    loader: &mut ModuleLoader,
    options: &CompileOptions,
) -> Result<Vec<Item>, CompileError> {
    let sysroot = resolve_sysroot_dir(options)?;

    let core_entry = sysroot.join("core").join("mod.rusk");
    let core_program = loader
        .load_program_from_file(&core_entry)
        .map_err(CompileError::from)?;

    let mut out = Vec::new();
    out.push(synthesize_sysroot_module("core", core_program.items));

    let want_std = options.load_std
        && options
            .host_modules
            .iter()
            .any(|(name, _decl)| name == STD_HOST_MODULE);
    if want_std {
        let std_entry = sysroot.join("std").join("mod.rusk");
        if std_entry.is_file() {
            let std_program = loader
                .load_program_from_file(&std_entry)
                .map_err(CompileError::from)?;
            out.push(synthesize_sysroot_module("std", std_program.items));
        }
    }

    Ok(out)
}

fn reject_reserved_module_names(items: &[Item]) -> Result<(), CompileError> {
    fn visit_items(items: &[Item]) -> Result<(), CompileError> {
        for item in items {
            let Item::Mod(m) = item else {
                continue;
            };
            if m.name.name == "core" {
                return Err(CompileError::new(
                    "module name `core` is reserved",
                    m.name.span,
                ));
            }
            if let ModKind::Inline { items } = &m.kind {
                visit_items(items)?;
            }
        }
        Ok(())
    }

    visit_items(items)
}

/// Compiles a single Rusk source file into MIR with host-module declarations.
pub fn compile_to_mir_with_options(
    source: &str,
    options: &CompileOptions,
) -> Result<Module, CompileError> {
    let mut loader = ModuleLoader::new();

    let result: Result<Module, CompileError> = (|| {
        let sysroot_items = load_sysroot_items(&mut loader, options)?;
        let mut program = loader.load_program_from_source("<string>", source)?;
        reject_reserved_module_names(&program.items)?;

        let mut items = sysroot_items;
        items.append(&mut program.items);
        let program = Program { items };

        compile_program_to_mir(&program, options)
    })();

    result.map_err(|e| e.with_source_map(loader.source_map()))
}

/// Compiles a single Rusk source string into bytecode with the given [`CompileOptions`].
///
/// This runs the full pipeline: parse → resolve → typecheck → lower to MIR → optimize MIR →
/// lower to bytecode → optimize bytecode.
pub fn compile_to_bytecode_with_options(
    source: &str,
    options: &CompileOptions,
) -> Result<rusk_bytecode::ExecutableModule, CompileError> {
    let mut mir = compile_to_mir_with_options(source, options)?;
    crate::mir_opt::optimize_mir_module(&mut mir, options.opt_level);
    let mut lower_options = crate::bytecode_lower::LowerOptions::default();
    for decl in &options.external_effects {
        let Some(sig) = abi_sig_from_host_sig(&decl.sig) else {
            return Err(CompileError::new(
                format!(
                    "external effect `{}`.`{}` has non-ABI-safe signature for bytecode v0",
                    decl.interface, decl.method
                ),
                Span::new(0, 0),
            ));
        };
        lower_options
            .external_effects
            .push(rusk_bytecode::ExternalEffectDecl {
                interface: decl.interface.clone(),
                method: decl.method.clone(),
                sig,
            });
    }

    crate::bytecode_lower::lower_mir_module_with_options(&mir, &lower_options)
        .map_err(|e| CompileError::new(e.message, Span::new(0, 0)))
        .and_then(|mut module| {
            rusk_bytecode::peephole_optimize_module(&mut module, options.opt_level)
                .map_err(|e| CompileError::new(e.message, Span::new(0, 0)))?;
            Ok(module)
        })
}

/// Compiles a single Rusk source string into bytecode, returning pipeline timing metrics.
pub fn compile_to_bytecode_with_options_and_metrics(
    source: &str,
    options: &CompileOptions,
) -> Result<(rusk_bytecode::ExecutableModule, CompileMetrics), CompileError> {
    let total_start = Instant::now();

    let (mut mir, mut metrics) = compile_to_mir_with_options_and_metrics(source, options)?;

    let mir_opt_start = Instant::now();
    crate::mir_opt::optimize_mir_module(&mut mir, options.opt_level);
    metrics.lower_time += mir_opt_start.elapsed();

    let mut lower_options = crate::bytecode_lower::LowerOptions::default();
    for decl in &options.external_effects {
        let Some(sig) = abi_sig_from_host_sig(&decl.sig) else {
            return Err(CompileError::new(
                format!(
                    "external effect `{}`.`{}` has non-ABI-safe signature for bytecode v0",
                    decl.interface, decl.method
                ),
                Span::new(0, 0),
            ));
        };
        lower_options
            .external_effects
            .push(rusk_bytecode::ExternalEffectDecl {
                interface: decl.interface.clone(),
                method: decl.method.clone(),
                sig,
            });
    }

    let bc_lower_start = Instant::now();
    let mut module = crate::bytecode_lower::lower_mir_module_with_options(&mir, &lower_options)
        .map_err(|e| CompileError::new(e.message, Span::new(0, 0)))?;
    let bc_lower_time = bc_lower_start.elapsed();

    let opt_start = Instant::now();
    rusk_bytecode::peephole_optimize_module(&mut module, options.opt_level)
        .map_err(|e| CompileError::new(e.message, Span::new(0, 0)))?;
    let bc_opt_time = opt_start.elapsed();

    metrics.lower_time += bc_lower_time + bc_opt_time;
    metrics.total_time = total_start.elapsed();

    Ok((module, metrics))
}

fn abi_type_from_host_type(ty: &crate::host::HostType) -> Option<rusk_bytecode::AbiType> {
    use crate::host::HostType as H;
    use rusk_bytecode::AbiType as A;

    match ty {
        H::Unit => Some(A::Unit),
        H::Bool => Some(A::Bool),
        H::Int => Some(A::Int),
        H::Float => Some(A::Float),
        H::String => Some(A::String),
        H::Bytes => Some(A::Bytes),
        H::Cont { .. } => Some(A::Continuation),
        H::Any | H::TypeRep | H::Array(_) | H::Tuple(_) => None,
    }
}

fn abi_sig_from_host_sig(sig: &crate::host::HostFnSig) -> Option<rusk_bytecode::HostFnSig> {
    let mut params = Vec::with_capacity(sig.params.len());
    for ty in &sig.params {
        params.push(abi_type_from_host_type(ty)?);
    }
    let ret = abi_type_from_host_type(&sig.ret)?;
    Some(rusk_bytecode::HostFnSig { params, ret })
}

/// Compiles a single Rusk source string into MIR, returning pipeline timing metrics.
pub fn compile_to_mir_with_options_and_metrics(
    source: &str,
    options: &CompileOptions,
) -> Result<(Module, CompileMetrics), CompileError> {
    let total_start = Instant::now();
    let mut metrics = CompileMetrics::default();

    let load_start = Instant::now();
    let mut loader = ModuleLoader::new();

    let sysroot_items = load_sysroot_items(&mut loader, options)?;
    let mut program = loader.load_program_from_source("<string>", source)?;
    reject_reserved_module_names(&program.items)?;
    metrics.load_time = load_start.elapsed();

    let loader_metrics = loader.metrics();
    metrics.files_read = loader_metrics.files_read;
    metrics.bytes_read = loader_metrics.bytes_read;
    metrics.read_time = loader_metrics.read_time;
    metrics.parse_time = loader_metrics.parse_time;

    let result: Result<Module, CompileError> = (|| {
        let mut items = sysroot_items;
        items.append(&mut program.items);
        let program = Program { items };

        let typecheck_start = Instant::now();
        let (env, types) = typecheck_program_with_entry_validation(&program, options)?;
        metrics.typecheck_time = typecheck_start.elapsed();

        let lower_start = Instant::now();
        let module = Compiler::new(env, types).compile_program(&program)?;
        metrics.lower_time = lower_start.elapsed();

        Ok(module)
    })();

    metrics.total_time = total_start.elapsed();
    result
        .map(|module| (module, metrics))
        .map_err(|e| e.with_source_map(loader.source_map()))
}

pub fn compile_file_to_bytecode(
    entry_path: &Path,
) -> Result<rusk_bytecode::ExecutableModule, CompileError> {
    compile_file_to_bytecode_with_options(entry_path, &CompileOptions::default())
}

/// Compiles an entry file (and its `mod foo;` dependencies) into MIR with host-module
/// declarations.
pub fn compile_file_to_mir_with_options(
    entry_path: &Path,
    options: &CompileOptions,
) -> Result<Module, CompileError> {
    let mut loader = ModuleLoader::new();

    let sysroot_items = load_sysroot_items(&mut loader, options)
        .map_err(|e| e.with_source_map(loader.source_map()))?;

    let program = match loader.load_program_from_file(entry_path) {
        Ok(program) => program,
        Err(err) => {
            let err = CompileError::from(err);

            // If we fail to read a file, the module loader never adds it to the `SourceMap`.
            // Since the sysroot is loaded first, span 0 would incorrectly map to a sysroot file.
            // Keep the error location stable by using a fallback map rooted at the unreadable
            // file path (best-effort extracted from the loader message).
            if err.span == Span::new(0, 0) && err.message.starts_with("failed to read `") {
                let mut fallback = SourceMap::new();
                let path = err
                    .message
                    .strip_prefix("failed to read `")
                    .and_then(|rest| rest.split_once('`').map(|(p, _)| PathBuf::from(p)))
                    .unwrap_or_else(|| entry_path.to_path_buf());
                fallback.add_source(SourceName::Path(path), Arc::<str>::from(""), 0);
                return Err(err.with_source_map(&fallback));
            }

            // For parse/resolve errors in entry or dependency modules, the loader has already
            // populated a correct `SourceMap`, so preserve the full mapping.
            return Err(err.with_source_map(loader.source_map()));
        }
    };

    let mut items = sysroot_items;
    items.extend(program.items);
    let program = Program { items };

    compile_program_to_mir(&program, options).map_err(|e| e.with_source_map(loader.source_map()))
}

pub fn compile_file_to_bytecode_with_options(
    entry_path: &Path,
    options: &CompileOptions,
) -> Result<rusk_bytecode::ExecutableModule, CompileError> {
    let mut mir = compile_file_to_mir_with_options(entry_path, options)?;
    crate::mir_opt::optimize_mir_module(&mut mir, options.opt_level);
    let mut lower_options = crate::bytecode_lower::LowerOptions::default();
    for decl in &options.external_effects {
        let Some(sig) = abi_sig_from_host_sig(&decl.sig) else {
            return Err(CompileError::new(
                format!(
                    "external effect `{}`.`{}` has non-ABI-safe signature for bytecode v0",
                    decl.interface, decl.method
                ),
                Span::new(0, 0),
            ));
        };
        lower_options
            .external_effects
            .push(rusk_bytecode::ExternalEffectDecl {
                interface: decl.interface.clone(),
                method: decl.method.clone(),
                sig,
            });
    }

    crate::bytecode_lower::lower_mir_module_with_options(&mir, &lower_options)
        .map_err(|e| CompileError::new(e.message, Span::new(0, 0)))
        .and_then(|mut module| {
            rusk_bytecode::peephole_optimize_module(&mut module, options.opt_level)
                .map_err(|e| CompileError::new(e.message, Span::new(0, 0)))?;
            Ok(module)
        })
}

/// Compiles an entry file (and its `mod foo;` dependencies) into bytecode with timing metrics.
pub fn compile_file_to_bytecode_with_options_and_metrics(
    entry_path: &Path,
    options: &CompileOptions,
) -> Result<(rusk_bytecode::ExecutableModule, CompileMetrics), CompileError> {
    let total_start = Instant::now();

    let (mut mir, mut metrics) = compile_file_to_mir_with_options_and_metrics(entry_path, options)?;

    let mir_opt_start = Instant::now();
    crate::mir_opt::optimize_mir_module(&mut mir, options.opt_level);
    metrics.lower_time += mir_opt_start.elapsed();

    let mut lower_options = crate::bytecode_lower::LowerOptions::default();
    for decl in &options.external_effects {
        let Some(sig) = abi_sig_from_host_sig(&decl.sig) else {
            return Err(CompileError::new(
                format!(
                    "external effect `{}`.`{}` has non-ABI-safe signature for bytecode v0",
                    decl.interface, decl.method
                ),
                Span::new(0, 0),
            ));
        };
        lower_options
            .external_effects
            .push(rusk_bytecode::ExternalEffectDecl {
                interface: decl.interface.clone(),
                method: decl.method.clone(),
                sig,
            });
    }

    let bc_lower_start = Instant::now();
    let mut module = crate::bytecode_lower::lower_mir_module_with_options(&mir, &lower_options)
        .map_err(|e| CompileError::new(e.message, Span::new(0, 0)))?;
    let bc_lower_time = bc_lower_start.elapsed();

    let opt_start = Instant::now();
    rusk_bytecode::peephole_optimize_module(&mut module, options.opt_level)
        .map_err(|e| CompileError::new(e.message, Span::new(0, 0)))?;
    let bc_opt_time = opt_start.elapsed();

    metrics.lower_time += bc_lower_time + bc_opt_time;
    metrics.total_time = total_start.elapsed();

    Ok((module, metrics))
}

/// Compiles an entry file (and its `mod foo;` dependencies) into MIR with timing metrics.
pub fn compile_file_to_mir_with_options_and_metrics(
    entry_path: &Path,
    options: &CompileOptions,
) -> Result<(Module, CompileMetrics), CompileError> {
    let total_start = Instant::now();
    let mut metrics = CompileMetrics::default();

    let load_start = Instant::now();
    let mut loader = ModuleLoader::new();

    let sysroot_items = load_sysroot_items(&mut loader, options)
        .map_err(|e| e.with_source_map(loader.source_map()))?;

    let program = match loader.load_program_from_file(entry_path) {
        Ok(program) => program,
        Err(err) => {
            let mut fallback = SourceMap::new();
            fallback.add_source(
                SourceName::Path(entry_path.to_path_buf()),
                Arc::<str>::from(""),
                0,
            );
            return Err(CompileError::from(err).with_source_map(&fallback));
        }
    };
    metrics.load_time = load_start.elapsed();

    let loader_metrics = loader.metrics();
    metrics.files_read = loader_metrics.files_read;
    metrics.bytes_read = loader_metrics.bytes_read;
    metrics.read_time = loader_metrics.read_time;
    metrics.parse_time = loader_metrics.parse_time;

    let result: Result<Module, CompileError> = (|| {
        let mut items = sysroot_items;
        items.extend(program.items);
        let program = Program { items };

        let typecheck_start = Instant::now();
        let (env, types) = typecheck_program_with_entry_validation(&program, options)?;
        metrics.typecheck_time = typecheck_start.elapsed();

        let lower_start = Instant::now();
        let module = Compiler::new(env, types).compile_program(&program)?;
        metrics.lower_time = lower_start.elapsed();

        Ok(module)
    })();

    metrics.total_time = total_start.elapsed();
    result
        .map(|module| (module, metrics))
        .map_err(|e| e.with_source_map(loader.source_map()))
}

fn compile_program_to_mir(
    program: &Program,
    options: &CompileOptions,
) -> Result<Module, CompileError> {
    let (env, types) = typecheck_program_with_entry_validation(program, options)?;
    Compiler::new(env, types).compile_program(program)
}

fn typecheck_program_with_entry_validation(
    program: &Program,
    options: &CompileOptions,
) -> Result<(ProgramEnv, TypeInfo), CompileError> {
    let env = typeck::build_env(program, options)?;
    validate_sysroot_lang_items(&env)?;
    let types = typeck::typecheck_program(program, &env)?;
    validate_entry_main_sig(&env)?;
    Ok((env, types))
}

fn validate_sysroot_lang_items(env: &ProgramEnv) -> Result<(), CompileError> {
    use typeck::TyCon;

    fn require_iface<'a>(
        env: &'a ProgramEnv,
        iface_name: &str,
    ) -> Result<&'a typeck::InterfaceDef, CompileError> {
        env.interfaces.get(iface_name).ok_or_else(|| {
            CompileError::new(
                format!("missing required sysroot interface `{iface_name}`"),
                Span::new(0, 0),
            )
        })
    }

    fn require_method<'a>(
        iface_def: &'a typeck::InterfaceDef,
        iface_name: &str,
        method_name: &str,
    ) -> Result<&'a typeck::InterfaceMethod, CompileError> {
        iface_def.all_methods.get(method_name).ok_or_else(|| {
            CompileError::new(
                format!("missing required method `{iface_name}::{method_name}`"),
                iface_def.span,
            )
        })
    }

    fn ensure(ok: bool, message: impl Into<String>, span: Span) -> Result<(), CompileError> {
        if ok {
            Ok(())
        } else {
            Err(CompileError::new(message, span))
        }
    }

    fn is_option_of_self_assoc(ret: &Ty, iface: &str, assoc: &str) -> bool {
        match ret {
            Ty::App(TyCon::Named(name), args) if name == "Option" && args.len() == 1 => {
                matches!(
                    &args[0],
                    Ty::AssocProj { iface: i, assoc: a, self_ty, .. }
                        if i == iface && a == assoc && matches!(self_ty.as_ref(), Ty::SelfType)
                )
            }
            _ => false,
        }
    }

    // `core::iter::Iterator` (used by `for` desugaring).
    {
        let iface_name = "core::iter::Iterator";
        let iface_def = require_iface(env, iface_name)?;

        ensure(
            iface_def.generics.is_empty(),
            format!("`{iface_name}` must not be generic"),
            iface_def.span,
        )?;
        ensure(
            iface_def.all_assoc_types.contains_key("Item"),
            format!("`{iface_name}` must declare associated type `Item`"),
            iface_def.span,
        )?;

        let next = require_method(iface_def, iface_name, "next")?;
        ensure(
            !next.receiver_readonly,
            format!("`{iface_name}::next` must be mutable (not `readonly`)"),
            iface_def.span,
        )?;
        ensure(
            next.sig.generics.is_empty(),
            format!("`{iface_name}::next` must not be generic"),
            iface_def.span,
        )?;
        ensure(
            next.sig.params.is_empty(),
            format!("`{iface_name}::next` must not take any parameters"),
            iface_def.span,
        )?;
        ensure(
            is_option_of_self_assoc(&next.sig.ret, iface_name, "Item"),
            format!("`{iface_name}::next` must return `Option<Self::Item>`"),
            iface_def.span,
        )?;
    }

    // `core::fmt::ToString` (used by f-string desugaring).
    {
        let iface_name = "core::fmt::ToString";
        let iface_def = require_iface(env, iface_name)?;
        ensure(
            iface_def.generics.is_empty(),
            format!("`{iface_name}` must not be generic"),
            iface_def.span,
        )?;

        let method = require_method(iface_def, iface_name, "to_string")?;
        ensure(
            method.receiver_readonly,
            format!("`{iface_name}::to_string` must be `readonly`"),
            iface_def.span,
        )?;
        ensure(
            method.sig.generics.is_empty(),
            format!("`{iface_name}::to_string` must not be generic"),
            iface_def.span,
        )?;
        ensure(
            method.sig.params.is_empty(),
            format!("`{iface_name}::to_string` must not take any parameters"),
            iface_def.span,
        )?;
        ensure(
            method.sig.ret == Ty::String,
            format!("`{iface_name}::to_string` must return `string`"),
            iface_def.span,
        )?;
    }

    // `core::ops::*` (used by operator lowering for non-primitive nominal types).
    //
    // Note: the language currently does not support source-authored impls for primitive types,
    // so these checks focus on the interface shapes used by the compiler.
    {
        #[derive(Clone, Copy, Debug)]
        enum OpsParamKind {
            SelfType,
            Int,
        }

        type OpsLangItemCheck = (
            &'static str,
            &'static str,
            bool,
            Option<OpsParamKind>,
            fn(&Ty) -> bool,
        );

        let checks: &[OpsLangItemCheck] = &[
            (
                "core::ops::Add",
                "add",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::SelfType),
            ),
            (
                "core::ops::Sub",
                "sub",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::SelfType),
            ),
            (
                "core::ops::Mul",
                "mul",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::SelfType),
            ),
            (
                "core::ops::Div",
                "div",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::SelfType),
            ),
            (
                "core::ops::Rem",
                "rem",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::SelfType),
            ),
            (
                "core::ops::BitAnd",
                "bitand",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::SelfType),
            ),
            (
                "core::ops::BitOr",
                "bitor",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::SelfType),
            ),
            (
                "core::ops::BitXor",
                "bitxor",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::SelfType),
            ),
            (
                "core::ops::Shl",
                "shl",
                true,
                Some(OpsParamKind::Int),
                |t| matches!(t, Ty::SelfType),
            ),
            (
                "core::ops::Shr",
                "shr",
                true,
                Some(OpsParamKind::Int),
                |t| matches!(t, Ty::SelfType),
            ),
            (
                "core::ops::UShr",
                "ushr",
                true,
                Some(OpsParamKind::Int),
                |t| matches!(t, Ty::SelfType),
            ),
            ("core::ops::Neg", "neg", true, None, |t| {
                matches!(t, Ty::SelfType)
            }),
            ("core::ops::Not", "not", true, None, |t| {
                matches!(t, Ty::SelfType)
            }),
            (
                "core::ops::Lt",
                "lt",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::Bool),
            ),
            (
                "core::ops::Le",
                "le",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::Bool),
            ),
            (
                "core::ops::Gt",
                "gt",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::Bool),
            ),
            (
                "core::ops::Ge",
                "ge",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::Bool),
            ),
            (
                "core::ops::Eq",
                "eq",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::Bool),
            ),
            (
                "core::ops::Ne",
                "ne",
                true,
                Some(OpsParamKind::SelfType),
                |t| matches!(t, Ty::Bool),
            ),
        ];

        for (iface_name, method_name, receiver_readonly, param_kind, ret_ok) in checks {
            let iface_def = require_iface(env, iface_name)?;
            ensure(
                iface_def.generics.is_empty(),
                format!("`{iface_name}` must not be generic"),
                iface_def.span,
            )?;

            let method = require_method(iface_def, iface_name, method_name)?;
            ensure(
                method.receiver_readonly == *receiver_readonly,
                format!(
                    "`{iface_name}::{method_name}` must be `{}`",
                    if *receiver_readonly {
                        "readonly"
                    } else {
                        "mutable"
                    }
                ),
                iface_def.span,
            )?;
            ensure(
                method.sig.generics.is_empty(),
                format!("`{iface_name}::{method_name}` must not be generic"),
                iface_def.span,
            )?;
            ensure(
                method.sig.params.len() == if param_kind.is_some() { 1 } else { 0 },
                format!(
                    "`{iface_name}::{method_name}` must take {} parameter(s)",
                    if param_kind.is_some() { 1 } else { 0 }
                ),
                iface_def.span,
            )?;
            if let Some(kind) = param_kind {
                match kind {
                    OpsParamKind::SelfType => ensure(
                        matches!(method.sig.params.first(), Some(Ty::SelfType)),
                        format!("`{iface_name}::{method_name}` parameter must be `Self`"),
                        iface_def.span,
                    )?,
                    OpsParamKind::Int => ensure(
                        matches!(method.sig.params.first(), Some(Ty::Int)),
                        format!("`{iface_name}::{method_name}` parameter must be `int`"),
                        iface_def.span,
                    )?,
                }
            }
            ensure(
                ret_ok(&method.sig.ret),
                format!("`{iface_name}::{method_name}` has an unexpected return type"),
                iface_def.span,
            )?;
        }
    }

    Ok(())
}

fn validate_entry_main_sig(env: &ProgramEnv) -> Result<(), CompileError> {
    let Some(main) = env.functions.get("main") else {
        return Err(CompileError::new(
            "missing required entry function `main`",
            Span::new(0, 0),
        ));
    };

    if !main.generics.is_empty() {
        return Err(CompileError::new(
            "entry function `main` must not be generic",
            main.span,
        ));
    }

    match main.params.as_slice() {
        [] => Ok(()),
        [Ty::Array(elem)] if **elem == Ty::String => Ok(()),
        [other] => Err(CompileError::new(
            format!("entry function `main` parameter must be `[string]` (argv), got `{other}`"),
            main.span,
        )),
        _ => Err(CompileError::new(
            "entry function `main` must have 0 parameters or 1 parameter (argv: [string])",
            main.span,
        )),
    }
}
