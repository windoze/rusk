use crate::ast::{
    BinaryOp, BindingKind, Block, Expr, FnItem, FnItemKind, ImplHeader, ImplItem, Item, MatchArm,
    MatchPat, MethodReceiverKind, PatLiteral, Pattern as AstPattern, Program, Stmt, UnaryOp,
};
use crate::host::CompileOptions;
use crate::modules::{DefKind, ModuleLoader, ModulePath};
use crate::parser::{ParseError, Parser};
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
use std::path::Path;
use std::sync::Arc;
use std::time::{Duration, Instant};

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
            binding,
            iter,
            body,
            ..
        } => {
            free_value_vars_in_expr(iter, bound, out);
            let mut inner_bound = bound.clone();
            inner_bound.insert(binding.name.clone());
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
            binding,
            iter,
            body,
            ..
        } => {
            collect_captured_vars_in_expr(iter, bound, captured);
            let mut inner_bound = bound.clone();
            inner_bound.insert(binding.name.clone());
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
pub struct CompileError {
    pub message: String,
    pub span: Span,
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
/// This is the front-end entrypoint used by tests and the CLI.
pub fn compile_to_mir(source: &str) -> Result<Module, CompileError> {
    compile_to_mir_with_options(source, &CompileOptions::default())
}

pub fn compile_to_bytecode(source: &str) -> Result<rusk_bytecode::ExecutableModule, CompileError> {
    compile_to_bytecode_with_options(source, &CompileOptions::default())
}

/// Compiles a single Rusk source file into MIR with host-module declarations.
pub fn compile_to_mir_with_options(
    source: &str,
    options: &CompileOptions,
) -> Result<Module, CompileError> {
    let mut source_map = SourceMap::new();
    source_map.add_source(
        SourceName::Virtual("<string>".to_string()),
        Arc::<str>::from(source),
        0,
    );

    let result = (|| {
        let mut parser = Parser::new(source)?;
        let program = parser.parse_program()?;
        compile_program_to_mir(&program, options)
    })();

    result.map_err(|e| e.with_source_map(&source_map))
}

pub fn compile_to_bytecode_with_options(
    source: &str,
    options: &CompileOptions,
) -> Result<rusk_bytecode::ExecutableModule, CompileError> {
    let mir = compile_to_mir_with_options(source, options)?;
    let mut lower_options = rusk_bytecode::LowerOptions::default();
    for decl in &options.external_effects {
        let Some(sig) = rusk_bytecode::HostFnSig::from_host_sig(&decl.sig) else {
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

    rusk_bytecode::lower_mir_module_with_options(&mir, &lower_options)
        .map_err(|e| CompileError::new(e.message, Span::new(0, 0)))
}

/// Compiles a single Rusk source string into MIR, returning pipeline timing metrics.
pub fn compile_to_mir_with_options_and_metrics(
    source: &str,
    options: &CompileOptions,
) -> Result<(Module, CompileMetrics), CompileError> {
    let total_start = Instant::now();
    let mut metrics = CompileMetrics::default();

    let mut source_map = SourceMap::new();
    source_map.add_source(
        SourceName::Virtual("<string>".to_string()),
        Arc::<str>::from(source),
        0,
    );

    let result: Result<Module, CompileError> = (|| {
        let parse_start = Instant::now();
        let mut parser = Parser::new(source)?;
        let program = parser.parse_program()?;
        metrics.parse_time = parse_start.elapsed();

        let typecheck_start = Instant::now();
        let env = typeck::build_env(&program, options)?;
        let types = typeck::typecheck_program(&program, &env)?;
        metrics.typecheck_time = typecheck_start.elapsed();

        let lower_start = Instant::now();
        let module = Compiler::new(env, types).compile_program(&program)?;
        metrics.lower_time = lower_start.elapsed();

        Ok(module)
    })();

    metrics.total_time = total_start.elapsed();
    result
        .map(|module| (module, metrics))
        .map_err(|e| e.with_source_map(&source_map))
}

/// Compiles an entry file (and its `mod foo;` dependencies) into MIR.
pub fn compile_file_to_mir(entry_path: &Path) -> Result<Module, CompileError> {
    compile_file_to_mir_with_options(entry_path, &CompileOptions::default())
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
    let program = match loader.load_program_from_file(entry_path) {
        Ok(program) => program,
        Err(err) => {
            let mut err = CompileError::from(err).with_source_map(loader.source_map());
            if err.rendered_location.is_none() {
                let mut fallback = SourceMap::new();
                fallback.add_source(
                    SourceName::Path(entry_path.to_path_buf()),
                    Arc::<str>::from(""),
                    0,
                );
                err = err.with_source_map(&fallback);
            }
            return Err(err);
        }
    };

    compile_program_to_mir(&program, options).map_err(|e| e.with_source_map(loader.source_map()))
}

pub fn compile_file_to_bytecode_with_options(
    entry_path: &Path,
    options: &CompileOptions,
) -> Result<rusk_bytecode::ExecutableModule, CompileError> {
    let mir = compile_file_to_mir_with_options(entry_path, options)?;
    let mut lower_options = rusk_bytecode::LowerOptions::default();
    for decl in &options.external_effects {
        let Some(sig) = rusk_bytecode::HostFnSig::from_host_sig(&decl.sig) else {
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

    rusk_bytecode::lower_mir_module_with_options(&mir, &lower_options)
        .map_err(|e| CompileError::new(e.message, Span::new(0, 0)))
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
    let program = match loader.load_program_from_file(entry_path) {
        Ok(program) => program,
        Err(err) => {
            let mut err = CompileError::from(err).with_source_map(loader.source_map());
            if err.rendered_location.is_none() {
                let mut fallback = SourceMap::new();
                fallback.add_source(
                    SourceName::Path(entry_path.to_path_buf()),
                    Arc::<str>::from(""),
                    0,
                );
                err = err.with_source_map(&fallback);
            }
            return Err(err);
        }
    };
    metrics.load_time = load_start.elapsed();

    let loader_metrics = loader.metrics();
    metrics.files_read = loader_metrics.files_read;
    metrics.bytes_read = loader_metrics.bytes_read;
    metrics.read_time = loader_metrics.read_time;
    metrics.parse_time = loader_metrics.parse_time;

    let result: Result<Module, CompileError> = (|| {
        let typecheck_start = Instant::now();
        let env = typeck::build_env(&program, options)?;
        let types = typeck::typecheck_program(&program, &env)?;
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
    let env = typeck::build_env(program, options)?;
    let types = typeck::typecheck_program(program, &env)?;
    Compiler::new(env, types).compile_program(program)
}

struct Compiler {
    module: Module,
    env: ProgramEnv,
    types: TypeInfo,
    next_internal_id: u64,
    fn_value_wrappers: BTreeMap<String, String>,
}

fn core_intrinsic_host_sig(name: &str) -> Option<rusk_mir::HostFnSig> {
    use rusk_mir::{HostFnSig, HostType};

    let sig = |params: Vec<HostType>, ret: HostType| HostFnSig { params, ret };

    match name {
        // f-string helpers.
        "core::intrinsics::string_concat" => Some(sig(
            vec![HostType::String, HostType::String],
            HostType::String,
        )),
        "core::intrinsics::to_string" => Some(sig(
            vec![HostType::TypeRep, HostType::Any],
            HostType::String,
        )),
        // Panic.
        "core::intrinsics::panic" => Some(sig(
            vec![HostType::TypeRep, HostType::String],
            HostType::Any,
        )),
        // Boolean.
        "core::intrinsics::bool_not" => Some(sig(vec![HostType::Bool], HostType::Bool)),
        "core::intrinsics::bool_eq" | "core::intrinsics::bool_ne" => {
            Some(sig(vec![HostType::Bool, HostType::Bool], HostType::Bool))
        }
        // Integer arithmetic & comparisons.
        "core::intrinsics::int_add"
        | "core::intrinsics::int_sub"
        | "core::intrinsics::int_mul"
        | "core::intrinsics::int_div"
        | "core::intrinsics::int_mod" => {
            Some(sig(vec![HostType::Int, HostType::Int], HostType::Int))
        }
        "core::intrinsics::int_eq"
        | "core::intrinsics::int_ne"
        | "core::intrinsics::int_lt"
        | "core::intrinsics::int_le"
        | "core::intrinsics::int_gt"
        | "core::intrinsics::int_ge" => {
            Some(sig(vec![HostType::Int, HostType::Int], HostType::Bool))
        }
        // Float arithmetic & comparisons.
        "core::intrinsics::float_add"
        | "core::intrinsics::float_sub"
        | "core::intrinsics::float_mul"
        | "core::intrinsics::float_div"
        | "core::intrinsics::float_mod" => {
            Some(sig(vec![HostType::Float, HostType::Float], HostType::Float))
        }
        "core::intrinsics::float_eq"
        | "core::intrinsics::float_ne"
        | "core::intrinsics::float_lt"
        | "core::intrinsics::float_le"
        | "core::intrinsics::float_gt"
        | "core::intrinsics::float_ge" => {
            Some(sig(vec![HostType::Float, HostType::Float], HostType::Bool))
        }
        // Primitive equality helpers.
        "core::intrinsics::string_eq" | "core::intrinsics::string_ne" => Some(sig(
            vec![HostType::String, HostType::String],
            HostType::Bool,
        )),
        "core::intrinsics::bytes_eq" | "core::intrinsics::bytes_ne" => {
            Some(sig(vec![HostType::Bytes, HostType::Bytes], HostType::Bool))
        }
        "core::intrinsics::unit_eq" | "core::intrinsics::unit_ne" => {
            Some(sig(vec![HostType::Unit, HostType::Unit], HostType::Bool))
        }
        // Iterator protocol.
        "core::intrinsics::into_iter" => Some(sig(
            vec![HostType::TypeRep, HostType::Array(Box::new(HostType::Any))],
            HostType::Any,
        )),
        "core::intrinsics::next" => {
            Some(sig(vec![HostType::TypeRep, HostType::Any], HostType::Any))
        }

        // Array operations.
        "core::intrinsics::array_len" | "core::intrinsics::array_len_ro" => Some(sig(
            vec![HostType::TypeRep, HostType::Array(Box::new(HostType::Any))],
            HostType::Int,
        )),
        "core::intrinsics::array_push" => Some(sig(
            vec![
                HostType::TypeRep,
                HostType::Array(Box::new(HostType::Any)),
                HostType::Any,
            ],
            HostType::Unit,
        )),
        "core::intrinsics::array_pop" => Some(sig(
            vec![HostType::TypeRep, HostType::Array(Box::new(HostType::Any))],
            HostType::Any,
        )),
        "core::intrinsics::array_clear" => Some(sig(
            vec![HostType::TypeRep, HostType::Array(Box::new(HostType::Any))],
            HostType::Unit,
        )),
        "core::intrinsics::array_resize" | "core::intrinsics::array_insert" => Some(sig(
            vec![
                HostType::TypeRep,
                HostType::Array(Box::new(HostType::Any)),
                HostType::Int,
                HostType::Any,
            ],
            HostType::Unit,
        )),
        "core::intrinsics::array_remove" => Some(sig(
            vec![
                HostType::TypeRep,
                HostType::Array(Box::new(HostType::Any)),
                HostType::Int,
            ],
            HostType::Any,
        )),
        "core::intrinsics::array_extend" => Some(sig(
            vec![
                HostType::TypeRep,
                HostType::Array(Box::new(HostType::Any)),
                HostType::Array(Box::new(HostType::Any)),
            ],
            HostType::Unit,
        )),
        "core::intrinsics::array_concat" | "core::intrinsics::array_concat_ro" => Some(sig(
            vec![
                HostType::TypeRep,
                HostType::Array(Box::new(HostType::Any)),
                HostType::Array(Box::new(HostType::Any)),
            ],
            HostType::Array(Box::new(HostType::Any)),
        )),
        "core::intrinsics::array_slice" | "core::intrinsics::array_slice_ro" => Some(sig(
            vec![
                HostType::TypeRep,
                HostType::Array(Box::new(HostType::Any)),
                HostType::Int,
                HostType::Int,
            ],
            HostType::Array(Box::new(HostType::Any)),
        )),

        _ => None,
    }
}

fn host_fn_sig_from_fn_sig(sig: &typeck::FnSig) -> Result<rusk_mir::HostFnSig, String> {
    let params = sig
        .params
        .iter()
        .map(host_type_from_ty)
        .collect::<Result<Vec<_>, _>>()?;
    let ret = host_type_from_ty(&sig.ret)?;
    Ok(rusk_mir::HostFnSig { params, ret })
}

fn host_type_from_ty(ty: &Ty) -> Result<rusk_mir::HostType, String> {
    use rusk_mir::HostType;

    match ty {
        Ty::Unit => Ok(HostType::Unit),
        Ty::Bool => Ok(HostType::Bool),
        Ty::Int => Ok(HostType::Int),
        Ty::Float => Ok(HostType::Float),
        Ty::String => Ok(HostType::String),
        Ty::Bytes => Ok(HostType::Bytes),
        Ty::Readonly(inner) => host_type_from_ty(inner),
        Ty::Array(elem) => Ok(HostType::Array(Box::new(host_type_from_ty(elem)?))),
        Ty::Tuple(items) => Ok(HostType::Tuple(
            items
                .iter()
                .map(host_type_from_ty)
                .collect::<Result<Vec<_>, _>>()?,
        )),
        other => Err(format!("unsupported host type: {other:?}")),
    }
}

impl Compiler {
    fn new(env: ProgramEnv, types: TypeInfo) -> Self {
        Self {
            module: Module::default(),
            env,
            types,
            next_internal_id: 0,
            fn_value_wrappers: BTreeMap::new(),
        }
    }

    fn fresh_internal_name(&mut self, prefix: &str) -> String {
        let id = self.next_internal_id;
        self.next_internal_id = self.next_internal_id.wrapping_add(1);
        format!("${prefix}::{id}")
    }

    fn compile_program(mut self, program: &Program) -> Result<Module, CompileError> {
        self.compile_module_items(&ModulePath::root(), &program.items)?;
        self.populate_interface_dispatch_table()?;
        self.populate_interface_impl_table();
        self.populate_struct_layouts();
        self.populate_host_imports()?;
        self.resolve_call_targets()?;
        self.inline_tiny_functions()?;
        self.resolve_function_constants()?;

        Ok(self.module)
    }

    fn populate_struct_layouts(&mut self) {
        for (type_name, def) in &self.env.structs {
            self.module.struct_layouts.insert(
                type_name.clone(),
                def.fields
                    .iter()
                    .map(|(field_name, _)| field_name.clone())
                    .collect(),
            );
        }

        // Internal lowering-only structs that are not declared in the source program.
        self.module
            .struct_layouts
            .entry(INTERNAL_CELL_STRUCT.to_string())
            .or_insert_with(|| vec![CELL_FIELD_VALUE.to_string()]);
        self.module
            .struct_layouts
            .entry(INTERNAL_CLOSURE_STRUCT.to_string())
            .or_insert_with(|| {
                vec![
                    CLOSURE_FIELD_FUNC.to_string(),
                    CLOSURE_FIELD_ENV.to_string(),
                ]
            });
    }

    fn populate_host_imports(&mut self) -> Result<(), CompileError> {
        let mut used = BTreeSet::<String>::new();

        for func in &self.module.functions {
            for block in &func.blocks {
                for instr in &block.instructions {
                    let Instruction::Call { func, .. } = instr else {
                        continue;
                    };
                    if !self.module.function_ids.contains_key(func) {
                        used.insert(func.clone());
                    }
                }
            }
        }

        for name in used {
            let sig = if let Some(sig) = core_intrinsic_host_sig(&name) {
                sig
            } else {
                let fn_sig = self.env.functions.get(&name).ok_or_else(|| {
                    CompileError::new(
                        format!("internal error: missing signature for host function `{name}`"),
                        Span::new(0, 0),
                    )
                })?;
                host_fn_sig_from_fn_sig(fn_sig).map_err(|message| {
                    CompileError::new(
                        format!("internal error: host import signature for `{name}`: {message}"),
                        Span::new(0, 0),
                    )
                })?
            };
            self.module
                .add_host_import(HostImport { name, sig })
                .map_err(|message| {
                    CompileError::new(format!("internal error: {message}"), Span::new(0, 0))
                })?;
        }

        Ok(())
    }

    fn populate_interface_dispatch_table(&mut self) -> Result<(), CompileError> {
        for ((type_name, origin_iface, method_name), impl_fn) in &self.env.interface_methods {
            let method_id = format!("{origin_iface}::{method_name}");
            let key = (type_name.clone(), method_id);
            let Some(impl_id) = self.module.function_id(impl_fn.as_str()) else {
                return Err(CompileError::new(
                    format!("internal error: missing interface impl function `{impl_fn}`"),
                    Span::new(0, 0),
                ));
            };
            if let Some(prev) = self.module.methods.insert(key.clone(), impl_id) {
                return Err(CompileError::new(
                    format!(
                        "internal error: duplicate dispatch entry for ({}, {}) (prev={:?})",
                        key.0, key.1, prev
                    ),
                    Span::new(0, 0),
                ));
            }
        }
        Ok(())
    }

    fn resolve_call_targets(&mut self) -> Result<(), CompileError> {
        let function_ids = self.module.function_ids.clone();
        let host_import_ids = self.module.host_import_ids.clone();
        for func in &mut self.module.functions {
            for block in &mut func.blocks {
                for instr in &mut block.instructions {
                    let Instruction::Call { dst, func, args } = instr else {
                        continue;
                    };

                    let name = std::mem::take(func);
                    let args = std::mem::take(args);
                    let dst = *dst;

                    let target = if let Some(id) = function_ids.get(name.as_str()).copied() {
                        CallTarget::Mir(id)
                    } else if let Some(id) = host_import_ids.get(name.as_str()).copied() {
                        CallTarget::Host(id)
                    } else {
                        return Err(CompileError::new(
                            format!("internal error: unresolved call target `{name}`"),
                            Span::new(0, 0),
                        ));
                    };

                    *instr = Instruction::CallId {
                        dst,
                        func: target,
                        args,
                    };
                }
            }
        }
        Ok(())
    }

    fn inline_tiny_functions(&mut self) -> Result<(), CompileError> {
        #[derive(Clone)]
        struct InlineBody {
            params: Vec<Param>,
            locals: usize,
            instructions: Vec<Instruction>,
            return_value: Operand,
        }

        const INLINE_MAX_INSTRUCTIONS: usize = 16;

        fn inline_body_for_function(func: &Function) -> Option<InlineBody> {
            if func.blocks.len() != 1 {
                return None;
            }
            let block = func.blocks.first()?;
            if !block.params.is_empty() {
                return None;
            }
            if block.instructions.len() > INLINE_MAX_INSTRUCTIONS {
                return None;
            }
            let Terminator::Return { value } = &block.terminator else {
                return None;
            };

            // Very conservative: avoid inlining any function that can perform effects or calls.
            for instr in &block.instructions {
                match instr {
                    Instruction::Call { .. }
                    | Instruction::CallId { .. }
                    | Instruction::ICall { .. }
                    | Instruction::VCall { .. }
                    | Instruction::PushHandler { .. }
                    | Instruction::PopHandler
                    | Instruction::Perform { .. }
                    | Instruction::Resume { .. } => return None,
                    _ => {}
                }
            }

            Some(InlineBody {
                params: func.params.clone(),
                locals: func.locals,
                instructions: block.instructions.clone(),
                return_value: value.clone(),
            })
        }

        fn fresh_local(next_local: &mut usize) -> Local {
            let local = Local(*next_local);
            *next_local = next_local.saturating_add(1);
            local
        }

        fn remap_local(map: &[Local], local: Local) -> Local {
            map.get(local.0)
                .copied()
                .unwrap_or_else(|| panic!("inline remap: invalid local {local:?}"))
        }

        fn remap_operand(map: &[Local], op: &Operand) -> Operand {
            match op {
                Operand::Local(local) => Operand::Local(remap_local(map, *local)),
                Operand::Literal(lit) => Operand::Literal(lit.clone()),
            }
        }

        fn remap_instr(map: &[Local], instr: Instruction) -> Instruction {
            match instr {
                Instruction::Const { dst, value } => Instruction::Const {
                    dst: remap_local(map, dst),
                    value,
                },
                Instruction::Copy { dst, src } => Instruction::Copy {
                    dst: remap_local(map, dst),
                    src: remap_local(map, src),
                },
                Instruction::Move { dst, src } => Instruction::Move {
                    dst: remap_local(map, dst),
                    src: remap_local(map, src),
                },
                Instruction::AsReadonly { dst, src } => Instruction::AsReadonly {
                    dst: remap_local(map, dst),
                    src: remap_local(map, src),
                },
                Instruction::IsType { dst, value, ty } => Instruction::IsType {
                    dst: remap_local(map, dst),
                    value: remap_operand(map, &value),
                    ty: remap_operand(map, &ty),
                },
                Instruction::CheckedCast { dst, value, ty } => Instruction::CheckedCast {
                    dst: remap_local(map, dst),
                    value: remap_operand(map, &value),
                    ty: remap_operand(map, &ty),
                },
                Instruction::MakeTypeRep { dst, base, args } => Instruction::MakeTypeRep {
                    dst: remap_local(map, dst),
                    base,
                    args: args.into_iter().map(|op| remap_operand(map, &op)).collect(),
                },
                Instruction::MakeStruct {
                    dst,
                    type_name,
                    type_args,
                    fields,
                } => Instruction::MakeStruct {
                    dst: remap_local(map, dst),
                    type_name,
                    type_args: type_args
                        .into_iter()
                        .map(|op| remap_operand(map, &op))
                        .collect(),
                    fields: fields
                        .into_iter()
                        .map(|(name, op)| (name, remap_operand(map, &op)))
                        .collect(),
                },
                Instruction::MakeArray { dst, items } => Instruction::MakeArray {
                    dst: remap_local(map, dst),
                    items: items
                        .into_iter()
                        .map(|op| remap_operand(map, &op))
                        .collect(),
                },
                Instruction::MakeTuple { dst, items } => Instruction::MakeTuple {
                    dst: remap_local(map, dst),
                    items: items
                        .into_iter()
                        .map(|op| remap_operand(map, &op))
                        .collect(),
                },
                Instruction::MakeEnum {
                    dst,
                    enum_name,
                    type_args,
                    variant,
                    fields,
                } => Instruction::MakeEnum {
                    dst: remap_local(map, dst),
                    enum_name,
                    type_args: type_args
                        .into_iter()
                        .map(|op| remap_operand(map, &op))
                        .collect(),
                    variant,
                    fields: fields
                        .into_iter()
                        .map(|op| remap_operand(map, &op))
                        .collect(),
                },
                Instruction::GetField { dst, obj, field } => Instruction::GetField {
                    dst: remap_local(map, dst),
                    obj: remap_operand(map, &obj),
                    field,
                },
                Instruction::SetField { obj, field, value } => Instruction::SetField {
                    obj: remap_operand(map, &obj),
                    field,
                    value: remap_operand(map, &value),
                },
                Instruction::StructGet { dst, obj, idx } => Instruction::StructGet {
                    dst: remap_local(map, dst),
                    obj: remap_operand(map, &obj),
                    idx,
                },
                Instruction::StructSet { obj, idx, value } => Instruction::StructSet {
                    obj: remap_operand(map, &obj),
                    idx,
                    value: remap_operand(map, &value),
                },
                Instruction::TupleGet { dst, tup, idx } => Instruction::TupleGet {
                    dst: remap_local(map, dst),
                    tup: remap_operand(map, &tup),
                    idx,
                },
                Instruction::TupleSet { tup, idx, value } => Instruction::TupleSet {
                    tup: remap_operand(map, &tup),
                    idx,
                    value: remap_operand(map, &value),
                },
                Instruction::IndexGet { dst, arr, idx } => Instruction::IndexGet {
                    dst: remap_local(map, dst),
                    arr: remap_operand(map, &arr),
                    idx: remap_operand(map, &idx),
                },
                Instruction::IndexSet { arr, idx, value } => Instruction::IndexSet {
                    arr: remap_operand(map, &arr),
                    idx: remap_operand(map, &idx),
                    value: remap_operand(map, &value),
                },
                Instruction::Len { dst, arr } => Instruction::Len {
                    dst: remap_local(map, dst),
                    arr: remap_operand(map, &arr),
                },
                Instruction::IntAdd { dst, a, b } => Instruction::IntAdd {
                    dst: remap_local(map, dst),
                    a: remap_operand(map, &a),
                    b: remap_operand(map, &b),
                },
                Instruction::IntSub { dst, a, b } => Instruction::IntSub {
                    dst: remap_local(map, dst),
                    a: remap_operand(map, &a),
                    b: remap_operand(map, &b),
                },
                Instruction::IntMul { dst, a, b } => Instruction::IntMul {
                    dst: remap_local(map, dst),
                    a: remap_operand(map, &a),
                    b: remap_operand(map, &b),
                },
                Instruction::IntDiv { dst, a, b } => Instruction::IntDiv {
                    dst: remap_local(map, dst),
                    a: remap_operand(map, &a),
                    b: remap_operand(map, &b),
                },
                Instruction::IntMod { dst, a, b } => Instruction::IntMod {
                    dst: remap_local(map, dst),
                    a: remap_operand(map, &a),
                    b: remap_operand(map, &b),
                },
                Instruction::IntLt { dst, a, b } => Instruction::IntLt {
                    dst: remap_local(map, dst),
                    a: remap_operand(map, &a),
                    b: remap_operand(map, &b),
                },
                Instruction::IntLe { dst, a, b } => Instruction::IntLe {
                    dst: remap_local(map, dst),
                    a: remap_operand(map, &a),
                    b: remap_operand(map, &b),
                },
                Instruction::IntGt { dst, a, b } => Instruction::IntGt {
                    dst: remap_local(map, dst),
                    a: remap_operand(map, &a),
                    b: remap_operand(map, &b),
                },
                Instruction::IntGe { dst, a, b } => Instruction::IntGe {
                    dst: remap_local(map, dst),
                    a: remap_operand(map, &a),
                    b: remap_operand(map, &b),
                },
                Instruction::IntEq { dst, a, b } => Instruction::IntEq {
                    dst: remap_local(map, dst),
                    a: remap_operand(map, &a),
                    b: remap_operand(map, &b),
                },
                Instruction::IntNe { dst, a, b } => Instruction::IntNe {
                    dst: remap_local(map, dst),
                    a: remap_operand(map, &a),
                    b: remap_operand(map, &b),
                },
                Instruction::BoolNot { dst, v } => Instruction::BoolNot {
                    dst: remap_local(map, dst),
                    v: remap_operand(map, &v),
                },
                Instruction::BoolEq { dst, a, b } => Instruction::BoolEq {
                    dst: remap_local(map, dst),
                    a: remap_operand(map, &a),
                    b: remap_operand(map, &b),
                },
                Instruction::BoolNe { dst, a, b } => Instruction::BoolNe {
                    dst: remap_local(map, dst),
                    a: remap_operand(map, &a),
                    b: remap_operand(map, &b),
                },
                Instruction::Call { .. }
                | Instruction::CallId { .. }
                | Instruction::VCall { .. }
                | Instruction::ICall { .. }
                | Instruction::PushHandler { .. }
                | Instruction::PopHandler
                | Instruction::Perform { .. }
                | Instruction::Resume { .. } => {
                    panic!("inline remap: unsupported instruction {instr:?}")
                }
            }
        }

        let bodies = self
            .module
            .functions
            .iter()
            .map(inline_body_for_function)
            .collect::<Vec<_>>();

        for caller_idx in 0..self.module.functions.len() {
            let Some(caller) = self.module.functions.get_mut(caller_idx) else {
                continue;
            };

            let mut next_local = caller.locals;
            for block in &mut caller.blocks {
                let old_instructions = std::mem::take(&mut block.instructions);
                let mut out = Vec::with_capacity(old_instructions.len());
                for instr in old_instructions {
                    let Instruction::CallId { dst, func, args } = instr else {
                        out.push(instr);
                        continue;
                    };

                    let CallTarget::Mir(callee_id) = func else {
                        out.push(Instruction::CallId { dst, func, args });
                        continue;
                    };

                    let callee_index = callee_id.0 as usize;
                    let Some(body) = bodies.get(callee_index).and_then(|v| v.as_ref()) else {
                        out.push(Instruction::CallId {
                            dst,
                            func: CallTarget::Mir(callee_id),
                            args,
                        });
                        continue;
                    };

                    if args.len() != body.params.len() {
                        out.push(Instruction::CallId {
                            dst,
                            func: CallTarget::Mir(callee_id),
                            args,
                        });
                        continue;
                    }

                    // Allocate fresh locals for the inlined callee frame and map callee locals to
                    // caller locals.
                    let mut local_map = vec![None; body.locals];
                    for (arg, param) in args.iter().zip(body.params.iter()) {
                        let param_local = fresh_local(&mut next_local);
                        local_map[param.local.0] = Some(param_local);

                        match arg {
                            Operand::Local(src) => out.push(Instruction::Copy {
                                dst: param_local,
                                src: *src,
                            }),
                            Operand::Literal(lit) => out.push(Instruction::Const {
                                dst: param_local,
                                value: lit.clone(),
                            }),
                        }

                        if param.mutability == Mutability::Readonly {
                            out.push(Instruction::AsReadonly {
                                dst: param_local,
                                src: param_local,
                            });
                        }
                    }

                    for slot in &mut local_map {
                        if slot.is_none() {
                            *slot = Some(fresh_local(&mut next_local));
                        }
                    }
                    let local_map = local_map
                        .into_iter()
                        .map(|l| l.expect("all locals mapped"))
                        .collect::<Vec<_>>();

                    for callee_instr in body.instructions.iter().cloned() {
                        out.push(remap_instr(&local_map, callee_instr));
                    }

                    let ret = remap_operand(&local_map, &body.return_value);
                    if let Some(dst_local) = dst {
                        match ret {
                            Operand::Local(src) => {
                                if dst_local != src {
                                    out.push(Instruction::Copy {
                                        dst: dst_local,
                                        src,
                                    });
                                }
                            }
                            Operand::Literal(lit) => out.push(Instruction::Const {
                                dst: dst_local,
                                value: lit,
                            }),
                        }
                    } else if let Operand::Local(src) = ret {
                        // Preserve return-value evaluation for trap semantics (e.g. uninitialized
                        // locals in invalid MIR), even when the value is ignored by the caller.
                        let sink = fresh_local(&mut next_local);
                        out.push(Instruction::Copy { dst: sink, src });
                    }
                }

                block.instructions = out;
            }

            caller.locals = next_local;
        }

        Ok(())
    }

    fn resolve_function_constants(&mut self) -> Result<(), CompileError> {
        fn resolve_const(
            function_ids: &BTreeMap<String, rusk_mir::FunctionId>,
            value: &mut ConstValue,
        ) -> Result<(), String> {
            match value {
                ConstValue::Function(name) => {
                    let Some(id) = function_ids.get(name.as_str()).copied() else {
                        return Err(format!("unknown function `{name}`"));
                    };
                    *value = ConstValue::FunctionId(id);
                    Ok(())
                }
                ConstValue::FunctionId(_) => Ok(()),
                ConstValue::Array(items) | ConstValue::Tuple(items) => {
                    for item in items {
                        resolve_const(function_ids, item)?;
                    }
                    Ok(())
                }
                ConstValue::Struct { fields, .. } => {
                    for (_name, value) in fields {
                        resolve_const(function_ids, value)?;
                    }
                    Ok(())
                }
                ConstValue::Enum { fields, .. } => {
                    for value in fields {
                        resolve_const(function_ids, value)?;
                    }
                    Ok(())
                }
                ConstValue::Unit
                | ConstValue::Bool(_)
                | ConstValue::Int(_)
                | ConstValue::Float(_)
                | ConstValue::String(_)
                | ConstValue::Bytes(_)
                | ConstValue::TypeRep(_) => Ok(()),
            }
        }

        fn resolve_operand(
            function_ids: &BTreeMap<String, rusk_mir::FunctionId>,
            op: &mut Operand,
        ) -> Result<(), String> {
            if let Operand::Literal(lit) = op {
                resolve_const(function_ids, lit)?;
            }
            Ok(())
        }

        fn resolve_pattern(
            function_ids: &BTreeMap<String, rusk_mir::FunctionId>,
            pat: &mut Pattern,
        ) -> Result<(), String> {
            match pat {
                Pattern::Wildcard | Pattern::Bind => Ok(()),
                Pattern::Literal(lit) => resolve_const(function_ids, lit),
                Pattern::Tuple {
                    prefix,
                    rest,
                    suffix,
                } => {
                    for p in prefix {
                        resolve_pattern(function_ids, p)?;
                    }
                    if let Some(rest) = rest.as_deref_mut() {
                        resolve_pattern(function_ids, rest)?;
                    }
                    for p in suffix {
                        resolve_pattern(function_ids, p)?;
                    }
                    Ok(())
                }
                Pattern::Enum { fields, .. } => {
                    for p in fields {
                        resolve_pattern(function_ids, p)?;
                    }
                    Ok(())
                }
                Pattern::Struct { fields, .. } => {
                    for (_name, p) in fields {
                        resolve_pattern(function_ids, p)?;
                    }
                    Ok(())
                }
                Pattern::Array {
                    prefix,
                    rest,
                    suffix,
                } => {
                    for p in prefix {
                        resolve_pattern(function_ids, p)?;
                    }
                    if let Some(rest) = rest.as_deref_mut() {
                        resolve_pattern(function_ids, rest)?;
                    }
                    for p in suffix {
                        resolve_pattern(function_ids, p)?;
                    }
                    Ok(())
                }
            }
        }

        fn resolve_handler_clause(
            function_ids: &BTreeMap<String, rusk_mir::FunctionId>,
            clause: &mut HandlerClause,
        ) -> Result<(), String> {
            for op in &mut clause.effect.interface_args {
                resolve_operand(function_ids, op)?;
            }
            for pat in &mut clause.arg_patterns {
                resolve_pattern(function_ids, pat)?;
            }
            Ok(())
        }

        fn resolve_instr(
            function_ids: &BTreeMap<String, rusk_mir::FunctionId>,
            instr: &mut Instruction,
        ) -> Result<(), String> {
            match instr {
                Instruction::Const { value, .. } => resolve_const(function_ids, value),
                Instruction::Copy { .. }
                | Instruction::Move { .. }
                | Instruction::AsReadonly { .. } => Ok(()),
                Instruction::IsType { value, ty, .. }
                | Instruction::CheckedCast { value, ty, .. } => {
                    resolve_operand(function_ids, value)?;
                    resolve_operand(function_ids, ty)
                }
                Instruction::MakeTypeRep { args, .. } => {
                    for op in args {
                        resolve_operand(function_ids, op)?;
                    }
                    Ok(())
                }
                Instruction::MakeStruct {
                    type_args, fields, ..
                } => {
                    for op in type_args {
                        resolve_operand(function_ids, op)?;
                    }
                    for (_name, op) in fields {
                        resolve_operand(function_ids, op)?;
                    }
                    Ok(())
                }
                Instruction::MakeArray { items, .. } | Instruction::MakeTuple { items, .. } => {
                    for op in items {
                        resolve_operand(function_ids, op)?;
                    }
                    Ok(())
                }
                Instruction::MakeEnum {
                    type_args, fields, ..
                } => {
                    for op in type_args {
                        resolve_operand(function_ids, op)?;
                    }
                    for op in fields {
                        resolve_operand(function_ids, op)?;
                    }
                    Ok(())
                }
                Instruction::GetField { obj, .. } => resolve_operand(function_ids, obj),
                Instruction::SetField { obj, value, .. } => {
                    resolve_operand(function_ids, obj)?;
                    resolve_operand(function_ids, value)
                }
                Instruction::StructGet { obj, .. } => resolve_operand(function_ids, obj),
                Instruction::StructSet { obj, value, .. } => {
                    resolve_operand(function_ids, obj)?;
                    resolve_operand(function_ids, value)
                }
                Instruction::TupleGet { tup, .. } => resolve_operand(function_ids, tup),
                Instruction::TupleSet { tup, value, .. } => {
                    resolve_operand(function_ids, tup)?;
                    resolve_operand(function_ids, value)
                }
                Instruction::IndexGet { arr, idx, .. } => {
                    resolve_operand(function_ids, arr)?;
                    resolve_operand(function_ids, idx)
                }
                Instruction::IndexSet { arr, idx, value } => {
                    resolve_operand(function_ids, arr)?;
                    resolve_operand(function_ids, idx)?;
                    resolve_operand(function_ids, value)
                }
                Instruction::Len { arr, .. } => resolve_operand(function_ids, arr),
                Instruction::IntAdd { a, b, .. }
                | Instruction::IntSub { a, b, .. }
                | Instruction::IntMul { a, b, .. }
                | Instruction::IntDiv { a, b, .. }
                | Instruction::IntMod { a, b, .. }
                | Instruction::IntLt { a, b, .. }
                | Instruction::IntLe { a, b, .. }
                | Instruction::IntGt { a, b, .. }
                | Instruction::IntGe { a, b, .. }
                | Instruction::IntEq { a, b, .. }
                | Instruction::IntNe { a, b, .. }
                | Instruction::BoolEq { a, b, .. }
                | Instruction::BoolNe { a, b, .. } => {
                    resolve_operand(function_ids, a)?;
                    resolve_operand(function_ids, b)
                }
                Instruction::BoolNot { v, .. } => resolve_operand(function_ids, v),
                Instruction::Call { args, .. } => {
                    for op in args {
                        resolve_operand(function_ids, op)?;
                    }
                    Ok(())
                }
                Instruction::CallId { args, .. } => {
                    for op in args {
                        resolve_operand(function_ids, op)?;
                    }
                    Ok(())
                }
                Instruction::VCall {
                    obj,
                    method_type_args,
                    args,
                    ..
                } => {
                    resolve_operand(function_ids, obj)?;
                    for op in method_type_args {
                        resolve_operand(function_ids, op)?;
                    }
                    for op in args {
                        resolve_operand(function_ids, op)?;
                    }
                    Ok(())
                }
                Instruction::ICall { fnptr, args, .. } => {
                    resolve_operand(function_ids, fnptr)?;
                    for op in args {
                        resolve_operand(function_ids, op)?;
                    }
                    Ok(())
                }
                Instruction::PushHandler { clauses, .. } => {
                    for clause in clauses {
                        resolve_handler_clause(function_ids, clause)?;
                    }
                    Ok(())
                }
                Instruction::PopHandler => Ok(()),
                Instruction::Perform { effect, args, .. } => {
                    for op in &mut effect.interface_args {
                        resolve_operand(function_ids, op)?;
                    }
                    for op in args {
                        resolve_operand(function_ids, op)?;
                    }
                    Ok(())
                }
                Instruction::Resume { k, value, .. } => {
                    resolve_operand(function_ids, k)?;
                    resolve_operand(function_ids, value)
                }
            }
        }

        fn resolve_term(
            function_ids: &BTreeMap<String, rusk_mir::FunctionId>,
            term: &mut Terminator,
        ) -> Result<(), String> {
            match term {
                Terminator::Br { args, .. } => {
                    for op in args {
                        resolve_operand(function_ids, op)?;
                    }
                    Ok(())
                }
                Terminator::CondBr {
                    cond,
                    then_args,
                    else_args,
                    ..
                } => {
                    resolve_operand(function_ids, cond)?;
                    for op in then_args {
                        resolve_operand(function_ids, op)?;
                    }
                    for op in else_args {
                        resolve_operand(function_ids, op)?;
                    }
                    Ok(())
                }
                Terminator::Switch { value, cases, .. } => {
                    resolve_operand(function_ids, value)?;
                    for case in cases {
                        resolve_pattern(function_ids, &mut case.pattern)?;
                    }
                    Ok(())
                }
                Terminator::Return { value } => resolve_operand(function_ids, value),
                Terminator::Trap { .. } => Ok(()),
            }
        }

        let function_ids = self.module.function_ids.clone();
        for func in &mut self.module.functions {
            for block in &mut func.blocks {
                for instr in &mut block.instructions {
                    resolve_instr(&function_ids, instr).map_err(|message| {
                        CompileError::new(
                            format!("internal error: resolve constant: {message}"),
                            Span::new(0, 0),
                        )
                    })?;
                }
                resolve_term(&function_ids, &mut block.terminator).map_err(|message| {
                    CompileError::new(
                        format!("internal error: resolve constant: {message}"),
                        Span::new(0, 0),
                    )
                })?;
            }
        }
        Ok(())
    }

    fn populate_interface_impl_table(&mut self) {
        for (type_name, iface) in &self.env.interface_impls {
            self.module
                .interface_impls
                .entry(type_name.clone())
                .or_default()
                .insert(iface.clone());
        }
    }

    fn compile_module_items(
        &mut self,
        module: &ModulePath,
        items: &[Item],
    ) -> Result<(), CompileError> {
        for item in items {
            match item {
                Item::Function(func) => {
                    self.compile_real_function(module, func, None)?;
                }
                Item::Impl(imp) => {
                    self.compile_impl_item(module, imp)?;
                }
                Item::Mod(m) => {
                    let child = module.child(&m.name.name);
                    match &m.kind {
                        crate::ast::ModKind::Inline { items } => {
                            self.compile_module_items(&child, items)?;
                        }
                        crate::ast::ModKind::File => {
                            return Err(CompileError::new(
                                "file modules must be loaded before compilation",
                                m.span,
                            ));
                        }
                    }
                }
                Item::Interface(iface) => {
                    let iface_name = module.qualify(&iface.name.name);
                    for member in &iface.members {
                        let Some(body) = &member.body else {
                            continue;
                        };
                        let default_fn_name =
                            format!("$default::{iface_name}::{}", member.name.name);
                        let synthetic = FnItem {
                            vis: crate::ast::Visibility::Private,
                            kind: FnItemKind::Method {
                                receiver: MethodReceiverKind::Instance {
                                    readonly: member.readonly,
                                },
                            },
                            name: member.name.clone(),
                            generics: member.generics.clone(),
                            params: member.params.clone(),
                            ret: member.ret.clone(),
                            body: body.clone(),
                            span: member.span,
                        };
                        self.compile_real_function(module, &synthetic, Some(default_fn_name))?;
                    }
                }
                Item::Struct(_) | Item::Enum(_) | Item::Use(_) => {}
            }
        }
        Ok(())
    }

    fn compile_impl_item(
        &mut self,
        module: &ModulePath,
        imp: &ImplItem,
    ) -> Result<(), CompileError> {
        match &imp.header {
            ImplHeader::Inherent { ty, .. } => {
                let segments: Vec<String> =
                    ty.segments.iter().map(|s| s.name.name.clone()).collect();
                let (_kind, type_name) = self
                    .env
                    .modules
                    .resolve_type_fqn(module, &segments, ty.span)
                    .map_err(|e| CompileError::new(e.message, e.span))?;
                for method in &imp.members {
                    let name_override = format!("{type_name}::{}", method.name.name);
                    self.compile_real_function(module, method, Some(name_override))?;
                }
            }
            ImplHeader::InterfaceForType { interface, ty, .. } => {
                let iface_segments: Vec<String> = interface
                    .segments
                    .iter()
                    .map(|s| s.name.name.clone())
                    .collect();
                let (_kind, iface_name) = self
                    .env
                    .modules
                    .resolve_type_fqn(module, &iface_segments, interface.span)
                    .map_err(|e| CompileError::new(e.message, e.span))?;
                let type_segments: Vec<String> =
                    ty.segments.iter().map(|s| s.name.name.clone()).collect();
                let (_kind, type_name) = self
                    .env
                    .modules
                    .resolve_type_fqn(module, &type_segments, ty.span)
                    .map_err(|e| CompileError::new(e.message, e.span))?;
                for method in &imp.members {
                    let mname = &method.name.name;
                    let name_override = format!("impl::{iface_name}::for::{type_name}::{mname}");
                    self.compile_real_function(module, method, Some(name_override))?;
                }

                // Synthesize wrappers for omitted default interface methods.
                let Some(iface_def) = self.env.interfaces.get(&iface_name) else {
                    return Err(CompileError::new(
                        format!("internal error: unknown interface `{iface_name}`"),
                        imp.span,
                    ));
                };
                let implemented: std::collections::BTreeSet<&str> =
                    imp.members.iter().map(|m| m.name.name.as_str()).collect();
                let all_methods: Vec<(String, typeck::InterfaceMethod)> = iface_def
                    .all_methods
                    .iter()
                    .map(|(name, info)| (name.clone(), info.clone()))
                    .collect();
                for (mname, info) in all_methods {
                    if !info.has_default || implemented.contains(mname.as_str()) {
                        continue;
                    }
                    let wrapper_name = format!("impl::{iface_name}::for::{type_name}::{mname}");
                    if self.module.function_ids.contains_key(&wrapper_name) {
                        continue;
                    }
                    let default_name = format!("$default::{}::{mname}", info.origin);
                    let origin_arity = self
                        .env
                        .interfaces
                        .get(&info.origin)
                        .map(|d| d.generics.len())
                        .unwrap_or(0);
                    self.compile_default_method_wrapper(
                        module,
                        &wrapper_name,
                        &default_name,
                        origin_arity,
                        info.sig.generics.len(),
                    )?;
                }
            }
        }
        Ok(())
    }

    fn compile_default_method_wrapper(
        &mut self,
        module: &ModulePath,
        wrapper_name: &str,
        default_target: &str,
        origin_iface_arity: usize,
        method_generics_len: usize,
    ) -> Result<(), CompileError> {
        let sig = self.env.functions.get(wrapper_name).ok_or_else(|| {
            CompileError::new(
                format!("internal error: missing signature for `{wrapper_name}`"),
                Span::new(0, 0),
            )
        })?;
        let sig = sig.clone();

        if !self.env.functions.contains_key(default_target) {
            return Err(CompileError::new(
                format!("internal error: missing default method `{default_target}`"),
                Span::new(0, 0),
            ));
        }

        let value_param_mutabilities: Vec<Mutability> = sig
            .params
            .iter()
            .map(|ty| match ty {
                Ty::Readonly(_) => Mutability::Readonly,
                _ => Mutability::Mutable,
            })
            .collect();
        let value_param_types: Vec<Option<Type>> = {
            let env = &self.env;
            sig.params
                .iter()
                .map(|ty| mir_type_from_ty(env, ty))
                .collect()
        };

        let mut lowerer = FunctionLowerer::new(
            self,
            FnKind::Real,
            module.clone(),
            wrapper_name.to_string(),
            sig.generics.clone(),
        );
        lowerer.bind_type_rep_params_for_signature();

        let mut value_param_locals = Vec::with_capacity(sig.params.len());
        for (mutability, ty) in value_param_mutabilities
            .into_iter()
            .zip(value_param_types.into_iter())
        {
            let local = lowerer.alloc_local();
            lowerer.params.push(Param {
                local,
                mutability,
                ty,
            });
            value_param_locals.push(local);
        }

        let impl_arity = sig
            .generics
            .len()
            .checked_sub(method_generics_len)
            .ok_or_else(|| {
                CompileError::new(
                    "internal error: wrapper method generics length mismatch",
                    Span::new(0, 0),
                )
            })?;

        let mut call_args = Vec::new();
        for gen_idx in 0..origin_iface_arity {
            let Some(local) = lowerer.generic_type_reps.get(gen_idx).and_then(|l| *l) else {
                return Err(CompileError::new(
                    "internal error: missing interface generic type rep in wrapper",
                    Span::new(0, 0),
                ));
            };
            call_args.push(Operand::Local(local));
        }
        for i in 0..method_generics_len {
            let gen_idx = impl_arity + i;
            if sig.generics.get(gen_idx).is_some_and(|gp| gp.arity == 0) {
                let Some(local) = lowerer.generic_type_reps.get(gen_idx).and_then(|l| *l) else {
                    return Err(CompileError::new(
                        "internal error: missing method generic type rep in wrapper",
                        Span::new(0, 0),
                    ));
                };
                call_args.push(Operand::Local(local));
            }
        }

        let Some((&recv_local, arg_locals)) = value_param_locals.split_first() else {
            return Err(CompileError::new(
                "internal error: wrapper missing receiver param",
                Span::new(0, 0),
            ));
        };
        call_args.push(Operand::Local(recv_local));
        for &local in arg_locals {
            call_args.push(Operand::Local(local));
        }

        let dst = lowerer.alloc_local();
        lowerer.emit(Instruction::Call {
            dst: Some(dst),
            func: default_target.to_string(),
            args: call_args,
        });
        lowerer.set_terminator(Terminator::Return {
            value: Operand::Local(dst),
        })?;

        let mut mir_fn = lowerer.finish()?;
        mir_fn.ret_type = mir_type_from_ty(&self.env, &sig.ret);
        self.module.add_function(mir_fn).map_err(|message| {
            CompileError::new(format!("internal error: {message}"), Span::new(0, 0))
        })?;
        Ok(())
    }

    fn compile_real_function(
        &mut self,
        module: &ModulePath,
        func: &FnItem,
        name_override: Option<String>,
    ) -> Result<(), CompileError> {
        let name = name_override.unwrap_or_else(|| module.qualify(&func.name.name));
        if self.module.function_ids.contains_key(&name) {
            return Err(CompileError::new(
                format!("duplicate function `{name}`"),
                func.name.span,
            ));
        }

        let sig = self
            .env
            .functions
            .get(&name)
            .ok_or_else(|| {
                CompileError::new(
                    format!("internal error: missing signature for `{name}`"),
                    func.span,
                )
            })?
            .clone();

        let param_mutabilities: Vec<Mutability> = sig
            .params
            .iter()
            .map(|ty| match ty {
                Ty::Readonly(_) => Mutability::Readonly,
                _ => Mutability::Mutable,
            })
            .collect();
        let param_types: Vec<Option<Type>> = sig
            .params
            .iter()
            .map(|ty| mir_type_from_ty(&self.env, ty))
            .collect();
        let ret_type = mir_type_from_ty(&self.env, &sig.ret);

        let mut lowerer = FunctionLowerer::new(
            self,
            FnKind::Real,
            module.clone(),
            name.clone(),
            sig.generics.clone(),
        );
        lowerer.bind_type_rep_params_for_signature();
        lowerer.bind_params_from_fn_item(func, &param_mutabilities, &param_types)?;
        lowerer.compute_captured_vars_for_block(&func.body);
        let value = lowerer.lower_block_expr(&func.body)?;
        if !lowerer.is_current_terminated() {
            lowerer.set_terminator(Terminator::Return {
                value: Operand::Local(value),
            })?;
        }

        let mut mir_fn = lowerer.finish()?;
        mir_fn.ret_type = ret_type;
        self.module.add_function(mir_fn).map_err(|message| {
            CompileError::new(format!("internal error: {message}"), func.name.span)
        })?;
        Ok(())
    }

    fn ensure_fn_value_wrapper(&mut self, target: &str) -> Result<String, CompileError> {
        if let Some(existing) = self.fn_value_wrappers.get(target) {
            return Ok(existing.clone());
        }

        let sig = self
            .env
            .functions
            .get(target)
            .ok_or_else(|| {
                CompileError::new(
                    format!("internal error: missing signature for `{target}`"),
                    Span::new(0, 0),
                )
            })?
            .clone();

        let wrapper_name = format!("$wrap::{target}");
        if self.module.function_ids.contains_key(&wrapper_name) {
            return Ok(wrapper_name);
        }

        let mut lowerer = FunctionLowerer::new(
            self,
            FnKind::Real,
            ModulePath::root(),
            wrapper_name.clone(),
            Vec::new(),
        );

        // Env parameter (ignored).
        let env_param_local = lowerer.alloc_local();
        lowerer.params.push(Param {
            local: env_param_local,
            mutability: Mutability::Mutable,
            ty: Some(Type::Array),
        });

        let mut forwarded_args = Vec::with_capacity(sig.params.len());
        for param_ty in &sig.params {
            let local = lowerer.alloc_local();
            let mutability = match param_ty {
                Ty::Readonly(_) => Mutability::Readonly,
                _ => Mutability::Mutable,
            };
            lowerer.params.push(Param {
                local,
                mutability,
                ty: None,
            });
            forwarded_args.push(Operand::Local(local));
        }

        let ret_local = lowerer.alloc_local();
        lowerer.emit(Instruction::Call {
            dst: Some(ret_local),
            func: sig.name.clone(),
            args: forwarded_args,
        });
        lowerer.set_terminator(Terminator::Return {
            value: Operand::Local(ret_local),
        })?;

        let mir_fn = lowerer.finish()?;
        self.module.add_function(mir_fn).map_err(|message| {
            CompileError::new(format!("internal error: {message}"), Span::new(0, 0))
        })?;
        self.fn_value_wrappers
            .insert(target.to_string(), wrapper_name.clone());
        Ok(wrapper_name)
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_lambda_entry(
        &mut self,
        module: &ModulePath,
        entry_name: String,
        generics: Vec<typeck::GenericParamInfo>,
        captures: &BTreeMap<String, VarInfo>,
        lambda_params: &[crate::ast::LambdaParam],
        body: &Block,
        lambda_expr_span: Span,
    ) -> Result<(), CompileError> {
        // Signature of the lambda expression (includes inferred readonly on params).
        let param_tys = match self.types.expr_types.get(&lambda_expr_span) {
            Some(Ty::Fn { params, .. }) => params.clone(),
            other => {
                return Err(CompileError::new(
                    format!("internal error: expected fn type for lambda, got {other:?}"),
                    lambda_expr_span,
                ));
            }
        };

        let mut lowerer = FunctionLowerer::new(
            self,
            FnKind::Real,
            module.clone(),
            entry_name.clone(),
            generics,
        );

        // First param: env array.
        let env_local = lowerer.alloc_local();
        lowerer.params.push(Param {
            local: env_local,
            mutability: Mutability::Mutable,
            ty: Some(Type::Array),
        });

        // Lambda params.
        if lambda_params.len() != param_tys.len() {
            return Err(CompileError::new(
                "internal error: lambda param arity mismatch",
                lambda_expr_span,
            ));
        }

        for (idx, _p) in lambda_params.iter().enumerate() {
            let local = lowerer.alloc_local();
            let mutability = match &param_tys[idx] {
                Ty::Readonly(_) => Mutability::Readonly,
                _ => Mutability::Mutable,
            };
            lowerer.params.push(Param {
                local,
                mutability,
                ty: None,
            });
        }

        // Bind captures into the root scope by reading from `env_local`.
        // Captures are stored in deterministic name order.
        for (idx, (name, info)) in captures.iter().enumerate() {
            let captured_val = lowerer.alloc_local();
            lowerer.emit(Instruction::IndexGet {
                dst: captured_val,
                arr: Operand::Local(env_local),
                idx: Operand::Literal(ConstValue::Int(idx as i64)),
            });
            if let Some(gen_id) = parse_type_rep_capture_name(name) {
                let Some(slot) = lowerer.generic_type_reps.get_mut(gen_id) else {
                    return Err(CompileError::new(
                        "internal error: invalid generic TypeRep capture id",
                        lambda_expr_span,
                    ));
                };
                *slot = Some(captured_val);
                continue;
            }

            lowerer.bind_var(
                name,
                VarInfo {
                    storage: match info.storage {
                        VarStorage::Local(_) => VarStorage::Local(captured_val),
                        VarStorage::Cell(_) => VarStorage::Cell(captured_val),
                    },
                    kind: info.kind,
                },
            );
        }

        // Bind lambda params (shadow captures if same name).
        for (idx, p) in lambda_params.iter().enumerate() {
            let param_local = lowerer.params[idx + 1].local;
            lowerer.bind_var(
                &p.name.name,
                VarInfo {
                    storage: VarStorage::Local(param_local),
                    kind: BindingKind::Const,
                },
            );
        }

        lowerer.compute_captured_vars_for_block(body);
        let result = lowerer.lower_block_expr(body)?;
        if !lowerer.is_current_terminated() {
            lowerer.set_terminator(Terminator::Return {
                value: Operand::Local(result),
            })?;
        }

        let mir_fn = lowerer.finish()?;
        self.module.add_function(mir_fn).map_err(|message| {
            CompileError::new(format!("internal error: {message}"), lambda_expr_span)
        })?;
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_match_helper(
        &mut self,
        module: &ModulePath,
        helper_name: String,
        generics: Vec<typeck::GenericParamInfo>,
        captured: &BTreeMap<String, VarInfo>,
        scrutinee: &Expr,
        arms: &[MatchArm],
        match_span: Span,
    ) -> Result<(), CompileError> {
        let mut lowerer = FunctionLowerer::new(
            self,
            FnKind::ExprHelper,
            module.clone(),
            helper_name.clone(),
            generics,
        );

        // Captured environment params (in deterministic name order).
        for (name, info) in captured.iter() {
            let local = lowerer.alloc_local();
            lowerer.params.push(Param {
                local,
                mutability: Mutability::Mutable,
                ty: None,
            });
            if let Some(gen_id) = parse_type_rep_capture_name(name) {
                let Some(slot) = lowerer.generic_type_reps.get_mut(gen_id) else {
                    return Err(CompileError::new(
                        "internal error: invalid generic TypeRep capture id",
                        match_span,
                    ));
                };
                *slot = Some(local);
                continue;
            }
            lowerer.bind_var(
                name,
                VarInfo {
                    storage: match info.storage {
                        VarStorage::Local(_) => VarStorage::Local(local),
                        VarStorage::Cell(_) => VarStorage::Cell(local),
                    },
                    kind: info.kind,
                },
            );
        }

        lowerer.compute_captured_vars_for_match(scrutinee, arms);
        let value = lowerer.lower_match_inline_with_effects(scrutinee, arms, match_span)?;
        if !lowerer.is_current_terminated() {
            let control = lowerer.make_control(CONTROL_VARIANT_VALUE, vec![Operand::Local(value)]);
            lowerer.set_terminator(Terminator::Return {
                value: Operand::Local(control),
            })?;
        }

        let mut mir_fn = lowerer.finish()?;
        mir_fn.ret_type = Some(Type::Enum(INTERNAL_CONTROL_ENUM.to_string()));
        self.module.add_function(mir_fn).map_err(|message| {
            CompileError::new(format!("internal error: {message}"), match_span)
        })?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FnKind {
    Real,
    ExprHelper,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum VarStorage {
    Local(Local),
    Cell(Local),
}

impl VarStorage {
    fn local(self) -> Local {
        match self {
            VarStorage::Local(local) | VarStorage::Cell(local) => local,
        }
    }
}

#[derive(Clone)]
struct VarInfo {
    storage: VarStorage,
    kind: BindingKind,
}

#[derive(Clone, Copy)]
struct LoopTargets {
    continue_block: BlockId,
    break_block: BlockId,
}

#[derive(Clone)]
struct BlockBuilder {
    label: String,
    params: Vec<Local>,
    instructions: Vec<Instruction>,
    terminator: Option<Terminator>,
}

struct FunctionLowerer<'a> {
    compiler: &'a mut Compiler,
    kind: FnKind,
    module: ModulePath,
    name: String,
    /// Generic parameters in scope for interpreting `Ty::Gen(n)` during lowering.
    ///
    /// This must match the typechecker's notion of "current generics environment" for the AST
    /// being lowered (not necessarily the MIR function's own signature, since lambdas/helpers are
    /// compiled from within a parent function).
    generics: Vec<typeck::GenericParamInfo>,
    /// Runtime locals holding `TypeRep` values for the in-scope generic parameters.
    ///
    /// `generic_type_reps[i]` is `Some(local)` iff `generics[i].arity == 0`.
    generic_type_reps: Vec<Option<Local>>,
    params: Vec<Param>,
    blocks: Vec<BlockBuilder>,
    current: BlockId,
    next_local: usize,
    captured_vars: BTreeSet<String>,
    scopes: Vec<BTreeMap<String, VarInfo>>,
    loop_stack: Vec<LoopTargets>,
}

impl<'a> FunctionLowerer<'a> {
    fn new(
        compiler: &'a mut Compiler,
        kind: FnKind,
        module: ModulePath,
        name: String,
        generics: Vec<typeck::GenericParamInfo>,
    ) -> Self {
        let generic_count = generics.len();
        Self {
            compiler,
            kind,
            module,
            name,
            generics,
            generic_type_reps: vec![None; generic_count],
            params: Vec::new(),
            blocks: vec![BlockBuilder {
                label: "block0".to_string(),
                params: Vec::new(),
                instructions: Vec::new(),
                terminator: None,
            }],
            current: BlockId(0),
            next_local: 0,
            captured_vars: BTreeSet::new(),
            scopes: vec![BTreeMap::new()],
            loop_stack: Vec::new(),
        }
    }

    fn compute_captured_vars_for_block(&mut self, body: &Block) {
        let mut bound = BTreeSet::new();
        for scope in &self.scopes {
            bound.extend(scope.keys().cloned());
        }

        let mut captured = BTreeSet::new();
        collect_captured_vars_in_block(body, &mut bound, &mut captured);
        self.captured_vars = captured;
    }

    fn compute_captured_vars_for_match(&mut self, scrutinee: &Expr, arms: &[MatchArm]) {
        let mut bound = BTreeSet::new();
        for scope in &self.scopes {
            bound.extend(scope.keys().cloned());
        }

        let mut captured = BTreeSet::new();
        collect_captured_vars_in_match_expr(scrutinee, arms, &bound, &mut captured);
        self.captured_vars = captured;
    }

    fn finish(mut self) -> Result<Function, CompileError> {
        // Ensure all blocks have terminators.
        for block in &mut self.blocks {
            if block.terminator.is_none() {
                block.terminator = Some(Terminator::Trap {
                    message: "unterminated block".to_string(),
                });
            }
        }

        let blocks = self
            .blocks
            .into_iter()
            .map(|b| BasicBlock {
                label: b.label,
                params: b.params,
                instructions: b.instructions,
                terminator: b.terminator.unwrap_or(Terminator::Trap {
                    message: "unterminated block".to_string(),
                }),
            })
            .collect();

        Ok(Function {
            name: self.name,
            params: self.params,
            ret_type: None,
            locals: self.next_local,
            blocks,
        })
    }

    fn bind_type_rep_params_for_signature(&mut self) {
        let type_param_ids: Vec<usize> = self
            .generics
            .iter()
            .enumerate()
            .filter_map(|(idx, gp)| (gp.arity == 0).then_some(idx))
            .collect();
        for idx in type_param_ids {
            let local = self.alloc_local();
            self.params.push(Param {
                local,
                mutability: Mutability::Readonly,
                ty: Some(Type::TypeRep),
            });
            self.generic_type_reps[idx] = Some(local);
        }
    }

    fn bind_params_from_fn_item(
        &mut self,
        func: &FnItem,
        param_mutabilities: &[Mutability],
        param_types: &[Option<Type>],
    ) -> Result<(), CompileError> {
        let implicit_receiver = matches!(
            func.kind,
            FnItemKind::Method {
                receiver: MethodReceiverKind::Instance { .. }
            }
        );
        let expected_params = if implicit_receiver {
            func.params.len() + 1
        } else {
            func.params.len()
        };
        if expected_params != param_mutabilities.len() || expected_params != param_types.len() {
            return Err(CompileError::new(
                "internal error: function signature arity mismatch",
                func.span,
            ));
        }

        let mut trap_block: Option<BlockId> = None;
        // Optional implicit receiver param (`self`).
        let start_idx = if implicit_receiver {
            let local = self.alloc_local();
            let mutability = param_mutabilities[0];
            self.params.push(Param {
                local,
                mutability,
                ty: param_types[0].clone(),
            });
            self.bind_var(
                "self",
                VarInfo {
                    storage: VarStorage::Local(local),
                    kind: BindingKind::Const,
                },
            );
            1usize
        } else {
            0usize
        };

        for (idx, p) in func.params.iter().enumerate() {
            let sig_idx = start_idx + idx;
            let local = self.alloc_local();
            let mutability = param_mutabilities[sig_idx];
            self.params.push(Param {
                local,
                mutability,
                ty: param_types[sig_idx].clone(),
            });

            match &p.pat {
                crate::ast::Pattern::Wildcard { .. } => {}
                crate::ast::Pattern::Bind { name, .. } => {
                    // Simple parameter binding.
                    self.bind_var(
                        &name.name,
                        VarInfo {
                            storage: VarStorage::Local(local),
                            kind: BindingKind::Const,
                        },
                    );
                }
                other_pat => {
                    let trap = match trap_block {
                        Some(id) => id,
                        None => {
                            let fail = self.new_block("param_pat_fail");
                            let prev = self.current;
                            self.set_current(fail);
                            self.set_terminator(Terminator::Trap {
                                message: "parameter pattern match failed".to_string(),
                            })?;
                            self.set_current(prev);
                            trap_block = Some(fail);
                            fail
                        }
                    };

                    let (mir_pat, bind_names) = self.lower_ast_pattern(other_pat)?;
                    let ok_block = self.new_block(format!("param_pat_{idx}_ok"));

                    let mut params = Vec::with_capacity(bind_names.len());
                    for _ in 0..bind_names.len() {
                        params.push(self.alloc_local());
                    }
                    self.blocks[ok_block.0].params = params.clone();

                    self.set_terminator(Terminator::Switch {
                        value: Operand::Local(local),
                        cases: vec![SwitchCase {
                            pattern: mir_pat,
                            target: ok_block,
                        }],
                        default: trap,
                    })?;

                    self.set_current(ok_block);
                    for (name, local) in bind_names.into_iter().zip(params.into_iter()) {
                        self.bind_var(
                            &name,
                            VarInfo {
                                storage: VarStorage::Local(local),
                                kind: BindingKind::Const,
                            },
                        );
                    }
                }
            }
        }
        Ok(())
    }

    fn lower_ast_pattern(
        &mut self,
        pat: &AstPattern,
    ) -> Result<(Pattern, Vec<String>), CompileError> {
        match pat {
            AstPattern::Wildcard { .. } => Ok((Pattern::Wildcard, Vec::new())),
            AstPattern::Bind { name, .. } => Ok((Pattern::Bind, vec![name.name.clone()])),
            AstPattern::Literal { lit, .. } => {
                let cv = match lit {
                    PatLiteral::Unit => ConstValue::Unit,
                    PatLiteral::Bool(v) => ConstValue::Bool(*v),
                    PatLiteral::Int(v) => ConstValue::Int(*v),
                    PatLiteral::Float(v) => ConstValue::Float(*v),
                    PatLiteral::String(v) => ConstValue::String(v.clone()),
                    PatLiteral::Bytes(v) => ConstValue::Bytes(v.clone()),
                };
                Ok((Pattern::Literal(cv), Vec::new()))
            }
            AstPattern::Tuple {
                prefix,
                rest,
                suffix,
                ..
            } => {
                let mut out_prefix = Vec::with_capacity(prefix.len());
                let mut out_suffix = Vec::with_capacity(suffix.len());
                let mut binds = Vec::new();

                for it in prefix {
                    let (p, b) = self.lower_ast_pattern(it)?;
                    out_prefix.push(p);
                    binds.extend(b);
                }

                let out_rest = rest.as_ref().map(|r| {
                    if let Some(binding) = &r.binding {
                        binds.push(binding.name.clone());
                        Box::new(Pattern::Bind)
                    } else {
                        Box::new(Pattern::Wildcard)
                    }
                });

                for it in suffix {
                    let (p, b) = self.lower_ast_pattern(it)?;
                    out_suffix.push(p);
                    binds.extend(b);
                }

                Ok((
                    Pattern::Tuple {
                        prefix: out_prefix,
                        rest: out_rest,
                        suffix: out_suffix,
                    },
                    binds,
                ))
            }
            AstPattern::Enum {
                enum_path,
                variant,
                fields,
                ..
            } => {
                let segments: Vec<String> =
                    enum_path.segments.iter().map(|s| s.name.clone()).collect();
                let (kind, enum_name) = self
                    .compiler
                    .env
                    .modules
                    .resolve_type_fqn(&self.module, &segments, enum_path.span)
                    .map_err(|e| CompileError::new(e.message, e.span))?;
                if kind != DefKind::Enum {
                    return Err(CompileError::new(
                        format!(
                            "internal error: expected an enum type in pattern, got `{enum_name}`"
                        ),
                        enum_path.span,
                    ));
                }

                let mut out_fields = Vec::with_capacity(fields.len());
                let mut binds = Vec::new();
                for f in fields {
                    let (p, b) = self.lower_ast_pattern(f)?;
                    out_fields.push(p);
                    binds.extend(b);
                }
                Ok((
                    Pattern::Enum {
                        enum_name,
                        variant: variant.name.clone(),
                        fields: out_fields,
                    },
                    binds,
                ))
            }
            AstPattern::Struct {
                type_path, fields, ..
            } => {
                let segments: Vec<String> =
                    type_path.segments.iter().map(|s| s.name.clone()).collect();
                let (kind, type_name) = self
                    .compiler
                    .env
                    .modules
                    .resolve_type_fqn(&self.module, &segments, type_path.span)
                    .map_err(|e| CompileError::new(e.message, e.span))?;
                if kind != DefKind::Struct {
                    return Err(CompileError::new(
                        format!(
                            "internal error: expected a struct type in pattern, got `{type_name}`"
                        ),
                        type_path.span,
                    ));
                }

                let mut out_fields = Vec::with_capacity(fields.len());
                let mut binds = Vec::new();
                for (fname, subpat) in fields {
                    let (p, b) = self.lower_ast_pattern(subpat)?;
                    out_fields.push((fname.name.clone(), p));
                    binds.extend(b);
                }
                Ok((
                    Pattern::Struct {
                        type_name,
                        fields: out_fields,
                    },
                    binds,
                ))
            }
            AstPattern::Ctor { path, args, .. } => {
                let mut out_args = Vec::with_capacity(args.len());
                let mut binds = Vec::new();
                for it in args {
                    let (p, b) = self.lower_ast_pattern(it)?;
                    out_args.push(p);
                    binds.extend(b);
                }

                let segments: Vec<String> = path.segments.iter().map(|s| s.name.clone()).collect();

                if segments.len() >= 2 {
                    let prefix = &segments[..segments.len() - 1];
                    let last = segments.last().expect("len >= 2");
                    let last_ident = path.segments.last().expect("len >= 2");

                    if let Some((kind, enum_name)) = self
                        .compiler
                        .env
                        .modules
                        .try_resolve_type_fqn(&self.module, prefix, path.span)
                        .map_err(|e| CompileError::new(e.message, e.span))?
                        && kind == DefKind::Enum
                    {
                        let Some(def) = self.compiler.env.enums.get(&enum_name) else {
                            return Err(CompileError::new(
                                format!("internal error: unknown enum `{enum_name}`"),
                                path.span,
                            ));
                        };
                        if !def.variants.contains_key(last) {
                            return Err(CompileError::new(
                                format!(
                                    "internal error: unknown enum variant `{enum_name}::{last}`"
                                ),
                                last_ident.span,
                            ));
                        }
                        return Ok((
                            Pattern::Enum {
                                enum_name,
                                variant: last_ident.name.clone(),
                                fields: out_args,
                            },
                            binds,
                        ));
                    }
                }

                let (kind, type_name) = self
                    .compiler
                    .env
                    .modules
                    .resolve_type_fqn(&self.module, &segments, path.span)
                    .map_err(|e| CompileError::new(e.message, e.span))?;
                if kind != DefKind::Struct {
                    return Err(CompileError::new(
                        format!(
                            "internal error: expected a struct type in pattern, got `{type_name}`"
                        ),
                        path.span,
                    ));
                }
                let Some(def) = self.compiler.env.structs.get(&type_name) else {
                    return Err(CompileError::new(
                        format!("internal error: unknown struct `{type_name}`"),
                        path.span,
                    ));
                };
                if !def.is_newtype {
                    return Err(CompileError::new(
                        format!(
                            "internal error: constructor pattern requires a new-type struct, got `{type_name}`"
                        ),
                        path.span,
                    ));
                }
                if out_args.len() != 1 {
                    return Err(CompileError::new(
                        format!(
                            "internal error: wrong number of fields for pattern `{type_name}`: expected 1, got {}",
                            out_args.len()
                        ),
                        path.span,
                    ));
                }
                Ok((
                    Pattern::Struct {
                        type_name,
                        fields: vec![(".0".to_string(), out_args.into_iter().next().unwrap())],
                    },
                    binds,
                ))
            }
            AstPattern::Array {
                prefix,
                rest,
                suffix,
                ..
            } => {
                let mut out_prefix = Vec::with_capacity(prefix.len());
                let mut out_suffix = Vec::with_capacity(suffix.len());
                let mut binds = Vec::new();

                for it in prefix {
                    let (p, b) = self.lower_ast_pattern(it)?;
                    out_prefix.push(p);
                    binds.extend(b);
                }

                let out_rest = rest.as_ref().map(|r| {
                    if let Some(binding) = &r.binding {
                        binds.push(binding.name.clone());
                        Box::new(Pattern::Bind)
                    } else {
                        Box::new(Pattern::Wildcard)
                    }
                });

                for it in suffix {
                    let (p, b) = self.lower_ast_pattern(it)?;
                    out_suffix.push(p);
                    binds.extend(b);
                }

                Ok((
                    Pattern::Array {
                        prefix: out_prefix,
                        rest: out_rest,
                        suffix: out_suffix,
                    },
                    binds,
                ))
            }
        }
    }

    fn alloc_local(&mut self) -> Local {
        let l = Local(self.next_local);
        self.next_local += 1;
        l
    }

    fn new_block(&mut self, label: impl Into<String>) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(BlockBuilder {
            label: label.into(),
            params: Vec::new(),
            instructions: Vec::new(),
            terminator: None,
        });
        id
    }

    fn set_current(&mut self, block: BlockId) {
        self.current = block;
    }

    fn is_current_terminated(&self) -> bool {
        self.blocks[self.current.0].terminator.is_some()
    }

    fn emit(&mut self, instr: Instruction) {
        self.blocks[self.current.0].instructions.push(instr);
    }

    fn set_terminator(&mut self, term: Terminator) -> Result<(), CompileError> {
        let block = &mut self.blocks[self.current.0];
        if block.terminator.is_some() {
            let label = block.label.clone();
            let func = self.name.clone();
            let id = self.current.0;
            return Err(CompileError::new(
                format!("internal error: block already terminated ({func}:{id} {label})"),
                Span::new(0, 0),
            ));
        }
        block.terminator = Some(term);
        Ok(())
    }

    fn push_scope(&mut self) {
        self.scopes.push(BTreeMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn bind_var(&mut self, name: &str, info: VarInfo) {
        self.scopes
            .last_mut()
            .expect("at least one scope")
            .insert(name.to_string(), info);
    }

    fn lookup_var(&self, name: &str) -> Option<&VarInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v);
            }
        }
        None
    }

    fn visible_bindings(&self) -> BTreeMap<String, VarInfo> {
        let mut out = BTreeMap::<String, VarInfo>::new();
        for scope in self.scopes.iter().rev() {
            for (name, info) in scope.iter() {
                out.entry(name.clone()).or_insert_with(|| info.clone());
            }
        }
        for (idx, local) in self.generic_type_reps.iter().copied().enumerate() {
            let Some(local) = local else {
                continue;
            };
            out.entry(format!("{CAPTURE_TYPE_REP_PREFIX}{idx}"))
                .or_insert_with(|| VarInfo {
                    storage: VarStorage::Local(local),
                    kind: BindingKind::Const,
                });
        }
        out
    }

    fn make_control(&mut self, variant: &str, fields: Vec<Operand>) -> Local {
        let dst = self.alloc_local();
        self.emit(Instruction::MakeEnum {
            dst,
            enum_name: INTERNAL_CONTROL_ENUM.to_string(),
            type_args: Vec::new(),
            variant: variant.to_string(),
            fields,
        });
        dst
    }

    fn unwrap_control(&mut self, control_local: Local, _span: Span) -> Result<Local, CompileError> {
        if self.is_current_terminated() {
            return Ok(self.alloc_unit());
        }

        let switch_block = self.current;
        let value_block = self.new_block("control_value");
        let value_param = self.alloc_local();
        self.blocks[value_block.0].params = vec![value_param];

        let mut cases = Vec::new();
        cases.push(SwitchCase {
            pattern: Pattern::Enum {
                enum_name: INTERNAL_CONTROL_ENUM.to_string(),
                variant: CONTROL_VARIANT_VALUE.to_string(),
                fields: vec![Pattern::Bind],
            },
            target: value_block,
        });

        match self.kind {
            FnKind::Real => {
                // Return: immediately return the payload from this function.
                let ret_block = self.new_block("control_return");
                let ret_param = self.alloc_local();
                self.blocks[ret_block.0].params = vec![ret_param];
                self.set_current(ret_block);
                self.set_terminator(Terminator::Return {
                    value: Operand::Local(ret_param),
                })?;

                cases.push(SwitchCase {
                    pattern: Pattern::Enum {
                        enum_name: INTERNAL_CONTROL_ENUM.to_string(),
                        variant: CONTROL_VARIANT_RETURN.to_string(),
                        fields: vec![Pattern::Bind],
                    },
                    target: ret_block,
                });

                // Break/Continue: jump to current loop targets.
                let break_block = self.new_block("control_break");
                self.set_current(break_block);
                if let Some(targets) = self.loop_stack.last().copied() {
                    self.set_terminator(Terminator::Br {
                        target: targets.break_block,
                        args: Vec::new(),
                    })?;
                } else {
                    self.set_terminator(Terminator::Trap {
                        message: "`break` outside of a loop".to_string(),
                    })?;
                }
                cases.push(SwitchCase {
                    pattern: Pattern::Enum {
                        enum_name: INTERNAL_CONTROL_ENUM.to_string(),
                        variant: CONTROL_VARIANT_BREAK.to_string(),
                        fields: Vec::new(),
                    },
                    target: break_block,
                });

                let continue_block = self.new_block("control_continue");
                self.set_current(continue_block);
                if let Some(targets) = self.loop_stack.last().copied() {
                    self.set_terminator(Terminator::Br {
                        target: targets.continue_block,
                        args: Vec::new(),
                    })?;
                } else {
                    self.set_terminator(Terminator::Trap {
                        message: "`continue` outside of a loop".to_string(),
                    })?;
                }
                cases.push(SwitchCase {
                    pattern: Pattern::Enum {
                        enum_name: INTERNAL_CONTROL_ENUM.to_string(),
                        variant: CONTROL_VARIANT_CONTINUE.to_string(),
                        fields: Vec::new(),
                    },
                    target: continue_block,
                });
            }
            FnKind::ExprHelper => {
                // Propagate non-value control outwards by returning the control token.
                for (variant, field_count) in [
                    (CONTROL_VARIANT_RETURN, 1),
                    (CONTROL_VARIANT_BREAK, 0),
                    (CONTROL_VARIANT_CONTINUE, 0),
                ] {
                    let block = self.new_block(format!("control_propagate_{variant}"));
                    self.set_current(block);
                    self.set_terminator(Terminator::Return {
                        value: Operand::Local(control_local),
                    })?;
                    let fields = if field_count == 0 {
                        Vec::new()
                    } else {
                        vec![Pattern::Wildcard]
                    };
                    cases.push(SwitchCase {
                        pattern: Pattern::Enum {
                            enum_name: INTERNAL_CONTROL_ENUM.to_string(),
                            variant: variant.to_string(),
                            fields,
                        },
                        target: block,
                    });
                }
            }
        }

        let default_block = self.new_block("control_default");
        self.set_current(default_block);
        self.set_terminator(Terminator::Trap {
            message: "invalid control flow token".to_string(),
        })?;

        self.set_current(switch_block);
        self.set_terminator(Terminator::Switch {
            value: Operand::Local(control_local),
            cases,
            default: default_block,
        })?;

        self.set_current(value_block);
        Ok(value_param)
    }

    fn alloc_unit(&mut self) -> Local {
        let dst = self.alloc_local();
        self.emit(Instruction::Const {
            dst,
            value: ConstValue::Unit,
        });
        dst
    }

    fn alloc_bool(&mut self, value: bool) -> Local {
        let dst = self.alloc_local();
        self.emit(Instruction::Const {
            dst,
            value: ConstValue::Bool(value),
        });
        dst
    }

    fn alloc_int(&mut self, value: i64) -> Local {
        let dst = self.alloc_local();
        self.emit(Instruction::Const {
            dst,
            value: ConstValue::Int(value),
        });
        dst
    }

    fn lower_block_expr(&mut self, block: &Block) -> Result<Local, CompileError> {
        self.push_scope();
        for stmt in &block.stmts {
            self.lower_stmt(stmt)?;
            if self.is_current_terminated() {
                self.pop_scope();
                return Ok(self.alloc_unit());
            }
        }

        let out = if let Some(tail) = &block.tail {
            self.lower_expr(tail)?
        } else {
            self.alloc_unit()
        };
        self.pop_scope();
        Ok(out)
    }

    fn lower_block_stmt(&mut self, block: &Block) -> Result<(), CompileError> {
        self.push_scope();
        for stmt in &block.stmts {
            self.lower_stmt(stmt)?;
            if self.is_current_terminated() {
                self.pop_scope();
                return Ok(());
            }
        }
        if let Some(tail) = &block.tail {
            let _ = self.lower_expr(tail)?;
        }
        self.pop_scope();
        Ok(())
    }

    fn lower_stmt(&mut self, stmt: &Stmt) -> Result<(), CompileError> {
        if self.is_current_terminated() {
            return Ok(());
        }

        match stmt {
            Stmt::Let {
                kind,
                pat,
                ty: _,
                init,
                span,
            } => self.lower_let_stmt(*kind, pat, init.as_ref(), *span)?,
            Stmt::Return { value, span } => self.lower_return_stmt(value.as_ref(), *span)?,
            Stmt::Break { span } => self.lower_break_continue(true, *span)?,
            Stmt::Continue { span } => self.lower_break_continue(false, *span)?,
            Stmt::Expr { expr, .. } => {
                let _ = self.lower_expr(expr)?;
            }
        }
        Ok(())
    }

    fn lower_return_stmt(&mut self, value: Option<&Expr>, span: Span) -> Result<(), CompileError> {
        let v = if let Some(expr) = value {
            self.lower_expr(expr)?
        } else {
            self.alloc_unit()
        };

        match self.kind {
            FnKind::Real => self.set_terminator(Terminator::Return {
                value: Operand::Local(v),
            })?,
            FnKind::ExprHelper => {
                let control = self.make_control(CONTROL_VARIANT_RETURN, vec![Operand::Local(v)]);
                self.set_terminator(Terminator::Return {
                    value: Operand::Local(control),
                })?;
            }
        }
        let _ = span;
        Ok(())
    }

    fn lower_break_continue(&mut self, is_break: bool, span: Span) -> Result<(), CompileError> {
        if let Some(targets) = self.loop_stack.last().copied() {
            let target = if is_break {
                targets.break_block
            } else {
                targets.continue_block
            };
            self.set_terminator(Terminator::Br {
                target,
                args: Vec::new(),
            })?;
            return Ok(());
        }

        // No local loop: this can only happen if we are compiling an extracted helper.
        match self.kind {
            FnKind::Real => Err(CompileError::new(
                if is_break {
                    "`break` outside of a loop"
                } else {
                    "`continue` outside of a loop"
                },
                span,
            )),
            FnKind::ExprHelper => {
                let variant = if is_break {
                    CONTROL_VARIANT_BREAK
                } else {
                    CONTROL_VARIANT_CONTINUE
                };
                let control = self.make_control(variant, Vec::new());
                self.set_terminator(Terminator::Return {
                    value: Operand::Local(control),
                })?;
                Ok(())
            }
        }
    }

    fn lower_let_stmt(
        &mut self,
        kind: BindingKind,
        pat: &crate::ast::Pattern,
        init: Option<&Expr>,
        span: Span,
    ) -> Result<(), CompileError> {
        // Fast paths for the common cases:
        // - `let x = expr;`
        // - `const x = expr;`
        // - `readonly x = expr;`
        // - `*_ _ = expr;` (no binding, just evaluate for effect / type assertion)
        match (kind, pat, init) {
            (BindingKind::Let, crate::ast::Pattern::Bind { .. }, None) => {
                return Err(CompileError::new(
                    "`let` bindings require an initializer",
                    span,
                ));
            }

            // `let x = expr;`
            (BindingKind::Let, crate::ast::Pattern::Bind { name, .. }, Some(init_expr)) => {
                let v = self.lower_expr(init_expr)?;
                if self.captured_vars.contains(&name.name) {
                    // Captured `let` bindings need a stable location shared across helper/closure
                    // boundaries, so we lower them to an internal `$Cell`.
                    let cell = self.alloc_local();
                    self.emit(Instruction::MakeStruct {
                        dst: cell,
                        type_name: INTERNAL_CELL_STRUCT.to_string(),
                        type_args: Vec::new(),
                        fields: vec![(CELL_FIELD_VALUE.to_string(), Operand::Local(v))],
                    });
                    self.bind_var(
                        &name.name,
                        VarInfo {
                            storage: VarStorage::Cell(cell),
                            kind: BindingKind::Let,
                        },
                    );
                } else {
                    // Uncaptured `let` bindings can live directly in a frame local.
                    let dst = self.alloc_local();
                    self.emit(Instruction::Copy { dst, src: v });
                    self.bind_var(
                        &name.name,
                        VarInfo {
                            storage: VarStorage::Local(dst),
                            kind: BindingKind::Let,
                        },
                    );
                }
                return Ok(());
            }

            // `const x = expr;` / `readonly x = expr;`
            (
                BindingKind::Const | BindingKind::Readonly,
                crate::ast::Pattern::Bind { name, .. },
                Some(init_expr),
            ) => {
                let value_local = self.lower_expr(init_expr)?;
                let dst = self.alloc_local();
                self.emit(Instruction::Copy {
                    dst,
                    src: value_local,
                });
                if kind == BindingKind::Readonly {
                    self.emit(Instruction::AsReadonly { dst, src: dst });
                }
                self.bind_var(
                    &name.name,
                    VarInfo {
                        storage: VarStorage::Local(dst),
                        kind,
                    },
                );
                return Ok(());
            }

            // `_` patterns: evaluate and discard. This is useful for type ascription, e.g.
            // `let _: T = expr;` / `const _: T = expr;`.
            (_, crate::ast::Pattern::Wildcard { .. }, Some(init_expr)) => {
                let _ = self.lower_expr(init_expr)?;
                return Ok(());
            }

            // Anything else falls through to the general destructuring lowering below.
            _ => {}
        }

        match kind {
            BindingKind::Let => {
                let Some(init_expr) = init else {
                    return Err(CompileError::new(
                        "`let` bindings require an initializer",
                        span,
                    ));
                };

                let tmp = self.lower_expr(init_expr)?;
                let (mir_pat, bind_names) = self.lower_ast_pattern(pat)?;

                let ok_block = self.new_block("let_pat_ok");
                let mut params = Vec::with_capacity(bind_names.len());
                for _ in 0..bind_names.len() {
                    params.push(self.alloc_local());
                }
                self.blocks[ok_block.0].params = params.clone();

                let trap_block = self.new_block("let_pat_fail");
                let prev = self.current;
                self.set_current(trap_block);
                self.set_terminator(Terminator::Trap {
                    message: "binding pattern match failed".to_string(),
                })?;
                self.set_current(prev);

                self.set_terminator(Terminator::Switch {
                    value: Operand::Local(tmp),
                    cases: vec![SwitchCase {
                        pattern: mir_pat,
                        target: ok_block,
                    }],
                    default: trap_block,
                })?;

                self.set_current(ok_block);
                for (name, value_local) in bind_names.into_iter().zip(params.into_iter()) {
                    if self.captured_vars.contains(&name) {
                        let cell = self.alloc_local();
                        self.emit(Instruction::MakeStruct {
                            dst: cell,
                            type_name: INTERNAL_CELL_STRUCT.to_string(),
                            type_args: Vec::new(),
                            fields: vec![(
                                CELL_FIELD_VALUE.to_string(),
                                Operand::Local(value_local),
                            )],
                        });
                        self.bind_var(
                            &name,
                            VarInfo {
                                storage: VarStorage::Cell(cell),
                                kind: BindingKind::Let,
                            },
                        );
                    } else {
                        let dst = self.alloc_local();
                        self.emit(Instruction::Copy {
                            dst,
                            src: value_local,
                        });
                        self.bind_var(
                            &name,
                            VarInfo {
                                storage: VarStorage::Local(dst),
                                kind: BindingKind::Let,
                            },
                        );
                    }
                }
                Ok(())
            }
            BindingKind::Const | BindingKind::Readonly => {
                let Some(init_expr) = init else {
                    return Err(CompileError::new(
                        "const/readonly bindings require an initializer",
                        span,
                    ));
                };

                let tmp = self.lower_expr(init_expr)?;
                let (mir_pat, bind_names) = self.lower_ast_pattern(pat)?;

                let ok_block = self.new_block("binding_pat_ok");
                let mut params = Vec::with_capacity(bind_names.len());
                for _ in 0..bind_names.len() {
                    params.push(self.alloc_local());
                }
                self.blocks[ok_block.0].params = params.clone();

                let trap_block = self.new_block("binding_pat_fail");
                let prev = self.current;
                self.set_current(trap_block);
                self.set_terminator(Terminator::Trap {
                    message: "binding pattern match failed".to_string(),
                })?;
                self.set_current(prev);

                self.set_terminator(Terminator::Switch {
                    value: Operand::Local(tmp),
                    cases: vec![SwitchCase {
                        pattern: mir_pat,
                        target: ok_block,
                    }],
                    default: trap_block,
                })?;

                self.set_current(ok_block);
                for (name, value_local) in bind_names.into_iter().zip(params.into_iter()) {
                    let dst = self.alloc_local();
                    self.emit(Instruction::Copy {
                        dst,
                        src: value_local,
                    });
                    if kind == BindingKind::Readonly {
                        self.emit(Instruction::AsReadonly { dst, src: dst });
                    }
                    self.bind_var(
                        &name,
                        VarInfo {
                            storage: VarStorage::Local(dst),
                            kind,
                        },
                    );
                }
                Ok(())
            }
        }
    }

    fn read_cell(&mut self, cell: Local, name: &str, span: Span) -> Result<Local, CompileError> {
        if self.is_current_terminated() {
            return Ok(self.alloc_unit());
        }

        let value_local = self.alloc_local();
        self.emit(Instruction::StructGet {
            dst: value_local,
            obj: Operand::Local(cell),
            idx: 0,
        });
        let _ = (name, span);
        Ok(value_local)
    }

    fn write_cell(&mut self, cell: Local, value: Local) {
        self.emit(Instruction::StructSet {
            obj: Operand::Local(cell),
            idx: 0,
            value: Operand::Local(value),
        });
    }

    fn expr_ty(&self, expr: &Expr) -> Option<&Ty> {
        self.compiler.types.expr_types.get(&expr.span())
    }

    fn expr_is_readonly(&self, expr: &Expr) -> bool {
        matches!(self.expr_ty(expr), Some(Ty::Readonly(_)))
    }

    fn strip_readonly_ty<'t>(&self, ty: &'t Ty) -> &'t Ty {
        match ty {
            Ty::Readonly(inner) => inner.as_ref(),
            other => other,
        }
    }

    fn resolve_struct_field_index(&self, base: &Expr, field: &str) -> Option<usize> {
        let ty = self.strip_readonly_ty(self.expr_ty(base)?);
        let Ty::App(typeck::TyCon::Named(type_name), _args) = ty else {
            return None;
        };
        let def = self.compiler.env.structs.get(type_name)?;
        def.fields
            .iter()
            .position(|(field_name, _)| field_name == field)
    }

    fn lower_type_rep_for_ty(&mut self, ty: &Ty, span: Span) -> Result<Operand, CompileError> {
        let rep = match ty {
            Ty::Readonly(inner) => return self.lower_type_rep_for_ty(inner, span),
            Ty::Unit => Operand::Literal(ConstValue::TypeRep(TypeRepLit::Unit)),
            Ty::Bool => Operand::Literal(ConstValue::TypeRep(TypeRepLit::Bool)),
            Ty::Int => Operand::Literal(ConstValue::TypeRep(TypeRepLit::Int)),
            Ty::Float => Operand::Literal(ConstValue::TypeRep(TypeRepLit::Float)),
            Ty::String => Operand::Literal(ConstValue::TypeRep(TypeRepLit::String)),
            Ty::Bytes => Operand::Literal(ConstValue::TypeRep(TypeRepLit::Bytes)),
            Ty::Array(elem) => {
                let elem_rep = self.lower_type_rep_for_ty(elem, span)?;
                let dst = self.alloc_local();
                self.emit(Instruction::MakeTypeRep {
                    dst,
                    base: TypeRepLit::Array,
                    args: vec![elem_rep],
                });
                Operand::Local(dst)
            }
            Ty::Tuple(items) => {
                if items.is_empty() {
                    return Ok(Operand::Literal(ConstValue::TypeRep(TypeRepLit::Unit)));
                }
                let mut args = Vec::with_capacity(items.len());
                for item in items {
                    args.push(self.lower_type_rep_for_ty(item, span)?);
                }
                let dst = self.alloc_local();
                self.emit(Instruction::MakeTypeRep {
                    dst,
                    base: TypeRepLit::Tuple(items.len()),
                    args,
                });
                Operand::Local(dst)
            }
            Ty::Fn { params, ret } => {
                let mut args = Vec::with_capacity(params.len() + 1);
                for p in params {
                    args.push(self.lower_type_rep_for_ty(p, span)?);
                }
                args.push(self.lower_type_rep_for_ty(ret, span)?);
                let dst = self.alloc_local();
                self.emit(Instruction::MakeTypeRep {
                    dst,
                    base: TypeRepLit::Fn,
                    args,
                });
                Operand::Local(dst)
            }
            Ty::Cont { param, ret } => {
                let args = vec![
                    self.lower_type_rep_for_ty(param, span)?,
                    self.lower_type_rep_for_ty(ret, span)?,
                ];
                let dst = self.alloc_local();
                self.emit(Instruction::MakeTypeRep {
                    dst,
                    base: TypeRepLit::Cont,
                    args,
                });
                Operand::Local(dst)
            }
            Ty::App(typeck::TyCon::Named(name), args) => {
                let base = if self.compiler.env.structs.contains_key(name) {
                    TypeRepLit::Struct(name.clone())
                } else if self.compiler.env.enums.contains_key(name) {
                    TypeRepLit::Enum(name.clone())
                } else if self.compiler.env.interfaces.contains_key(name) {
                    TypeRepLit::Interface(name.clone())
                } else {
                    return Err(CompileError::new(
                        format!("internal error: unknown nominal type `{name}`"),
                        span,
                    ));
                };

                if args.is_empty() {
                    Operand::Literal(ConstValue::TypeRep(base))
                } else {
                    let mut arg_reps = Vec::with_capacity(args.len());
                    for a in args {
                        arg_reps.push(self.lower_type_rep_for_ty(a, span)?);
                    }
                    let dst = self.alloc_local();
                    self.emit(Instruction::MakeTypeRep {
                        dst,
                        base,
                        args: arg_reps,
                    });
                    Operand::Local(dst)
                }
            }
            Ty::App(typeck::TyCon::Gen(_) | typeck::TyCon::Var(_), _) => {
                return Err(CompileError::new(
                    "cannot reify higher-kinded types as `TypeRep` in this stage",
                    span,
                ));
            }
            Ty::Gen(id) => {
                let Some(Some(local)) = self.generic_type_reps.get(*id) else {
                    return Err(CompileError::new(
                        "internal error: missing runtime `TypeRep` for generic parameter",
                        span,
                    ));
                };
                Operand::Local(*local)
            }
            Ty::Var(_) => {
                return Err(CompileError::new(
                    "cannot infer type required for runtime type argument reification; add a type annotation",
                    span,
                ));
            }
        };
        Ok(rep)
    }

    fn lower_type_rep_for_type_expr(
        &mut self,
        ty: &crate::ast::TypeExpr,
    ) -> Result<Operand, CompileError> {
        use crate::ast::TypeExpr;
        match ty {
            TypeExpr::Readonly { inner, .. } => self.lower_type_rep_for_type_expr(inner),
            TypeExpr::Prim { prim, span: _ } => {
                Ok(Operand::Literal(ConstValue::TypeRep(match prim {
                    crate::ast::PrimType::Unit => TypeRepLit::Unit,
                    crate::ast::PrimType::Bool => TypeRepLit::Bool,
                    crate::ast::PrimType::Int => TypeRepLit::Int,
                    crate::ast::PrimType::Float => TypeRepLit::Float,
                    crate::ast::PrimType::String => TypeRepLit::String,
                    crate::ast::PrimType::Bytes => TypeRepLit::Bytes,
                })))
            }
            TypeExpr::Array { elem, span } => {
                let elem_rep = self.lower_type_rep_for_type_expr(elem)?;
                let dst = self.alloc_local();
                self.emit(Instruction::MakeTypeRep {
                    dst,
                    base: TypeRepLit::Array,
                    args: vec![elem_rep],
                });
                let _ = span;
                Ok(Operand::Local(dst))
            }
            TypeExpr::Tuple { items, span } => {
                if items.is_empty() {
                    return Ok(Operand::Literal(ConstValue::TypeRep(TypeRepLit::Unit)));
                }
                let mut args = Vec::with_capacity(items.len());
                for item in items {
                    args.push(self.lower_type_rep_for_type_expr(item)?);
                }
                let dst = self.alloc_local();
                self.emit(Instruction::MakeTypeRep {
                    dst,
                    base: TypeRepLit::Tuple(items.len()),
                    args,
                });
                let _ = span;
                Ok(Operand::Local(dst))
            }
            TypeExpr::Fn { params, ret, span } => {
                let mut args = Vec::with_capacity(params.len() + 1);
                for p in params {
                    args.push(self.lower_type_rep_for_type_expr(p)?);
                }
                args.push(self.lower_type_rep_for_type_expr(ret)?);
                let dst = self.alloc_local();
                self.emit(Instruction::MakeTypeRep {
                    dst,
                    base: TypeRepLit::Fn,
                    args,
                });
                let _ = span;
                Ok(Operand::Local(dst))
            }
            TypeExpr::Cont { param, ret, span } => {
                let args = vec![
                    self.lower_type_rep_for_type_expr(param)?,
                    self.lower_type_rep_for_type_expr(ret)?,
                ];
                let dst = self.alloc_local();
                self.emit(Instruction::MakeTypeRep {
                    dst,
                    base: TypeRepLit::Cont,
                    args,
                });
                let _ = span;
                Ok(Operand::Local(dst))
            }
            TypeExpr::Path(path) => self.lower_type_rep_for_path_type(path),
        }
    }

    fn lower_type_rep_for_path_type(
        &mut self,
        path: &crate::ast::PathType,
    ) -> Result<Operand, CompileError> {
        if path.segments.is_empty() {
            return Err(CompileError::new(
                "internal error: empty type path",
                path.span,
            ));
        }

        // Generic type parameter `T` in a type position: reify via the hidden runtime `TypeRep` value.
        if path.segments.len() == 1 && path.segments[0].args.is_empty() {
            let name = path.segments[0].name.name.as_str();
            if let Some((idx, _gp)) = self
                .generics
                .iter()
                .enumerate()
                .find(|(_idx, g)| g.arity == 0 && g.name.as_str() == name)
            {
                let Some(Some(local)) = self.generic_type_reps.get(idx) else {
                    return Err(CompileError::new(
                        "internal error: missing runtime `TypeRep` for generic parameter",
                        path.span,
                    ));
                };
                return Ok(Operand::Local(*local));
            }
        }

        let segments: Vec<String> = path
            .segments
            .iter()
            .map(|seg| seg.name.name.clone())
            .collect();
        let (kind, fqn) = self
            .compiler
            .env
            .modules
            .resolve_type_fqn(&self.module, &segments, path.span)
            .map_err(|e| CompileError::new(e.message, e.span))?;

        let base = match kind {
            DefKind::Struct => TypeRepLit::Struct(fqn),
            DefKind::Enum => TypeRepLit::Enum(fqn),
            DefKind::Interface => TypeRepLit::Interface(fqn),
        };

        let arg_count: usize = path.segments.iter().map(|seg| seg.args.len()).sum();
        if arg_count == 0 {
            return Ok(Operand::Literal(ConstValue::TypeRep(base)));
        }

        let mut arg_reps = Vec::with_capacity(arg_count);
        for seg in &path.segments {
            for arg in &seg.args {
                arg_reps.push(self.lower_type_rep_for_type_expr(arg)?);
            }
        }
        let dst = self.alloc_local();
        self.emit(Instruction::MakeTypeRep {
            dst,
            base,
            args: arg_reps,
        });
        Ok(Operand::Local(dst))
    }

    fn lower_expr(&mut self, expr: &Expr) -> Result<Local, CompileError> {
        if self.is_current_terminated() {
            return Ok(self.alloc_unit());
        }

        match expr {
            Expr::Unit { .. } => Ok(self.alloc_unit()),
            Expr::Bool { value, .. } => Ok(self.alloc_bool(*value)),
            Expr::Int { value, .. } => Ok(self.alloc_int(*value)),
            Expr::Float { value, .. } => {
                let dst = self.alloc_local();
                self.emit(Instruction::Const {
                    dst,
                    value: ConstValue::Float(*value),
                });
                Ok(dst)
            }
            Expr::String { value, .. } => {
                let dst = self.alloc_local();
                self.emit(Instruction::Const {
                    dst,
                    value: ConstValue::String(value.clone()),
                });
                Ok(dst)
            }
            Expr::Bytes { value, .. } => {
                let dst = self.alloc_local();
                self.emit(Instruction::Const {
                    dst,
                    value: ConstValue::Bytes(value.clone()),
                });
                Ok(dst)
            }

            Expr::Path { path, span } => self.lower_path_expr(path, *span),
            Expr::Array { items, .. } => {
                let mut ops = Vec::with_capacity(items.len());
                for item in items {
                    let v = self.lower_expr(item)?;
                    ops.push(Operand::Local(v));
                }
                let dst = self.alloc_local();
                self.emit(Instruction::MakeArray { dst, items: ops });
                Ok(dst)
            }
            Expr::Tuple { items, .. } => {
                let mut ops = Vec::with_capacity(items.len());
                for item in items {
                    let v = self.lower_expr(item)?;
                    ops.push(Operand::Local(v));
                }
                let dst = self.alloc_local();
                self.emit(Instruction::MakeTuple { dst, items: ops });
                Ok(dst)
            }
            Expr::StructLit {
                type_path, fields, ..
            } => {
                let mut mir_fields = Vec::with_capacity(fields.len());
                for (name, value_expr) in fields {
                    let v = self.lower_expr(value_expr)?;
                    mir_fields.push((name.name.clone(), Operand::Local(v)));
                }
                let dst = self.alloc_local();
                let segments: Vec<String> =
                    type_path.segments.iter().map(|s| s.name.clone()).collect();
                let (kind, type_name) = self
                    .compiler
                    .env
                    .modules
                    .resolve_type_fqn(&self.module, &segments, type_path.span)
                    .map_err(|e| CompileError::new(e.message, e.span))?;
                if kind != DefKind::Struct {
                    return Err(CompileError::new(
                        format!(
                            "internal error: expected a struct type for struct literal, got `{type_name}`"
                        ),
                        type_path.span,
                    ));
                }

                let type_args = match self.expr_ty(expr) {
                    Some(ty) => match self.strip_readonly_ty(ty) {
                        Ty::App(typeck::TyCon::Named(_name), args) => args.clone(),
                        _ => Vec::new(),
                    },
                    None => Vec::new(),
                };
                let mut type_arg_reps = Vec::with_capacity(type_args.len());
                for arg in &type_args {
                    type_arg_reps.push(self.lower_type_rep_for_ty(arg, type_path.span)?);
                }
                self.emit(Instruction::MakeStruct {
                    dst,
                    type_name,
                    type_args: type_arg_reps,
                    fields: mir_fields,
                });
                Ok(dst)
            }
            Expr::EffectCall {
                interface,
                method,
                args,
                span: call_span,
                ..
            } => {
                let mut mir_args = Vec::with_capacity(args.len());
                for a in args {
                    let v = self.lower_expr(a)?;
                    mir_args.push(Operand::Local(v));
                }
                let dst = self.alloc_local();
                let segments: Vec<String> = interface
                    .segments
                    .iter()
                    .map(|s| s.name.name.clone())
                    .collect();
                let (kind, interface_name) = self
                    .compiler
                    .env
                    .modules
                    .resolve_type_fqn(&self.module, &segments, interface.span)
                    .map_err(|e| CompileError::new(e.message, e.span))?;
                if kind != DefKind::Interface {
                    return Err(CompileError::new(
                        format!(
                            "internal error: expected an interface for effect call, got `{interface_name}`"
                        ),
                        interface.span,
                    ));
                }
                let Some(iface_def) = self.compiler.env.interfaces.get(&interface_name) else {
                    return Err(CompileError::new(
                        format!("internal error: unknown interface `{interface_name}`"),
                        interface.span,
                    ));
                };
                let origin = {
                    let Some(method_info) = iface_def.all_methods.get(&method.name) else {
                        return Err(CompileError::new(
                            format!(
                                "internal error: unknown effect/method `{}.{}`",
                                interface_name, method.name
                            ),
                            method.span,
                        ));
                    };
                    method_info.origin.clone()
                };
                let effect_id = format!("{origin}::{}", method.name);
                let interface_arg_tys = self
                    .compiler
                    .types
                    .effect_interface_args
                    .get(&(*call_span, effect_id.clone()))
                    .cloned()
                    .ok_or_else(|| {
                        CompileError::new(
                            format!(
                                "internal error: missing interface type args for effect `{effect_id}`"
                            ),
                            *call_span,
                        )
                    })?;
                let mut interface_arg_reps = Vec::with_capacity(interface_arg_tys.len());
                for ty in &interface_arg_tys {
                    interface_arg_reps.push(self.lower_type_rep_for_ty(ty, *call_span)?);
                }
                self.emit(Instruction::Perform {
                    dst: Some(dst),
                    effect: EffectSpec {
                        interface: origin,
                        interface_args: interface_arg_reps,
                        method: method.name.clone(),
                    },
                    args: mir_args,
                });
                Ok(dst)
            }

            Expr::Lambda { params, body, span } => self.lower_lambda_expr(params, body, *span),

            Expr::If {
                cond,
                then_block,
                else_branch,
                span,
            } => self.lower_if_expr(cond, then_block, else_branch.as_deref(), *span),

            Expr::Match {
                scrutinee,
                arms,
                span,
            } => self.lower_match_expr(scrutinee, arms, *span),

            Expr::Loop { body, span } => self.lower_loop_expr(body, *span),
            Expr::While { cond, body, span } => self.lower_while_expr(cond, body, *span),
            Expr::For {
                binding,
                iter,
                body,
                span,
            } => self.lower_for_expr(binding, iter, body, *span),

            Expr::Block { block, .. } => self.lower_block_expr(block),

            Expr::Call {
                callee,
                type_args: _,
                args,
                span,
            } => self.lower_call_expr(callee, args, *span),
            Expr::Field { base, name, .. } => self.lower_field_expr(base, name),
            Expr::Index { base, index, .. } => self.lower_index_expr(base, index),
            Expr::Unary { op, expr, span } => self.lower_unary_expr(*op, expr, *span),
            Expr::Binary {
                op,
                left,
                right,
                span,
            } => self.lower_binary_expr(*op, left, right, *span),
            Expr::Assign {
                target,
                value,
                span,
            } => self.lower_assign_expr(target, value, *span),
            Expr::As { expr, .. } => self.lower_expr(expr),
            Expr::AsQuestion { expr, ty, span } => self.lower_checked_cast_expr(expr, ty, *span),
            Expr::Is { expr, ty, span } => self.lower_is_expr(expr, ty, *span),
        }
    }

    fn lower_path_expr(
        &mut self,
        path: &crate::ast::Path,
        span: Span,
    ) -> Result<Local, CompileError> {
        // Local bindings.
        if path.segments.len() == 1 {
            let name = &path.segments[0].name;
            if let Some(var) = self.lookup_var(name) {
                match var.kind {
                    BindingKind::Let => match var.storage {
                        VarStorage::Cell(cell) => return self.read_cell(cell, name, span),
                        VarStorage::Local(local) => {
                            let dst = self.alloc_local();
                            self.emit(Instruction::Copy { dst, src: local });
                            return Ok(dst);
                        }
                    },
                    BindingKind::Const | BindingKind::Readonly => {
                        let local = var.storage.local();
                        let dst = self.alloc_local();
                        self.emit(Instruction::Copy { dst, src: local });
                        return Ok(dst);
                    }
                }
            }
        }

        let segments: Vec<String> = path.segments.iter().map(|s| s.name.clone()).collect();

        // Bare enum variant value: `Enum::Variant` where `Variant` has zero fields.
        //
        // This is sugar for `Enum::Variant()`. The typechecker ensures it is only used for
        // zero-field variants, but we also validate here for better errors and robustness.
        if segments.len() >= 2 {
            let prefix = &segments[..segments.len() - 1];
            let last = segments.last().expect("len >= 2");
            let last_ident = path.segments.last().expect("len >= 2");
            if let Some((kind, type_fqn)) = self
                .compiler
                .env
                .modules
                .try_resolve_type_fqn(&self.module, prefix, span)
                .map_err(|e| CompileError::new(e.message, e.span))?
                && kind == DefKind::Enum
                && let Some(def) = self.compiler.env.enums.get(&type_fqn)
                && def
                    .variants
                    .get(last)
                    .is_some_and(|variant_fields| variant_fields.is_empty())
            {
                let dst = self.alloc_local();
                let type_args = match self.compiler.types.expr_types.get(&span) {
                    Some(ty) => match self.strip_readonly_ty(ty) {
                        Ty::App(typeck::TyCon::Named(_name), args) => args.clone(),
                        _ => Vec::new(),
                    },
                    None => Vec::new(),
                };
                let mut type_arg_reps = Vec::with_capacity(type_args.len());
                for arg in &type_args {
                    type_arg_reps.push(self.lower_type_rep_for_ty(arg, span)?);
                }
                self.emit(Instruction::MakeEnum {
                    dst,
                    enum_name: type_fqn,
                    type_args: type_arg_reps,
                    variant: last_ident.name.clone(),
                    fields: Vec::new(),
                });
                return Ok(dst);
            }
        }

        // Function item used as a value.
        let mut func_name: Option<String> = None;
        if segments.len() >= 2 {
            let prefix = &segments[..segments.len() - 1];
            let last = segments.last().expect("len >= 2");
            let prefix_ty = self
                .compiler
                .env
                .modules
                .try_resolve_type_fqn(&self.module, prefix, span)
                .map_err(|e| CompileError::new(e.message, e.span))?;
            if let Some((kind, type_fqn)) = prefix_ty
                && kind != DefKind::Interface
            {
                let candidate = format!("{type_fqn}::{last}");
                if self.compiler.env.functions.contains_key(&candidate) {
                    func_name = Some(candidate);
                }
            }
        }

        let name = match func_name {
            Some(name) => name,
            None => self
                .compiler
                .env
                .modules
                .resolve_value_fqn(&self.module, &segments, span)
                .map_err(|e| CompileError::new(e.message, e.span))?,
        };
        let wrapper = {
            let compiler = &mut *self.compiler;
            compiler.ensure_fn_value_wrapper(&name)?
        };
        let env = self.alloc_local();
        self.emit(Instruction::MakeArray {
            dst: env,
            items: Vec::new(),
        });
        let closure = self.alloc_local();
        self.emit(Instruction::MakeStruct {
            dst: closure,
            type_name: INTERNAL_CLOSURE_STRUCT.to_string(),
            type_args: Vec::new(),
            fields: vec![
                (
                    CLOSURE_FIELD_FUNC.to_string(),
                    Operand::Literal(ConstValue::Function(wrapper)),
                ),
                (CLOSURE_FIELD_ENV.to_string(), Operand::Local(env)),
            ],
        });
        Ok(closure)
    }

    fn lower_lambda_expr(
        &mut self,
        params: &[crate::ast::LambdaParam],
        body: &Block,
        span: Span,
    ) -> Result<Local, CompileError> {
        let visible = self.visible_bindings();
        let mut lambda_bound = BTreeSet::new();
        for p in params {
            lambda_bound.insert(p.name.name.clone());
        }
        let free_vars = free_value_vars_in_block(body, &mut lambda_bound);
        let captures = visible
            .iter()
            .filter_map(|(name, info)| {
                if parse_type_rep_capture_name(name).is_some() || free_vars.contains(name) {
                    Some((name.clone(), info.clone()))
                } else {
                    None
                }
            })
            .collect::<BTreeMap<_, _>>();
        let entry_name = self.compiler.fresh_internal_name("lambda");

        {
            let compiler = &mut *self.compiler;
            compiler.compile_lambda_entry(
                &self.module,
                entry_name.clone(),
                self.generics.clone(),
                &captures,
                params,
                body,
                span,
            )?;
        }

        // Capture values into an env array (name order).
        let mut captured_ops = Vec::with_capacity(captures.len());
        for (_name, info) in captures.iter() {
            captured_ops.push(Operand::Local(info.storage.local()));
        }
        let env = self.alloc_local();
        self.emit(Instruction::MakeArray {
            dst: env,
            items: captured_ops,
        });

        let closure = self.alloc_local();
        self.emit(Instruction::MakeStruct {
            dst: closure,
            type_name: INTERNAL_CLOSURE_STRUCT.to_string(),
            type_args: Vec::new(),
            fields: vec![
                (
                    CLOSURE_FIELD_FUNC.to_string(),
                    Operand::Literal(ConstValue::Function(entry_name)),
                ),
                (CLOSURE_FIELD_ENV.to_string(), Operand::Local(env)),
            ],
        });
        Ok(closure)
    }

    fn lower_if_expr(
        &mut self,
        cond: &Expr,
        then_block: &Block,
        else_expr: Option<&Expr>,
        span: Span,
    ) -> Result<Local, CompileError> {
        let cond_local = self.lower_expr(cond)?;
        let then_block_id = self.new_block("if_then");
        let else_block_id = self.new_block("if_else");
        let join_block_id = self.new_block("if_join");
        let join_param = self.alloc_local();
        self.blocks[join_block_id.0].params = vec![join_param];

        self.set_terminator(Terminator::CondBr {
            cond: Operand::Local(cond_local),
            then_target: then_block_id,
            then_args: Vec::new(),
            else_target: else_block_id,
            else_args: Vec::new(),
        })?;

        self.set_current(then_block_id);
        let then_val = self.lower_block_expr(then_block)?;
        if !self.is_current_terminated() {
            self.set_terminator(Terminator::Br {
                target: join_block_id,
                args: vec![Operand::Local(then_val)],
            })?;
        }

        self.set_current(else_block_id);
        let else_val = if let Some(e) = else_expr {
            self.lower_expr(e)?
        } else {
            self.alloc_unit()
        };
        if !self.is_current_terminated() {
            self.set_terminator(Terminator::Br {
                target: join_block_id,
                args: vec![Operand::Local(else_val)],
            })?;
        }

        self.set_current(join_block_id);
        let _ = span;
        Ok(join_param)
    }

    fn lower_loop_expr(&mut self, body: &Block, span: Span) -> Result<Local, CompileError> {
        let loop_block = self.new_block("loop_body");
        let after_block = self.new_block("loop_after");

        self.set_terminator(Terminator::Br {
            target: loop_block,
            args: Vec::new(),
        })?;

        self.loop_stack.push(LoopTargets {
            continue_block: loop_block,
            break_block: after_block,
        });

        self.set_current(loop_block);
        self.lower_block_stmt(body)?;
        if !self.is_current_terminated() {
            self.set_terminator(Terminator::Br {
                target: loop_block,
                args: Vec::new(),
            })?;
        }

        self.loop_stack.pop();
        self.set_current(after_block);
        let _ = span;
        Ok(self.alloc_unit())
    }

    fn lower_while_expr(
        &mut self,
        cond: &Expr,
        body: &Block,
        span: Span,
    ) -> Result<Local, CompileError> {
        let cond_block = self.new_block("while_cond");
        let body_block = self.new_block("while_body");
        let after_block = self.new_block("while_after");

        self.set_terminator(Terminator::Br {
            target: cond_block,
            args: Vec::new(),
        })?;

        self.loop_stack.push(LoopTargets {
            continue_block: cond_block,
            break_block: after_block,
        });

        self.set_current(cond_block);
        let cond_val = self.lower_expr(cond)?;
        self.set_terminator(Terminator::CondBr {
            cond: Operand::Local(cond_val),
            then_target: body_block,
            then_args: Vec::new(),
            else_target: after_block,
            else_args: Vec::new(),
        })?;

        self.set_current(body_block);
        self.lower_block_stmt(body)?;
        if !self.is_current_terminated() {
            self.set_terminator(Terminator::Br {
                target: cond_block,
                args: Vec::new(),
            })?;
        }

        self.loop_stack.pop();
        self.set_current(after_block);
        let _ = span;
        Ok(self.alloc_unit())
    }

    fn lower_for_expr(
        &mut self,
        binding: &crate::ast::Ident,
        iter: &Expr,
        body: &Block,
        span: Span,
    ) -> Result<Local, CompileError> {
        // Desugar:
        //   let it = core::intrinsics::into_iter(iter);
        //   loop {
        //     match core::intrinsics::next(it) {
        //       Option::Some(x) => { body; }
        //       Option::None => break;
        //     }
        //   }
        let iter_val = self.lower_expr(iter)?;
        let iter_ty = self.expr_ty(iter).ok_or_else(|| {
            CompileError::new(
                "internal error: missing type for `for` iterator expression",
                iter.span(),
            )
        })?;
        let elem_ty = match self.strip_readonly_ty(iter_ty) {
            Ty::Array(elem) => elem.as_ref().clone(),
            other => {
                return Err(CompileError::new(
                    format!("internal error: `for` expects an array iterator, got `{other}`"),
                    iter.span(),
                ));
            }
        };
        let elem_rep = self.lower_type_rep_for_ty(&elem_ty, iter.span())?;

        let it_local = self.alloc_local();
        self.emit(Instruction::Call {
            dst: Some(it_local),
            func: "core::intrinsics::into_iter".to_string(),
            args: vec![elem_rep.clone(), Operand::Local(iter_val)],
        });

        let loop_head = self.new_block("for_head");
        let loop_body = self.new_block("for_body");
        let loop_none = self.new_block("for_none");
        let after_block = self.new_block("for_after");

        self.set_terminator(Terminator::Br {
            target: loop_head,
            args: Vec::new(),
        })?;

        self.loop_stack.push(LoopTargets {
            continue_block: loop_head,
            break_block: after_block,
        });

        self.set_current(loop_head);
        let next_local = self.alloc_local();
        self.emit(Instruction::Call {
            dst: Some(next_local),
            func: "core::intrinsics::next".to_string(),
            args: vec![elem_rep, Operand::Local(it_local)],
        });

        // `Option::Some(x)` binds one value, `Option::None` binds none.
        let some_param = self.alloc_local();
        self.blocks[loop_body.0].params = vec![some_param];

        self.set_terminator(Terminator::Switch {
            value: Operand::Local(next_local),
            cases: vec![
                SwitchCase {
                    pattern: Pattern::Enum {
                        enum_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Pattern::Bind],
                    },
                    target: loop_body,
                },
                SwitchCase {
                    pattern: Pattern::Enum {
                        enum_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    },
                    target: loop_none,
                },
            ],
            default: loop_none,
        })?;

        self.set_current(loop_body);
        self.push_scope();
        self.bind_var(
            &binding.name,
            VarInfo {
                storage: VarStorage::Local(some_param),
                kind: BindingKind::Const,
            },
        );
        self.lower_block_stmt(body)?;
        self.pop_scope();
        if !self.is_current_terminated() {
            self.set_terminator(Terminator::Br {
                target: loop_head,
                args: Vec::new(),
            })?;
        }

        self.set_current(loop_none);
        if !self.is_current_terminated() {
            self.set_terminator(Terminator::Br {
                target: after_block,
                args: Vec::new(),
            })?;
        }

        self.loop_stack.pop();
        self.set_current(after_block);
        let _ = span;
        Ok(self.alloc_unit())
    }

    fn lower_field_expr(
        &mut self,
        base: &Expr,
        name: &crate::ast::FieldName,
    ) -> Result<Local, CompileError> {
        let base_local = self.lower_expr(base)?;
        let dst = self.alloc_local();

        match name {
            crate::ast::FieldName::Named(name) => {
                if let Some(idx) = self.resolve_struct_field_index(base, name.name.as_str()) {
                    self.emit(Instruction::StructGet {
                        dst,
                        obj: Operand::Local(base_local),
                        idx,
                    });
                } else {
                    self.emit(Instruction::GetField {
                        dst,
                        obj: Operand::Local(base_local),
                        field: name.name.clone(),
                    });
                }
            }
            crate::ast::FieldName::Index { index, .. } => {
                let is_tuple = self
                    .expr_ty(base)
                    .is_some_and(|ty| matches!(self.strip_readonly_ty(ty), Ty::Tuple(_)));
                if is_tuple {
                    self.emit(Instruction::TupleGet {
                        dst,
                        tup: Operand::Local(base_local),
                        idx: *index,
                    });
                } else {
                    let field = format!(".{index}");
                    if let Some(idx) = self.resolve_struct_field_index(base, field.as_str()) {
                        self.emit(Instruction::StructGet {
                            dst,
                            obj: Operand::Local(base_local),
                            idx,
                        });
                    } else {
                        self.emit(Instruction::GetField {
                            dst,
                            obj: Operand::Local(base_local),
                            field,
                        });
                    }
                }
            }
        }

        if self.expr_is_readonly(base) {
            self.emit(Instruction::AsReadonly { dst, src: dst });
        }
        Ok(dst)
    }

    fn lower_index_expr(&mut self, base: &Expr, index: &Expr) -> Result<Local, CompileError> {
        let base_local = self.lower_expr(base)?;
        let idx_local = self.lower_expr(index)?;
        let dst = self.alloc_local();
        self.emit(Instruction::IndexGet {
            dst,
            arr: Operand::Local(base_local),
            idx: Operand::Local(idx_local),
        });
        if self.expr_is_readonly(base) {
            self.emit(Instruction::AsReadonly { dst, src: dst });
        }
        Ok(dst)
    }

    fn lower_unary_expr(
        &mut self,
        op: UnaryOp,
        expr: &Expr,
        span: Span,
    ) -> Result<Local, CompileError> {
        match op {
            UnaryOp::Not => {
                let v = self.lower_expr(expr)?;
                let dst = self.alloc_local();
                self.emit(Instruction::BoolNot {
                    dst,
                    v: Operand::Local(v),
                });
                Ok(dst)
            }
            UnaryOp::Neg => {
                let ty = self
                    .expr_ty(expr)
                    .ok_or_else(|| {
                        CompileError::new("internal error: missing type for unary expression", span)
                    })?
                    .clone();
                let v = self.lower_expr(expr)?;
                match ty {
                    Ty::Int => {
                        let zero = self.alloc_int(0);
                        let dst = self.alloc_local();
                        self.emit(Instruction::IntSub {
                            dst,
                            a: Operand::Local(zero),
                            b: Operand::Local(v),
                        });
                        Ok(dst)
                    }
                    Ty::Float => {
                        let zero = self.alloc_local();
                        self.emit(Instruction::Const {
                            dst: zero,
                            value: ConstValue::Float(0.0),
                        });
                        self.lower_named_call(
                            "core::intrinsics::float_sub",
                            vec![Operand::Local(zero), Operand::Local(v)],
                        )
                    }
                    other => Err(CompileError::new(
                        format!("internal error: unary - not supported for {other}"),
                        span,
                    )),
                }
            }
        }
    }

    fn lower_binary_expr(
        &mut self,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
        span: Span,
    ) -> Result<Local, CompileError> {
        match op {
            BinaryOp::And => {
                // a && b  ==> if a { b } else { false }
                let cond_local = self.lower_expr(left)?;
                let then_block = self.new_block("and_then");
                let else_block = self.new_block("and_else");
                let join_block = self.new_block("and_join");
                let join_param = self.alloc_local();
                self.blocks[join_block.0].params = vec![join_param];

                self.set_terminator(Terminator::CondBr {
                    cond: Operand::Local(cond_local),
                    then_target: then_block,
                    then_args: Vec::new(),
                    else_target: else_block,
                    else_args: Vec::new(),
                })?;

                self.set_current(then_block);
                let b_local = self.lower_expr(right)?;
                if !self.is_current_terminated() {
                    self.set_terminator(Terminator::Br {
                        target: join_block,
                        args: vec![Operand::Local(b_local)],
                    })?;
                }

                self.set_current(else_block);
                let f = self.alloc_bool(false);
                if !self.is_current_terminated() {
                    self.set_terminator(Terminator::Br {
                        target: join_block,
                        args: vec![Operand::Local(f)],
                    })?;
                }

                self.set_current(join_block);
                Ok(join_param)
            }
            BinaryOp::Or => {
                // a || b  ==> if a { true } else { b }
                let cond_local = self.lower_expr(left)?;
                let then_block = self.new_block("or_then");
                let else_block = self.new_block("or_else");
                let join_block = self.new_block("or_join");
                let join_param = self.alloc_local();
                self.blocks[join_block.0].params = vec![join_param];

                self.set_terminator(Terminator::CondBr {
                    cond: Operand::Local(cond_local),
                    then_target: then_block,
                    then_args: Vec::new(),
                    else_target: else_block,
                    else_args: Vec::new(),
                })?;

                self.set_current(then_block);
                let t = self.alloc_bool(true);
                if !self.is_current_terminated() {
                    self.set_terminator(Terminator::Br {
                        target: join_block,
                        args: vec![Operand::Local(t)],
                    })?;
                }

                self.set_current(else_block);
                let b_local = self.lower_expr(right)?;
                if !self.is_current_terminated() {
                    self.set_terminator(Terminator::Br {
                        target: join_block,
                        args: vec![Operand::Local(b_local)],
                    })?;
                }

                self.set_current(join_block);
                Ok(join_param)
            }
            _ => {
                let ty = self
                    .expr_ty(left)
                    .ok_or_else(|| {
                        CompileError::new("internal error: missing type for binary operator", span)
                    })?
                    .clone();
                let l = self.lower_expr(left)?;
                let r = self.lower_expr(right)?;
                let operand_ty = self.strip_readonly_ty(&ty);
                match (op, operand_ty) {
                    (BinaryOp::Add, Ty::Int) => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::IntAdd {
                            dst,
                            a: Operand::Local(l),
                            b: Operand::Local(r),
                        });
                        Ok(dst)
                    }
                    (BinaryOp::Sub, Ty::Int) => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::IntSub {
                            dst,
                            a: Operand::Local(l),
                            b: Operand::Local(r),
                        });
                        Ok(dst)
                    }
                    (BinaryOp::Mul, Ty::Int) => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::IntMul {
                            dst,
                            a: Operand::Local(l),
                            b: Operand::Local(r),
                        });
                        Ok(dst)
                    }
                    (BinaryOp::Div, Ty::Int) => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::IntDiv {
                            dst,
                            a: Operand::Local(l),
                            b: Operand::Local(r),
                        });
                        Ok(dst)
                    }
                    (BinaryOp::Mod, Ty::Int) => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::IntMod {
                            dst,
                            a: Operand::Local(l),
                            b: Operand::Local(r),
                        });
                        Ok(dst)
                    }
                    (BinaryOp::Eq, Ty::Int) => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::IntEq {
                            dst,
                            a: Operand::Local(l),
                            b: Operand::Local(r),
                        });
                        Ok(dst)
                    }
                    (BinaryOp::Ne, Ty::Int) => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::IntNe {
                            dst,
                            a: Operand::Local(l),
                            b: Operand::Local(r),
                        });
                        Ok(dst)
                    }
                    (BinaryOp::Lt, Ty::Int) => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::IntLt {
                            dst,
                            a: Operand::Local(l),
                            b: Operand::Local(r),
                        });
                        Ok(dst)
                    }
                    (BinaryOp::Le, Ty::Int) => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::IntLe {
                            dst,
                            a: Operand::Local(l),
                            b: Operand::Local(r),
                        });
                        Ok(dst)
                    }
                    (BinaryOp::Gt, Ty::Int) => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::IntGt {
                            dst,
                            a: Operand::Local(l),
                            b: Operand::Local(r),
                        });
                        Ok(dst)
                    }
                    (BinaryOp::Ge, Ty::Int) => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::IntGe {
                            dst,
                            a: Operand::Local(l),
                            b: Operand::Local(r),
                        });
                        Ok(dst)
                    }
                    (BinaryOp::Eq, Ty::Bool) => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::BoolEq {
                            dst,
                            a: Operand::Local(l),
                            b: Operand::Local(r),
                        });
                        Ok(dst)
                    }
                    (BinaryOp::Ne, Ty::Bool) => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::BoolNe {
                            dst,
                            a: Operand::Local(l),
                            b: Operand::Local(r),
                        });
                        Ok(dst)
                    }
                    _ => {
                        let func = select_binop_fn(op, operand_ty).ok_or_else(|| {
                            CompileError::new(
                                format!(
                                    "internal error: unsupported binary op `{op:?}` for `{ty}`"
                                ),
                                span,
                            )
                        })?;
                        self.lower_named_call(func, vec![Operand::Local(l), Operand::Local(r)])
                    }
                }
            }
        }
    }

    fn lower_assign_expr(
        &mut self,
        target: &Expr,
        value: &Expr,
        span: Span,
    ) -> Result<Local, CompileError> {
        match target {
            Expr::Path { path, .. } => {
                if path.segments.len() != 1 {
                    return Err(CompileError::new(
                        "assignment target must be a local name",
                        span,
                    ));
                }
                let name = &path.segments[0].name;
                let Some(var) = self.lookup_var(name).cloned() else {
                    return Err(CompileError::new(
                        format!("unknown name `{name}`"),
                        path.segments[0].span,
                    ));
                };
                if var.kind != BindingKind::Let {
                    return Err(CompileError::new(
                        format!("cannot assign to `{name}` (not a `let` binding)"),
                        path.segments[0].span,
                    ));
                }
                let rhs = self.lower_expr(value)?;
                match var.storage {
                    VarStorage::Cell(cell) => self.write_cell(cell, rhs),
                    VarStorage::Local(local) => {
                        self.emit(Instruction::Copy {
                            dst: local,
                            src: rhs,
                        });
                    }
                }
                Ok(self.alloc_unit())
            }
            Expr::Field { base, name, .. } => {
                let obj = self.lower_expr(base)?;
                let rhs = self.lower_expr(value)?;
                match name {
                    crate::ast::FieldName::Named(name) => {
                        if let Some(idx) = self.resolve_struct_field_index(base, name.name.as_str())
                        {
                            self.emit(Instruction::StructSet {
                                obj: Operand::Local(obj),
                                idx,
                                value: Operand::Local(rhs),
                            });
                        } else {
                            self.emit(Instruction::SetField {
                                obj: Operand::Local(obj),
                                field: name.name.clone(),
                                value: Operand::Local(rhs),
                            });
                        }
                    }
                    crate::ast::FieldName::Index { index, .. } => {
                        let is_tuple = self
                            .expr_ty(base)
                            .is_some_and(|ty| matches!(self.strip_readonly_ty(ty), Ty::Tuple(_)));
                        if is_tuple {
                            self.emit(Instruction::TupleSet {
                                tup: Operand::Local(obj),
                                idx: *index,
                                value: Operand::Local(rhs),
                            });
                        } else {
                            let field = format!(".{index}");
                            if let Some(idx) = self.resolve_struct_field_index(base, field.as_str())
                            {
                                self.emit(Instruction::StructSet {
                                    obj: Operand::Local(obj),
                                    idx,
                                    value: Operand::Local(rhs),
                                });
                            } else {
                                self.emit(Instruction::SetField {
                                    obj: Operand::Local(obj),
                                    field,
                                    value: Operand::Local(rhs),
                                });
                            }
                        }
                    }
                }
                Ok(self.alloc_unit())
            }
            Expr::Index { base, index, .. } => {
                let arr = self.lower_expr(base)?;
                let idx = self.lower_expr(index)?;
                let rhs = self.lower_expr(value)?;
                self.emit(Instruction::IndexSet {
                    arr: Operand::Local(arr),
                    idx: Operand::Local(idx),
                    value: Operand::Local(rhs),
                });
                Ok(self.alloc_unit())
            }
            _ => Err(CompileError::new("invalid assignment target", span)),
        }
    }

    fn lower_is_expr(
        &mut self,
        expr: &Expr,
        ty: &crate::ast::TypeExpr,
        span: Span,
    ) -> Result<Local, CompileError> {
        let value = self.lower_expr(expr)?;
        let target_ty = match ty {
            crate::ast::TypeExpr::Path(path) => self.lower_type_rep_for_path_type(path)?,
            other => {
                return Err(CompileError::new(
                    "`is` target must be a nominal type",
                    other.span(),
                ));
            }
        };
        let dst = self.alloc_local();
        self.emit(Instruction::IsType {
            dst,
            value: Operand::Local(value),
            ty: target_ty,
        });
        let _ = span;
        Ok(dst)
    }

    fn lower_checked_cast_expr(
        &mut self,
        expr: &Expr,
        ty: &crate::ast::TypeExpr,
        span: Span,
    ) -> Result<Local, CompileError> {
        let value = self.lower_expr(expr)?;
        let target_ty = match ty {
            crate::ast::TypeExpr::Path(path) => self.lower_type_rep_for_path_type(path)?,
            other => {
                return Err(CompileError::new(
                    "`as?` target must be a nominal type",
                    other.span(),
                ));
            }
        };
        let dst = self.alloc_local();
        self.emit(Instruction::CheckedCast {
            dst,
            value: Operand::Local(value),
            ty: target_ty,
        });
        let _ = span;
        Ok(dst)
    }

    fn lower_named_call(&mut self, func: &str, args: Vec<Operand>) -> Result<Local, CompileError> {
        let dst = self.alloc_local();
        self.emit(Instruction::Call {
            dst: Some(dst),
            func: func.to_string(),
            args,
        });
        Ok(dst)
    }

    fn lower_call_expr(
        &mut self,
        callee: &Expr,
        args: &[Expr],
        span: Span,
    ) -> Result<Local, CompileError> {
        // First-class continuation call: `cont(value_for_effect_call)`.
        if matches!(self.expr_ty(callee), Some(Ty::Cont { .. })) {
            if args.len() != 1 {
                return Err(CompileError::new(
                    "continuation call takes exactly one argument",
                    span,
                ));
            }
            let k_local = self.lower_expr(callee)?;
            let v = self.lower_expr(&args[0])?;
            let control = self.alloc_local();
            self.emit(Instruction::Resume {
                dst: Some(control),
                k: Operand::Local(k_local),
                value: Operand::Local(v),
            });
            return self.unwrap_control(control, span);
        }

        // Direct call by path.
        if let Expr::Path { path, .. } = callee {
            if path.segments.len() == 1 && self.lookup_var(&path.segments[0].name).is_some() {
                // Calling a local function value.
                return self.lower_call_closure(callee, args);
            }

            let segments: Vec<String> = path.segments.iter().map(|s| s.name.clone()).collect();

            // New-type struct constructor call: `Type(value)`.
            if let Some((kind, type_fqn)) = self
                .compiler
                .env
                .modules
                .try_resolve_type_fqn(&self.module, &segments, path.span)
                .map_err(|e| CompileError::new(e.message, e.span))?
                && kind == DefKind::Struct
                && let Some(def) = self.compiler.env.structs.get(&type_fqn)
                && def.is_newtype
            {
                if args.len() != 1 {
                    return Err(CompileError::new(
                        "new-type struct constructor takes exactly one argument",
                        span,
                    ));
                }
                let inner = self.lower_expr(&args[0])?;

                let type_args = match self.compiler.types.expr_types.get(&span) {
                    Some(ty) => match self.strip_readonly_ty(ty) {
                        Ty::App(typeck::TyCon::Named(_name), args) => args.clone(),
                        _ => Vec::new(),
                    },
                    None => Vec::new(),
                };
                let mut type_arg_reps = Vec::with_capacity(type_args.len());
                for arg in &type_args {
                    type_arg_reps.push(self.lower_type_rep_for_ty(arg, span)?);
                }

                let dst = self.alloc_local();
                self.emit(Instruction::MakeStruct {
                    dst,
                    type_name: type_fqn,
                    type_args: type_arg_reps,
                    fields: vec![(".0".to_string(), Operand::Local(inner))],
                });
                return Ok(dst);
            }
            let mut func_name: Option<String> = None;

            if segments.len() >= 2 {
                let prefix = &segments[..segments.len() - 1];
                let last = segments.last().expect("len >= 2");
                let last_ident = path.segments.last().expect("len >= 2");

                if let Some((kind, type_fqn)) = self
                    .compiler
                    .env
                    .modules
                    .try_resolve_type_fqn(&self.module, prefix, span)
                    .map_err(|e| CompileError::new(e.message, e.span))?
                {
                    match kind {
                        DefKind::Enum => {
                            if let Some(def) = self.compiler.env.enums.get(&type_fqn)
                                && def.variants.contains_key(last)
                            {
                                let mut ops = Vec::with_capacity(args.len());
                                for a in args {
                                    let v = self.lower_expr(a)?;
                                    ops.push(Operand::Local(v));
                                }
                                let dst = self.alloc_local();
                                let type_args = match self.compiler.types.expr_types.get(&span) {
                                    Some(ty) => match self.strip_readonly_ty(ty) {
                                        Ty::App(typeck::TyCon::Named(_name), args) => args.clone(),
                                        _ => Vec::new(),
                                    },
                                    None => Vec::new(),
                                };
                                let mut type_arg_reps = Vec::with_capacity(type_args.len());
                                for arg in &type_args {
                                    type_arg_reps.push(self.lower_type_rep_for_ty(arg, span)?);
                                }
                                self.emit(Instruction::MakeEnum {
                                    dst,
                                    enum_name: type_fqn,
                                    type_args: type_arg_reps,
                                    variant: last_ident.name.clone(),
                                    fields: ops,
                                });
                                return Ok(dst);
                            }

                            // Not a variant: allow inherent enum methods via `Enum::method(...)`.
                            let candidate = format!("{type_fqn}::{last}");
                            if self.compiler.env.functions.contains_key(&candidate) {
                                func_name = Some(candidate);
                            }
                        }
                        DefKind::Interface => {
                            return self.lower_interface_method_call(
                                type_fqn,
                                last_ident.name.clone(),
                                args,
                                span,
                            );
                        }
                        DefKind::Struct => {
                            let candidate = format!("{type_fqn}::{last}");
                            if self.compiler.env.functions.contains_key(&candidate) {
                                func_name = Some(candidate);
                            }
                        }
                    }
                }
            }

            let func_name = match func_name {
                Some(name) => name,
                None => self
                    .compiler
                    .env
                    .modules
                    .resolve_value_fqn(&self.module, &segments, span)
                    .map_err(|e| CompileError::new(e.message, e.span))?,
            };
            let call_key = (span, func_name.clone());
            let type_args = self
                .compiler
                .types
                .call_type_args
                .get(&call_key)
                .cloned()
                .unwrap_or_default();
            let mut call_args = Vec::with_capacity(type_args.len() + args.len());
            for ty in &type_args {
                call_args.push(self.lower_type_rep_for_ty(ty, span)?);
            }
            for arg in args {
                let v = self.lower_expr(arg)?;
                call_args.push(Operand::Local(v));
            }
            let dst = self.alloc_local();
            self.emit(Instruction::Call {
                dst: Some(dst),
                func: func_name,
                args: call_args,
            });
            return Ok(dst);
        }

        // Method call sugar: receiver.method(args...)
        if let Expr::Field {
            base,
            name: crate::ast::FieldName::Named(name),
            ..
        } = callee
        {
            return self.lower_method_call(base, name, args, span);
        }

        // Otherwise: call via a function value (closure).
        self.lower_call_closure(callee, args)
    }

    fn lower_call_closure(&mut self, callee: &Expr, args: &[Expr]) -> Result<Local, CompileError> {
        let closure_val = self.lower_expr(callee)?;

        let fnptr = self.alloc_local();
        self.emit(Instruction::StructGet {
            dst: fnptr,
            obj: Operand::Local(closure_val),
            idx: 0,
        });

        let env = self.alloc_local();
        self.emit(Instruction::StructGet {
            dst: env,
            obj: Operand::Local(closure_val),
            idx: 1,
        });

        let mut call_args = Vec::with_capacity(args.len() + 1);
        call_args.push(Operand::Local(env));
        for a in args {
            let v = self.lower_expr(a)?;
            call_args.push(Operand::Local(v));
        }

        let dst = self.alloc_local();
        self.emit(Instruction::ICall {
            dst: Some(dst),
            fnptr: Operand::Local(fnptr),
            args: call_args,
        });
        Ok(dst)
    }

    fn lower_interface_method_call(
        &mut self,
        iface_name: String,
        method_name: String,
        args: &[Expr],
        span: Span,
    ) -> Result<Local, CompileError> {
        if args.is_empty() {
            return Err(CompileError::new(
                "interface method call requires an explicit receiver argument",
                span,
            ));
        }

        let (origin_iface, method_id, receiver_readonly) = {
            let Some(iface_def) = self.compiler.env.interfaces.get(&iface_name) else {
                return Err(CompileError::new(
                    format!("internal error: unknown interface `{iface_name}`"),
                    span,
                ));
            };
            let Some(method_info) = iface_def.all_methods.get(&method_name) else {
                return Err(CompileError::new(
                    format!(
                        "internal error: unknown interface method `{iface_name}::{method_name}`"
                    ),
                    span,
                ));
            };
            let origin = method_info.origin.clone();
            let method_id = format!("{origin}::{method_name}");
            (origin, method_id, method_info.receiver_readonly)
        };

        let recv_ty = self
            .expr_ty(&args[0])
            .ok_or_else(|| {
                CompileError::new(
                    "internal error: missing type for interface call receiver",
                    args[0].span(),
                )
            })?
            .clone();

        let recv_for_dispatch = match &recv_ty {
            Ty::Readonly(inner) => inner.as_ref(),
            other => other,
        };

        // Static dispatch is only valid when the static receiver type is a concrete nominal type.
        if let Ty::App(typeck::TyCon::Named(type_name), recv_type_args) = recv_for_dispatch
            && !self.compiler.env.interfaces.contains_key(type_name)
        {
            let Some(impl_fn) = self.compiler.env.interface_methods.get(&(
                type_name.clone(),
                origin_iface.clone(),
                method_name.clone(),
            )) else {
                return Err(CompileError::new(
                    format!("no impl found for `{origin_iface}::{method_name}` on `{type_name}`"),
                    span,
                ));
            };
            let impl_fn = impl_fn.clone();

            let method_type_args = self
                .compiler
                .types
                .method_type_args
                .get(&(span, method_id.clone()))
                .cloned()
                .unwrap_or_default();
            let mut call_args =
                Vec::with_capacity(recv_type_args.len() + method_type_args.len() + args.len());
            for ty in recv_type_args {
                call_args.push(self.lower_type_rep_for_ty(ty, span)?);
            }
            for ty in &method_type_args {
                call_args.push(self.lower_type_rep_for_ty(ty, span)?);
            }
            let recv_local = self.lower_expr(&args[0])?;
            let recv_local = if receiver_readonly && !matches!(recv_ty, Ty::Readonly(_)) {
                let ro = self.alloc_local();
                self.emit(Instruction::AsReadonly {
                    dst: ro,
                    src: recv_local,
                });
                ro
            } else {
                recv_local
            };
            call_args.push(Operand::Local(recv_local));
            for a in &args[1..] {
                let v = self.lower_expr(a)?;
                call_args.push(Operand::Local(v));
            }
            let dst = self.alloc_local();
            self.emit(Instruction::Call {
                dst: Some(dst),
                func: impl_fn,
                args: call_args,
            });
            return Ok(dst);
        }

        // Otherwise, lower to dynamic dispatch (`vcall`).
        let recv_local = self.lower_expr(&args[0])?;
        let recv_local = if receiver_readonly && !matches!(recv_ty, Ty::Readonly(_)) {
            let ro = self.alloc_local();
            self.emit(Instruction::AsReadonly {
                dst: ro,
                src: recv_local,
            });
            ro
        } else {
            recv_local
        };
        let method_type_args = self
            .compiler
            .types
            .method_type_args
            .get(&(span, method_id.clone()))
            .cloned()
            .unwrap_or_default();
        let mut method_type_arg_reps = Vec::with_capacity(method_type_args.len());
        for ty in &method_type_args {
            method_type_arg_reps.push(self.lower_type_rep_for_ty(ty, span)?);
        }
        let mut vcall_args = Vec::with_capacity(args.len().saturating_sub(1));
        for a in &args[1..] {
            let v = self.lower_expr(a)?;
            vcall_args.push(Operand::Local(v));
        }
        let dst = self.alloc_local();
        self.emit(Instruction::VCall {
            dst: Some(dst),
            obj: Operand::Local(recv_local),
            method: method_id,
            method_type_args: method_type_arg_reps,
            args: vcall_args,
        });
        Ok(dst)
    }

    fn lower_method_call(
        &mut self,
        receiver: &Expr,
        method: &crate::ast::Ident,
        args: &[Expr],
        span: Span,
    ) -> Result<Local, CompileError> {
        let recv_ty = self
            .expr_ty(receiver)
            .ok_or_else(|| {
                CompileError::new(
                    "internal error: missing type for method receiver",
                    receiver.span(),
                )
            })?
            .clone();

        // Inherent methods first (if the receiver has a nominal type name).
        if let Some(recv_nom) = nominal_type_name(&recv_ty) {
            let inherent_name = format!("{recv_nom}::{}", method.name);
            let inherent_kind = self
                .compiler
                .env
                .inherent_method_kinds
                .get(&inherent_name)
                .copied();
            if matches!(
                inherent_kind,
                Some(typeck::InherentMethodKind::Instance { .. })
            ) && self.compiler.env.functions.contains_key(&inherent_name)
            {
                let recv_local = self.lower_expr(receiver)?;
                let recv_local = if matches!(
                    inherent_kind,
                    Some(typeck::InherentMethodKind::Instance { readonly: true })
                ) && !matches!(recv_ty, Ty::Readonly(_))
                {
                    let ro = self.alloc_local();
                    self.emit(Instruction::AsReadonly {
                        dst: ro,
                        src: recv_local,
                    });
                    ro
                } else {
                    recv_local
                };
                let call_key = (span, inherent_name.clone());
                let type_args = self
                    .compiler
                    .types
                    .call_type_args
                    .get(&call_key)
                    .cloned()
                    .unwrap_or_default();
                let mut call_args = Vec::with_capacity(type_args.len() + args.len() + 1);
                for ty in &type_args {
                    call_args.push(self.lower_type_rep_for_ty(ty, span)?);
                }
                call_args.push(Operand::Local(recv_local));
                for a in args {
                    let v = self.lower_expr(a)?;
                    call_args.push(Operand::Local(v));
                }
                let dst = self.alloc_local();
                self.emit(Instruction::Call {
                    dst: Some(dst),
                    func: inherent_name,
                    args: call_args,
                });
                return Ok(dst);
            }
        }

        let recv_for_dispatch = match &recv_ty {
            Ty::Readonly(inner) => inner.as_ref(),
            other => other,
        };

        // Constrained generic receiver (`T: I`) => dynamic dispatch within `I`.
        if let Ty::Gen(id) = recv_for_dispatch {
            let Some(gp) = self.generics.get(*id) else {
                return Err(CompileError::new(
                    "internal error: unknown generic parameter in method call",
                    span,
                ));
            };
            let mut candidates: BTreeMap<String, String> = BTreeMap::new();
            for bound in &gp.bounds {
                let Ty::App(crate::typeck::TyCon::Named(iface_name), _args) = bound else {
                    continue;
                };
                let Some(iface_def) = self.compiler.env.interfaces.get(iface_name) else {
                    continue;
                };
                let Some(info) = iface_def.all_methods.get(&method.name) else {
                    continue;
                };
                candidates
                    .entry(info.origin.clone())
                    .or_insert_with(|| iface_name.clone());
            }
            if candidates.len() != 1 {
                return Err(CompileError::new(
                    "internal error: ambiguous constrained generic method call",
                    span,
                ));
            }
            let origin = candidates.into_keys().next().expect("len == 1");
            let method_id = format!("{origin}::{}", method.name);

            let recv_local = self.lower_expr(receiver)?;
            let method_type_args = self
                .compiler
                .types
                .method_type_args
                .get(&(span, method_id.clone()))
                .cloned()
                .unwrap_or_default();
            let mut method_type_arg_reps = Vec::with_capacity(method_type_args.len());
            for ty in &method_type_args {
                method_type_arg_reps.push(self.lower_type_rep_for_ty(ty, span)?);
            }
            let mut vcall_args = Vec::with_capacity(args.len());
            for a in args {
                let v = self.lower_expr(a)?;
                vcall_args.push(Operand::Local(v));
            }
            let dst = self.alloc_local();
            self.emit(Instruction::VCall {
                dst: Some(dst),
                obj: Operand::Local(recv_local),
                method: method_id,
                method_type_args: method_type_arg_reps,
                args: vcall_args,
            });
            return Ok(dst);
        }

        // Interface-typed receiver => dynamic dispatch within that interface.
        if let Ty::App(typeck::TyCon::Named(iface_name), _args) = recv_for_dispatch
            && self.compiler.env.interfaces.contains_key(iface_name)
        {
            return self.lower_method_vcall(receiver, iface_name, &method.name, args, span);
        }

        // Concrete receiver => static dispatch via the canonical interface method id.
        let recv_nom = nominal_type_name(&recv_ty).ok_or_else(|| {
            CompileError::new("method receiver must be a nominal type", receiver.span())
        })?;

        let mut origins: BTreeMap<String, String> = BTreeMap::new();
        for (iface_name, iface) in &self.compiler.env.interfaces {
            if let Some(def) = self.compiler.env.modules.def(iface_name)
                && !def.vis.is_public()
                && !self.module.is_descendant_of(&def.defining_module)
            {
                continue;
            }
            let Some(method_info) = iface.all_methods.get(&method.name) else {
                continue;
            };
            if type_implements_interface(&self.compiler.env, recv_nom, iface_name) {
                origins
                    .entry(method_info.origin.clone())
                    .or_insert_with(|| iface_name.clone());
            }
        }
        if origins.is_empty() {
            return Err(CompileError::new(
                format!("unknown method `{}` on `{recv_nom}`", method.name),
                method.span,
            ));
        }
        if origins.len() != 1 {
            let candidates: Vec<String> = origins.into_keys().collect();
            return Err(CompileError::new(
                format!(
                    "ambiguous method `{}` on `{recv_nom}`; candidates: {}",
                    method.name,
                    candidates.join(", ")
                ),
                method.span,
            ));
        }

        let origin_iface = origins.into_keys().next().expect("origins.len()==1");
        let receiver_readonly = self
            .compiler
            .env
            .interfaces
            .get(&origin_iface)
            .and_then(|d| d.all_methods.get(&method.name))
            .is_some_and(|m| m.receiver_readonly);
        let Some(impl_fn) = self.compiler.env.interface_methods.get(&(
            recv_nom.to_string(),
            origin_iface.clone(),
            method.name.clone(),
        )) else {
            return Err(CompileError::new(
                format!(
                    "internal error: missing impl entry for `{origin_iface}::{}`",
                    method.name
                ),
                span,
            ));
        };
        let impl_fn = impl_fn.clone();

        let recv_local = self.lower_expr(receiver)?;
        let recv_local = if receiver_readonly && !matches!(recv_ty, Ty::Readonly(_)) {
            let ro = self.alloc_local();
            self.emit(Instruction::AsReadonly {
                dst: ro,
                src: recv_local,
            });
            ro
        } else {
            recv_local
        };
        let recv_type_args = match self.strip_readonly_ty(&recv_ty) {
            Ty::App(typeck::TyCon::Named(_), args) => args.clone(),
            _ => Vec::new(),
        };
        let method_id = format!("{origin_iface}::{}", method.name);
        let method_type_args = self
            .compiler
            .types
            .method_type_args
            .get(&(span, method_id.clone()))
            .cloned()
            .unwrap_or_default();

        let mut call_args =
            Vec::with_capacity(recv_type_args.len() + method_type_args.len() + args.len() + 1);
        for ty in &recv_type_args {
            call_args.push(self.lower_type_rep_for_ty(ty, span)?);
        }
        for ty in &method_type_args {
            call_args.push(self.lower_type_rep_for_ty(ty, span)?);
        }
        call_args.push(Operand::Local(recv_local));
        for a in args {
            let v = self.lower_expr(a)?;
            call_args.push(Operand::Local(v));
        }
        let dst = self.alloc_local();
        self.emit(Instruction::Call {
            dst: Some(dst),
            func: impl_fn,
            args: call_args,
        });
        Ok(dst)
    }

    fn lower_method_vcall(
        &mut self,
        receiver: &Expr,
        iface_name: &str,
        method_name: &str,
        args: &[Expr],
        span: Span,
    ) -> Result<Local, CompileError> {
        let (method_id, receiver_readonly) = {
            let Some(iface_def) = self.compiler.env.interfaces.get(iface_name) else {
                return Err(CompileError::new(
                    format!("internal error: unknown interface `{iface_name}`"),
                    span,
                ));
            };
            let Some(info) = iface_def.all_methods.get(method_name) else {
                return Err(CompileError::new(
                    format!("unknown method `{method_name}` on `{iface_name}`"),
                    span,
                ));
            };
            (
                format!("{}::{method_name}", info.origin),
                info.receiver_readonly,
            )
        };

        let recv_ty = self
            .expr_ty(receiver)
            .ok_or_else(|| {
                CompileError::new(
                    "internal error: missing type for vcall receiver",
                    receiver.span(),
                )
            })?
            .clone();
        let recv_local = self.lower_expr(receiver)?;
        let recv_local = if receiver_readonly && !matches!(recv_ty, Ty::Readonly(_)) {
            let ro = self.alloc_local();
            self.emit(Instruction::AsReadonly {
                dst: ro,
                src: recv_local,
            });
            ro
        } else {
            recv_local
        };
        let method_type_args = self
            .compiler
            .types
            .method_type_args
            .get(&(span, method_id.clone()))
            .cloned()
            .unwrap_or_default();
        let mut method_type_arg_reps = Vec::with_capacity(method_type_args.len());
        for ty in &method_type_args {
            method_type_arg_reps.push(self.lower_type_rep_for_ty(ty, span)?);
        }
        let mut vcall_args = Vec::with_capacity(args.len());
        for a in args {
            let v = self.lower_expr(a)?;
            vcall_args.push(Operand::Local(v));
        }
        let dst = self.alloc_local();
        self.emit(Instruction::VCall {
            dst: Some(dst),
            obj: Operand::Local(recv_local),
            method: method_id,
            method_type_args: method_type_arg_reps,
            args: vcall_args,
        });
        Ok(dst)
    }

    fn lower_match_expr(
        &mut self,
        scrutinee: &Expr,
        arms: &[MatchArm],
        span: Span,
    ) -> Result<Local, CompileError> {
        let has_effect_arms = arms.iter().any(|a| matches!(a.pat, MatchPat::Effect(_)));
        if has_effect_arms {
            return self.lower_match_via_helper(scrutinee, arms, span);
        }

        self.lower_match_value_only(scrutinee, arms, span)
    }

    fn lower_match_via_helper(
        &mut self,
        scrutinee: &Expr,
        arms: &[MatchArm],
        span: Span,
    ) -> Result<Local, CompileError> {
        let visible = self.visible_bindings();
        let mut free_vars = BTreeSet::new();
        let bound = BTreeSet::new();
        free_value_vars_in_expr(scrutinee, &bound, &mut free_vars);
        for arm in arms {
            let mut arm_bound = bound.clone();
            free_value_vars_in_match_pat(&arm.pat, &mut arm_bound);
            free_value_vars_in_expr(&arm.body, &arm_bound, &mut free_vars);
        }
        let captured = visible
            .iter()
            .filter_map(|(name, info)| {
                if parse_type_rep_capture_name(name).is_some() || free_vars.contains(name) {
                    Some((name.clone(), info.clone()))
                } else {
                    None
                }
            })
            .collect::<BTreeMap<_, _>>();
        let helper_name = self.compiler.fresh_internal_name("match");
        {
            let compiler = &mut *self.compiler;
            compiler.compile_match_helper(
                &self.module,
                helper_name.clone(),
                self.generics.clone(),
                &captured,
                scrutinee,
                arms,
                span,
            )?;
        }

        let mut args = Vec::with_capacity(captured.len());
        for (_name, info) in captured.iter() {
            args.push(Operand::Local(info.storage.local()));
        }

        let control_local = self.alloc_local();
        self.emit(Instruction::Call {
            dst: Some(control_local),
            func: helper_name,
            args,
        });

        self.unwrap_control(control_local, span)
    }

    fn lower_match_value_only(
        &mut self,
        scrutinee: &Expr,
        arms: &[MatchArm],
        span: Span,
    ) -> Result<Local, CompileError> {
        let scrut_local = self.lower_expr(scrutinee)?;
        let switch_block = self.current;

        let join_block = self.new_block("match_join");
        let join_param = self.alloc_local();
        self.blocks[join_block.0].params = vec![join_param];

        let default_block = self.new_block("match_default");
        self.set_current(default_block);
        self.set_terminator(Terminator::Trap {
            message: "non-exhaustive match".to_string(),
        })?;

        let mut cases = Vec::new();
        for (idx, arm) in arms.iter().enumerate() {
            let MatchPat::Value(pat) = &arm.pat else {
                continue;
            };
            let (mir_pat, bind_names) = self.lower_ast_pattern(pat)?;
            let arm_block = self.new_block(format!("match_arm_{idx}"));

            let mut params = Vec::with_capacity(bind_names.len());
            for _ in 0..bind_names.len() {
                params.push(self.alloc_local());
            }
            self.blocks[arm_block.0].params = params.clone();

            cases.push(SwitchCase {
                pattern: mir_pat,
                target: arm_block,
            });

            self.set_current(arm_block);
            self.push_scope();
            for (name, local) in bind_names.into_iter().zip(params.into_iter()) {
                self.bind_var(
                    &name,
                    VarInfo {
                        storage: VarStorage::Local(local),
                        kind: BindingKind::Const,
                    },
                );
            }
            let body_val = self.lower_expr(&arm.body)?;
            self.pop_scope();
            if !self.is_current_terminated() {
                self.set_terminator(Terminator::Br {
                    target: join_block,
                    args: vec![Operand::Local(body_val)],
                })?;
            }
        }

        self.set_current(switch_block);
        self.set_terminator(Terminator::Switch {
            value: Operand::Local(scrut_local),
            cases,
            default: default_block,
        })?;

        self.set_current(join_block);
        let _ = span;
        Ok(join_param)
    }

    fn lower_match_inline_with_effects(
        &mut self,
        scrutinee: &Expr,
        arms: &[MatchArm],
        span: Span,
    ) -> Result<Local, CompileError> {
        let entry_block = self.current;

        let mut effect_arms = Vec::new();
        let mut value_arms = Vec::new();
        for arm in arms {
            match &arm.pat {
                MatchPat::Effect(p) => effect_arms.push((p, &arm.body, arm.span)),
                MatchPat::Value(_) => value_arms.push(arm),
            }
        }
        if value_arms.is_empty() {
            return Err(CompileError::new(
                "`match` must have at least one value arm",
                span,
            ));
        }

        // Build handler clauses and handler blocks.
        let mut clauses = Vec::new();
        for (idx, (effect_pat, body, _arm_span)) in effect_arms.into_iter().enumerate() {
            let segments: Vec<String> = effect_pat
                .interface
                .segments
                .iter()
                .map(|s| s.name.name.clone())
                .collect();
            let (kind, interface) = self
                .compiler
                .env
                .modules
                .resolve_type_fqn(&self.module, &segments, effect_pat.interface.span)
                .map_err(|e| CompileError::new(e.message, e.span))?;
            if kind != DefKind::Interface {
                return Err(CompileError::new(
                    format!(
                        "internal error: expected interface in handler clause, got `{interface}`"
                    ),
                    effect_pat.interface.span,
                ));
            }
            let method = effect_pat.method.name.clone();
            let interface = {
                let Some(iface_def) = self.compiler.env.interfaces.get(&interface) else {
                    return Err(CompileError::new(
                        format!("internal error: unknown interface `{interface}`"),
                        effect_pat.interface.span,
                    ));
                };
                let Some(method_info) = iface_def.all_methods.get(&method) else {
                    return Err(CompileError::new(
                        format!("unknown effect/method `{}.{}`", interface, method),
                        effect_pat.method.span,
                    ));
                };
                method_info.origin.clone()
            };
            let effect_id = format!("{interface}::{method}");
            let interface_arg_tys = self
                .compiler
                .types
                .effect_interface_args
                .get(&(effect_pat.span, effect_id.clone()))
                .cloned()
                .ok_or_else(|| {
                    CompileError::new(
                        format!(
                            "internal error: missing interface type args for handler clause effect `{effect_id}`"
                        ),
                        effect_pat.span,
                    )
                })?;
            let mut interface_arg_reps = Vec::with_capacity(interface_arg_tys.len());
            for ty in &interface_arg_tys {
                interface_arg_reps.push(self.lower_type_rep_for_ty(ty, effect_pat.span)?);
            }

            let mut arg_patterns = Vec::with_capacity(effect_pat.args.len());
            let mut bind_names = Vec::new();
            for p in &effect_pat.args {
                let (mp, binds) = self.lower_ast_pattern(p)?;
                arg_patterns.push(mp);
                bind_names.extend(binds);
            }

            let handler_block = self.new_block(format!("handler_{idx}"));
            let mut params = Vec::with_capacity(bind_names.len() + 1);
            for _ in 0..bind_names.len() {
                params.push(self.alloc_local());
            }
            let k_local = self.alloc_local();
            params.push(k_local);
            self.blocks[handler_block.0].params = params.clone();

            clauses.push(HandlerClause {
                effect: EffectSpec {
                    interface,
                    interface_args: interface_arg_reps,
                    method,
                },
                arg_patterns,
                target: handler_block,
            });

            // Compile handler block: bind args, enable `resume`, compute value, wrap and return.
            self.set_current(handler_block);
            self.push_scope();
            for (name, local) in bind_names
                .into_iter()
                .zip(params[..params.len() - 1].iter().copied())
            {
                self.bind_var(
                    &name,
                    VarInfo {
                        storage: VarStorage::Local(local),
                        kind: BindingKind::Const,
                    },
                );
            }
            let cont_name = effect_pat
                .cont
                .as_ref()
                .map(|c| c.name.clone())
                .unwrap_or_else(|| "resume".to_string());
            self.bind_var(
                &cont_name,
                VarInfo {
                    storage: VarStorage::Local(k_local),
                    kind: BindingKind::Const,
                },
            );
            let value_local = self.lower_expr(body)?;
            self.pop_scope();

            if !self.is_current_terminated() {
                let control =
                    self.make_control(CONTROL_VARIANT_VALUE, vec![Operand::Local(value_local)]);
                self.set_terminator(Terminator::Return {
                    value: Operand::Local(control),
                })?;
            }

            // Continue emitting the match scrutinee evaluation in the entry block.
            self.set_current(entry_block);
        }

        // Evaluate scrutinee under handlers.
        self.emit(Instruction::PushHandler {
            handler_id: format!("H{}", span.start),
            clauses,
        });
        let scrut_local = self.lower_expr(scrutinee)?;
        self.emit(Instruction::PopHandler);
        let switch_block = self.current;

        // Switch over value arms.
        let join_block = self.new_block("match_join");
        let join_param = self.alloc_local();
        self.blocks[join_block.0].params = vec![join_param];

        let default_block = self.new_block("match_default");
        self.set_current(default_block);
        self.set_terminator(Terminator::Trap {
            message: "non-exhaustive match".to_string(),
        })?;

        let mut cases = Vec::new();
        for (idx, arm) in value_arms.into_iter().enumerate() {
            let MatchPat::Value(pat) = &arm.pat else {
                continue;
            };
            let (mir_pat, bind_names) = self.lower_ast_pattern(pat)?;
            let arm_block = self.new_block(format!("match_arm_{idx}"));

            let mut params = Vec::with_capacity(bind_names.len());
            for _ in 0..bind_names.len() {
                params.push(self.alloc_local());
            }
            self.blocks[arm_block.0].params = params.clone();
            cases.push(SwitchCase {
                pattern: mir_pat,
                target: arm_block,
            });

            self.set_current(arm_block);
            self.push_scope();
            for (name, local) in bind_names.into_iter().zip(params.into_iter()) {
                self.bind_var(
                    &name,
                    VarInfo {
                        storage: VarStorage::Local(local),
                        kind: BindingKind::Const,
                    },
                );
            }
            let body_val = self.lower_expr(&arm.body)?;
            self.pop_scope();
            if !self.is_current_terminated() {
                self.set_terminator(Terminator::Br {
                    target: join_block,
                    args: vec![Operand::Local(body_val)],
                })?;
            }
        }

        self.set_current(switch_block);
        self.set_terminator(Terminator::Switch {
            value: Operand::Local(scrut_local),
            cases,
            default: default_block,
        })?;

        self.set_current(join_block);
        Ok(join_param)
    }
}

fn nominal_type_name(ty: &Ty) -> Option<&str> {
    match ty {
        Ty::Readonly(inner) => nominal_type_name(inner),
        Ty::App(crate::typeck::TyCon::Named(name), _) => Some(name.as_str()),
        _ => None,
    }
}

fn type_implements_interface(env: &ProgramEnv, type_name: &str, iface: &str) -> bool {
    env.interface_impls
        .contains(&(type_name.to_string(), iface.to_string()))
}

fn mir_type_from_ty(env: &ProgramEnv, ty: &Ty) -> Option<Type> {
    match ty {
        Ty::Unit => Some(Type::Unit),
        Ty::Bool => Some(Type::Bool),
        Ty::Int => Some(Type::Int),
        Ty::Float => Some(Type::Float),
        Ty::String => Some(Type::String),
        Ty::Bytes => Some(Type::Bytes),
        Ty::Array(_) => Some(Type::Array),
        Ty::Tuple(items) => {
            if items.is_empty() {
                Some(Type::Unit)
            } else {
                Some(Type::Tuple(items.len()))
            }
        }
        Ty::Fn { .. } => Some(Type::Fn),
        Ty::Cont { .. } => Some(Type::Cont),
        Ty::Readonly(inner) => mir_type_from_ty(env, inner),
        Ty::App(crate::typeck::TyCon::Named(name), _) => {
            if env.enums.contains_key(name) {
                Some(Type::Enum(name.clone()))
            } else if env.interfaces.contains_key(name) {
                Some(Type::Interface(name.clone()))
            } else {
                Some(Type::Struct(name.clone()))
            }
        }
        Ty::App(_, _) | Ty::Gen(_) | Ty::Var(_) => None,
    }
}

fn select_binop_fn(op: BinaryOp, ty: &Ty) -> Option<&'static str> {
    match (op, ty) {
        (BinaryOp::Add, Ty::Int) => Some("core::intrinsics::int_add"),
        (BinaryOp::Sub, Ty::Int) => Some("core::intrinsics::int_sub"),
        (BinaryOp::Mul, Ty::Int) => Some("core::intrinsics::int_mul"),
        (BinaryOp::Div, Ty::Int) => Some("core::intrinsics::int_div"),
        (BinaryOp::Mod, Ty::Int) => Some("core::intrinsics::int_mod"),

        (BinaryOp::Eq, Ty::Int) => Some("core::intrinsics::int_eq"),
        (BinaryOp::Ne, Ty::Int) => Some("core::intrinsics::int_ne"),
        (BinaryOp::Lt, Ty::Int) => Some("core::intrinsics::int_lt"),
        (BinaryOp::Le, Ty::Int) => Some("core::intrinsics::int_le"),
        (BinaryOp::Gt, Ty::Int) => Some("core::intrinsics::int_gt"),
        (BinaryOp::Ge, Ty::Int) => Some("core::intrinsics::int_ge"),

        (BinaryOp::Add, Ty::Float) => Some("core::intrinsics::float_add"),
        (BinaryOp::Sub, Ty::Float) => Some("core::intrinsics::float_sub"),
        (BinaryOp::Mul, Ty::Float) => Some("core::intrinsics::float_mul"),
        (BinaryOp::Div, Ty::Float) => Some("core::intrinsics::float_div"),
        (BinaryOp::Mod, Ty::Float) => Some("core::intrinsics::float_mod"),

        (BinaryOp::Eq, Ty::Float) => Some("core::intrinsics::float_eq"),
        (BinaryOp::Ne, Ty::Float) => Some("core::intrinsics::float_ne"),
        (BinaryOp::Lt, Ty::Float) => Some("core::intrinsics::float_lt"),
        (BinaryOp::Le, Ty::Float) => Some("core::intrinsics::float_le"),
        (BinaryOp::Gt, Ty::Float) => Some("core::intrinsics::float_gt"),
        (BinaryOp::Ge, Ty::Float) => Some("core::intrinsics::float_ge"),

        (BinaryOp::Eq, Ty::Bool) => Some("core::intrinsics::bool_eq"),
        (BinaryOp::Ne, Ty::Bool) => Some("core::intrinsics::bool_ne"),
        (BinaryOp::Eq, Ty::String) => Some("core::intrinsics::string_eq"),
        (BinaryOp::Ne, Ty::String) => Some("core::intrinsics::string_ne"),
        (BinaryOp::Eq, Ty::Bytes) => Some("core::intrinsics::bytes_eq"),
        (BinaryOp::Ne, Ty::Bytes) => Some("core::intrinsics::bytes_ne"),
        (BinaryOp::Eq, Ty::Unit) => Some("core::intrinsics::unit_eq"),
        (BinaryOp::Ne, Ty::Unit) => Some("core::intrinsics::unit_ne"),
        _ => None,
    }
}
