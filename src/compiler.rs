use crate::ast::{
    BinaryOp, BindingKind, Block, Expr, FnItem, ImplHeader, ImplItem, Item, MatchArm, MatchPat,
    PatLiteral, Pattern as AstPattern, Program, Stmt, UnaryOp,
};
use crate::mir::{
    BasicBlock, BlockId, ConstValue, EffectId, Function, HandlerClause, Instruction, Local, Module,
    Mutability, Operand, Param, Pattern, SwitchCase, Terminator, Type,
};
use crate::modules::{DefKind, ModuleLoader, ModulePath};
use crate::parser::{ParseError, Parser};
use crate::source::Span;
use crate::typeck::{self, ProgramEnv, Ty, TypeError as TypeckError, TypeInfo};
use std::collections::BTreeMap;
use std::fmt;
use std::path::Path;

const INTERNAL_CELL_STRUCT: &str = "$Cell";
const CELL_FIELD_SET: &str = "set";
const CELL_FIELD_VALUE: &str = "v";

const INTERNAL_CLOSURE_STRUCT: &str = "$Closure";
const CLOSURE_FIELD_FUNC: &str = "func";
const CLOSURE_FIELD_ENV: &str = "env";

const INTERNAL_CONTROL_ENUM: &str = "$Control";
const CONTROL_VARIANT_VALUE: &str = "Value";
const CONTROL_VARIANT_RETURN: &str = "Return";
const CONTROL_VARIANT_BREAK: &str = "Break";
const CONTROL_VARIANT_CONTINUE: &str = "Continue";

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompileError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at {}..{}",
            self.message, self.span.start, self.span.end
        )
    }
}

impl std::error::Error for CompileError {}

impl From<ParseError> for CompileError {
    fn from(err: ParseError) -> Self {
        Self {
            message: err.message,
            span: err.span,
        }
    }
}

impl From<TypeckError> for CompileError {
    fn from(err: TypeckError) -> Self {
        Self {
            message: err.message,
            span: err.span,
        }
    }
}

impl From<crate::modules::LoadError> for CompileError {
    fn from(err: crate::modules::LoadError) -> Self {
        Self {
            message: err.message,
            span: err.span,
        }
    }
}

/// Compiles a single Rusk source file into MIR.
///
/// This is the front-end entrypoint used by tests and the CLI.
pub fn compile_to_mir(source: &str) -> Result<Module, CompileError> {
    let mut parser = Parser::new(source)?;
    let program = parser.parse_program()?;
    compile_program_to_mir(&program)
}

/// Compiles an entry file (and its `mod foo;` dependencies) into MIR.
pub fn compile_file_to_mir(entry_path: &Path) -> Result<Module, CompileError> {
    let mut loader = ModuleLoader::new();
    let program = loader.load_program_from_file(entry_path)?;
    compile_program_to_mir(&program)
}

fn compile_program_to_mir(program: &Program) -> Result<Module, CompileError> {
    let env = typeck::build_env(program)?;
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

        Ok(self.module)
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
                            return Err(CompileError {
                                message: "file modules must be loaded before compilation"
                                    .to_string(),
                                span: m.span,
                            });
                        }
                    }
                }
                Item::Struct(_) | Item::Enum(_) | Item::Interface(_) | Item::Use(_) => {}
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
                    .map_err(|e| CompileError {
                        message: e.message,
                        span: e.span,
                    })?;
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
                    .map_err(|e| CompileError {
                        message: e.message,
                        span: e.span,
                    })?;
                let type_segments: Vec<String> =
                    ty.segments.iter().map(|s| s.name.name.clone()).collect();
                let (_kind, type_name) = self
                    .env
                    .modules
                    .resolve_type_fqn(module, &type_segments, ty.span)
                    .map_err(|e| CompileError {
                        message: e.message,
                        span: e.span,
                    })?;
                for method in &imp.members {
                    let mname = &method.name.name;
                    let name_override = format!("impl::{iface_name}::for::{type_name}::{mname}");
                    self.compile_real_function(module, method, Some(name_override))?;
                }
            }
        }
        Ok(())
    }

    fn compile_real_function(
        &mut self,
        module: &ModulePath,
        func: &FnItem,
        name_override: Option<String>,
    ) -> Result<(), CompileError> {
        let name = name_override.unwrap_or_else(|| module.qualify(&func.name.name));
        if self.module.functions.contains_key(&name) {
            return Err(CompileError {
                message: format!("duplicate function `{name}`"),
                span: func.name.span,
            });
        }

        let sig = self
            .env
            .functions
            .get(&name)
            .ok_or_else(|| CompileError {
                message: format!("internal error: missing signature for `{name}`"),
                span: func.span,
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

        let mut lowerer = FunctionLowerer::new(self, FnKind::Real, module.clone(), name.clone());
        lowerer.bind_params_from_fn_item(func, &param_mutabilities, &param_types)?;
        let value = lowerer.lower_block_expr(&func.body)?;
        if !lowerer.is_current_terminated() {
            lowerer.set_terminator(Terminator::Return {
                value: Operand::Local(value),
            })?;
        }

        let mut mir_fn = lowerer.finish()?;
        mir_fn.ret_type = ret_type;
        self.module.functions.insert(name, mir_fn);
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
            .ok_or_else(|| CompileError {
                message: format!("internal error: missing signature for `{target}`"),
                span: Span::new(0, 0),
            })?
            .clone();

        let wrapper_name = format!("$wrap::{target}");
        if self.module.functions.contains_key(&wrapper_name) {
            return Ok(wrapper_name);
        }

        let mut lowerer =
            FunctionLowerer::new(self, FnKind::Real, ModulePath::root(), wrapper_name.clone());

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
        self.module.functions.insert(wrapper_name.clone(), mir_fn);
        self.fn_value_wrappers
            .insert(target.to_string(), wrapper_name.clone());
        Ok(wrapper_name)
    }

    fn compile_lambda_entry(
        &mut self,
        module: &ModulePath,
        entry_name: String,
        captures: &BTreeMap<String, VarInfo>,
        lambda_params: &[crate::ast::LambdaParam],
        body: &Block,
        lambda_expr_span: Span,
    ) -> Result<(), CompileError> {
        // Signature of the lambda expression (includes inferred readonly on params).
        let param_tys = match self.types.expr_types.get(&lambda_expr_span) {
            Some(Ty::Fn { params, .. }) => params.clone(),
            other => {
                return Err(CompileError {
                    message: format!("internal error: expected fn type for lambda, got {other:?}"),
                    span: lambda_expr_span,
                });
            }
        };

        let mut lowerer =
            FunctionLowerer::new(self, FnKind::Real, module.clone(), entry_name.clone());

        // First param: env array.
        let env_local = lowerer.alloc_local();
        lowerer.params.push(Param {
            local: env_local,
            mutability: Mutability::Mutable,
            ty: Some(Type::Array),
        });

        // Lambda params.
        if lambda_params.len() != param_tys.len() {
            return Err(CompileError {
                message: "internal error: lambda param arity mismatch".to_string(),
                span: lambda_expr_span,
            });
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
            lowerer.bind_var(
                name,
                VarInfo {
                    local: captured_val,
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
                    local: param_local,
                    kind: BindingKind::Const,
                },
            );
        }

        let result = lowerer.lower_block_expr(body)?;
        if !lowerer.is_current_terminated() {
            lowerer.set_terminator(Terminator::Return {
                value: Operand::Local(result),
            })?;
        }

        let mir_fn = lowerer.finish()?;
        self.module.functions.insert(entry_name, mir_fn);
        Ok(())
    }

    fn compile_match_helper(
        &mut self,
        module: &ModulePath,
        helper_name: String,
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
        );

        // Captured environment params (in deterministic name order).
        for (name, info) in captured.iter() {
            let local = lowerer.alloc_local();
            lowerer.params.push(Param {
                local,
                mutability: Mutability::Mutable,
                ty: None,
            });
            lowerer.bind_var(
                name,
                VarInfo {
                    local,
                    kind: info.kind,
                },
            );
        }

        let value = lowerer.lower_match_inline_with_effects(scrutinee, arms, match_span)?;
        if !lowerer.is_current_terminated() {
            let control = lowerer.make_control(CONTROL_VARIANT_VALUE, vec![Operand::Local(value)]);
            lowerer.set_terminator(Terminator::Return {
                value: Operand::Local(control),
            })?;
        }

        let mut mir_fn = lowerer.finish()?;
        mir_fn.ret_type = Some(Type::Enum(INTERNAL_CONTROL_ENUM.to_string()));
        self.module.functions.insert(helper_name, mir_fn);
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FnKind {
    Real,
    ExprHelper,
}

#[derive(Clone)]
struct VarInfo {
    local: Local,
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
    params: Vec<Param>,
    blocks: Vec<BlockBuilder>,
    current: BlockId,
    next_local: usize,
    scopes: Vec<BTreeMap<String, VarInfo>>,
    loop_stack: Vec<LoopTargets>,
}

impl<'a> FunctionLowerer<'a> {
    fn new(compiler: &'a mut Compiler, kind: FnKind, module: ModulePath, name: String) -> Self {
        Self {
            compiler,
            kind,
            module,
            name,
            params: Vec::new(),
            blocks: vec![BlockBuilder {
                label: "block0".to_string(),
                params: Vec::new(),
                instructions: Vec::new(),
                terminator: None,
            }],
            current: BlockId(0),
            next_local: 0,
            scopes: vec![BTreeMap::new()],
            loop_stack: Vec::new(),
        }
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

    fn bind_params_from_fn_item(
        &mut self,
        func: &FnItem,
        param_mutabilities: &[Mutability],
        param_types: &[Option<Type>],
    ) -> Result<(), CompileError> {
        if func.params.len() != param_mutabilities.len() || func.params.len() != param_types.len() {
            return Err(CompileError {
                message: "internal error: function signature arity mismatch".to_string(),
                span: func.span,
            });
        }

        let mut trap_block: Option<BlockId> = None;
        for (idx, p) in func.params.iter().enumerate() {
            let local = self.alloc_local();
            let mutability = param_mutabilities[idx];
            self.params.push(Param {
                local,
                mutability,
                ty: param_types[idx].clone(),
            });

            match &p.pat {
                crate::ast::Pattern::Wildcard { .. } => {}
                crate::ast::Pattern::Bind { name, .. } => {
                    // Simple parameter binding.
                    self.bind_var(
                        &name.name,
                        VarInfo {
                            local,
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
                                local,
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
                    .map_err(|e| CompileError {
                        message: e.message,
                        span: e.span,
                    })?;
                if kind != DefKind::Enum {
                    return Err(CompileError {
                        message: format!(
                            "internal error: expected an enum type in pattern, got `{enum_name}`"
                        ),
                        span: enum_path.span,
                    });
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
                    .map_err(|e| CompileError {
                        message: e.message,
                        span: e.span,
                    })?;
                if kind != DefKind::Struct {
                    return Err(CompileError {
                        message: format!(
                            "internal error: expected a struct type in pattern, got `{type_name}`"
                        ),
                        span: type_path.span,
                    });
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
            return Err(CompileError {
                message: format!("internal error: block already terminated ({func}:{id} {label})"),
                span: Span::new(0, 0),
            });
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
        out
    }

    fn make_control(&mut self, variant: &str, fields: Vec<Operand>) -> Local {
        let dst = self.alloc_local();
        self.emit(Instruction::MakeEnum {
            dst,
            enum_name: INTERNAL_CONTROL_ENUM.to_string(),
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
                name,
                ty: _,
                init,
                span,
            } => self.lower_let_stmt(*kind, name, init.as_ref(), *span)?,
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
            FnKind::Real => Err(CompileError {
                message: if is_break {
                    "`break` outside of a loop".to_string()
                } else {
                    "`continue` outside of a loop".to_string()
                },
                span,
            }),
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
        name: &crate::ast::Ident,
        init: Option<&Expr>,
        span: Span,
    ) -> Result<(), CompileError> {
        match kind {
            BindingKind::Let => {
                // Represent `let` bindings as boxed cells so they can be captured across helper
                // function boundaries (required for delimited effects compilation).
                let cell = self.alloc_local();
                let (set_value, initial_value) = if let Some(init_expr) = init {
                    let v = self.lower_expr(init_expr)?;
                    (true, Operand::Local(v))
                } else {
                    (false, Operand::Literal(ConstValue::Unit))
                };
                self.emit(Instruction::MakeStruct {
                    dst: cell,
                    type_name: INTERNAL_CELL_STRUCT.to_string(),
                    fields: vec![
                        (
                            CELL_FIELD_SET.to_string(),
                            Operand::Literal(ConstValue::Bool(set_value)),
                        ),
                        (CELL_FIELD_VALUE.to_string(), initial_value),
                    ],
                });
                self.bind_var(
                    &name.name,
                    VarInfo {
                        local: cell,
                        kind: BindingKind::Let,
                    },
                );
                Ok(())
            }
            BindingKind::Const | BindingKind::Readonly => {
                let Some(init_expr) = init else {
                    return Err(CompileError {
                        message: "const/readonly bindings require an initializer".to_string(),
                        span,
                    });
                };
                let value_local = self.lower_expr(init_expr)?;
                let dst = self.alloc_local();
                self.emit(Instruction::Copy {
                    dst,
                    src: value_local,
                });
                if kind == BindingKind::Readonly {
                    self.emit(Instruction::AsReadonly { dst, src: dst });
                }
                self.bind_var(&name.name, VarInfo { local: dst, kind });
                Ok(())
            }
        }
    }

    fn read_cell(&mut self, cell: Local, name: &str, span: Span) -> Result<Local, CompileError> {
        if self.is_current_terminated() {
            return Ok(self.alloc_unit());
        }

        let set_local = self.alloc_local();
        self.emit(Instruction::GetField {
            dst: set_local,
            obj: Operand::Local(cell),
            field: CELL_FIELD_SET.to_string(),
        });

        let then_block = self.new_block(format!("cell_read_{name}_ok"));
        let else_block = self.new_block(format!("cell_read_{name}_uninit"));
        let join_block = self.new_block(format!("cell_read_{name}_join"));
        let value_param = self.alloc_local();
        self.blocks[join_block.0].params = vec![value_param];

        self.set_terminator(Terminator::CondBr {
            cond: Operand::Local(set_local),
            then_target: then_block,
            then_args: Vec::new(),
            else_target: else_block,
            else_args: Vec::new(),
        })?;

        self.set_current(then_block);
        let value_local = self.alloc_local();
        self.emit(Instruction::GetField {
            dst: value_local,
            obj: Operand::Local(cell),
            field: CELL_FIELD_VALUE.to_string(),
        });
        self.set_terminator(Terminator::Br {
            target: join_block,
            args: vec![Operand::Local(value_local)],
        })?;

        self.set_current(else_block);
        self.set_terminator(Terminator::Trap {
            message: format!("uninitialized local `{name}`"),
        })?;

        self.set_current(join_block);
        let _ = span;
        Ok(value_param)
    }

    fn write_cell(&mut self, cell: Local, value: Local) {
        self.emit(Instruction::SetField {
            obj: Operand::Local(cell),
            field: CELL_FIELD_VALUE.to_string(),
            value: Operand::Local(value),
        });
        self.emit(Instruction::SetField {
            obj: Operand::Local(cell),
            field: CELL_FIELD_SET.to_string(),
            value: Operand::Literal(ConstValue::Bool(true)),
        });
    }

    fn expr_ty(&self, expr: &Expr) -> Option<&Ty> {
        self.compiler.types.expr_types.get(&expr.span())
    }

    fn expr_is_readonly(&self, expr: &Expr) -> bool {
        matches!(self.expr_ty(expr), Some(Ty::Readonly(_)))
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
                    .map_err(|e| CompileError {
                        message: e.message,
                        span: e.span,
                    })?;
                if kind != DefKind::Struct {
                    return Err(CompileError {
                        message: format!(
                            "internal error: expected a struct type for struct literal, got `{type_name}`"
                        ),
                        span: type_path.span,
                    });
                }
                self.emit(Instruction::MakeStruct {
                    dst,
                    type_name,
                    fields: mir_fields,
                });
                Ok(dst)
            }
            Expr::EffectCall {
                interface,
                method,
                args,
                ..
            } => {
                let mut mir_args = Vec::with_capacity(args.len());
                for a in args {
                    let v = self.lower_expr(a)?;
                    mir_args.push(Operand::Local(v));
                }
                let dst = self.alloc_local();
                let segments: Vec<String> =
                    interface.segments.iter().map(|s| s.name.clone()).collect();
                let (kind, interface_name) = self
                    .compiler
                    .env
                    .modules
                    .resolve_type_fqn(&self.module, &segments, interface.span)
                    .map_err(|e| CompileError {
                        message: e.message,
                        span: e.span,
                    })?;
                if kind != DefKind::Interface {
                    return Err(CompileError {
                        message: format!(
                            "internal error: expected an interface for effect call, got `{interface_name}`"
                        ),
                        span: interface.span,
                    });
                }
                self.emit(Instruction::Perform {
                    dst: Some(dst),
                    effect: EffectId {
                        interface: interface_name,
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

            Expr::Call { callee, args, span } => self.lower_call_expr(callee, args, *span),
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
                let local = var.local;
                match var.kind {
                    BindingKind::Let => return self.read_cell(local, name, span),
                    BindingKind::Const | BindingKind::Readonly => {
                        let dst = self.alloc_local();
                        self.emit(Instruction::Copy { dst, src: local });
                        return Ok(dst);
                    }
                }
            }
        }

        // Function item used as a value.
        let segments: Vec<String> = path.segments.iter().map(|s| s.name.clone()).collect();
        let mut func_name: Option<String> = None;
        if segments.len() >= 2 {
            let prefix = &segments[..segments.len() - 1];
            let last = segments.last().expect("len >= 2");
            let prefix_ty = self
                .compiler
                .env
                .modules
                .try_resolve_type_fqn(&self.module, prefix, span)
                .map_err(|e| CompileError {
                    message: e.message,
                    span: e.span,
                })?;
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
                .map_err(|e| CompileError {
                    message: e.message,
                    span: e.span,
                })?,
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
        let captures = self.visible_bindings();
        let entry_name = self.compiler.fresh_internal_name("lambda");

        {
            let compiler = &mut *self.compiler;
            compiler.compile_lambda_entry(
                &self.module,
                entry_name.clone(),
                &captures,
                params,
                body,
                span,
            )?;
        }

        // Capture values into an env array (name order).
        let mut captured_ops = Vec::with_capacity(captures.len());
        for (_name, info) in captures.iter() {
            captured_ops.push(Operand::Local(info.local));
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
        //       Option::None(()) => break;
        //     }
        //   }
        let iter_val = self.lower_expr(iter)?;
        let it_local = self.alloc_local();
        self.emit(Instruction::Call {
            dst: Some(it_local),
            func: "core::intrinsics::into_iter".to_string(),
            args: vec![Operand::Local(iter_val)],
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
            args: vec![Operand::Local(it_local)],
        });

        // `Option::Some(x)` binds one value, `Option::None(())` binds none.
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
                        fields: vec![Pattern::Wildcard],
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
                local: some_param,
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
        let field = match name {
            crate::ast::FieldName::Named(name) => name.name.clone(),
            crate::ast::FieldName::Index { index, .. } => format!(".{index}"),
        };
        let dst = self.alloc_local();
        self.emit(Instruction::GetField {
            dst,
            obj: Operand::Local(base_local),
            field,
        });
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
                self.emit(Instruction::Call {
                    dst: Some(dst),
                    func: "core::intrinsics::bool_not".to_string(),
                    args: vec![Operand::Local(v)],
                });
                Ok(dst)
            }
            UnaryOp::Neg => {
                let ty = self
                    .expr_ty(expr)
                    .ok_or_else(|| CompileError {
                        message: "internal error: missing type for unary expression".to_string(),
                        span,
                    })?
                    .clone();
                let v = self.lower_expr(expr)?;
                match ty {
                    Ty::Int => {
                        let zero = self.alloc_int(0);
                        self.lower_named_call(
                            "core::intrinsics::int_sub",
                            vec![Operand::Local(zero), Operand::Local(v)],
                        )
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
                    other => Err(CompileError {
                        message: format!("internal error: unary - not supported for {other}"),
                        span,
                    }),
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
                    .ok_or_else(|| CompileError {
                        message: "internal error: missing type for binary operator".to_string(),
                        span,
                    })?
                    .clone();
                let l = self.lower_expr(left)?;
                let r = self.lower_expr(right)?;
                let func = select_binop_fn(op, &ty).ok_or_else(|| CompileError {
                    message: format!("internal error: unsupported binary op `{op:?}` for `{ty}`"),
                    span,
                })?;
                self.lower_named_call(func, vec![Operand::Local(l), Operand::Local(r)])
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
                    return Err(CompileError {
                        message: "assignment target must be a local name".to_string(),
                        span,
                    });
                }
                let name = &path.segments[0].name;
                let Some(var) = self.lookup_var(name) else {
                    return Err(CompileError {
                        message: format!("unknown name `{name}`"),
                        span: path.segments[0].span,
                    });
                };
                if var.kind != BindingKind::Let {
                    return Err(CompileError {
                        message: format!("cannot assign to `{name}` (not a `let` binding)"),
                        span: path.segments[0].span,
                    });
                }
                let local = var.local;
                let rhs = self.lower_expr(value)?;
                self.write_cell(local, rhs);
                Ok(self.alloc_unit())
            }
            Expr::Field { base, name, .. } => {
                let obj = self.lower_expr(base)?;
                let rhs = self.lower_expr(value)?;
                let field = match name {
                    crate::ast::FieldName::Named(name) => name.name.clone(),
                    crate::ast::FieldName::Index { index, .. } => format!(".{index}"),
                };
                self.emit(Instruction::SetField {
                    obj: Operand::Local(obj),
                    field,
                    value: Operand::Local(rhs),
                });
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
            _ => Err(CompileError {
                message: "invalid assignment target".to_string(),
                span,
            }),
        }
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
                return Err(CompileError {
                    message: "continuation call takes exactly one argument".to_string(),
                    span,
                });
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
                    .map_err(|e| CompileError {
                        message: e.message,
                        span: e.span,
                    })?
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
                                self.emit(Instruction::MakeEnum {
                                    dst,
                                    enum_name: type_fqn,
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
                    .map_err(|e| CompileError {
                        message: e.message,
                        span: e.span,
                    })?,
            };
            let mut call_args = Vec::with_capacity(args.len());
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
        self.emit(Instruction::GetField {
            dst: fnptr,
            obj: Operand::Local(closure_val),
            field: CLOSURE_FIELD_FUNC.to_string(),
        });

        let env = self.alloc_local();
        self.emit(Instruction::GetField {
            dst: env,
            obj: Operand::Local(closure_val),
            field: CLOSURE_FIELD_ENV.to_string(),
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
            return Err(CompileError {
                message: "interface method call requires an explicit receiver argument".to_string(),
                span,
            });
        }

        let recv_ty = self
            .expr_ty(&args[0])
            .ok_or_else(|| CompileError {
                message: "internal error: missing type for interface call receiver".to_string(),
                span: args[0].span(),
            })?
            .clone();
        let recv_nom = nominal_type_name(&recv_ty).ok_or_else(|| CompileError {
            message: "interface call receiver must be a nominal type".to_string(),
            span: args[0].span(),
        })?;
        let Some(impl_fn) = self.compiler.env.interface_methods.get(&(
            recv_nom.to_string(),
            iface_name.clone(),
            method_name.clone(),
        )) else {
            return Err(CompileError {
                message: format!("no impl found for `{iface_name}::{method_name}` on `{recv_nom}`"),
                span,
            });
        };
        let impl_fn = impl_fn.clone();

        let mut call_args = Vec::with_capacity(args.len());
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

    fn lower_method_call(
        &mut self,
        receiver: &Expr,
        method: &crate::ast::Ident,
        args: &[Expr],
        span: Span,
    ) -> Result<Local, CompileError> {
        let recv_ty = self.expr_ty(receiver).ok_or_else(|| CompileError {
            message: "internal error: missing type for method receiver".to_string(),
            span: receiver.span(),
        })?;
        let recv_nom = nominal_type_name(recv_ty).ok_or_else(|| CompileError {
            message: "method receiver must be a nominal type".to_string(),
            span: receiver.span(),
        })?;

        // Inherent methods first.
        let inherent_name = format!("{recv_nom}::{}", method.name);
        if self.compiler.env.functions.contains_key(&inherent_name) {
            let recv_local = self.lower_expr(receiver)?;
            let mut call_args = Vec::with_capacity(args.len() + 1);
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

        // Otherwise: interface method (must be unambiguous).
        let mut candidates = Vec::new();
        for (iface_name, iface) in &self.compiler.env.interfaces {
            if let Some(def) = self.compiler.env.modules.def(iface_name)
                && !def.vis.is_public()
                && !self.module.is_descendant_of(&def.defining_module)
            {
                continue;
            }
            if iface.methods.contains_key(&method.name)
                && type_implements_interface(&self.compiler.env, recv_nom, iface_name)
            {
                candidates.push(iface_name.clone());
            }
        }
        if candidates.is_empty() {
            return Err(CompileError {
                message: format!("unknown method `{}` on `{recv_nom}`", method.name),
                span: method.span,
            });
        }
        if candidates.len() != 1 {
            return Err(CompileError {
                message: format!(
                    "ambiguous method `{}` on `{recv_nom}`; candidates: {}",
                    method.name,
                    candidates.join(", ")
                ),
                span: method.span,
            });
        }

        let iface = &candidates[0];
        let Some(impl_fn) = self.compiler.env.interface_methods.get(&(
            recv_nom.to_string(),
            iface.clone(),
            method.name.clone(),
        )) else {
            return Err(CompileError {
                message: format!(
                    "internal error: missing impl entry for `{iface}::{}`",
                    method.name
                ),
                span,
            });
        };
        let impl_fn = impl_fn.clone();

        let recv_local = self.lower_expr(receiver)?;
        let mut call_args = Vec::with_capacity(args.len() + 1);
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
        let captured = self.visible_bindings();
        let helper_name = self.compiler.fresh_internal_name("match");
        {
            let compiler = &mut *self.compiler;
            compiler.compile_match_helper(
                &self.module,
                helper_name.clone(),
                &captured,
                scrutinee,
                arms,
                span,
            )?;
        }

        let mut args = Vec::with_capacity(captured.len());
        for (_name, info) in captured.iter() {
            args.push(Operand::Local(info.local));
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
                        local,
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
            return Err(CompileError {
                message: "`match` must have at least one value arm".to_string(),
                span,
            });
        }

        // Build handler clauses and handler blocks.
        let mut clauses = Vec::new();
        for (idx, (effect_pat, body, _arm_span)) in effect_arms.into_iter().enumerate() {
            let segments: Vec<String> = effect_pat
                .interface
                .segments
                .iter()
                .map(|s| s.name.clone())
                .collect();
            let (kind, interface) = self
                .compiler
                .env
                .modules
                .resolve_type_fqn(&self.module, &segments, effect_pat.interface.span)
                .map_err(|e| CompileError {
                    message: e.message,
                    span: e.span,
                })?;
            if kind != DefKind::Interface {
                return Err(CompileError {
                    message: format!(
                        "internal error: expected interface in handler clause, got `{interface}`"
                    ),
                    span: effect_pat.interface.span,
                });
            }
            let method = effect_pat.method.name.clone();

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
                effect: EffectId { interface, method },
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
                        local,
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
                    local: k_local,
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
                        local,
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
    let Some(iface_def) = env.interfaces.get(iface) else {
        return false;
    };
    iface_def.methods.keys().all(|m| {
        env.interface_methods.contains_key(&(
            type_name.to_string(),
            iface.to_string(),
            m.to_string(),
        ))
    })
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
