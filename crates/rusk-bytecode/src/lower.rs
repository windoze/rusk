#![forbid(unsafe_code)]

extern crate alloc;

use alloc::collections::{BTreeMap, BTreeSet};
use alloc::string::String;
use alloc::vec::Vec;

use crate::{
    CallTarget, ConstValue, ExecutableModule, ExternalEffectDecl, Function, FunctionId, HostFnSig,
    HostImport, HostImportId, Instruction, Reg,
};
use rusk_mir::{
    BlockId, CallTarget as MirCallTarget, ConstValue as MirConstValue, Operand,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LowerError {
    pub message: String,
}

impl LowerError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl core::fmt::Display for LowerError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "bytecode lowering error: {}", self.message)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for LowerError {}

pub fn lower_mir_module(mir: &rusk_mir::Module) -> Result<ExecutableModule, LowerError> {
    lower_mir_module_with_options(mir, &LowerOptions::default())
}

#[derive(Clone, Debug, Default)]
pub struct LowerOptions {
    pub external_effects: Vec<ExternalEffectDecl>,
}

pub fn lower_mir_module_with_options(
    mir: &rusk_mir::Module,
    options: &LowerOptions,
) -> Result<ExecutableModule, LowerError> {
    let mut out = ExecutableModule::default();

    for decl in &options.external_effects {
        out.add_external_effect(decl.clone())
            .map_err(LowerError::new)?;
    }
    let external_effect_ids = out.external_effect_ids.clone();

    let mut host_id_map: Vec<Option<HostImportId>> = Vec::with_capacity(mir.host_imports.len());
    host_id_map.resize(mir.host_imports.len(), None);

    for (idx, import) in mir.host_imports.iter().enumerate() {
        let Some(sig) = HostFnSig::from_host_sig(&import.sig) else {
            continue;
        };
        let id = out
            .add_host_import(HostImport {
                name: import.name.clone(),
                sig,
            })
            .map_err(LowerError::new)?;
        host_id_map[idx] = Some(id);
    }

    for (idx, func) in mir.functions.iter().enumerate() {
        let bc_func = lower_mir_function(mir, func, &host_id_map, &external_effect_ids)?;
        let id = out.add_function(bc_func).map_err(LowerError::new)?;
        let expect = FunctionId(idx as u32);
        if id != expect {
            return Err(LowerError::new(format!(
                "internal error: function id mismatch (got {}, expected {})",
                id.0, expect.0
            )));
        }
    }

    let Some(main_id) = mir.function_id("main") else {
        return Err(LowerError::new("missing required entry function `main`"));
    };
    out.entry = FunctionId(main_id.0);

    Ok(out)
}

struct TempAlloc {
    next: Reg,
}

impl TempAlloc {
    fn new(base: Reg) -> Self {
        Self { next: base }
    }

    fn alloc(&mut self) -> Reg {
        let r = self.next;
        self.next = self.next.checked_add(1).expect("register overflow");
        r
    }
}

#[derive(Clone, Copy, Debug)]
struct JumpPatch {
    instr_index: usize,
    target: BlockId,
}

fn lower_mir_function(
    mir_module: &rusk_mir::Module,
    mir_func: &rusk_mir::Function,
    host_id_map: &[Option<HostImportId>],
    external_effect_ids: &BTreeMap<(String, String), crate::EffectId>,
) -> Result<Function, LowerError> {
    for (idx, param) in mir_func.params.iter().enumerate() {
        if param.local.0 != idx {
            return Err(LowerError::new(format!(
                "bytecode v0 requires params to occupy locals 0..N-1; param {idx} uses local {}",
                param.local.0
            )));
        }
    }

    let base_reg_count: Reg = mir_func
        .locals
        .try_into()
        .map_err(|_| LowerError::new("mir local count overflow (expected <= u32::MAX)"))?;

    let mut temps = TempAlloc::new(base_reg_count);

    let mut block_pcs: Vec<u32> = Vec::with_capacity(mir_func.blocks.len());
    block_pcs.resize(mir_func.blocks.len(), 0);

    let mut code: Vec<Instruction> = Vec::new();
    let mut patches: Vec<JumpPatch> = Vec::new();

    for (block_idx, block) in mir_func.blocks.iter().enumerate() {
        block_pcs[block_idx] = code
            .len()
            .try_into()
            .map_err(|_| LowerError::new("function too large (pc overflow)"))?;

        for instr in &block.instructions {
            lower_mir_instruction(
                mir_module,
                mir_func,
                instr,
                host_id_map,
                external_effect_ids,
                &mut temps,
                &mut code,
            )?;
        }

        lower_mir_terminator(
            mir_module,
            mir_func,
            &block.terminator,
            host_id_map,
            &mut temps,
            &mut code,
            &mut patches,
        )?;
    }

    for patch in patches {
        let target_pc = *block_pcs
            .get(patch.target.0)
            .ok_or_else(|| LowerError::new("invalid jump target block id"))?;
        let Some(slot) = code.get_mut(patch.instr_index) else {
            return Err(LowerError::new("invalid jump patch pc"));
        };
        let Instruction::Jump { target_pc: pc } = slot else {
            return Err(LowerError::new("internal error: jump patch points at non-jump opcode"));
        };
        *pc = target_pc;
    }

    Ok(Function {
        name: mir_func.name.clone(),
        reg_count: temps.next,
        param_count: mir_func
            .params
            .len()
            .try_into()
            .map_err(|_| LowerError::new("param count overflow"))?,
        code,
    })
}

fn lower_mir_instruction(
    mir_module: &rusk_mir::Module,
    mir_func: &rusk_mir::Function,
    instr: &rusk_mir::Instruction,
    host_id_map: &[Option<HostImportId>],
    external_effect_ids: &BTreeMap<(String, String), crate::EffectId>,
    temps: &mut TempAlloc,
    code: &mut Vec<Instruction>,
) -> Result<(), LowerError> {
    use rusk_mir::Instruction as I;

    let local = |l: rusk_mir::Local| -> Reg { l.0 as Reg };

    let op_reg = |op: &Operand, code: &mut Vec<Instruction>, temps: &mut TempAlloc| {
        lower_operand_to_reg(op, code, temps)
    };

    match instr {
        I::Const { dst, value } => {
            let value = lower_const_value(value)?;
            code.push(Instruction::Const {
                dst: local(*dst),
                value,
            });
        }
        I::Copy { dst, src } => {
            code.push(Instruction::Copy {
                dst: local(*dst),
                src: local(*src),
            });
        }
        I::Move { dst, src } => {
            code.push(Instruction::Move {
                dst: local(*dst),
                src: local(*src),
            });
        }

        I::IntAdd { dst, a, b } => {
            let a = op_reg(a, code, temps)?;
            let b = op_reg(b, code, temps)?;
            code.push(Instruction::IntAdd {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntSub { dst, a, b } => {
            let a = op_reg(a, code, temps)?;
            let b = op_reg(b, code, temps)?;
            code.push(Instruction::IntSub {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntMul { dst, a, b } => {
            let a = op_reg(a, code, temps)?;
            let b = op_reg(b, code, temps)?;
            code.push(Instruction::IntMul {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntDiv { dst, a, b } => {
            let a = op_reg(a, code, temps)?;
            let b = op_reg(b, code, temps)?;
            code.push(Instruction::IntDiv {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntMod { dst, a, b } => {
            let a = op_reg(a, code, temps)?;
            let b = op_reg(b, code, temps)?;
            code.push(Instruction::IntMod {
                dst: local(*dst),
                a,
                b,
            });
        }

        I::IntLt { dst, a, b } => {
            let a = op_reg(a, code, temps)?;
            let b = op_reg(b, code, temps)?;
            code.push(Instruction::IntLt {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntLe { dst, a, b } => {
            let a = op_reg(a, code, temps)?;
            let b = op_reg(b, code, temps)?;
            code.push(Instruction::IntLe {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntGt { dst, a, b } => {
            let a = op_reg(a, code, temps)?;
            let b = op_reg(b, code, temps)?;
            code.push(Instruction::IntGt {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntGe { dst, a, b } => {
            let a = op_reg(a, code, temps)?;
            let b = op_reg(b, code, temps)?;
            code.push(Instruction::IntGe {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntEq { dst, a, b } => {
            let a = op_reg(a, code, temps)?;
            let b = op_reg(b, code, temps)?;
            code.push(Instruction::IntEq {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntNe { dst, a, b } => {
            let a = op_reg(a, code, temps)?;
            let b = op_reg(b, code, temps)?;
            code.push(Instruction::IntNe {
                dst: local(*dst),
                a,
                b,
            });
        }

        I::BoolNot { dst, v } => {
            let v = op_reg(v, code, temps)?;
            code.push(Instruction::BoolNot {
                dst: local(*dst),
                v,
            });
        }
        I::BoolEq { dst, a, b } => {
            let a = op_reg(a, code, temps)?;
            let b = op_reg(b, code, temps)?;
            code.push(Instruction::BoolEq {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::BoolNe { dst, a, b } => {
            let a = op_reg(a, code, temps)?;
            let b = op_reg(b, code, temps)?;
            code.push(Instruction::BoolNe {
                dst: local(*dst),
                a,
                b,
            });
        }

        I::CallId { dst, func, args } => {
            let mut bc_args = Vec::with_capacity(args.len());
            for arg in args {
                bc_args.push(op_reg(arg, code, temps)?);
            }

            let func = match func {
                MirCallTarget::Mir(fid) => CallTarget::Bc(FunctionId(fid.0)),
                MirCallTarget::Host(hid) => {
                    let idx = hid.0 as usize;
                    let Some(bc_id) = host_id_map.get(idx).and_then(|v| *v) else {
                        let name = mir_module
                            .host_import(*hid)
                            .map(|h| h.name.as_str())
                            .unwrap_or("<unknown>");
                        return Err(LowerError::new(format!(
                            "host import `{name}` is not ABI-safe for bytecode v0"
                        )));
                    };
                    CallTarget::Host(bc_id)
                }
            };

            code.push(Instruction::Call {
                dst: dst.map(local),
                func,
                args: bc_args,
            });
        }

        I::Call { dst, func, args } => {
            let mut bc_args = Vec::with_capacity(args.len());
            for arg in args {
                bc_args.push(op_reg(arg, code, temps)?);
            }

            let func = if let Some(fid) = mir_module.function_id(func) {
                CallTarget::Bc(FunctionId(fid.0))
            } else if let Some(hid) = mir_module.host_import_id(func) {
                let idx = hid.0 as usize;
                let Some(bc_id) = host_id_map.get(idx).and_then(|v| *v) else {
                    return Err(LowerError::new(format!(
                        "host import `{func}` is not ABI-safe for bytecode v0"
                    )));
                };
                CallTarget::Host(bc_id)
            } else {
                return Err(LowerError::new(format!(
                    "unresolved call target `{func}`"
                )));
            };

            code.push(Instruction::Call {
                dst: dst.map(local),
                func,
                args: bc_args,
            });
        }

        I::Perform { dst, effect, args } => {
            if !effect.interface_args.is_empty() {
                return Err(LowerError::new(
                    "bytecode v0 does not yet support generic interface args for external effects",
                ));
            }

            let key = (effect.interface.clone(), effect.method.clone());
            let Some(effect_id) = external_effect_ids.get(&key).copied() else {
                // Not externalized (yet): lower as an immediate trap at runtime.
                code.push(Instruction::Trap {
                    message: format!(
                        "unhandled effect (not externalized): {}.{}",
                        effect.interface, effect.method
                    ),
                });
                return Ok(());
            };

            let mut bc_args = Vec::with_capacity(args.len());
            for arg in args {
                bc_args.push(op_reg(arg, code, temps)?);
            }

            code.push(Instruction::Perform {
                dst: dst.map(local),
                effect_id,
                args: bc_args,
            });
        }

        other => {
            return Err(LowerError::new(format!(
                "unsupported MIR instruction in v0 bytecode lowering: {other:?}"
            )));
        }
    }

    let _ = mir_func;
    let _ = external_effect_ids;
    Ok(())
}

fn lower_mir_terminator(
    mir_module: &rusk_mir::Module,
    mir_func: &rusk_mir::Function,
    term: &rusk_mir::Terminator,
    host_id_map: &[Option<HostImportId>],
    temps: &mut TempAlloc,
    code: &mut Vec<Instruction>,
    patches: &mut Vec<JumpPatch>,
) -> Result<(), LowerError> {
    use rusk_mir::Terminator as T;

    let op_reg = |op: &Operand, code: &mut Vec<Instruction>, temps: &mut TempAlloc| {
        lower_operand_to_reg(op, code, temps)
    };

    match term {
        T::Return { value } => {
            let value = op_reg(value, code, temps)?;
            code.push(Instruction::Return { value });
        }
        T::Trap { message } => {
            code.push(Instruction::Trap {
                message: message.clone(),
            });
        }
        T::Br { target, args } => {
            let params = &mir_func
                .blocks
                .get(target.0)
                .ok_or_else(|| LowerError::new("invalid br target block id"))?
                .params;
            emit_branch_arg_copies(mir_module, params, args, host_id_map, temps, code)?;
            let instr_index = code.len();
            code.push(Instruction::Jump { target_pc: 0 });
            patches.push(JumpPatch {
                instr_index,
                target: *target,
            });
        }
        T::CondBr {
            cond,
            then_target,
            then_args,
            else_target,
            else_args,
        } => {
            let cond_reg = op_reg(cond, code, temps)?;

            let jumpif_index = code.len();
            code.push(Instruction::JumpIf {
                cond: cond_reg,
                then_pc: 0,
                else_pc: 0,
            });

            let then_edge_pc: u32 = code
                .len()
                .try_into()
                .map_err(|_| LowerError::new("function too large (pc overflow)"))?;
            {
                let params = &mir_func
                    .blocks
                    .get(then_target.0)
                    .ok_or_else(|| LowerError::new("invalid condbr then target block id"))?
                    .params;
                emit_branch_arg_copies(mir_module, params, then_args, host_id_map, temps, code)?;
                let instr_index = code.len();
                code.push(Instruction::Jump { target_pc: 0 });
                patches.push(JumpPatch {
                    instr_index,
                    target: *then_target,
                });
            }

            let else_edge_pc: u32 = code
                .len()
                .try_into()
                .map_err(|_| LowerError::new("function too large (pc overflow)"))?;
            {
                let params = &mir_func
                    .blocks
                    .get(else_target.0)
                    .ok_or_else(|| LowerError::new("invalid condbr else target block id"))?
                    .params;
                emit_branch_arg_copies(mir_module, params, else_args, host_id_map, temps, code)?;
                let instr_index = code.len();
                code.push(Instruction::Jump { target_pc: 0 });
                patches.push(JumpPatch {
                    instr_index,
                    target: *else_target,
                });
            }

            code[jumpif_index] = Instruction::JumpIf {
                cond: cond_reg,
                then_pc: then_edge_pc,
                else_pc: else_edge_pc,
            };
        }

        other => {
            return Err(LowerError::new(format!(
                "unsupported MIR terminator in v0 bytecode lowering: {other:?}"
            )));
        }
    }

    let _ = host_id_map;
    Ok(())
}

fn emit_branch_arg_copies(
    mir_module: &rusk_mir::Module,
    params: &[rusk_mir::Local],
    args: &[Operand],
    host_id_map: &[Option<HostImportId>],
    temps: &mut TempAlloc,
    code: &mut Vec<Instruction>,
) -> Result<(), LowerError> {
    if params.len() != args.len() {
        return Err(LowerError::new(format!(
            "branch arg arity mismatch: target expects {} args but got {}",
            params.len(),
            args.len()
        )));
    }

    // Pre-lower operands to registers (literals become fresh temps).
    let mut moves: Vec<(Reg, Reg)> = Vec::with_capacity(params.len());
    for (dst_local, arg) in params.iter().copied().zip(args.iter()) {
        let dst = dst_local.0 as Reg;
        let src = lower_operand_to_reg(arg, code, temps)?;
        moves.push((dst, src));
    }

    // Destinations must be unique for parallel assignment to be well-defined.
    let mut seen = BTreeSet::new();
    for (dst, _src) in &moves {
        if !seen.insert(*dst) {
            return Err(LowerError::new("branch target block params contain duplicates"));
        }
    }

    emit_parallel_copies(moves, temps, code);
    let _ = mir_module;
    let _ = host_id_map;
    Ok(())
}

fn emit_parallel_copies(mut moves: Vec<(Reg, Reg)>, temps: &mut TempAlloc, code: &mut Vec<Instruction>) {
    moves.retain(|(dst, src)| dst != src);
    if moves.is_empty() {
        return;
    }

    // Remaining-use count for each source register.
    let mut remaining_uses: BTreeMap<Reg, usize> = BTreeMap::new();
    for (_dst, src) in &moves {
        *remaining_uses.entry(*src).or_insert(0) += 1;
    }

    // Mapping from a register to a temp holding its original value (saved before overwrite).
    let mut saved: BTreeMap<Reg, Reg> = BTreeMap::new();

    for (dst, src) in moves {
        // If we're about to overwrite `dst` but its current value is still needed as a source for
        // some *remaining* move, preserve it first.
        if remaining_uses.get(&dst).copied().unwrap_or(0) > 0 && !saved.contains_key(&dst) {
            let temp = temps.alloc();
            code.push(Instruction::Copy { dst: temp, src: dst });
            saved.insert(dst, temp);
        }

        let actual_src = saved.get(&src).copied().unwrap_or(src);
        code.push(Instruction::Copy {
            dst,
            src: actual_src,
        });

        if let Some(n) = remaining_uses.get_mut(&src) {
            *n = n.saturating_sub(1);
            if *n == 0 {
                remaining_uses.remove(&src);
            }
        }
    }
}

fn lower_operand_to_reg(
    op: &Operand,
    code: &mut Vec<Instruction>,
    temps: &mut TempAlloc,
) -> Result<Reg, LowerError> {
    match op {
        Operand::Local(l) => Ok(l.0 as Reg),
        Operand::Literal(v) => {
            let dst = temps.alloc();
            let value = lower_const_value(v)?;
            code.push(Instruction::Const { dst, value });
            Ok(dst)
        }
    }
}

fn lower_const_value(v: &MirConstValue) -> Result<ConstValue, LowerError> {
    Ok(match v {
        MirConstValue::Unit => ConstValue::Unit,
        MirConstValue::Bool(b) => ConstValue::Bool(*b),
        MirConstValue::Int(n) => ConstValue::Int(*n),
        MirConstValue::Float(x) => ConstValue::Float(*x),
        MirConstValue::String(s) => ConstValue::String(s.clone()),
        MirConstValue::Bytes(b) => ConstValue::Bytes(b.clone()),

        MirConstValue::TypeRep(_)
        | MirConstValue::Function(_)
        | MirConstValue::FunctionId(_)
        | MirConstValue::Array(_)
        | MirConstValue::Tuple(_)
        | MirConstValue::Struct { .. }
        | MirConstValue::Enum { .. } => {
            return Err(LowerError::new(format!(
                "unsupported constant in bytecode v0: {v:?}"
            )));
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rusk_mir::{
        BasicBlock, CallTarget, ConstValue, Function, HostFnSig, HostImport, HostType, Instruction,
        Local, Module, Mutability, Operand, Param, Terminator,
    };

    #[test]
    fn lower_const_and_return() {
        let mut module = Module::default();
        module
            .add_function(Function {
                name: "main".to_string(),
                params: vec![],
                ret_type: None,
                locals: 1,
                blocks: vec![BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![Instruction::Const {
                        dst: Local(0),
                        value: ConstValue::Int(42),
                    }],
                    terminator: Terminator::Return {
                        value: Operand::Local(Local(0)),
                    },
                }],
            })
            .unwrap();

        let bc = lower_mir_module(&module).expect("lower");
        assert_eq!(bc.entry, FunctionId(0));
        let main = bc.function(bc.entry).expect("main exists");
        assert_eq!(main.reg_count, 1);
        assert_eq!(
            main.code,
            vec![
                crate::Instruction::Const {
                    dst: 0,
                    value: crate::ConstValue::Int(42)
                },
                crate::Instruction::Return { value: 0 }
            ]
        );
    }

    #[test]
    fn lower_simple_branch_with_block_args_parallel_move() {
        let mut module = Module::default();
        module
            .add_function(Function {
                name: "main".to_string(),
                params: vec![
                    Param {
                        local: Local(0),
                        mutability: Mutability::Readonly,
                        ty: None,
                    },
                    Param {
                        local: Local(1),
                        mutability: Mutability::Readonly,
                        ty: None,
                    },
                ],
                ret_type: None,
                locals: 2,
                blocks: vec![
                    BasicBlock {
                        label: "block0".to_string(),
                        params: vec![],
                        instructions: vec![],
                        terminator: Terminator::Br {
                            target: BlockId(1),
                            args: vec![Operand::Local(Local(1)), Operand::Local(Local(0))],
                        },
                    },
                    BasicBlock {
                        label: "block1".to_string(),
                        params: vec![Local(0), Local(1)],
                        instructions: vec![],
                        terminator: Terminator::Return {
                            value: Operand::Local(Local(0)),
                        },
                    },
                ],
            })
            .unwrap();

        let bc = lower_mir_module(&module).expect("lower");
        let main = bc.function(bc.entry).expect("main exists");

        // Need at least one temp register to preserve values during the swap.
        assert_eq!(main.reg_count, 3);
        assert!(
            main.code
                .iter()
                .any(|i| matches!(i, crate::Instruction::Copy { dst: 2, .. })),
            "expected parallel move lowering to use a temp register"
        );
    }

    #[test]
    fn reject_non_abi_safe_host_import_signature() {
        let mut module = Module::default();
        let hid = module
            .add_host_import(HostImport {
                name: "bad".to_string(),
                sig: HostFnSig {
                    params: vec![HostType::TypeRep],
                    ret: HostType::Unit,
                },
            })
            .unwrap();

        module
            .add_function(Function {
                name: "main".to_string(),
                params: vec![],
                ret_type: None,
                locals: 0,
                blocks: vec![BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![Instruction::CallId {
                        dst: None,
                        func: CallTarget::Host(hid),
                        args: vec![],
                    }],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                }],
            })
            .unwrap();

        let err = lower_mir_module(&module).expect_err("expected error");
        assert!(err.message.contains("not ABI-safe"), "{err}");
    }
}
