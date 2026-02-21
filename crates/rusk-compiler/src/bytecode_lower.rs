//! MIR -> bytecode lowering.
//!
//! Note: this is a compiler-internal module; bytecode execution does not depend on MIR.

#![forbid(unsafe_code)]

extern crate alloc;

use alloc::boxed::Box;
use alloc::collections::{BTreeMap, BTreeSet};
use alloc::string::String;
use alloc::vec::Vec;

use rusk_bytecode::{
    AbiType, CallTarget, ConstValue, EffectId, EffectSpec, ExecutableModule, ExternalEffectDecl,
    Function, FunctionId, HandlerClause, HostFnSig, HostImport, HostImportId, Instruction,
    Intrinsic, Pattern, Reg, SwitchCase, TypeRepLit,
};
use rusk_mir::{BlockId, CallTarget as MirCallTarget, ConstValue as MirConstValue, Operand};

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

fn abi_type_from_host_type(ty: &rusk_mir::HostType) -> Option<AbiType> {
    use rusk_mir::HostType as H;

    match ty {
        H::Unit => Some(AbiType::Unit),
        H::Bool => Some(AbiType::Bool),
        H::Int => Some(AbiType::Int),
        H::Float => Some(AbiType::Float),
        H::String => Some(AbiType::String),
        H::Bytes => Some(AbiType::Bytes),
        H::Cont { .. } => Some(AbiType::Continuation),
        H::Any | H::TypeRep | H::Array(_) | H::Tuple(_) => None,
    }
}

fn abi_sig_from_host_sig(sig: &rusk_mir::HostFnSig) -> Option<HostFnSig> {
    let mut params = Vec::with_capacity(sig.params.len());
    for ty in &sig.params {
        params.push(abi_type_from_host_type(ty)?);
    }
    let ret = abi_type_from_host_type(&sig.ret)?;
    Some(HostFnSig { params, ret })
}

fn lower_type_rep_lit(
    out: &mut ExecutableModule,
    lit: &rusk_mir::TypeRepLit,
) -> Result<TypeRepLit, LowerError> {
    Ok(match lit {
        rusk_mir::TypeRepLit::Unit => TypeRepLit::Unit,
        rusk_mir::TypeRepLit::Never => TypeRepLit::Never,
        rusk_mir::TypeRepLit::Bool => TypeRepLit::Bool,
        rusk_mir::TypeRepLit::Int => TypeRepLit::Int,
        rusk_mir::TypeRepLit::Float => TypeRepLit::Float,
        rusk_mir::TypeRepLit::Byte => TypeRepLit::Byte,
        rusk_mir::TypeRepLit::Char => TypeRepLit::Char,
        rusk_mir::TypeRepLit::String => TypeRepLit::String,
        rusk_mir::TypeRepLit::Bytes => TypeRepLit::Bytes,
        rusk_mir::TypeRepLit::Array => TypeRepLit::Array,
        rusk_mir::TypeRepLit::Tuple(arity) => TypeRepLit::Tuple(*arity),
        rusk_mir::TypeRepLit::Struct(name) => {
            out.intern_type(name.clone()).map_err(LowerError::new)?;
            TypeRepLit::Struct(name.clone())
        }
        rusk_mir::TypeRepLit::Enum(name) => {
            out.intern_type(name.clone()).map_err(LowerError::new)?;
            TypeRepLit::Enum(name.clone())
        }
        rusk_mir::TypeRepLit::Interface(name) => {
            out.intern_type(name.clone()).map_err(LowerError::new)?;
            TypeRepLit::Interface(name.clone())
        }
        rusk_mir::TypeRepLit::Fn => TypeRepLit::Fn,
        rusk_mir::TypeRepLit::Cont => TypeRepLit::Cont,
    })
}

fn lower_pattern(
    out: &mut ExecutableModule,
    mir: &rusk_mir::Module,
    pat: &rusk_mir::Pattern,
) -> Result<Pattern, LowerError> {
    use rusk_mir::Pattern as P;

    Ok(match pat {
        P::Wildcard => Pattern::Wildcard,
        P::Bind => Pattern::Bind,
        P::Literal(lit) => Pattern::Literal(lower_pattern_lit(out, mir, lit)?),
        P::Tuple {
            prefix,
            rest,
            suffix,
        } => Pattern::Tuple {
            prefix: prefix
                .iter()
                .map(|p| lower_pattern(out, mir, p))
                .collect::<Result<Vec<_>, _>>()?,
            rest: match rest {
                None => None,
                Some(p) => Some(Box::new(lower_pattern(out, mir, p)?)),
            },
            suffix: suffix
                .iter()
                .map(|p| lower_pattern(out, mir, p))
                .collect::<Result<Vec<_>, _>>()?,
        },
        P::Enum {
            enum_name,
            variant,
            fields,
        } => Pattern::Enum {
            enum_name: enum_name.clone(),
            variant: variant.clone(),
            fields: fields
                .iter()
                .map(|p| lower_pattern(out, mir, p))
                .collect::<Result<Vec<_>, _>>()?,
        },
        P::Struct { type_name, fields } => Pattern::Struct {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(name, p)| Ok((name.clone(), lower_pattern(out, mir, p)?)))
                .collect::<Result<Vec<_>, _>>()?,
        },
        P::Array {
            prefix,
            rest,
            suffix,
        } => Pattern::Array {
            prefix: prefix
                .iter()
                .map(|p| lower_pattern(out, mir, p))
                .collect::<Result<Vec<_>, _>>()?,
            rest: match rest {
                None => None,
                Some(p) => Some(Box::new(lower_pattern(out, mir, p)?)),
            },
            suffix: suffix
                .iter()
                .map(|p| lower_pattern(out, mir, p))
                .collect::<Result<Vec<_>, _>>()?,
        },
    })
}

fn lower_pattern_lit(
    out: &mut ExecutableModule,
    mir: &rusk_mir::Module,
    lit: &rusk_mir::ConstValue,
) -> Result<ConstValue, LowerError> {
    match lit {
        rusk_mir::ConstValue::Unit => Ok(ConstValue::Unit),
        rusk_mir::ConstValue::Bool(b) => Ok(ConstValue::Bool(*b)),
        rusk_mir::ConstValue::Int(n) => Ok(ConstValue::Int(*n)),
        rusk_mir::ConstValue::Float(x) => Ok(ConstValue::Float(*x)),
        rusk_mir::ConstValue::String(s) => Ok(ConstValue::String(s.clone())),
        rusk_mir::ConstValue::Bytes(b) => Ok(ConstValue::Bytes(b.clone())),
        rusk_mir::ConstValue::TypeRep(rep) => {
            Ok(ConstValue::TypeRep(lower_type_rep_lit(out, rep)?))
        }
        rusk_mir::ConstValue::Function(name) => {
            let Some(id) = mir.function_id(name.as_str()) else {
                return Err(LowerError::new(format!(
                    "pattern literal refers to unknown function `{name}`"
                )));
            };
            Ok(ConstValue::Function(FunctionId(id.0)))
        }
        rusk_mir::ConstValue::FunctionId(id) => Ok(ConstValue::Function(FunctionId(id.0))),
        other => Err(LowerError::new(format!(
            "unsupported pattern literal value: {other:?}"
        ))),
    }
}

fn core_intrinsic(name: &str) -> Option<Intrinsic> {
    Some(match name {
        "core::intrinsics::string_concat" => Intrinsic::StringConcat,
        "core::intrinsics::to_string" => Intrinsic::ToString,
        "core::intrinsics::panic" => Intrinsic::Panic,

        "core::intrinsics::bool_not" => Intrinsic::BoolNot,
        "core::intrinsics::bool_eq" => Intrinsic::BoolEq,
        "core::intrinsics::bool_ne" => Intrinsic::BoolNe,

        "core::intrinsics::int_add" => Intrinsic::IntAdd,
        "core::intrinsics::int_sub" => Intrinsic::IntSub,
        "core::intrinsics::int_mul" => Intrinsic::IntMul,
        "core::intrinsics::int_div" => Intrinsic::IntDiv,
        "core::intrinsics::int_mod" => Intrinsic::IntMod,
        "core::intrinsics::int_and" => Intrinsic::IntAnd,
        "core::intrinsics::int_or" => Intrinsic::IntOr,
        "core::intrinsics::int_xor" => Intrinsic::IntXor,
        "core::intrinsics::int_not" => Intrinsic::IntNot,
        "core::intrinsics::int_shl" => Intrinsic::IntShl,
        "core::intrinsics::int_shr" => Intrinsic::IntShr,
        "core::intrinsics::int_ushr" => Intrinsic::IntUShr,
        "core::intrinsics::int_eq" => Intrinsic::IntEq,
        "core::intrinsics::int_ne" => Intrinsic::IntNe,
        "core::intrinsics::int_lt" => Intrinsic::IntLt,
        "core::intrinsics::int_le" => Intrinsic::IntLe,
        "core::intrinsics::int_gt" => Intrinsic::IntGt,
        "core::intrinsics::int_ge" => Intrinsic::IntGe,

        "core::intrinsics::float_add" => Intrinsic::FloatAdd,
        "core::intrinsics::float_sub" => Intrinsic::FloatSub,
        "core::intrinsics::float_mul" => Intrinsic::FloatMul,
        "core::intrinsics::float_div" => Intrinsic::FloatDiv,
        "core::intrinsics::float_mod" => Intrinsic::FloatMod,
        "core::intrinsics::float_eq" => Intrinsic::FloatEq,
        "core::intrinsics::float_ne" => Intrinsic::FloatNe,
        "core::intrinsics::float_lt" => Intrinsic::FloatLt,
        "core::intrinsics::float_le" => Intrinsic::FloatLe,
        "core::intrinsics::float_gt" => Intrinsic::FloatGt,
        "core::intrinsics::float_ge" => Intrinsic::FloatGe,

        "core::intrinsics::string_eq" => Intrinsic::StringEq,
        "core::intrinsics::string_ne" => Intrinsic::StringNe,
        "core::intrinsics::bytes_eq" => Intrinsic::BytesEq,
        "core::intrinsics::bytes_ne" => Intrinsic::BytesNe,
        "core::intrinsics::unit_eq" => Intrinsic::UnitEq,
        "core::intrinsics::unit_ne" => Intrinsic::UnitNe,

        "core::intrinsics::array_len" => Intrinsic::ArrayLen,
        "core::intrinsics::array_len_ro" => Intrinsic::ArrayLenRo,
        "core::intrinsics::array_push" => Intrinsic::ArrayPush,
        "core::intrinsics::array_pop" => Intrinsic::ArrayPop,
        "core::intrinsics::array_clear" => Intrinsic::ArrayClear,
        "core::intrinsics::array_resize" => Intrinsic::ArrayResize,
        "core::intrinsics::array_insert" => Intrinsic::ArrayInsert,
        "core::intrinsics::array_remove" => Intrinsic::ArrayRemove,
        "core::intrinsics::array_extend" => Intrinsic::ArrayExtend,
        "core::intrinsics::array_concat" => Intrinsic::ArrayConcat,
        "core::intrinsics::array_concat_ro" => Intrinsic::ArrayConcatRo,
        "core::intrinsics::array_slice" => Intrinsic::ArraySlice,
        "core::intrinsics::array_slice_ro" => Intrinsic::ArraySliceRo,

        "core::intrinsics::int_to_byte" => Intrinsic::IntToByte,
        "core::intrinsics::int_try_byte" => Intrinsic::IntTryByte,
        "core::intrinsics::byte_to_int" => Intrinsic::ByteToInt,
        "core::intrinsics::byte_and" => Intrinsic::ByteAnd,
        "core::intrinsics::byte_or" => Intrinsic::ByteOr,
        "core::intrinsics::byte_xor" => Intrinsic::ByteXor,
        "core::intrinsics::byte_not" => Intrinsic::ByteNot,
        "core::intrinsics::byte_shl" => Intrinsic::ByteShl,
        "core::intrinsics::byte_shr" => Intrinsic::ByteShr,
        "core::intrinsics::byte_ushr" => Intrinsic::ByteUShr,

        "core::intrinsics::int_to_char" => Intrinsic::IntToChar,
        "core::intrinsics::int_try_char" => Intrinsic::IntTryChar,
        "core::intrinsics::char_to_int" => Intrinsic::CharToInt,

        "core::intrinsics::bytes_get" => Intrinsic::BytesGet,
        "core::intrinsics::bytes_len" => Intrinsic::BytesLen,
        "core::intrinsics::bytes_slice" => Intrinsic::BytesSlice,
        "core::intrinsics::bytes_to_array" => Intrinsic::BytesToArray,
        "core::intrinsics::bytes_from_array" => Intrinsic::BytesFromArray,

        "core::intrinsics::string_slice" => Intrinsic::StringSlice,
        "core::intrinsics::string_byte_slice" => Intrinsic::StringByteSlice,
        "core::intrinsics::string_next_index" => Intrinsic::StringNextIndex,
        "core::intrinsics::string_codepoint_at" => Intrinsic::StringCodepointAt,
        "core::intrinsics::string_from_chars" => Intrinsic::StringFromChars,
        "core::intrinsics::string_from_utf8" => Intrinsic::StringFromUtf8,
        "core::intrinsics::string_from_utf8_strict" => Intrinsic::StringFromUtf8Strict,
        "core::intrinsics::string_from_utf16_le" => Intrinsic::StringFromUtf16Le,
        "core::intrinsics::string_from_utf16_le_strict" => Intrinsic::StringFromUtf16LeStrict,
        "core::intrinsics::string_from_utf16_be" => Intrinsic::StringFromUtf16Be,
        "core::intrinsics::string_from_utf16_be_strict" => Intrinsic::StringFromUtf16BeStrict,

        "core::intrinsics::hash_int" => Intrinsic::HashInt,
        "core::intrinsics::hash_string" => Intrinsic::HashString,
        "core::intrinsics::hash_bytes" => Intrinsic::HashBytes,
        "core::intrinsics::hash_combine" => Intrinsic::HashCombine,

        _ => return None,
    })
}

#[cfg(test)]
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
        if core_intrinsic(import.name.as_str()).is_some() {
            continue;
        }
        let Some(sig) = abi_sig_from_host_sig(&import.sig) else {
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
        let generic_param_count: u32 = func
            .params
            .iter()
            .take_while(|p| matches!(p.ty, Some(rusk_mir::Type::TypeRep)))
            .count()
            .try_into()
            .map_err(|_| LowerError::new("generic param count overflow"))?;

        let bc_func = lower_mir_function(&mut out, mir, func, &host_id_map, &external_effect_ids)?;
        let id = out.add_function(bc_func).map_err(LowerError::new)?;
        let expect = FunctionId(idx as u32);
        if id != expect {
            return Err(LowerError::new(format!(
                "internal error: function id mismatch (got {}, expected {})",
                id.0, expect.0
            )));
        }
        let Some(slot) = out.function_generic_params.get_mut(idx) else {
            return Err(LowerError::new(
                "internal error: function generic param table mismatch",
            ));
        };
        *slot = generic_param_count;
    }

    let Some(main_id) = mir.function_id("main") else {
        return Err(LowerError::new("missing required entry function `main`"));
    };
    out.entry = FunctionId(main_id.0);

    // Populate dynamic dispatch tables + runtime type metadata.
    for ((ty, method), id) in &mir.methods {
        let type_id = out.intern_type(ty.clone()).map_err(LowerError::new)?;
        let method_id = out.intern_method(method.clone()).map_err(LowerError::new)?;
        out.add_vcall_entry(type_id, method_id, FunctionId(id.0))
            .map_err(LowerError::new)?;
    }

    for (ty, ifaces) in &mir.interface_impls {
        let type_id = out.intern_type(ty.clone()).map_err(LowerError::new)?;
        let mut ids = Vec::with_capacity(ifaces.len());
        for iface in ifaces {
            ids.push(out.intern_type(iface.clone()).map_err(LowerError::new)?);
        }
        out.set_interface_impls(type_id, ids)
            .map_err(LowerError::new)?;
    }

    for (ty, fields) in &mir.struct_layouts {
        let type_id = out.intern_type(ty.clone()).map_err(LowerError::new)?;
        out.set_struct_layout(type_id, fields.clone())
            .map_err(LowerError::new)?;
    }

    for ((ty, iface, assoc), id) in &mir.assoc_type_reps {
        let type_id = out.intern_type(ty.clone()).map_err(LowerError::new)?;
        let iface_id = out.intern_type(iface.clone()).map_err(LowerError::new)?;
        out.add_assoc_type_entry(type_id, iface_id, assoc.clone(), FunctionId(id.0))
            .map_err(LowerError::new)?;
    }

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
enum PcPatch {
    Jump {
        instr_index: usize,
        target: BlockId,
    },
    SwitchCase {
        instr_index: usize,
        case_index: usize,
        target: BlockId,
    },
    SwitchDefault {
        instr_index: usize,
        target: BlockId,
    },
    HandlerClause {
        instr_index: usize,
        clause_index: usize,
        target: BlockId,
    },
}

fn lower_mir_function(
    out: &mut ExecutableModule,
    mir_module: &rusk_mir::Module,
    mir_func: &rusk_mir::Function,
    host_id_map: &[Option<HostImportId>],
    external_effect_ids: &BTreeMap<(String, String), EffectId>,
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

    let mut block_pcs: Vec<u32> = vec![0; mir_func.blocks.len()];

    let mut code: Vec<Instruction> = Vec::new();
    let mut patches: Vec<PcPatch> = Vec::new();

    for (block_idx, block) in mir_func.blocks.iter().enumerate() {
        block_pcs[block_idx] = code
            .len()
            .try_into()
            .map_err(|_| LowerError::new("function too large (pc overflow)"))?;

        for instr in &block.instructions {
            lower_mir_instruction(
                out,
                mir_module,
                mir_func,
                instr,
                host_id_map,
                external_effect_ids,
                &mut temps,
                &mut code,
                &mut patches,
            )?;
        }

        lower_mir_terminator(
            out,
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
        match patch {
            PcPatch::Jump {
                instr_index,
                target,
            } => {
                let target_pc = *block_pcs
                    .get(target.0)
                    .ok_or_else(|| LowerError::new("invalid jump target block id"))?;
                let Some(slot) = code.get_mut(instr_index) else {
                    return Err(LowerError::new("invalid jump patch pc"));
                };
                let Instruction::Jump { target_pc: pc } = slot else {
                    return Err(LowerError::new(
                        "internal error: jump patch points at non-jump opcode",
                    ));
                };
                *pc = target_pc;
            }
            PcPatch::SwitchCase {
                instr_index,
                case_index,
                target,
            } => {
                let target_pc = *block_pcs
                    .get(target.0)
                    .ok_or_else(|| LowerError::new("invalid switch target block id"))?;
                let Some(slot) = code.get_mut(instr_index) else {
                    return Err(LowerError::new("invalid switch patch pc"));
                };
                let Instruction::Switch { cases, .. } = slot else {
                    return Err(LowerError::new(
                        "internal error: switch patch points at non-switch opcode",
                    ));
                };
                let Some(case) = cases.get_mut(case_index) else {
                    return Err(LowerError::new("invalid switch case index"));
                };
                case.target_pc = target_pc;
            }
            PcPatch::SwitchDefault {
                instr_index,
                target,
            } => {
                let target_pc = *block_pcs
                    .get(target.0)
                    .ok_or_else(|| LowerError::new("invalid switch default block id"))?;
                let Some(slot) = code.get_mut(instr_index) else {
                    return Err(LowerError::new("invalid switch patch pc"));
                };
                let Instruction::Switch { default_pc, .. } = slot else {
                    return Err(LowerError::new(
                        "internal error: switch patch points at non-switch opcode",
                    ));
                };
                *default_pc = target_pc;
            }
            PcPatch::HandlerClause {
                instr_index,
                clause_index,
                target,
            } => {
                let target_pc = *block_pcs
                    .get(target.0)
                    .ok_or_else(|| LowerError::new("invalid handler target block id"))?;
                let Some(slot) = code.get_mut(instr_index) else {
                    return Err(LowerError::new("invalid handler patch pc"));
                };
                let Instruction::PushHandler { clauses } = slot else {
                    return Err(LowerError::new(
                        "internal error: handler patch points at non-push_handler opcode",
                    ));
                };
                let Some(clause) = clauses.get_mut(clause_index) else {
                    return Err(LowerError::new("invalid handler clause index"));
                };
                clause.target_pc = target_pc;
            }
        }
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

#[allow(clippy::too_many_arguments)]
fn lower_mir_instruction(
    out: &mut ExecutableModule,
    mir_module: &rusk_mir::Module,
    mir_func: &rusk_mir::Function,
    instr: &rusk_mir::Instruction,
    host_id_map: &[Option<HostImportId>],
    external_effect_ids: &BTreeMap<(String, String), EffectId>,
    temps: &mut TempAlloc,
    code: &mut Vec<Instruction>,
    patches: &mut Vec<PcPatch>,
) -> Result<(), LowerError> {
    use rusk_mir::Instruction as I;

    let local = |l: rusk_mir::Local| -> Reg { l.0 as Reg };

    let op_reg =
        |out: &mut ExecutableModule,
         op: &Operand,
         code: &mut Vec<Instruction>,
         temps: &mut TempAlloc| { lower_operand_to_reg(out, mir_module, op, code, temps) };

    match instr {
        I::Const { dst, value } => {
            let value = lower_const_value(out, mir_module, value)?;
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
        I::AsReadonly { dst, src } => {
            code.push(Instruction::AsReadonly {
                dst: local(*dst),
                src: local(*src),
            });
        }
        I::IsType { dst, value, ty } => {
            let value = op_reg(out, value, code, temps)?;
            let ty = op_reg(out, ty, code, temps)?;
            code.push(Instruction::IsType {
                dst: local(*dst),
                value,
                ty,
            });
        }

        I::MakeTypeRep { dst, base, args } => {
            let mut bc_args = Vec::with_capacity(args.len());
            for arg in args {
                bc_args.push(op_reg(out, arg, code, temps)?);
            }
            code.push(Instruction::MakeTypeRep {
                dst: local(*dst),
                base: lower_type_rep_lit(out, base)?,
                args: bc_args,
            });
        }

        I::AssocTypeRep {
            dst,
            recv,
            iface,
            assoc,
        } => {
            let recv = op_reg(out, recv, code, temps)?;
            let iface_type_id = out.intern_type(iface.clone()).map_err(LowerError::new)?;
            code.push(Instruction::AssocTypeRep {
                dst: local(*dst),
                recv,
                iface_type_id,
                assoc: assoc.clone(),
            });
        }

        I::MakeStruct {
            dst,
            type_name,
            type_args,
            fields,
        } => {
            let type_id = out
                .intern_type(type_name.clone())
                .map_err(LowerError::new)?;
            let mut bc_type_args = Vec::with_capacity(type_args.len());
            for arg in type_args {
                bc_type_args.push(op_reg(out, arg, code, temps)?);
            }

            let mut bc_fields = Vec::with_capacity(fields.len());
            for (field, op) in fields {
                bc_fields.push((field.clone(), op_reg(out, op, code, temps)?));
            }

            code.push(Instruction::MakeStruct {
                dst: local(*dst),
                type_id,
                type_args: bc_type_args,
                fields: bc_fields,
            });
        }
        I::MakeArray { dst, items } => {
            let mut bc_items = Vec::with_capacity(items.len());
            for op in items {
                bc_items.push(op_reg(out, op, code, temps)?);
            }
            code.push(Instruction::MakeArray {
                dst: local(*dst),
                items: bc_items,
            });
        }
        I::MakeTuple { dst, items } => {
            let mut bc_items = Vec::with_capacity(items.len());
            for op in items {
                bc_items.push(op_reg(out, op, code, temps)?);
            }
            code.push(Instruction::MakeTuple {
                dst: local(*dst),
                items: bc_items,
            });
        }
        I::MakeEnum {
            dst,
            enum_name,
            type_args,
            variant,
            fields,
        } => {
            let enum_type_id = out
                .intern_type(enum_name.clone())
                .map_err(LowerError::new)?;
            let mut bc_type_args = Vec::with_capacity(type_args.len());
            for arg in type_args {
                bc_type_args.push(op_reg(out, arg, code, temps)?);
            }

            let mut bc_fields = Vec::with_capacity(fields.len());
            for op in fields {
                bc_fields.push(op_reg(out, op, code, temps)?);
            }

            code.push(Instruction::MakeEnum {
                dst: local(*dst),
                enum_type_id,
                type_args: bc_type_args,
                variant: variant.clone(),
                fields: bc_fields,
            });
        }

        I::GetField { dst, obj, field } => {
            let obj = op_reg(out, obj, code, temps)?;
            code.push(Instruction::GetField {
                dst: local(*dst),
                obj,
                field: field.clone(),
            });
        }
        I::SetField { obj, field, value } => {
            let obj = op_reg(out, obj, code, temps)?;
            let value = op_reg(out, value, code, temps)?;
            code.push(Instruction::SetField {
                obj,
                field: field.clone(),
                value,
            });
        }

        I::StructGet { dst, obj, idx } => {
            let obj = op_reg(out, obj, code, temps)?;
            code.push(Instruction::StructGet {
                dst: local(*dst),
                obj,
                idx: *idx,
            });
        }
        I::StructSet { obj, idx, value } => {
            let obj = op_reg(out, obj, code, temps)?;
            let value = op_reg(out, value, code, temps)?;
            code.push(Instruction::StructSet {
                obj,
                idx: *idx,
                value,
            });
        }

        I::TupleGet { dst, tup, idx } => {
            let tup = op_reg(out, tup, code, temps)?;
            code.push(Instruction::TupleGet {
                dst: local(*dst),
                tup,
                idx: *idx,
            });
        }
        I::TupleSet { tup, idx, value } => {
            let tup = op_reg(out, tup, code, temps)?;
            let value = op_reg(out, value, code, temps)?;
            code.push(Instruction::TupleSet {
                tup,
                idx: *idx,
                value,
            });
        }

        I::IndexGet { dst, arr, idx } => {
            let arr = op_reg(out, arr, code, temps)?;
            let idx = op_reg(out, idx, code, temps)?;
            code.push(Instruction::IndexGet {
                dst: local(*dst),
                arr,
                idx,
            });
        }
        I::IndexSet { arr, idx, value } => {
            let arr = op_reg(out, arr, code, temps)?;
            let idx = op_reg(out, idx, code, temps)?;
            let value = op_reg(out, value, code, temps)?;
            code.push(Instruction::IndexSet { arr, idx, value });
        }
        I::Len { dst, arr } => {
            let arr = op_reg(out, arr, code, temps)?;
            code.push(Instruction::Len {
                dst: local(*dst),
                arr,
            });
        }

        I::IntAdd { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntAdd {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntSub { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntSub {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntMul { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntMul {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntDiv { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntDiv {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntMod { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntMod {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntAnd { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntAnd {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntOr { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntOr {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntXor { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntXor {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntShl { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntShl {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntShr { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntShr {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntUShr { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntUShr {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntNot { dst, v } => {
            let v = op_reg(out, v, code, temps)?;
            code.push(Instruction::IntNot {
                dst: local(*dst),
                v,
            });
        }
        I::ByteAnd { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::ByteAnd {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::ByteOr { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::ByteOr {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::ByteXor { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::ByteXor {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::ByteShl { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::ByteShl {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::ByteShr { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::ByteShr {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::ByteUShr { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::ByteUShr {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::ByteNot { dst, v } => {
            let v = op_reg(out, v, code, temps)?;
            code.push(Instruction::ByteNot {
                dst: local(*dst),
                v,
            });
        }

        I::IntLt { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntLt {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntLe { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntLe {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntGt { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntGt {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntGe { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntGe {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntEq { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntEq {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::IntNe { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::IntNe {
                dst: local(*dst),
                a,
                b,
            });
        }

        I::BoolNot { dst, v } => {
            let v = op_reg(out, v, code, temps)?;
            code.push(Instruction::BoolNot {
                dst: local(*dst),
                v,
            });
        }
        I::BoolEq { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::BoolEq {
                dst: local(*dst),
                a,
                b,
            });
        }
        I::BoolNe { dst, a, b } => {
            let a = op_reg(out, a, code, temps)?;
            let b = op_reg(out, b, code, temps)?;
            code.push(Instruction::BoolNe {
                dst: local(*dst),
                a,
                b,
            });
        }

        I::CallId { dst, func, args } => {
            let mut bc_args = Vec::with_capacity(args.len());
            for arg in args {
                bc_args.push(op_reg(out, arg, code, temps)?);
            }

            let func = match func {
                MirCallTarget::Mir(fid) => CallTarget::Bc(FunctionId(fid.0)),
                MirCallTarget::Host(hid) => {
                    let name = mir_module
                        .host_import(*hid)
                        .map(|h| h.name.as_str())
                        .unwrap_or("<unknown>");
                    if let Some(intr) = core_intrinsic(name) {
                        CallTarget::Intrinsic(intr)
                    } else {
                        let idx = hid.0 as usize;
                        let Some(bc_id) = host_id_map.get(idx).and_then(|v| *v) else {
                            return Err(LowerError::new(format!(
                                "host import `{name}` is not ABI-safe for bytecode v0"
                            )));
                        };
                        CallTarget::Host(bc_id)
                    }
                }
            };

            code.push(Instruction::Call {
                dst: dst.map(local),
                func,
                args: bc_args,
            });
        }

        I::CallIdMulti { dsts, func, args } => {
            let mut bc_args = Vec::with_capacity(args.len());
            for arg in args {
                bc_args.push(op_reg(out, arg, code, temps)?);
            }

            let func = match func {
                MirCallTarget::Mir(fid) => CallTarget::Bc(FunctionId(fid.0)),
                MirCallTarget::Host(hid) => {
                    let name = mir_module
                        .host_import(*hid)
                        .map(|h| h.name.as_str())
                        .unwrap_or("<unknown>");
                    return Err(LowerError::new(format!(
                        "CallIdMulti to host import `{name}` is not supported"
                    )));
                }
            };

            code.push(Instruction::CallMulti {
                dsts: dsts.iter().copied().map(local).collect(),
                func,
                args: bc_args,
            });
        }

        I::Call { dst, func, args } => {
            let mut bc_args = Vec::with_capacity(args.len());
            for arg in args {
                bc_args.push(op_reg(out, arg, code, temps)?);
            }

            let func = if let Some(intr) = core_intrinsic(func.as_str()) {
                CallTarget::Intrinsic(intr)
            } else if let Some(fid) = mir_module.function_id(func) {
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
                return Err(LowerError::new(format!("unresolved call target `{func}`")));
            };

            code.push(Instruction::Call {
                dst: dst.map(local),
                func,
                args: bc_args,
            });
        }

        I::ICall { dst, fnptr, args } => {
            let fnptr = op_reg(out, fnptr, code, temps)?;
            let mut bc_args = Vec::with_capacity(args.len());
            for arg in args {
                bc_args.push(op_reg(out, arg, code, temps)?);
            }
            code.push(Instruction::ICall {
                dst: dst.map(local),
                fnptr,
                args: bc_args,
            });
        }

        I::VCall {
            dst,
            obj,
            method,
            method_type_args,
            args,
        } => {
            let obj = op_reg(out, obj, code, temps)?;
            let method_id = out.intern_method(method.clone()).map_err(LowerError::new)?;

            let mut bc_method_type_args = Vec::with_capacity(method_type_args.len());
            for arg in method_type_args {
                bc_method_type_args.push(op_reg(out, arg, code, temps)?);
            }

            let mut bc_args = Vec::with_capacity(args.len());
            for arg in args {
                bc_args.push(op_reg(out, arg, code, temps)?);
            }

            code.push(Instruction::VCall {
                dst: dst.map(local),
                obj,
                method: method_id,
                method_type_args: bc_method_type_args,
                args: bc_args,
            });
        }

        I::PushHandler {
            handler_id: _,
            clauses,
        } => {
            let mut bc_clauses = Vec::with_capacity(clauses.len());
            for clause in clauses {
                let mut interface_args = Vec::with_capacity(clause.effect.interface_args.len());
                for op in &clause.effect.interface_args {
                    interface_args.push(op_reg(out, op, code, temps)?);
                }

                let bind_count = count_binds_in_patterns(&clause.arg_patterns);
                let params = &mir_func
                    .blocks
                    .get(clause.target.0)
                    .ok_or_else(|| LowerError::new("invalid handler target block id"))?
                    .params;
                let got = params.len();
                let expected_min = bind_count;
                let expected_max = bind_count + 1;
                if got != expected_min && got != expected_max {
                    return Err(LowerError::new(format!(
                        "invalid handler target params for {}.{}: expected {expected_min} or {expected_max}, got {got}",
                        clause.effect.interface, clause.effect.method
                    )));
                }

                let mut bc_arg_patterns = Vec::with_capacity(clause.arg_patterns.len());
                for pat in &clause.arg_patterns {
                    bc_arg_patterns.push(lower_pattern(out, mir_module, pat)?);
                }

                bc_clauses.push(HandlerClause {
                    effect: EffectSpec {
                        interface: clause.effect.interface.clone(),
                        interface_args,
                        method: clause.effect.method.clone(),
                    },
                    arg_patterns: bc_arg_patterns,
                    target_pc: 0,
                    param_regs: params.iter().map(|l| l.0 as Reg).collect(),
                });
            }

            let instr_index = code.len();
            code.push(Instruction::PushHandler {
                clauses: bc_clauses,
            });
            for (clause_index, clause) in clauses.iter().enumerate() {
                patches.push(PcPatch::HandlerClause {
                    instr_index,
                    clause_index,
                    target: clause.target,
                });
            }
        }
        I::PopHandler => {
            code.push(Instruction::PopHandler);
        }

        I::Perform { dst, effect, args } => {
            let mut interface_args = Vec::with_capacity(effect.interface_args.len());
            for op in &effect.interface_args {
                interface_args.push(op_reg(out, op, code, temps)?);
            }
            let mut bc_args = Vec::with_capacity(args.len());
            for arg in args {
                bc_args.push(op_reg(out, arg, code, temps)?);
            }

            code.push(Instruction::Perform {
                dst: dst.map(local),
                effect: EffectSpec {
                    interface: effect.interface.clone(),
                    interface_args,
                    method: effect.method.clone(),
                },
                args: bc_args,
            });
        }

        I::Resume { dst, k, value } => {
            let k = op_reg(out, k, code, temps)?;
            let value = op_reg(out, value, code, temps)?;
            code.push(Instruction::Resume {
                dst: dst.map(local),
                k,
                value,
            });
        }
    }

    let _ = mir_func;
    let _ = external_effect_ids;
    let _ = patches;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn lower_mir_terminator(
    out: &mut ExecutableModule,
    mir_module: &rusk_mir::Module,
    mir_func: &rusk_mir::Function,
    term: &rusk_mir::Terminator,
    host_id_map: &[Option<HostImportId>],
    temps: &mut TempAlloc,
    code: &mut Vec<Instruction>,
    patches: &mut Vec<PcPatch>,
) -> Result<(), LowerError> {
    use rusk_mir::Terminator as T;

    let op_reg =
        |out: &mut ExecutableModule,
         op: &Operand,
         code: &mut Vec<Instruction>,
         temps: &mut TempAlloc| { lower_operand_to_reg(out, mir_module, op, code, temps) };

    match term {
        T::Return { value } => {
            let value = op_reg(out, value, code, temps)?;
            code.push(Instruction::Return { value });
        }
        T::ReturnMulti { values } => {
            let mut regs = Vec::with_capacity(values.len());
            for op in values {
                regs.push(op_reg(out, op, code, temps)?);
            }
            code.push(Instruction::ReturnMulti { values: regs });
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
            emit_branch_arg_copies(out, mir_module, params, args, host_id_map, temps, code)?;
            let instr_index = code.len();
            code.push(Instruction::Jump { target_pc: 0 });
            patches.push(PcPatch::Jump {
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
            let cond_reg = op_reg(out, cond, code, temps)?;

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
                emit_branch_arg_copies(
                    out,
                    mir_module,
                    params,
                    then_args,
                    host_id_map,
                    temps,
                    code,
                )?;
                let instr_index = code.len();
                code.push(Instruction::Jump { target_pc: 0 });
                patches.push(PcPatch::Jump {
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
                emit_branch_arg_copies(
                    out,
                    mir_module,
                    params,
                    else_args,
                    host_id_map,
                    temps,
                    code,
                )?;
                let instr_index = code.len();
                code.push(Instruction::Jump { target_pc: 0 });
                patches.push(PcPatch::Jump {
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

        T::Switch {
            value,
            cases,
            default,
        } => {
            let scrut = op_reg(out, value, code, temps)?;

            let instr_index = code.len();
            let mut bc_cases = Vec::with_capacity(cases.len());
            for (case_index, case) in cases.iter().enumerate() {
                let bind_count = count_binds_in_pattern(&case.pattern);
                let params = &mir_func
                    .blocks
                    .get(case.target.0)
                    .ok_or_else(|| LowerError::new("invalid switch target block id"))?
                    .params;
                if params.len() != bind_count {
                    return Err(LowerError::new(format!(
                        "invalid switch target params for {:?}: expected {bind_count}, got {}",
                        case.target.0,
                        params.len()
                    )));
                }
                let param_regs = params.iter().map(|l| l.0 as Reg).collect::<Vec<_>>();
                bc_cases.push(SwitchCase {
                    pattern: lower_pattern(out, mir_module, &case.pattern)?,
                    target_pc: 0,
                    param_regs,
                });
                patches.push(PcPatch::SwitchCase {
                    instr_index,
                    case_index,
                    target: case.target,
                });
            }

            let default_params = &mir_func
                .blocks
                .get(default.0)
                .ok_or_else(|| LowerError::new("invalid switch default block id"))?
                .params;
            if !default_params.is_empty() {
                return Err(LowerError::new(
                    "switch default block must not take params (no binds)".to_string(),
                ));
            }

            code.push(Instruction::Switch {
                value: scrut,
                cases: bc_cases,
                default_pc: 0,
            });
            patches.push(PcPatch::SwitchDefault {
                instr_index,
                target: *default,
            });
        }
    }

    let _ = host_id_map;
    Ok(())
}

fn count_binds_in_pattern(pat: &rusk_mir::Pattern) -> usize {
    use rusk_mir::Pattern as P;
    match pat {
        P::Wildcard | P::Literal(_) => 0,
        P::Bind => 1,
        P::Enum { fields, .. } => fields.iter().map(count_binds_in_pattern).sum(),
        P::Struct { fields, .. } => fields
            .iter()
            .map(|(_name, pat)| count_binds_in_pattern(pat))
            .sum(),
        P::Tuple {
            prefix,
            rest,
            suffix,
        }
        | P::Array {
            prefix,
            rest,
            suffix,
        } => {
            prefix.iter().map(count_binds_in_pattern).sum::<usize>()
                + rest.as_deref().map(count_binds_in_pattern).unwrap_or(0)
                + suffix.iter().map(count_binds_in_pattern).sum::<usize>()
        }
    }
}

fn count_binds_in_patterns(pats: &[rusk_mir::Pattern]) -> usize {
    pats.iter().map(count_binds_in_pattern).sum()
}

fn emit_branch_arg_copies(
    out: &mut ExecutableModule,
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
        let src = lower_operand_to_reg(out, mir_module, arg, code, temps)?;
        moves.push((dst, src));
    }

    // Destinations must be unique for parallel assignment to be well-defined.
    let mut seen = BTreeSet::new();
    for (dst, _src) in &moves {
        if !seen.insert(*dst) {
            return Err(LowerError::new(
                "branch target block params contain duplicates",
            ));
        }
    }

    emit_parallel_copies(moves, temps, code);
    let _ = mir_module;
    let _ = host_id_map;
    Ok(())
}

fn emit_parallel_copies(
    mut moves: Vec<(Reg, Reg)>,
    temps: &mut TempAlloc,
    code: &mut Vec<Instruction>,
) {
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
            code.push(Instruction::Copy {
                dst: temp,
                src: dst,
            });
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
    out: &mut ExecutableModule,
    mir_module: &rusk_mir::Module,
    op: &Operand,
    code: &mut Vec<Instruction>,
    temps: &mut TempAlloc,
) -> Result<Reg, LowerError> {
    match op {
        Operand::Local(l) => Ok(l.0 as Reg),
        Operand::Literal(v) => {
            let dst = temps.alloc();
            let value = lower_const_value(out, mir_module, v)?;
            code.push(Instruction::Const { dst, value });
            Ok(dst)
        }
    }
}

fn lower_const_value(
    out: &mut ExecutableModule,
    mir_module: &rusk_mir::Module,
    v: &MirConstValue,
) -> Result<ConstValue, LowerError> {
    Ok(match v {
        MirConstValue::Unit => ConstValue::Unit,
        MirConstValue::Bool(b) => ConstValue::Bool(*b),
        MirConstValue::Int(n) => ConstValue::Int(*n),
        MirConstValue::Float(x) => ConstValue::Float(*x),
        MirConstValue::String(s) => ConstValue::String(s.clone()),
        MirConstValue::Bytes(b) => ConstValue::Bytes(b.clone()),
        MirConstValue::TypeRep(rep) => ConstValue::TypeRep(lower_type_rep_lit(out, rep)?),
        MirConstValue::Function(name) => {
            let Some(id) = mir_module.function_id(name.as_str()) else {
                return Err(LowerError::new(format!(
                    "unresolved function constant `{name}`"
                )));
            };
            ConstValue::Function(FunctionId(id.0))
        }
        MirConstValue::FunctionId(id) => ConstValue::Function(FunctionId(id.0)),

        MirConstValue::Array(_)
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
                rusk_bytecode::Instruction::Const {
                    dst: 0,
                    value: rusk_bytecode::ConstValue::Int(42)
                },
                rusk_bytecode::Instruction::Return { value: 0 }
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
                .any(|i| matches!(i, rusk_bytecode::Instruction::Copy { dst: 2, .. })),
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
