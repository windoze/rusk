extern crate alloc;

use alloc::collections::BTreeMap;
use alloc::string::String;

use crate::{CallTarget, EffectSpec, ExecutableModule, Function, FunctionId, Instruction, Reg};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerifyError {
    pub message: String,
}

impl core::fmt::Display for VerifyError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "verify error: {}", self.message)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for VerifyError {}

pub fn verify_module(module: &ExecutableModule) -> Result<(), VerifyError> {
    // Table length consistency.
    if module.function_generic_params.len() != module.functions.len() {
        return Err(VerifyError {
            message: format!(
                "function_generic_params length {} does not match functions length {}",
                module.function_generic_params.len(),
                module.functions.len()
            ),
        });
    }

    // Entry validity.
    if (module.entry.0 as usize) >= module.functions.len() {
        return Err(VerifyError {
            message: format!(
                "entry function id {} out of range (functions={})",
                module.entry.0,
                module.functions.len()
            ),
        });
    }

    // Recompute name -> id maps and ensure they match the stored ones.
    let mut expected_function_ids = BTreeMap::new();
    for (idx, func) in module.functions.iter().enumerate() {
        let id = FunctionId(idx as u32);
        if expected_function_ids
            .insert(func.name.clone(), id)
            .is_some()
        {
            return Err(VerifyError {
                message: format!("duplicate function name `{}`", func.name),
            });
        }
    }
    if expected_function_ids != module.function_ids {
        return Err(VerifyError {
            message: "function_ids map does not match functions table".to_string(),
        });
    }

    let mut expected_host_import_ids = BTreeMap::new();
    for (idx, import) in module.host_imports.iter().enumerate() {
        let id = crate::HostImportId(idx as u32);
        if expected_host_import_ids
            .insert(import.name.clone(), id)
            .is_some()
        {
            return Err(VerifyError {
                message: format!("duplicate host import name `{}`", import.name),
            });
        }
    }
    if expected_host_import_ids != module.host_import_ids {
        return Err(VerifyError {
            message: "host_import_ids map does not match host_imports table".to_string(),
        });
    }

    let mut expected_external_effect_ids = BTreeMap::new();
    for (idx, decl) in module.external_effects.iter().enumerate() {
        let id = crate::EffectId(idx as u32);
        let key = (decl.interface.clone(), decl.method.clone());
        if expected_external_effect_ids.insert(key, id).is_some() {
            return Err(VerifyError {
                message: format!(
                    "duplicate external effect declaration `{}.{}`",
                    decl.interface, decl.method
                ),
            });
        }
    }
    if expected_external_effect_ids != module.external_effect_ids {
        return Err(VerifyError {
            message: "external_effect_ids map does not match external_effects table".to_string(),
        });
    }

    // Method table integrity.
    for ((type_name, method_name), fn_id) in &module.methods {
        if (fn_id.0 as usize) >= module.functions.len() {
            return Err(VerifyError {
                message: format!(
                    "method table entry ({type_name},{method_name}) points to invalid function id {}",
                    fn_id.0
                ),
            });
        }
    }

    // Interface impls are only string metadata; ensure there are no empty names.
    for (type_name, interfaces) in &module.interface_impls {
        if type_name.is_empty() {
            return Err(VerifyError {
                message: "interface_impls contains empty type name".to_string(),
            });
        }
        for iface in interfaces {
            if iface.is_empty() {
                return Err(VerifyError {
                    message: format!("interface_impls for `{type_name}` contains empty interface"),
                });
            }
        }
    }

    // Struct layout indices must be consistent with instruction use; we can only check basic
    // constraints here.
    for (type_name, fields) in &module.struct_layouts {
        if type_name.is_empty() {
            return Err(VerifyError {
                message: "struct_layouts contains empty type name".to_string(),
            });
        }
        for field in fields {
            if field.is_empty() {
                return Err(VerifyError {
                    message: format!("struct_layouts for `{type_name}` contains empty field name"),
                });
            }
        }
    }

    // Function bodies.
    for (fn_idx, func) in module.functions.iter().enumerate() {
        let fn_id = FunctionId(fn_idx as u32);
        verify_function(module, fn_id, func)?;
    }

    Ok(())
}

fn verify_function(
    module: &ExecutableModule,
    fn_id: FunctionId,
    func: &Function,
) -> Result<(), VerifyError> {
    if func.param_count > func.reg_count {
        return Err(VerifyError {
            message: format!(
                "function `{}` has param_count {} > reg_count {}",
                func.name, func.param_count, func.reg_count
            ),
        });
    }

    let code_len_u32: u32 = func.code.len().try_into().map_err(|_| VerifyError {
        message: format!("function `{}` instruction stream too large", func.name),
    })?;

    let reg_count = func.reg_count;
    for (pc, inst) in func.code.iter().enumerate() {
        verify_instruction(
            module,
            func,
            fn_id,
            pc as u32,
            code_len_u32,
            reg_count,
            inst,
        )?;
    }
    Ok(())
}

fn verify_reg(reg_count: u32, reg: Reg, context: &str) -> Result<(), VerifyError> {
    if reg >= reg_count {
        return Err(VerifyError {
            message: format!("{context}: reg {reg} out of range (reg_count={reg_count})"),
        });
    }
    Ok(())
}

fn verify_pc(code_len: u32, pc: u32, context: &str) -> Result<(), VerifyError> {
    if pc >= code_len {
        return Err(VerifyError {
            message: format!("{context}: pc {pc} out of range (code_len={code_len})"),
        });
    }
    Ok(())
}

fn count_pattern_binds(p: &rusk_mir::Pattern) -> usize {
    match p {
        rusk_mir::Pattern::Wildcard => 0,
        rusk_mir::Pattern::Bind => 1,
        rusk_mir::Pattern::Literal(_) => 0,
        rusk_mir::Pattern::Tuple {
            prefix,
            rest,
            suffix,
        } => {
            prefix.iter().map(count_pattern_binds).sum::<usize>()
                + rest.as_deref().map(count_pattern_binds).unwrap_or(0)
                + suffix.iter().map(count_pattern_binds).sum::<usize>()
        }
        rusk_mir::Pattern::Enum { fields, .. } => fields.iter().map(count_pattern_binds).sum(),
        rusk_mir::Pattern::Struct { fields, .. } => {
            fields.iter().map(|(_, pat)| count_pattern_binds(pat)).sum()
        }
        rusk_mir::Pattern::Array {
            prefix,
            rest,
            suffix,
        } => {
            prefix.iter().map(count_pattern_binds).sum::<usize>()
                + rest.as_deref().map(count_pattern_binds).unwrap_or(0)
                + suffix.iter().map(count_pattern_binds).sum::<usize>()
        }
    }
}

fn verify_instruction(
    module: &ExecutableModule,
    func: &Function,
    fn_id: FunctionId,
    pc: u32,
    code_len: u32,
    reg_count: u32,
    inst: &Instruction,
) -> Result<(), VerifyError> {
    let here = || format!("function `{}` pc {pc}", func.name);

    match inst {
        Instruction::Const { dst, .. } => {
            verify_reg(reg_count, *dst, &format!("{}: const dst", here()))?
        }
        Instruction::Copy { dst, src }
        | Instruction::Move { dst, src }
        | Instruction::AsReadonly { dst, src } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_reg(reg_count, *src, &format!("{}: src", here()))?;
        }
        Instruction::IsType { dst, value, ty } | Instruction::CheckedCast { dst, value, ty } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_reg(reg_count, *value, &format!("{}: value", here()))?;
            verify_reg(reg_count, *ty, &format!("{}: ty", here()))?;
        }
        Instruction::MakeTypeRep { dst, args, .. } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            for r in args {
                verify_reg(reg_count, *r, &format!("{}: MakeTypeRep arg", here()))?;
            }
        }
        Instruction::MakeStruct {
            dst,
            type_args,
            fields,
            type_name: _,
        } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            for r in type_args {
                verify_reg(reg_count, *r, &format!("{}: type arg", here()))?;
            }
            for (_name, r) in fields {
                verify_reg(reg_count, *r, &format!("{}: field value", here()))?;
            }
        }
        Instruction::MakeArray { dst, items } | Instruction::MakeTuple { dst, items } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            for r in items {
                verify_reg(reg_count, *r, &format!("{}: item", here()))?;
            }
        }
        Instruction::MakeEnum {
            dst,
            type_args,
            fields,
            enum_name: _,
            variant: _,
        } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            for r in type_args {
                verify_reg(reg_count, *r, &format!("{}: type arg", here()))?;
            }
            for r in fields {
                verify_reg(reg_count, *r, &format!("{}: enum field", here()))?;
            }
        }
        Instruction::GetField { dst, obj, .. } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_reg(reg_count, *obj, &format!("{}: obj", here()))?;
        }
        Instruction::SetField { obj, value, .. } => {
            verify_reg(reg_count, *obj, &format!("{}: obj", here()))?;
            verify_reg(reg_count, *value, &format!("{}: value", here()))?;
        }
        Instruction::StructGet { dst, obj, idx: _ } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_reg(reg_count, *obj, &format!("{}: obj", here()))?;
        }
        Instruction::StructSet { obj, idx: _, value } => {
            verify_reg(reg_count, *obj, &format!("{}: obj", here()))?;
            verify_reg(reg_count, *value, &format!("{}: value", here()))?;
        }
        Instruction::TupleGet { dst, tup, idx: _ } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_reg(reg_count, *tup, &format!("{}: tup", here()))?;
        }
        Instruction::TupleSet { tup, idx: _, value } => {
            verify_reg(reg_count, *tup, &format!("{}: tup", here()))?;
            verify_reg(reg_count, *value, &format!("{}: value", here()))?;
        }
        Instruction::IndexGet { dst, arr, idx } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_reg(reg_count, *arr, &format!("{}: arr", here()))?;
            verify_reg(reg_count, *idx, &format!("{}: idx", here()))?;
        }
        Instruction::IndexSet { arr, idx, value } => {
            verify_reg(reg_count, *arr, &format!("{}: arr", here()))?;
            verify_reg(reg_count, *idx, &format!("{}: idx", here()))?;
            verify_reg(reg_count, *value, &format!("{}: value", here()))?;
        }
        Instruction::Len { dst, arr } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_reg(reg_count, *arr, &format!("{}: arr", here()))?;
        }
        Instruction::IntAdd { dst, a, b }
        | Instruction::IntSub { dst, a, b }
        | Instruction::IntMul { dst, a, b }
        | Instruction::IntDiv { dst, a, b }
        | Instruction::IntMod { dst, a, b }
        | Instruction::IntLt { dst, a, b }
        | Instruction::IntLe { dst, a, b }
        | Instruction::IntGt { dst, a, b }
        | Instruction::IntGe { dst, a, b }
        | Instruction::IntEq { dst, a, b }
        | Instruction::IntNe { dst, a, b }
        | Instruction::BoolEq { dst, a, b }
        | Instruction::BoolNe { dst, a, b } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_reg(reg_count, *a, &format!("{}: a", here()))?;
            verify_reg(reg_count, *b, &format!("{}: b", here()))?;
        }
        Instruction::BoolNot { dst, v } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_reg(reg_count, *v, &format!("{}: v", here()))?;
        }
        Instruction::Call {
            dst,
            func: target,
            args,
        } => {
            if let Some(dst) = dst {
                verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            }
            verify_call_target(module, target, &here())?;
            for r in args {
                verify_reg(reg_count, *r, &format!("{}: arg", here()))?;
            }
        }
        Instruction::ICall { dst, fnptr, args } => {
            if let Some(dst) = dst {
                verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            }
            verify_reg(reg_count, *fnptr, &format!("{}: fnptr", here()))?;
            for r in args {
                verify_reg(reg_count, *r, &format!("{}: arg", here()))?;
            }
        }
        Instruction::ICallTypeArgs {
            dst,
            fnptr,
            recv,
            method_type_args,
            dict_args,
            args,
        } => {
            if let Some(dst) = dst {
                verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            }
            verify_reg(reg_count, *fnptr, &format!("{}: fnptr", here()))?;
            verify_reg(reg_count, *recv, &format!("{}: recv", here()))?;
            for r in method_type_args {
                verify_reg(reg_count, *r, &format!("{}: method type arg", here()))?;
            }
            for r in dict_args {
                verify_reg(reg_count, *r, &format!("{}: dict arg", here()))?;
            }
            for r in args {
                verify_reg(reg_count, *r, &format!("{}: arg", here()))?;
            }
        }
        Instruction::VCall {
            dst,
            obj,
            method: _,
            method_type_args,
            dict_args,
            args,
        } => {
            if let Some(dst) = dst {
                verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            }
            verify_reg(reg_count, *obj, &format!("{}: obj", here()))?;
            for r in method_type_args {
                verify_reg(reg_count, *r, &format!("{}: method type arg", here()))?;
            }
            for r in dict_args {
                verify_reg(reg_count, *r, &format!("{}: dict arg", here()))?;
            }
            for r in args {
                verify_reg(reg_count, *r, &format!("{}: arg", here()))?;
            }
        }
        Instruction::PushHandler { clauses } => {
            for clause in clauses {
                verify_effect_spec(
                    reg_count,
                    &clause.effect,
                    &format!("{}: handler effect", here()),
                )?;
                verify_pc(
                    code_len,
                    clause.target_pc,
                    &format!("{}: handler target", here()),
                )?;
                for r in &clause.param_regs {
                    verify_reg(reg_count, *r, &format!("{}: handler param reg", here()))?;
                }
                let bind_count = clause
                    .arg_patterns
                    .iter()
                    .map(count_pattern_binds)
                    .sum::<usize>();
                let expected_min = bind_count;
                let expected_max = bind_count + 1;
                if clause.param_regs.len() != expected_min
                    && clause.param_regs.len() != expected_max
                {
                    return Err(VerifyError {
                        message: format!(
                            "{}: handler clause param_regs length {} does not match expected {} or {}",
                            here(),
                            clause.param_regs.len(),
                            expected_min,
                            expected_max
                        ),
                    });
                }
            }
        }
        Instruction::PopHandler => {}
        Instruction::Perform { dst, effect, args } => {
            if let Some(dst) = dst {
                verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            }
            verify_effect_spec(reg_count, effect, &format!("{}: perform effect", here()))?;
            for r in args {
                verify_reg(reg_count, *r, &format!("{}: perform arg", here()))?;
            }
        }
        Instruction::Resume { dst, k, value } => {
            if let Some(dst) = dst {
                verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            }
            verify_reg(reg_count, *k, &format!("{}: k", here()))?;
            verify_reg(reg_count, *value, &format!("{}: value", here()))?;
        }
        Instruction::ResumeTail { k, value } => {
            verify_reg(reg_count, *k, &format!("{}: k", here()))?;
            verify_reg(reg_count, *value, &format!("{}: value", here()))?;
        }
        Instruction::Jump { target_pc } => {
            verify_pc(code_len, *target_pc, &format!("{}: jump", here()))?;
        }
        Instruction::JumpIf {
            cond,
            then_pc,
            else_pc,
        } => {
            verify_reg(reg_count, *cond, &format!("{}: cond", here()))?;
            verify_pc(code_len, *then_pc, &format!("{}: then", here()))?;
            verify_pc(code_len, *else_pc, &format!("{}: else", here()))?;
        }
        Instruction::Switch {
            value,
            cases,
            default_pc,
        } => {
            verify_reg(reg_count, *value, &format!("{}: value", here()))?;
            verify_pc(code_len, *default_pc, &format!("{}: default", here()))?;
            for case in cases {
                verify_pc(
                    code_len,
                    case.target_pc,
                    &format!("{}: case target", here()),
                )?;
                for r in &case.param_regs {
                    verify_reg(reg_count, *r, &format!("{}: case param", here()))?;
                }
                let expected = count_pattern_binds(&case.pattern);
                if case.param_regs.len() != expected {
                    return Err(VerifyError {
                        message: format!(
                            "{}: switch case param_regs length {} does not match expected {}",
                            here(),
                            case.param_regs.len(),
                            expected
                        ),
                    });
                }
            }
        }
        Instruction::Return { value } => {
            verify_reg(reg_count, *value, &format!("{}: return value", here()))?;
        }
        Instruction::Trap { .. } => {}
    }

    // Verify that generic arity doesn't exceed param_count.
    if func.param_count < module.function_generic_param_count(fn_id).unwrap_or(0) {
        return Err(VerifyError {
            message: format!(
                "function `{}` has param_count {} smaller than generic param count {}",
                func.name,
                func.param_count,
                module.function_generic_param_count(fn_id).unwrap_or(0)
            ),
        });
    }

    Ok(())
}

fn verify_call_target(
    module: &ExecutableModule,
    target: &CallTarget,
    context: &str,
) -> Result<(), VerifyError> {
    match target {
        CallTarget::Bc(fid) => {
            if (fid.0 as usize) >= module.functions.len() {
                return Err(VerifyError {
                    message: format!("{context}: invalid function id {}", fid.0),
                });
            }
        }
        CallTarget::Host(hid) => {
            if (hid.0 as usize) >= module.host_imports.len() {
                return Err(VerifyError {
                    message: format!("{context}: invalid host import id {}", hid.0),
                });
            }
        }
        CallTarget::Intrinsic(_) => {}
    }
    Ok(())
}

fn verify_effect_spec(reg_count: u32, spec: &EffectSpec, context: &str) -> Result<(), VerifyError> {
    if spec.interface.is_empty() {
        return Err(VerifyError {
            message: format!("{context}: empty interface name"),
        });
    }
    if spec.method.is_empty() {
        return Err(VerifyError {
            message: format!("{context}: empty method name"),
        });
    }
    for r in &spec.interface_args {
        verify_reg(reg_count, *r, &format!("{context}: interface arg"))?;
    }
    Ok(())
}
