extern crate alloc;

use alloc::collections::BTreeMap;
use alloc::string::String;

use crate::{
    CallTarget, ConstValue, EffectSpec, ExecutableModule, Function, FunctionId, Instruction,
    MethodId, Reg, TypeId, TypeRepLit,
};

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

    // Parallel tables must match the type table.
    let types_len = module.type_names.len();
    if module.vcall_dispatch.len() != types_len {
        return Err(VerifyError {
            message: format!(
                "vcall_dispatch length {} does not match type_names length {}",
                module.vcall_dispatch.len(),
                types_len
            ),
        });
    }
    if module.assoc_type_dispatch.len() != types_len {
        return Err(VerifyError {
            message: format!(
                "assoc_type_dispatch length {} does not match type_names length {}",
                module.assoc_type_dispatch.len(),
                types_len
            ),
        });
    }
    if module.interface_impls.len() != types_len {
        return Err(VerifyError {
            message: format!(
                "interface_impls length {} does not match type_names length {}",
                module.interface_impls.len(),
                types_len
            ),
        });
    }
    if module.struct_layouts.len() != types_len {
        return Err(VerifyError {
            message: format!(
                "struct_layouts length {} does not match type_names length {}",
                module.struct_layouts.len(),
                types_len
            ),
        });
    }

    // Recompute interning maps and ensure they match the stored ones.
    let mut expected_type_ids = BTreeMap::new();
    for (idx, name) in module.type_names.iter().enumerate() {
        if name.is_empty() {
            return Err(VerifyError {
                message: "type_names contains empty type name".to_string(),
            });
        }
        let id = TypeId(idx as u32);
        if expected_type_ids.insert(name.clone(), id).is_some() {
            return Err(VerifyError {
                message: format!("duplicate type name `{name}`"),
            });
        }
    }
    if expected_type_ids != module.type_ids {
        return Err(VerifyError {
            message: "type_ids map does not match type_names table".to_string(),
        });
    }

    let mut expected_method_ids = BTreeMap::new();
    for (idx, name) in module.method_names.iter().enumerate() {
        if name.is_empty() {
            return Err(VerifyError {
                message: "method_names contains empty method name".to_string(),
            });
        }
        let id = MethodId(idx as u32);
        if expected_method_ids.insert(name.clone(), id).is_some() {
            return Err(VerifyError {
                message: format!("duplicate method name `{name}`"),
            });
        }
    }
    if expected_method_ids != module.method_ids {
        return Err(VerifyError {
            message: "method_ids map does not match method_names table".to_string(),
        });
    }

    // Ensure primitive type names are present (needed for unified runtime dispatch and diagnostics).
    for name in [
        "unit", "bool", "int", "float", "byte", "char", "string", "bytes",
    ] {
        if module.type_id(name).is_none() {
            return Err(VerifyError {
                message: format!("missing required primitive type name `{name}` in type table"),
            });
        }
    }

    // VCall dispatch table integrity.
    let method_count = module.method_names.len() as u32;
    for (type_idx, entries) in module.vcall_dispatch.iter().enumerate() {
        let mut prev: Option<MethodId> = None;
        for (method_id, fn_id) in entries {
            if method_id.0 >= method_count {
                return Err(VerifyError {
                    message: format!(
                        "vcall dispatch entry for TypeId {type_idx} has invalid MethodId {} (methods={method_count})",
                        method_id.0
                    ),
                });
            }
            if let Some(prev) = prev
                && prev.0 >= method_id.0
            {
                return Err(VerifyError {
                    message: format!(
                        "vcall dispatch list for TypeId {type_idx} is not strictly sorted/unique by MethodId"
                    ),
                });
            }
            if (fn_id.0 as usize) >= module.functions.len() {
                return Err(VerifyError {
                    message: format!(
                        "vcall dispatch entry (TypeId {type_idx}, MethodId {}) points to invalid function id {}",
                        method_id.0, fn_id.0
                    ),
                });
            }
            prev = Some(*method_id);
        }
    }

    let type_count = module.type_names.len() as u32;

    // Assoc type dispatch table integrity.
    for (type_idx, entries) in module.assoc_type_dispatch.iter().enumerate() {
        let mut prev_iface: Option<u32> = None;
        let mut prev_assoc: Option<&str> = None;
        for (iface_type_id, assoc, fn_id) in entries {
            if iface_type_id.0 >= type_count {
                return Err(VerifyError {
                    message: format!(
                        "assoc_type_dispatch for TypeId {type_idx} contains invalid interface TypeId {} (types={type_count})",
                        iface_type_id.0
                    ),
                });
            }
            if assoc.is_empty() {
                return Err(VerifyError {
                    message: format!(
                        "assoc_type_dispatch for TypeId {type_idx} contains empty assoc name"
                    ),
                });
            }
            if let Some(prev_iface) = prev_iface
                && let Some(prev_assoc) = prev_assoc
                && (prev_iface, prev_assoc) >= (iface_type_id.0, assoc.as_str())
            {
                return Err(VerifyError {
                    message: format!(
                        "assoc_type_dispatch list for TypeId {type_idx} is not strictly sorted/unique by (interface, assoc)"
                    ),
                });
            }
            if (fn_id.0 as usize) >= module.functions.len() {
                return Err(VerifyError {
                    message: format!(
                        "assoc_type_dispatch entry (TypeId {type_idx}, interface TypeId {}, assoc `{}`) points to invalid function id {}",
                        iface_type_id.0, assoc, fn_id.0
                    ),
                });
            }
            prev_iface = Some(iface_type_id.0);
            prev_assoc = Some(assoc.as_str());
        }
    }

    // Interface impl tables are sorted lists of TypeId.
    for (type_idx, ifaces) in module.interface_impls.iter().enumerate() {
        let mut prev: Option<TypeId> = None;
        for iface_id in ifaces {
            if iface_id.0 >= type_count {
                return Err(VerifyError {
                    message: format!(
                        "interface_impls for TypeId {type_idx} contains invalid interface TypeId {} (types={type_count})",
                        iface_id.0
                    ),
                });
            }
            if let Some(prev) = prev
                && prev.0 >= iface_id.0
            {
                return Err(VerifyError {
                    message: format!(
                        "interface_impls list for TypeId {type_idx} is not strictly sorted/unique"
                    ),
                });
            }
            prev = Some(*iface_id);
        }
    }

    // Struct layouts.
    for (type_idx, layout) in module.struct_layouts.iter().enumerate() {
        let Some(fields) = layout.as_ref() else {
            continue;
        };
        for field in fields {
            if field.is_empty() {
                let type_name = module
                    .type_name(TypeId(type_idx as u32))
                    .unwrap_or("<unknown>");
                return Err(VerifyError {
                    message: format!("struct_layouts for `{type_name}` contains empty field name"),
                });
            }
        }
    }

    let return_arities = compute_return_arities(module)?;

    // Function bodies.
    for (fn_idx, func) in module.functions.iter().enumerate() {
        let fn_id = FunctionId(fn_idx as u32);
        verify_function(module, fn_id, func)?;
    }

    verify_call_return_arities(module, &return_arities)?;

    Ok(())
}

fn compute_return_arities(module: &ExecutableModule) -> Result<Vec<usize>, VerifyError> {
    #[derive(Clone, Copy, Debug)]
    enum ReturnKind {
        Single,
        Multi(usize),
    }

    let mut out = Vec::with_capacity(module.functions.len());
    for func in &module.functions {
        let mut kind: Option<ReturnKind> = None;
        for inst in &func.code {
            match inst {
                Instruction::Return { .. } => {
                    if matches!(kind, Some(ReturnKind::Multi(_))) {
                        return Err(VerifyError {
                            message: format!(
                                "function `{}` mixes Return and ReturnMulti",
                                func.name
                            ),
                        });
                    }
                    kind = Some(ReturnKind::Single);
                }
                Instruction::ReturnMulti { values } => {
                    let arity = values.len();
                    if arity == 0 {
                        return Err(VerifyError {
                            message: format!(
                                "function `{}` has invalid ReturnMulti with 0 values",
                                func.name
                            ),
                        });
                    }
                    match kind {
                        None => kind = Some(ReturnKind::Multi(arity)),
                        Some(ReturnKind::Single) => {
                            return Err(VerifyError {
                                message: format!(
                                    "function `{}` mixes Return and ReturnMulti",
                                    func.name
                                ),
                            });
                        }
                        Some(ReturnKind::Multi(existing)) if existing == arity => {}
                        Some(ReturnKind::Multi(existing)) => {
                            return Err(VerifyError {
                                message: format!(
                                    "function `{}` has inconsistent ReturnMulti arity: expected {existing}, got {arity}",
                                    func.name
                                ),
                            });
                        }
                    }
                }
                _ => {}
            }
        }

        // Functions that fall off the end return `unit` (single return) implicitly.
        let arity = match kind {
            None | Some(ReturnKind::Single) => 1,
            Some(ReturnKind::Multi(n)) => n,
        };
        out.push(arity);
    }

    Ok(out)
}

fn verify_call_return_arities(
    module: &ExecutableModule,
    return_arities: &[usize],
) -> Result<(), VerifyError> {
    for func in &module.functions {
        let here = |pc: usize| format!("function `{}` pc {}", func.name, pc);
        for (pc, inst) in func.code.iter().enumerate() {
            match inst {
                Instruction::Call { func: target, .. } => {
                    let CallTarget::Bc(fid) = target else {
                        continue;
                    };
                    let callee_arity = return_arities.get(fid.0 as usize).copied().unwrap_or(1);
                    if callee_arity != 1 {
                        return Err(VerifyError {
                            message: format!(
                                "{}: Call expects single return, but callee `{}` returns {callee_arity} values",
                                here(pc),
                                module
                                    .function(*fid)
                                    .map(|f| f.name.as_str())
                                    .unwrap_or("<unknown>")
                            ),
                        });
                    }
                }
                Instruction::CallMulti {
                    dsts, func: target, ..
                } => {
                    let CallTarget::Bc(fid) = target else {
                        return Err(VerifyError {
                            message: format!(
                                "{}: CallMulti only supports bytecode functions",
                                here(pc)
                            ),
                        });
                    };
                    let expected = dsts.len();
                    if expected == 0 {
                        return Err(VerifyError {
                            message: format!(
                                "{}: CallMulti requires at least one dst register",
                                here(pc)
                            ),
                        });
                    }
                    let callee_arity = return_arities.get(fid.0 as usize).copied().unwrap_or(1);
                    if callee_arity != expected {
                        return Err(VerifyError {
                            message: format!(
                                "{}: CallMulti dst count {expected} does not match callee `{}` return arity {callee_arity}",
                                here(pc),
                                module
                                    .function(*fid)
                                    .map(|f| f.name.as_str())
                                    .unwrap_or("<unknown>")
                            ),
                        });
                    }
                }
                _ => {}
            }
        }
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

fn verify_type_id(module: &ExecutableModule, id: TypeId, context: &str) -> Result<(), VerifyError> {
    let len = module.type_names.len();
    if (id.0 as usize) >= len {
        return Err(VerifyError {
            message: format!("{context}: TypeId {} out of range (types={len})", id.0),
        });
    }
    Ok(())
}

fn verify_method_id(
    module: &ExecutableModule,
    id: MethodId,
    context: &str,
) -> Result<(), VerifyError> {
    let len = module.method_names.len();
    if (id.0 as usize) >= len {
        return Err(VerifyError {
            message: format!("{context}: MethodId {} out of range (methods={len})", id.0),
        });
    }
    Ok(())
}

fn verify_type_rep_lit_interned(
    module: &ExecutableModule,
    lit: &TypeRepLit,
    context: &str,
) -> Result<(), VerifyError> {
    match lit {
        TypeRepLit::Struct(name) | TypeRepLit::Enum(name) | TypeRepLit::Interface(name) => {
            if module.type_id(name.as_str()).is_none() {
                return Err(VerifyError {
                    message: format!("{context}: unknown type name `{name}` (not interned)"),
                });
            }
        }
        TypeRepLit::Unit
        | TypeRepLit::Never
        | TypeRepLit::Bool
        | TypeRepLit::Int
        | TypeRepLit::Float
        | TypeRepLit::Byte
        | TypeRepLit::Char
        | TypeRepLit::String
        | TypeRepLit::Bytes
        | TypeRepLit::Array
        | TypeRepLit::Tuple(_)
        | TypeRepLit::Fn
        | TypeRepLit::Cont => {}
    }
    Ok(())
}

fn count_pattern_binds(p: &crate::Pattern) -> usize {
    match p {
        crate::Pattern::Wildcard => 0,
        crate::Pattern::Bind => 1,
        crate::Pattern::Literal(_) => 0,
        crate::Pattern::Tuple {
            prefix,
            rest,
            suffix,
        } => {
            prefix.iter().map(count_pattern_binds).sum::<usize>()
                + rest.as_deref().map(count_pattern_binds).unwrap_or(0)
                + suffix.iter().map(count_pattern_binds).sum::<usize>()
        }
        crate::Pattern::Enum { fields, .. } => fields.iter().map(count_pattern_binds).sum(),
        crate::Pattern::Struct { fields, .. } => {
            fields.iter().map(|(_, pat)| count_pattern_binds(pat)).sum()
        }
        crate::Pattern::Array {
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
        Instruction::Const { dst, value } => {
            verify_reg(reg_count, *dst, &format!("{}: const dst", here()))?;
            if let ConstValue::TypeRep(lit) = value {
                verify_type_rep_lit_interned(module, lit, &format!("{}: const typerep", here()))?;
            }
        }
        Instruction::Copy { dst, src }
        | Instruction::Move { dst, src }
        | Instruction::AsReadonly { dst, src } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_reg(reg_count, *src, &format!("{}: src", here()))?;
        }
        Instruction::IsType { dst, value, ty } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_reg(reg_count, *value, &format!("{}: value", here()))?;
            verify_reg(reg_count, *ty, &format!("{}: ty", here()))?;
        }
        Instruction::MakeTypeRep { dst, base, args } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_type_rep_lit_interned(module, base, &format!("{}: MakeTypeRep base", here()))?;
            for r in args {
                verify_reg(reg_count, *r, &format!("{}: MakeTypeRep arg", here()))?;
            }
        }
        Instruction::AssocTypeRep {
            dst,
            recv,
            iface_type_id,
            assoc,
        } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_reg(reg_count, *recv, &format!("{}: recv", here()))?;
            verify_type_id(
                module,
                *iface_type_id,
                &format!("{}: iface type id", here()),
            )?;
            if assoc.is_empty() {
                return Err(VerifyError {
                    message: format!("{}: AssocTypeRep has empty assoc name", here()),
                });
            }
        }
        Instruction::MakeStruct {
            dst,
            type_args,
            fields,
            type_id,
        } => {
            verify_type_id(module, *type_id, &format!("{}: type id", here()))?;
            let Some(layout) = module.struct_layouts.get(type_id.0 as usize) else {
                return Err(VerifyError {
                    message: format!("{}: invalid TypeId {}", here(), type_id.0),
                });
            };
            if layout.is_none() {
                let ty = module.type_name(*type_id).unwrap_or("<unknown>");
                return Err(VerifyError {
                    message: format!("{}: missing struct layout for `{ty}`", here()),
                });
            }
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
            enum_type_id,
            variant: _,
        } => {
            verify_type_id(module, *enum_type_id, &format!("{}: enum type id", here()))?;
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
        | Instruction::IntAnd { dst, a, b }
        | Instruction::IntOr { dst, a, b }
        | Instruction::IntXor { dst, a, b }
        | Instruction::IntShl { dst, a, b }
        | Instruction::IntShr { dst, a, b }
        | Instruction::IntUShr { dst, a, b }
        | Instruction::IntLt { dst, a, b }
        | Instruction::IntLe { dst, a, b }
        | Instruction::IntGt { dst, a, b }
        | Instruction::IntGe { dst, a, b }
        | Instruction::IntEq { dst, a, b }
        | Instruction::IntNe { dst, a, b }
        | Instruction::ByteAnd { dst, a, b }
        | Instruction::ByteOr { dst, a, b }
        | Instruction::ByteXor { dst, a, b }
        | Instruction::ByteShl { dst, a, b }
        | Instruction::ByteShr { dst, a, b }
        | Instruction::ByteUShr { dst, a, b }
        | Instruction::BoolEq { dst, a, b }
        | Instruction::BoolNe { dst, a, b } => {
            verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            verify_reg(reg_count, *a, &format!("{}: a", here()))?;
            verify_reg(reg_count, *b, &format!("{}: b", here()))?;
        }
        Instruction::BoolNot { dst, v }
        | Instruction::IntNot { dst, v }
        | Instruction::ByteNot { dst, v } => {
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
        Instruction::CallMulti {
            dsts,
            func: target,
            args,
        } => {
            for dst in dsts {
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
        Instruction::VCall {
            dst,
            obj,
            method,
            method_type_args,
            args,
        } => {
            verify_method_id(module, *method, &format!("{}: method id", here()))?;
            if let Some(dst) = dst {
                verify_reg(reg_count, *dst, &format!("{}: dst", here()))?;
            }
            verify_reg(reg_count, *obj, &format!("{}: obj", here()))?;
            for r in method_type_args {
                verify_reg(reg_count, *r, &format!("{}: method type arg", here()))?;
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
        Instruction::ReturnMulti { values } => {
            if values.is_empty() {
                return Err(VerifyError {
                    message: format!("{}: ReturnMulti must return at least one value", here()),
                });
            }
            for r in values {
                verify_reg(reg_count, *r, &format!("{}: return value", here()))?;
            }
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
