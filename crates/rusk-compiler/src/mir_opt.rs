//! MIR-level optimization passes (compiler-internal).
//!
//! This module currently implements a conservative subset of the "escape analysis + SROA" proposal
//! (`proposals/escape-analysis.md`), focused on eliminating short-lived `Option<T>` allocations.
//!
//! Enabled at `OptLevel::O2`.

#![forbid(unsafe_code)]

use rusk_bytecode::OptLevel;
use rusk_mir::{
    BlockId, CallTarget, ConstValue, Function, FunctionId, Instruction, Local, Module, Operand,
    Pattern, SwitchCase, Terminator, Type,
};
use std::collections::BTreeMap;

const OPTION_ENUM: &str = "Option";
const OPTION_VARIANT_SOME: &str = "Some";
const OPTION_VARIANT_NONE: &str = "None";

pub(crate) fn optimize_mir_module(module: &mut Module, opt_level: OptLevel) {
    if opt_level != OptLevel::O2 {
        return;
    }

    // 1) Create `$unboxed` variants for eligible `-> Option<T>` functions.
    let unboxed = synthesize_option_unboxed_variants(module);

    // 2) Rewrite `call -> switch` patterns to use the `$unboxed` ABI (CallIdMulti + CondBr).
    //
    // 3) Rewrite local `MakeEnum Option::* -> switch` patterns.
    //
    // These rewrites are block-local and intentionally conservative.
    for func in &mut module.functions {
        rewrite_option_switches(func, &unboxed);
    }
}

#[derive(Clone, Copy, Debug)]
struct OptionSwitchTargets {
    some_target: BlockId,
    none_target: BlockId,
    binds_payload: bool,
}

fn parse_option_switch_cases(
    block_param_counts: &[usize],
    cases: &[SwitchCase],
    _default: BlockId,
) -> Option<OptionSwitchTargets> {
    // Keep this strict for now: exact `Option::Some(_)` and `Option::None`, and default must be
    // `None`'s target.
    if cases.len() != 2 {
        return None;
    }

    let mut some: Option<(BlockId, bool)> = None;
    let mut none: Option<BlockId> = None;

    for case in cases {
        match &case.pattern {
            Pattern::Enum {
                enum_name,
                variant,
                fields,
            } if enum_name == OPTION_ENUM && variant == OPTION_VARIANT_SOME => {
                if fields.len() != 1 {
                    return None;
                }
                let binds_payload = matches!(fields[0], Pattern::Bind);
                if !binds_payload && !matches!(fields[0], Pattern::Wildcard) {
                    return None;
                }
                let expected_params = if binds_payload { 1 } else { 0 };
                if block_param_counts.get(case.target.0).copied()? != expected_params {
                    return None;
                }
                if some.replace((case.target, binds_payload)).is_some() {
                    return None;
                }
            }
            Pattern::Enum {
                enum_name,
                variant,
                fields,
            } if enum_name == OPTION_ENUM && variant == OPTION_VARIANT_NONE => {
                if !fields.is_empty() {
                    return None;
                }
                if block_param_counts.get(case.target.0).copied()? != 0 {
                    return None;
                }
                if none.replace(case.target).is_some() {
                    return None;
                }
            }
            _ => return None,
        }
    }

    let (some_target, binds_payload) = some?;
    let none_target = none?;

    Some(OptionSwitchTargets {
        some_target,
        none_target,
        binds_payload,
    })
}

fn alloc_local(next_local: &mut usize) -> Local {
    let local = Local(*next_local);
    *next_local = next_local.saturating_add(1);
    local
}

fn rewrite_option_switches(func: &mut Function, unboxed: &BTreeMap<FunctionId, FunctionId>) {
    let block_param_counts: Vec<usize> = func.blocks.iter().map(|b| b.params.len()).collect();

    for block_index in 0..func.blocks.len() {
        let term = func.blocks[block_index].terminator.clone();
        let Terminator::Switch {
            value,
            cases,
            default,
        } = term
        else {
            continue;
        };

        let Operand::Local(scrutinee) = value else {
            continue;
        };

        let Some(targets) = parse_option_switch_cases(&block_param_counts, &cases, default) else {
            continue;
        };

        let next_local = &mut func.locals;
        let block = &mut func.blocks[block_index];

        // We only rewrite when the switch scrutinee is produced by the last instruction in the
        // same block. This ensures the temporary is immediately consumed and never escapes.
        let Some(last_index) = block.instructions.len().checked_sub(1) else {
            continue;
        };

        // Prefer the highest-value cross-function optimization first.
        if try_rewrite_call_option_switch(
            next_local, block, scrutinee, last_index, targets, unboxed,
        ) {
            continue;
        }

        let _ = try_rewrite_make_enum_option_switch(next_local, block, scrutinee, targets);
    }
}

fn try_rewrite_call_option_switch(
    next_local: &mut usize,
    block: &mut rusk_mir::BasicBlock,
    scrutinee: Local,
    last_index: usize,
    targets: OptionSwitchTargets,
    unboxed: &BTreeMap<FunctionId, FunctionId>,
) -> bool {
    let last_instr = block.instructions.get(last_index).cloned();
    let Some(Instruction::CallId {
        dst,
        func: callee,
        args,
    }) = last_instr
    else {
        return false;
    };

    if dst != Some(scrutinee) {
        return false;
    }

    let CallTarget::Mir(orig_id) = callee else {
        return false;
    };
    let Some(unboxed_id) = unboxed.get(&orig_id).copied() else {
        return false;
    };

    let tag_local = alloc_local(next_local);
    let payload_local = alloc_local(next_local);

    block.instructions[last_index] = Instruction::CallIdMulti {
        dsts: vec![tag_local, payload_local],
        func: CallTarget::Mir(unboxed_id),
        args,
    };

    block.terminator = Terminator::CondBr {
        cond: Operand::Local(tag_local),
        then_target: targets.some_target,
        then_args: if targets.binds_payload {
            vec![Operand::Local(payload_local)]
        } else {
            Vec::new()
        },
        else_target: targets.none_target,
        else_args: Vec::new(),
    };

    true
}

fn try_rewrite_make_enum_option_switch(
    next_local: &mut usize,
    block: &mut rusk_mir::BasicBlock,
    scrutinee: Local,
    targets: OptionSwitchTargets,
) -> bool {
    let Some(last_instr) = block.instructions.last().cloned() else {
        return false;
    };

    let Instruction::MakeEnum {
        dst,
        enum_name,
        variant,
        fields,
        ..
    } = last_instr
    else {
        return false;
    };

    if dst != scrutinee || enum_name != OPTION_ENUM {
        return false;
    }

    let tag_local = alloc_local(next_local);
    let payload_local = alloc_local(next_local);

    let (tag, payload_op) = match (variant.as_str(), fields.as_slice()) {
        (OPTION_VARIANT_SOME, [payload]) => (true, payload.clone()),
        (OPTION_VARIANT_NONE, []) => (false, Operand::Literal(ConstValue::Unit)),
        _ => return false,
    };

    // Replace the `MakeEnum` with scalar materialization.
    block.instructions.pop();
    block.instructions.push(Instruction::Const {
        dst: tag_local,
        value: ConstValue::Bool(tag),
    });
    match payload_op {
        Operand::Local(src) => {
            block.instructions.push(Instruction::Copy {
                dst: payload_local,
                src,
            });
        }
        Operand::Literal(lit) => {
            block.instructions.push(Instruction::Const {
                dst: payload_local,
                value: lit,
            });
        }
    }

    block.terminator = Terminator::CondBr {
        cond: Operand::Local(tag_local),
        then_target: targets.some_target,
        then_args: if targets.binds_payload {
            vec![Operand::Local(payload_local)]
        } else {
            Vec::new()
        },
        else_target: targets.none_target,
        else_args: Vec::new(),
    };

    true
}

fn synthesize_option_unboxed_variants(module: &mut Module) -> BTreeMap<FunctionId, FunctionId> {
    let mut pending = Vec::<(FunctionId, Function)>::new();

    for (idx, func) in module.functions.iter().enumerate() {
        if func.name.ends_with("$unboxed") {
            continue;
        }
        if func.ret_type != Some(Type::Enum(OPTION_ENUM.to_string())) {
            continue;
        }

        let Some(unboxed_func) = build_option_unboxed_variant(func) else {
            continue;
        };
        let orig_id = FunctionId(idx as u32);
        pending.push((orig_id, unboxed_func));
    }

    let mut out = BTreeMap::<FunctionId, FunctionId>::new();
    for (orig_id, func) in pending {
        if module.function_id(func.name.as_str()).is_some() {
            continue;
        }
        // `add_function` updates `function_ids` for us.
        if let Ok(new_id) = module.add_function(func) {
            out.insert(orig_id, new_id);
        }
    }
    out
}

fn build_option_unboxed_variant(func: &Function) -> Option<Function> {
    let mut out = func.clone();
    out.name = format!("{}$unboxed", func.name);
    // `ReturnMulti` is an internal ABI; the MIR `ret_type` field cannot describe it today.
    out.ret_type = None;

    let use_counts = count_local_uses(&out);

    for block_idx in 0..out.blocks.len() {
        let term = out.blocks[block_idx].terminator.clone();
        let Terminator::Return { value } = term else {
            continue;
        };

        // Common lowering shape: `if .. { Some(..) } else { None }` produces a join block whose
        // sole param is returned.
        if let Operand::Local(ret_local) = &value
            && out.blocks[block_idx].params.as_slice() == [*ret_local]
        {
            unbox_return_param_join(&mut out, block_idx, *ret_local, &use_counts)?;
            continue;
        }

        let (tag, payload, remove_make_enum_dst) = match value {
            Operand::Local(ret_local) => {
                // Ensure this return value is not used anywhere else in the function (so removing
                // its `MakeEnum` is safe without needing a more global rewrite).
                let idx = ret_local.0;
                if idx >= use_counts.len() || use_counts[idx] != 1 {
                    return None;
                }

                let (instr_index, variant, fields) =
                    find_make_enum_option_in_block(&out.blocks[block_idx], ret_local)?;

                match (variant.as_str(), fields.as_slice()) {
                    (OPTION_VARIANT_SOME, [payload]) => (
                        Operand::Literal(ConstValue::Bool(true)),
                        payload.clone(),
                        Some(instr_index),
                    ),
                    (OPTION_VARIANT_NONE, []) => (
                        Operand::Literal(ConstValue::Bool(false)),
                        Operand::Literal(ConstValue::Unit),
                        Some(instr_index),
                    ),
                    _ => return None,
                }
            }
            Operand::Literal(ConstValue::Enum {
                enum_name,
                variant,
                fields,
            }) if enum_name == OPTION_ENUM => match (variant.as_str(), fields.as_slice()) {
                (OPTION_VARIANT_SOME, [payload]) => (
                    Operand::Literal(ConstValue::Bool(true)),
                    Operand::Literal(payload.clone()),
                    None,
                ),
                (OPTION_VARIANT_NONE, []) => (
                    Operand::Literal(ConstValue::Bool(false)),
                    Operand::Literal(ConstValue::Unit),
                    None,
                ),
                _ => return None,
            },
            _ => return None,
        };

        if let Some(instr_index) = remove_make_enum_dst {
            out.blocks[block_idx].instructions.remove(instr_index);
        }

        out.blocks[block_idx].terminator = Terminator::ReturnMulti {
            values: vec![tag, payload],
        };
    }

    Some(out)
}

fn unbox_return_param_join(
    func: &mut Function,
    join_block_idx: usize,
    ret_local: Local,
    use_counts: &[usize],
) -> Option<()> {
    if ret_local.0 >= use_counts.len() || use_counts[ret_local.0] != 1 {
        return None;
    }

    #[derive(Clone, Copy, Debug)]
    enum EdgeKind {
        Br,
        CondBrThen,
        CondBrElse,
    }

    #[derive(Clone, Debug)]
    struct PredEdge {
        pred_idx: usize,
        kind: EdgeKind,
        arg: Operand,
    }

    let mut edges = Vec::<PredEdge>::new();
    for (pred_idx, block) in func.blocks.iter().enumerate() {
        match &block.terminator {
            Terminator::Br { target, args } if target.0 == join_block_idx => {
                if args.len() != 1 {
                    return None;
                }
                edges.push(PredEdge {
                    pred_idx,
                    kind: EdgeKind::Br,
                    arg: args[0].clone(),
                });
            }
            Terminator::CondBr {
                then_target,
                then_args,
                else_target,
                else_args,
                ..
            } => {
                if then_target.0 == join_block_idx {
                    if then_args.len() != 1 {
                        return None;
                    }
                    edges.push(PredEdge {
                        pred_idx,
                        kind: EdgeKind::CondBrThen,
                        arg: then_args[0].clone(),
                    });
                }
                if else_target.0 == join_block_idx {
                    if else_args.len() != 1 {
                        return None;
                    }
                    edges.push(PredEdge {
                        pred_idx,
                        kind: EdgeKind::CondBrElse,
                        arg: else_args[0].clone(),
                    });
                }
            }
            _ => {}
        }
    }

    if edges.is_empty() {
        return None;
    }

    #[derive(Clone, Debug)]
    struct EdgeRewrite {
        pred_idx: usize,
        kind: EdgeKind,
        tag: Operand,
        payload: Operand,
        remove_make_enum: Option<usize>,
    }

    let mut rewrites = Vec::<EdgeRewrite>::with_capacity(edges.len());
    for edge in edges {
        let (tag, payload, remove_make_enum) =
            unbox_option_operand_in_block(&func.blocks[edge.pred_idx], &edge.arg, use_counts)?;
        rewrites.push(EdgeRewrite {
            pred_idx: edge.pred_idx,
            kind: edge.kind,
            tag,
            payload,
            remove_make_enum,
        });
    }

    // Update join block signature and terminator.
    let tag_param = alloc_local(&mut func.locals);
    let payload_param = alloc_local(&mut func.locals);
    {
        let join = func.blocks.get_mut(join_block_idx)?;
        join.params = vec![tag_param, payload_param];
        join.terminator = Terminator::ReturnMulti {
            values: vec![Operand::Local(tag_param), Operand::Local(payload_param)],
        };
    }

    // Apply predecessor rewrites and remove now-dead `MakeEnum` instructions.
    let mut removals_by_block: BTreeMap<usize, Vec<usize>> = BTreeMap::new();
    for r in &rewrites {
        if let Some(idx) = r.remove_make_enum {
            removals_by_block.entry(r.pred_idx).or_default().push(idx);
        }

        let pred = func.blocks.get_mut(r.pred_idx)?;
        match (&mut pred.terminator, r.kind) {
            (Terminator::Br { target, args }, EdgeKind::Br) if target.0 == join_block_idx => {
                *args = vec![r.tag.clone(), r.payload.clone()];
            }
            (
                Terminator::CondBr {
                    then_target,
                    then_args,
                    else_target: _,
                    else_args: _,
                    ..
                },
                EdgeKind::CondBrThen,
            ) if then_target.0 == join_block_idx => {
                *then_args = vec![r.tag.clone(), r.payload.clone()];
            }
            (
                Terminator::CondBr {
                    then_target: _,
                    then_args: _,
                    else_target,
                    else_args,
                    ..
                },
                EdgeKind::CondBrElse,
            ) if else_target.0 == join_block_idx => {
                *else_args = vec![r.tag.clone(), r.payload.clone()];
            }
            _ => return None,
        }
    }

    for (block_idx, mut indices) in removals_by_block {
        indices.sort_unstable();
        indices.dedup();
        indices.sort_unstable_by(|a, b| b.cmp(a));
        let block = func.blocks.get_mut(block_idx)?;
        for idx in indices {
            if idx < block.instructions.len() {
                block.instructions.remove(idx);
            }
        }
    }

    Some(())
}

fn unbox_option_operand_in_block(
    block: &rusk_mir::BasicBlock,
    op: &Operand,
    use_counts: &[usize],
) -> Option<(Operand, Operand, Option<usize>)> {
    match op {
        Operand::Local(l) => {
            if l.0 >= use_counts.len() || use_counts[l.0] != 1 {
                return None;
            }
            let (instr_index, variant, fields) = find_make_enum_option_in_block(block, *l)?;
            match (variant.as_str(), fields.as_slice()) {
                (OPTION_VARIANT_SOME, [payload]) => Some((
                    Operand::Literal(ConstValue::Bool(true)),
                    payload.clone(),
                    Some(instr_index),
                )),
                (OPTION_VARIANT_NONE, []) => Some((
                    Operand::Literal(ConstValue::Bool(false)),
                    Operand::Literal(ConstValue::Unit),
                    Some(instr_index),
                )),
                _ => None,
            }
        }
        Operand::Literal(ConstValue::Enum {
            enum_name,
            variant,
            fields,
        }) if enum_name == OPTION_ENUM => match (variant.as_str(), fields.as_slice()) {
            (OPTION_VARIANT_SOME, [payload]) => Some((
                Operand::Literal(ConstValue::Bool(true)),
                Operand::Literal(payload.clone()),
                None,
            )),
            (OPTION_VARIANT_NONE, []) => Some((
                Operand::Literal(ConstValue::Bool(false)),
                Operand::Literal(ConstValue::Unit),
                None,
            )),
            _ => None,
        },
        _ => None,
    }
}

fn count_local_uses(func: &Function) -> Vec<usize> {
    let mut counts = vec![0usize; func.locals];

    fn bump(counts: &mut [usize], local: Local) {
        if local.0 < counts.len() {
            counts[local.0] = counts[local.0].saturating_add(1);
        }
    }

    fn bump_operand(counts: &mut [usize], op: &Operand) {
        if let Operand::Local(l) = op {
            bump(counts, *l);
        }
    }

    for block in &func.blocks {
        for instr in &block.instructions {
            match instr {
                Instruction::Const { .. } => {}
                Instruction::Copy { src, .. }
                | Instruction::Move { src, .. }
                | Instruction::AsReadonly { src, .. } => bump(&mut counts, *src),

                Instruction::IsType { value, ty, .. } => {
                    bump_operand(&mut counts, value);
                    bump_operand(&mut counts, ty);
                }

                Instruction::MakeTypeRep { args, .. } => {
                    for op in args {
                        bump_operand(&mut counts, op);
                    }
                }
                Instruction::AssocTypeRep { recv, .. } => {
                    bump_operand(&mut counts, recv);
                }

                Instruction::MakeStruct {
                    type_args, fields, ..
                } => {
                    for op in type_args {
                        bump_operand(&mut counts, op);
                    }
                    for (_, v) in fields {
                        bump_operand(&mut counts, v);
                    }
                }
                Instruction::MakeArray { items, .. } | Instruction::MakeTuple { items, .. } => {
                    for op in items {
                        bump_operand(&mut counts, op);
                    }
                }
                Instruction::MakeEnum {
                    type_args, fields, ..
                } => {
                    for op in type_args {
                        bump_operand(&mut counts, op);
                    }
                    for op in fields {
                        bump_operand(&mut counts, op);
                    }
                }

                Instruction::GetField { obj, .. } => bump_operand(&mut counts, obj),
                Instruction::SetField { obj, value, .. } => {
                    bump_operand(&mut counts, obj);
                    bump_operand(&mut counts, value);
                }
                Instruction::StructGet { obj, .. } => bump_operand(&mut counts, obj),
                Instruction::StructSet { obj, value, .. } => {
                    bump_operand(&mut counts, obj);
                    bump_operand(&mut counts, value);
                }
                Instruction::TupleGet { tup, .. } => bump_operand(&mut counts, tup),
                Instruction::TupleSet { tup, value, .. } => {
                    bump_operand(&mut counts, tup);
                    bump_operand(&mut counts, value);
                }
                Instruction::IndexGet { arr, idx, .. } => {
                    bump_operand(&mut counts, arr);
                    bump_operand(&mut counts, idx);
                }
                Instruction::IndexSet { arr, idx, value } => {
                    bump_operand(&mut counts, arr);
                    bump_operand(&mut counts, idx);
                    bump_operand(&mut counts, value);
                }
                Instruction::Len { arr, .. } => bump_operand(&mut counts, arr),

                Instruction::IntAdd { a, b, .. }
                | Instruction::IntSub { a, b, .. }
                | Instruction::IntMul { a, b, .. }
                | Instruction::IntDiv { a, b, .. }
                | Instruction::IntMod { a, b, .. }
                | Instruction::IntAnd { a, b, .. }
                | Instruction::IntOr { a, b, .. }
                | Instruction::IntXor { a, b, .. }
                | Instruction::IntShl { a, b, .. }
                | Instruction::IntShr { a, b, .. }
                | Instruction::IntUShr { a, b, .. }
                | Instruction::ByteAnd { a, b, .. }
                | Instruction::ByteOr { a, b, .. }
                | Instruction::ByteXor { a, b, .. }
                | Instruction::ByteShl { a, b, .. }
                | Instruction::ByteShr { a, b, .. }
                | Instruction::ByteUShr { a, b, .. }
                | Instruction::IntLt { a, b, .. }
                | Instruction::IntLe { a, b, .. }
                | Instruction::IntGt { a, b, .. }
                | Instruction::IntGe { a, b, .. }
                | Instruction::IntEq { a, b, .. }
                | Instruction::IntNe { a, b, .. }
                | Instruction::BoolEq { a, b, .. }
                | Instruction::BoolNe { a, b, .. } => {
                    bump_operand(&mut counts, a);
                    bump_operand(&mut counts, b);
                }
                Instruction::BoolNot { v, .. }
                | Instruction::IntNot { v, .. }
                | Instruction::ByteNot { v, .. } => bump_operand(&mut counts, v),

                Instruction::Call { args, .. }
                | Instruction::CallId { args, .. }
                | Instruction::CallIdMulti { args, .. } => {
                    for op in args {
                        bump_operand(&mut counts, op);
                    }
                }

                Instruction::VCall {
                    obj,
                    method_type_args,
                    args,
                    ..
                } => {
                    bump_operand(&mut counts, obj);
                    for op in method_type_args {
                        bump_operand(&mut counts, op);
                    }
                    for op in args {
                        bump_operand(&mut counts, op);
                    }
                }
                Instruction::SCall {
                    self_ty,
                    method_type_args,
                    args,
                    ..
                } => {
                    bump_operand(&mut counts, self_ty);
                    for op in method_type_args {
                        bump_operand(&mut counts, op);
                    }
                    for op in args {
                        bump_operand(&mut counts, op);
                    }
                }
                Instruction::ICall { fnptr, args, .. } => {
                    bump_operand(&mut counts, fnptr);
                    for op in args {
                        bump_operand(&mut counts, op);
                    }
                }

                Instruction::PushHandler { clauses, .. } => {
                    for clause in clauses {
                        for op in &clause.effect.interface_args {
                            bump_operand(&mut counts, op);
                        }
                    }
                }
                Instruction::PopHandler => {}
                Instruction::Perform { effect, args, .. } => {
                    for op in &effect.interface_args {
                        bump_operand(&mut counts, op);
                    }
                    for op in args {
                        bump_operand(&mut counts, op);
                    }
                }
                Instruction::Resume { k, value, .. } => {
                    bump_operand(&mut counts, k);
                    bump_operand(&mut counts, value);
                }
            }
        }

        match &block.terminator {
            Terminator::Br { args, .. } => {
                for op in args {
                    bump_operand(&mut counts, op);
                }
            }
            Terminator::CondBr {
                cond,
                then_args,
                else_args,
                ..
            } => {
                bump_operand(&mut counts, cond);
                for op in then_args {
                    bump_operand(&mut counts, op);
                }
                for op in else_args {
                    bump_operand(&mut counts, op);
                }
            }
            Terminator::Switch { value, .. } => bump_operand(&mut counts, value),
            Terminator::Return { value } => bump_operand(&mut counts, value),
            Terminator::ReturnMulti { values } => {
                for op in values {
                    bump_operand(&mut counts, op);
                }
            }
            Terminator::Trap { .. } => {}
        }
    }

    counts
}

fn find_make_enum_option_in_block(
    block: &rusk_mir::BasicBlock,
    dst: Local,
) -> Option<(usize, &String, &Vec<Operand>)> {
    for (i, instr) in block.instructions.iter().enumerate().rev() {
        let Instruction::MakeEnum {
            dst: make_dst,
            enum_name,
            variant,
            fields,
            ..
        } = instr
        else {
            continue;
        };
        if *make_dst != dst || enum_name != OPTION_ENUM {
            continue;
        }
        return Some((i, variant, fields));
    }
    None
}
