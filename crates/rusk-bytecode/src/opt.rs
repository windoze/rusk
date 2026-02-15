#![forbid(unsafe_code)]

extern crate alloc;

use alloc::collections::{BTreeMap, BTreeSet};
use alloc::string::String;
use alloc::vec::Vec;

use crate::{ConstValue, ExecutableModule, Function, Instruction, Reg};

/// Bytecode optimization level.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum OptLevel {
    /// No bytecode-level optimizations.
    O0,
    /// Peephole optimizations after lowering.
    O1,
    /// Peephole optimizations + block-local copy propagation and DCE.
    #[default]
    O2,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OptError {
    pub message: String,
}

impl OptError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl core::fmt::Display for OptError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "bytecode opt error: {}", self.message)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for OptError {}

/// Runs the bytecode peephole optimizer on an [`ExecutableModule`].
///
/// This pass is intended to run after MIR lowering, when all control-flow targets are patched to
/// absolute PCs.
pub fn peephole_optimize_module(
    module: &mut ExecutableModule,
    level: OptLevel,
) -> Result<(), OptError> {
    match level {
        OptLevel::O0 => Ok(()),
        OptLevel::O1 | OptLevel::O2 => {
            for func in &mut module.functions {
                peephole_optimize_function(func, level)?;
            }
            Ok(())
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ScalarConst {
    Bool(bool),
    Int(i64),
}

#[derive(Clone, Debug)]
struct IndexedInstr {
    old_pc: usize,
    instr: Instruction,
}

fn peephole_optimize_function(func: &mut Function, level: OptLevel) -> Result<(), OptError> {
    if func.code.is_empty() {
        return Ok(());
    }

    // Phase 1: Jump threading / trampoline folding (rewrite-only, no relocation needed).
    //
    // This is done before any deletion so we can safely discover trampolines on the original PC
    // layout, and then relocate everything once.
    {
        let label_pcs = collect_label_pcs(&func.code)?;
        let trampolines = collect_trampolines(&func.code, &label_pcs)?;
        if !trampolines.is_empty() {
            rewrite_pc_targets_in_code(&mut func.code, |pc| resolve_trampoline(pc, &trampolines));
        }
    }

    // Phase 2: Per-block local rewrites with relocation.
    let label_pcs = collect_label_pcs(&func.code)?;
    let old_len = func.code.len();
    let code_len: u32 = old_len
        .try_into()
        .map_err(|_| OptError::new("function too large (pc overflow)"))?;

    let blocks = block_ranges(&label_pcs, code_len);

    let (reads_per_block, read_block_count) = if level == OptLevel::O2 {
        collect_reg_reads_by_block(&func.code, &blocks)
    } else {
        (Vec::new(), BTreeMap::new())
    };

    let mut new_code: Vec<Instruction> = Vec::with_capacity(old_len);
    let mut old_to_new: Vec<Option<u32>> = Vec::with_capacity(old_len);
    old_to_new.resize(old_len, None);

    for (block_index, (start_pc, end_pc)) in blocks.iter().copied().enumerate() {
        let external_live = if level == OptLevel::O2 {
            external_live_regs(block_index, &reads_per_block, &read_block_count)
        } else {
            BTreeSet::new()
        };

        let mut const_env: BTreeMap<Reg, ScalarConst> = BTreeMap::new();
        let mut block_instrs: Vec<IndexedInstr> =
            Vec::with_capacity(end_pc.saturating_sub(start_pc) as usize);

        let start_usize = start_pc as usize;
        let end_usize = end_pc as usize;
        for (offset, instr) in func.code[start_usize..end_usize].iter().enumerate() {
            let old_pc_usize = start_usize + offset;
            let old_pc: u32 = old_pc_usize
                .try_into()
                .map_err(|_| OptError::new("function too large (pc overflow)"))?;

            // Rule B1: delete `Copy dst <- src` when `dst == src`.
            if let Instruction::Copy { dst, src } = instr {
                if dst == src {
                    continue;
                }
            }

            // Rule B2: delete `Jump next_pc`.
            if let Instruction::Jump { target_pc } = instr {
                let next_pc: u32 = old_pc.saturating_add(1);
                if *target_pc == next_pc {
                    continue;
                }
            }

            // Rule C: constant folding (local, block-scoped).
            let out = fold_const(instr, &const_env).unwrap_or_else(|| instr.clone());

            // Track scalar constant values produced by this instruction for subsequent folds.
            update_const_env(&mut const_env, &out);

            block_instrs.push(IndexedInstr {
                old_pc: old_pc_usize,
                instr: out,
            });
        }

        if level == OptLevel::O2 {
            optimize_block_o2(&mut block_instrs, &external_live);
        }

        remove_redundant_block_instructions(&mut block_instrs);

        for item in block_instrs {
            let new_pc: u32 = new_code.len().try_into().map_err(|_| {
                OptError::new("function too large after optimization (pc overflow)")
            })?;
            if let Some(slot) = old_to_new.get_mut(item.old_pc) {
                *slot = Some(new_pc);
            } else {
                return Err(OptError::new(format!(
                    "internal error: old pc {} out of bounds",
                    item.old_pc
                )));
            }
            new_code.push(item.instr);
        }
    }

    // Fill deleted PCs to point at the next kept instruction, so jumps to deleted instructions
    // remain valid (e.g. `Jump next_pc` removed at a label).
    let mut next_live: Option<u32> = None;
    for slot in old_to_new.iter_mut().rev() {
        match *slot {
            Some(pc) => next_live = Some(pc),
            None => *slot = next_live,
        }
    }

    relocate_pc_targets_in_code(&mut new_code, &old_to_new)?;
    validate_pc_targets(&new_code)?;

    func.code = new_code;
    Ok(())
}

fn block_ranges(label_pcs: &BTreeSet<u32>, code_len: u32) -> Vec<(u32, u32)> {
    let mut labels = label_pcs.iter().copied().collect::<Vec<_>>();
    labels.sort_unstable();
    labels.dedup();

    let mut blocks = Vec::with_capacity(labels.len().max(1));
    for (idx, start) in labels.iter().copied().enumerate() {
        if start >= code_len {
            continue;
        }
        let end = labels
            .get(idx + 1)
            .copied()
            .unwrap_or(code_len)
            .min(code_len);
        if start < end {
            blocks.push((start, end));
        }
    }

    if blocks.is_empty() && code_len > 0 {
        blocks.push((0, code_len));
    }

    blocks
}

fn collect_reg_reads_by_block(
    code: &[Instruction],
    blocks: &[(u32, u32)],
) -> (Vec<BTreeSet<Reg>>, BTreeMap<Reg, u32>) {
    let mut reads_per_block = Vec::with_capacity(blocks.len());
    let mut read_block_count: BTreeMap<Reg, u32> = BTreeMap::new();

    for (start, end) in blocks.iter().copied() {
        let mut reads = BTreeSet::<Reg>::new();
        for instr in &code[start as usize..end as usize] {
            for r in instr_read_regs(instr) {
                reads.insert(r);
            }
        }
        for r in &reads {
            *read_block_count.entry(*r).or_insert(0) += 1;
        }
        reads_per_block.push(reads);
    }

    (reads_per_block, read_block_count)
}

fn external_live_regs(
    block_index: usize,
    reads_per_block: &[BTreeSet<Reg>],
    read_block_count: &BTreeMap<Reg, u32>,
) -> BTreeSet<Reg> {
    let mut out = BTreeSet::<Reg>::new();
    let Some(this_reads) = reads_per_block.get(block_index) else {
        return out;
    };

    for (&reg, &count) in read_block_count {
        let this_has = this_reads.contains(&reg);
        let other = count.saturating_sub(if this_has { 1 } else { 0 });
        if other > 0 {
            out.insert(reg);
        }
    }
    out
}

fn optimize_block_o2(block: &mut Vec<IndexedInstr>, external_live: &BTreeSet<Reg>) {
    let _ = block_copy_sink_elim(block, external_live);
    block_copy_propagation(block);
    block_dce(block, external_live);
    let _ = block_copy_sink_elim(block, external_live);
    block_copy_propagation(block);
    block_dce(block, external_live);
}

fn block_copy_sink_elim(block: &mut Vec<IndexedInstr>, external_live: &BTreeSet<Reg>) -> bool {
    let mut changed = false;
    loop {
        let read_counts = count_reg_reads(block);
        let mut did_any = false;

        let mut i = 0;
        while i + 1 < block.len() {
            let Some(tmp) = instr_def_reg(&block[i].instr) else {
                i += 1;
                continue;
            };
            let Instruction::Copy { dst, src } = &block[i + 1].instr else {
                i += 1;
                continue;
            };
            if *src != tmp {
                i += 1;
                continue;
            }
            // If the temp register is live out of this block (read by other blocks), we cannot
            // retarget its defining instruction to another register; doing so would make `tmp`
            // uninitialized in successor blocks.
            if external_live.contains(&tmp) {
                i += 1;
                continue;
            }
            if read_counts.get(&tmp).copied().unwrap_or(0) != 1 {
                i += 1;
                continue;
            }

            let mut new_def = block[i].instr.clone();
            if !set_instr_dst(&mut new_def, *dst) {
                i += 1;
                continue;
            }

            block[i].instr = new_def;
            block.remove(i + 1);
            did_any = true;
            changed = true;
        }

        if !did_any {
            break;
        }
    }

    changed
}

fn block_copy_propagation(block: &mut [IndexedInstr]) {
    let mut alias: BTreeMap<Reg, Reg> = BTreeMap::new();

    for item in block {
        let resolve = |r: Reg, alias: &BTreeMap<Reg, Reg>| resolve_alias(r, alias);

        map_instr_reads(&mut item.instr, |r| resolve(r, &alias));

        // Invalidate aliases clobbered by writes (including `Move` killing its source).
        let writes = instr_write_regs(&item.instr);
        for w in &writes {
            alias.remove(w);
        }
        for w in &writes {
            let dependents = alias
                .iter()
                .filter_map(|(&dst, &src)| if src == *w { Some(dst) } else { None })
                .collect::<Vec<_>>();
            for dst in dependents {
                alias.remove(&dst);
            }
        }

        if let Instruction::Copy { dst, src } = &item.instr {
            if dst != src {
                let src = resolve(*src, &alias);
                alias.insert(*dst, src);
            }
        }
    }
}

fn block_dce(block: &mut Vec<IndexedInstr>, external_live: &BTreeSet<Reg>) {
    if block.is_empty() {
        return;
    }

    let mut live: BTreeSet<Reg> = external_live.clone();
    let mut keep = vec![true; block.len()];

    for (idx, item) in block.iter().enumerate().rev() {
        let reads = instr_read_regs(&item.instr);
        let writes = instr_write_regs(&item.instr);

        let removable = matches!(
            item.instr,
            Instruction::Const { .. } | Instruction::Copy { .. } | Instruction::AsReadonly { .. }
        );

        let all_defs_dead = writes.iter().all(|r| !live.contains(r));
        if removable && !writes.is_empty() && all_defs_dead {
            keep[idx] = false;
            continue;
        }

        for w in writes {
            let _ = live.remove(&w);
        }
        for r in reads {
            live.insert(r);
        }
    }

    let mut out = Vec::with_capacity(block.len());
    for (idx, item) in block.iter().cloned().enumerate() {
        if keep[idx] {
            out.push(item);
        }
    }
    *block = out;
}

fn remove_redundant_block_instructions(block: &mut Vec<IndexedInstr>) {
    if block.is_empty() {
        return;
    }

    let mut out = Vec::with_capacity(block.len());
    for (idx, item) in block.iter().cloned().enumerate() {
        match &item.instr {
            Instruction::Copy { dst, src } if dst == src => continue,
            Instruction::Jump { target_pc } => {
                if let Some(next) = block.get(idx + 1) {
                    let next_pc: u32 = next.old_pc.try_into().unwrap_or(u32::MAX);
                    if *target_pc == next_pc {
                        continue;
                    }
                }
            }
            _ => {}
        }

        out.push(item);
    }

    *block = out;
}

fn count_reg_reads(block: &[IndexedInstr]) -> BTreeMap<Reg, u32> {
    let mut counts: BTreeMap<Reg, u32> = BTreeMap::new();
    for item in block {
        for r in instr_read_regs(&item.instr) {
            *counts.entry(r).or_insert(0) += 1;
        }
    }
    counts
}

fn instr_def_reg(instr: &Instruction) -> Option<Reg> {
    match instr {
        Instruction::Const { dst, .. }
        | Instruction::Copy { dst, .. }
        | Instruction::Move { dst, .. }
        | Instruction::AsReadonly { dst, .. }
        | Instruction::IsType { dst, .. }
        | Instruction::CheckedCast { dst, .. }
        | Instruction::MakeTypeRep { dst, .. }
        | Instruction::MakeStruct { dst, .. }
        | Instruction::MakeArray { dst, .. }
        | Instruction::MakeTuple { dst, .. }
        | Instruction::MakeEnum { dst, .. }
        | Instruction::GetField { dst, .. }
        | Instruction::StructGet { dst, .. }
        | Instruction::TupleGet { dst, .. }
        | Instruction::IndexGet { dst, .. }
        | Instruction::Len { dst, .. }
        | Instruction::IntAdd { dst, .. }
        | Instruction::IntSub { dst, .. }
        | Instruction::IntMul { dst, .. }
        | Instruction::IntDiv { dst, .. }
        | Instruction::IntMod { dst, .. }
        | Instruction::IntLt { dst, .. }
        | Instruction::IntLe { dst, .. }
        | Instruction::IntGt { dst, .. }
        | Instruction::IntGe { dst, .. }
        | Instruction::IntEq { dst, .. }
        | Instruction::IntNe { dst, .. }
        | Instruction::BoolNot { dst, .. }
        | Instruction::BoolEq { dst, .. }
        | Instruction::BoolNe { dst, .. } => Some(*dst),

        Instruction::Call { dst: Some(dst), .. }
        | Instruction::ICall { dst: Some(dst), .. }
        | Instruction::VCall { dst: Some(dst), .. }
        | Instruction::Perform { dst: Some(dst), .. }
        | Instruction::Resume { dst: Some(dst), .. } => Some(*dst),

        _ => None,
    }
}

fn set_instr_dst(instr: &mut Instruction, new_dst: Reg) -> bool {
    match instr {
        Instruction::Const { dst, .. }
        | Instruction::Copy { dst, .. }
        | Instruction::Move { dst, .. }
        | Instruction::AsReadonly { dst, .. }
        | Instruction::IsType { dst, .. }
        | Instruction::CheckedCast { dst, .. }
        | Instruction::MakeTypeRep { dst, .. }
        | Instruction::MakeStruct { dst, .. }
        | Instruction::MakeArray { dst, .. }
        | Instruction::MakeTuple { dst, .. }
        | Instruction::MakeEnum { dst, .. }
        | Instruction::GetField { dst, .. }
        | Instruction::StructGet { dst, .. }
        | Instruction::TupleGet { dst, .. }
        | Instruction::IndexGet { dst, .. }
        | Instruction::Len { dst, .. }
        | Instruction::IntAdd { dst, .. }
        | Instruction::IntSub { dst, .. }
        | Instruction::IntMul { dst, .. }
        | Instruction::IntDiv { dst, .. }
        | Instruction::IntMod { dst, .. }
        | Instruction::IntLt { dst, .. }
        | Instruction::IntLe { dst, .. }
        | Instruction::IntGt { dst, .. }
        | Instruction::IntGe { dst, .. }
        | Instruction::IntEq { dst, .. }
        | Instruction::IntNe { dst, .. }
        | Instruction::BoolNot { dst, .. }
        | Instruction::BoolEq { dst, .. }
        | Instruction::BoolNe { dst, .. } => {
            *dst = new_dst;
            true
        }

        Instruction::Call { dst, .. }
        | Instruction::ICall { dst, .. }
        | Instruction::VCall { dst, .. }
        | Instruction::Perform { dst, .. }
        | Instruction::Resume { dst, .. } => {
            if dst.is_some() {
                *dst = Some(new_dst);
                true
            } else {
                false
            }
        }

        _ => false,
    }
}

fn instr_read_regs(instr: &Instruction) -> Vec<Reg> {
    match instr {
        Instruction::Const { .. } => Vec::new(),
        Instruction::Copy { src, .. } => vec![*src],
        Instruction::Move { src, .. } => vec![*src],
        Instruction::AsReadonly { src, .. } => vec![*src],
        Instruction::IsType { value, ty, .. } => vec![*value, *ty],
        Instruction::CheckedCast { value, ty, .. } => vec![*value, *ty],

        Instruction::MakeTypeRep { args, .. } => args.clone(),
        Instruction::MakeStruct {
            type_args, fields, ..
        } => type_args
            .iter()
            .copied()
            .chain(fields.iter().map(|(_, r)| *r))
            .collect(),
        Instruction::MakeArray { items, .. } => items.clone(),
        Instruction::MakeTuple { items, .. } => items.clone(),
        Instruction::MakeEnum {
            type_args, fields, ..
        } => type_args
            .iter()
            .copied()
            .chain(fields.iter().copied())
            .collect(),

        Instruction::GetField { obj, .. } => vec![*obj],
        Instruction::SetField { obj, value, .. } => vec![*obj, *value],
        Instruction::StructGet { obj, .. } => vec![*obj],
        Instruction::StructSet { obj, value, .. } => vec![*obj, *value],
        Instruction::TupleGet { tup, .. } => vec![*tup],
        Instruction::TupleSet { tup, value, .. } => vec![*tup, *value],
        Instruction::IndexGet { arr, idx, .. } => vec![*arr, *idx],
        Instruction::IndexSet { arr, idx, value } => vec![*arr, *idx, *value],
        Instruction::Len { arr, .. } => vec![*arr],

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
        | Instruction::BoolNe { a, b, .. } => vec![*a, *b],

        Instruction::BoolNot { v, .. } => vec![*v],

        Instruction::Call { args, .. } => args.clone(),
        Instruction::ICall { fnptr, args, .. } => {
            let mut out = Vec::with_capacity(1 + args.len());
            out.push(*fnptr);
            out.extend(args.iter().copied());
            out
        }
        Instruction::VCall {
            obj,
            method_type_args,
            args,
            ..
        } => {
            let mut out = Vec::with_capacity(1 + method_type_args.len() + args.len());
            out.push(*obj);
            out.extend(method_type_args.iter().copied());
            out.extend(args.iter().copied());
            out
        }

        Instruction::PushHandler { clauses } => clauses
            .iter()
            .flat_map(|c| c.effect.interface_args.iter().copied())
            .collect(),
        Instruction::PopHandler => Vec::new(),
        Instruction::Perform { effect, args, .. } => effect
            .interface_args
            .iter()
            .copied()
            .chain(args.iter().copied())
            .collect(),
        Instruction::Resume { k, value, .. } => vec![*k, *value],

        Instruction::Jump { .. } => Vec::new(),
        Instruction::JumpIf { cond, .. } => vec![*cond],
        Instruction::Switch { value, .. } => vec![*value],

        Instruction::Return { value } => vec![*value],
        Instruction::Trap { .. } => Vec::new(),
    }
}

fn instr_write_regs(instr: &Instruction) -> Vec<Reg> {
    match instr {
        Instruction::Const { dst, .. }
        | Instruction::Copy { dst, .. }
        | Instruction::AsReadonly { dst, .. }
        | Instruction::IsType { dst, .. }
        | Instruction::CheckedCast { dst, .. }
        | Instruction::MakeTypeRep { dst, .. }
        | Instruction::MakeStruct { dst, .. }
        | Instruction::MakeArray { dst, .. }
        | Instruction::MakeTuple { dst, .. }
        | Instruction::MakeEnum { dst, .. }
        | Instruction::GetField { dst, .. }
        | Instruction::StructGet { dst, .. }
        | Instruction::TupleGet { dst, .. }
        | Instruction::IndexGet { dst, .. }
        | Instruction::Len { dst, .. }
        | Instruction::IntAdd { dst, .. }
        | Instruction::IntSub { dst, .. }
        | Instruction::IntMul { dst, .. }
        | Instruction::IntDiv { dst, .. }
        | Instruction::IntMod { dst, .. }
        | Instruction::IntLt { dst, .. }
        | Instruction::IntLe { dst, .. }
        | Instruction::IntGt { dst, .. }
        | Instruction::IntGe { dst, .. }
        | Instruction::IntEq { dst, .. }
        | Instruction::IntNe { dst, .. }
        | Instruction::BoolNot { dst, .. }
        | Instruction::BoolEq { dst, .. }
        | Instruction::BoolNe { dst, .. } => vec![*dst],

        Instruction::Move { dst, src } => vec![*dst, *src],

        Instruction::Call { dst: Some(dst), .. }
        | Instruction::ICall { dst: Some(dst), .. }
        | Instruction::VCall { dst: Some(dst), .. }
        | Instruction::Perform { dst: Some(dst), .. }
        | Instruction::Resume { dst: Some(dst), .. } => vec![*dst],

        _ => Vec::new(),
    }
}

fn resolve_alias(mut reg: Reg, alias: &BTreeMap<Reg, Reg>) -> Reg {
    let mut visited = BTreeSet::<Reg>::new();
    while let Some(next) = alias.get(&reg).copied() {
        if !visited.insert(reg) {
            break;
        }
        reg = next;
    }
    reg
}

fn map_instr_reads(instr: &mut Instruction, mut f: impl FnMut(Reg) -> Reg) {
    match instr {
        Instruction::Const { .. } => {}
        Instruction::Copy { src, .. } => *src = f(*src),
        Instruction::Move { .. } => {
            // Do not rewrite `Move { src }`: it consumes/clears the source register.
        }
        Instruction::AsReadonly { src, .. } => *src = f(*src),
        Instruction::IsType { value, ty, .. } => {
            *value = f(*value);
            *ty = f(*ty);
        }
        Instruction::CheckedCast { value, ty, .. } => {
            *value = f(*value);
            *ty = f(*ty);
        }

        Instruction::MakeTypeRep { args, .. } => {
            for r in args {
                *r = f(*r);
            }
        }
        Instruction::MakeStruct {
            type_args, fields, ..
        } => {
            for r in type_args {
                *r = f(*r);
            }
            for (_, r) in fields {
                *r = f(*r);
            }
        }
        Instruction::MakeArray { items, .. } | Instruction::MakeTuple { items, .. } => {
            for r in items {
                *r = f(*r);
            }
        }
        Instruction::MakeEnum {
            type_args, fields, ..
        } => {
            for r in type_args {
                *r = f(*r);
            }
            for r in fields {
                *r = f(*r);
            }
        }

        Instruction::GetField { obj, .. } => *obj = f(*obj),
        Instruction::SetField { obj, value, .. } => {
            *obj = f(*obj);
            *value = f(*value);
        }
        Instruction::StructGet { obj, .. } => *obj = f(*obj),
        Instruction::StructSet { obj, value, .. } => {
            *obj = f(*obj);
            *value = f(*value);
        }
        Instruction::TupleGet { tup, .. } => *tup = f(*tup),
        Instruction::TupleSet { tup, value, .. } => {
            *tup = f(*tup);
            *value = f(*value);
        }
        Instruction::IndexGet { arr, idx, .. } => {
            *arr = f(*arr);
            *idx = f(*idx);
        }
        Instruction::IndexSet { arr, idx, value } => {
            *arr = f(*arr);
            *idx = f(*idx);
            *value = f(*value);
        }
        Instruction::Len { arr, .. } => *arr = f(*arr),

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
            *a = f(*a);
            *b = f(*b);
        }
        Instruction::BoolNot { v, .. } => *v = f(*v),

        Instruction::Call { args, .. } => {
            for r in args {
                *r = f(*r);
            }
        }
        Instruction::ICall { fnptr, args, .. } => {
            *fnptr = f(*fnptr);
            for r in args {
                *r = f(*r);
            }
        }
        Instruction::VCall {
            obj,
            method_type_args,
            args,
            ..
        } => {
            *obj = f(*obj);
            for r in method_type_args {
                *r = f(*r);
            }
            for r in args {
                *r = f(*r);
            }
        }

        Instruction::PushHandler { clauses } => {
            for clause in clauses {
                for r in &mut clause.effect.interface_args {
                    *r = f(*r);
                }
            }
        }
        Instruction::PopHandler => {}
        Instruction::Perform { effect, args, .. } => {
            for r in &mut effect.interface_args {
                *r = f(*r);
            }
            for r in args {
                *r = f(*r);
            }
        }
        Instruction::Resume { k, value, .. } => {
            *k = f(*k);
            *value = f(*value);
        }

        Instruction::Jump { .. } => {}
        Instruction::JumpIf { cond, .. } => *cond = f(*cond),
        Instruction::Switch { value, .. } => *value = f(*value),

        Instruction::Return { value } => *value = f(*value),
        Instruction::Trap { .. } => {}
    }
}

fn collect_label_pcs(code: &[Instruction]) -> Result<BTreeSet<u32>, OptError> {
    let mut labels = BTreeSet::<u32>::new();
    labels.insert(0);
    for instr in code {
        match instr {
            Instruction::Jump { target_pc } => {
                labels.insert(*target_pc);
            }
            Instruction::JumpIf {
                then_pc, else_pc, ..
            } => {
                labels.insert(*then_pc);
                labels.insert(*else_pc);
            }
            Instruction::Switch {
                cases, default_pc, ..
            } => {
                labels.insert(*default_pc);
                for case in cases {
                    labels.insert(case.target_pc);
                }
            }
            Instruction::PushHandler { clauses } => {
                for clause in clauses {
                    labels.insert(clause.target_pc);
                }
            }
            _ => {}
        }
    }

    // Defensive: ensure 0 is within the code bounds if non-empty.
    if !code.is_empty() && labels.first().copied() != Some(0) {
        return Err(OptError::new("internal error: label set missing pc 0"));
    }

    Ok(labels)
}

fn collect_trampolines(
    code: &[Instruction],
    label_pcs: &BTreeSet<u32>,
) -> Result<BTreeMap<u32, u32>, OptError> {
    let mut out = BTreeMap::<u32, u32>::new();
    let code_len: u32 = code
        .len()
        .try_into()
        .map_err(|_| OptError::new("function too large (pc overflow)"))?;

    // Iterate over basic blocks as ranges [label_i, label_{i+1}) to see if the block has exactly
    // one instruction and that instruction is an unconditional jump.
    let labels = label_pcs.iter().copied().collect::<Vec<_>>();
    for (idx, start_pc) in labels.iter().copied().enumerate() {
        if start_pc >= code_len {
            // Label target out of bounds (should not happen for compiler-produced bytecode, but we
            // keep the optimizer robust as a library API).
            continue;
        }
        let end_pc = labels
            .get(idx + 1)
            .copied()
            .unwrap_or(code_len)
            .min(code_len);

        if end_pc != start_pc.saturating_add(1) {
            continue;
        }

        let Some(instr) = code.get(start_pc as usize) else {
            continue;
        };
        let Instruction::Jump { target_pc } = instr else {
            continue;
        };
        out.insert(start_pc, *target_pc);
    }

    Ok(out)
}

fn resolve_trampoline(mut pc: u32, trampolines: &BTreeMap<u32, u32>) -> u32 {
    // Follow transitive trampoline chains; if a cycle is present, stop at the first repeated PC.
    let mut visited = BTreeSet::<u32>::new();
    while let Some(next) = trampolines.get(&pc).copied() {
        if !visited.insert(pc) {
            break;
        }
        pc = next;
    }
    pc
}

fn rewrite_pc_targets_in_code(code: &mut [Instruction], mut f: impl FnMut(u32) -> u32) {
    for instr in code {
        match instr {
            Instruction::Jump { target_pc } => *target_pc = f(*target_pc),
            Instruction::JumpIf {
                then_pc, else_pc, ..
            } => {
                *then_pc = f(*then_pc);
                *else_pc = f(*else_pc);
            }
            Instruction::Switch {
                cases, default_pc, ..
            } => {
                *default_pc = f(*default_pc);
                for case in cases {
                    case.target_pc = f(case.target_pc);
                }
            }
            Instruction::PushHandler { clauses } => {
                for clause in clauses {
                    clause.target_pc = f(clause.target_pc);
                }
            }
            _ => {}
        }
    }
}

fn relocate_pc_targets_in_code(
    code: &mut [Instruction],
    old_to_new: &[Option<u32>],
) -> Result<(), OptError> {
    let map = |old_pc: u32| -> Result<u32, OptError> {
        let idx: usize = old_pc
            .try_into()
            .map_err(|_| OptError::new("pc overflow"))?;
        let Some(slot) = old_to_new.get(idx) else {
            return Err(OptError::new(format!(
                "invalid jump target pc {old_pc} (out of bounds after relocation)"
            )));
        };
        slot.ok_or_else(|| {
            OptError::new(format!(
                "jump target pc {old_pc} was deleted and could not be remapped"
            ))
        })
    };

    for instr in code {
        match instr {
            Instruction::Jump { target_pc } => *target_pc = map(*target_pc)?,
            Instruction::JumpIf {
                then_pc, else_pc, ..
            } => {
                *then_pc = map(*then_pc)?;
                *else_pc = map(*else_pc)?;
            }
            Instruction::Switch {
                cases, default_pc, ..
            } => {
                *default_pc = map(*default_pc)?;
                for case in cases {
                    case.target_pc = map(case.target_pc)?;
                }
            }
            Instruction::PushHandler { clauses } => {
                for clause in clauses {
                    clause.target_pc = map(clause.target_pc)?;
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn validate_pc_targets(code: &[Instruction]) -> Result<(), OptError> {
    let len: u32 = code
        .len()
        .try_into()
        .map_err(|_| OptError::new("function too large (pc overflow)"))?;
    for (pc, instr) in code.iter().enumerate() {
        let pc: u32 = pc
            .try_into()
            .map_err(|_| OptError::new("function too large (pc overflow)"))?;
        let here = || format!("pc {pc}");
        match instr {
            Instruction::Jump { target_pc } => {
                if *target_pc >= len {
                    return Err(OptError::new(format!(
                        "{}: invalid jump target {} (len={})",
                        here(),
                        target_pc,
                        len
                    )));
                }
            }
            Instruction::JumpIf {
                then_pc, else_pc, ..
            } => {
                if *then_pc >= len || *else_pc >= len {
                    return Err(OptError::new(format!(
                        "{}: invalid jumpif targets then={} else={} (len={})",
                        here(),
                        then_pc,
                        else_pc,
                        len
                    )));
                }
            }
            Instruction::Switch {
                cases, default_pc, ..
            } => {
                if *default_pc >= len {
                    return Err(OptError::new(format!(
                        "{}: invalid switch default {} (len={})",
                        here(),
                        default_pc,
                        len
                    )));
                }
                for case in cases {
                    if case.target_pc >= len {
                        return Err(OptError::new(format!(
                            "{}: invalid switch case target {} (len={})",
                            here(),
                            case.target_pc,
                            len
                        )));
                    }
                }
            }
            Instruction::PushHandler { clauses } => {
                for clause in clauses {
                    if clause.target_pc >= len {
                        return Err(OptError::new(format!(
                            "{}: invalid handler target {} (len={})",
                            here(),
                            clause.target_pc,
                            len
                        )));
                    }
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn fold_const(instr: &Instruction, env: &BTreeMap<Reg, ScalarConst>) -> Option<Instruction> {
    match instr {
        Instruction::IntAdd { dst, a, b } => {
            let (a, b) = (read_int(env, *a)?, read_int(env, *b)?);
            let v = a.checked_add(b)?;
            Some(Instruction::Const {
                dst: *dst,
                value: ConstValue::Int(v),
            })
        }
        Instruction::IntSub { dst, a, b } => {
            let (a, b) = (read_int(env, *a)?, read_int(env, *b)?);
            let v = a.checked_sub(b)?;
            Some(Instruction::Const {
                dst: *dst,
                value: ConstValue::Int(v),
            })
        }
        Instruction::IntMul { dst, a, b } => {
            let (a, b) = (read_int(env, *a)?, read_int(env, *b)?);
            let v = a.checked_mul(b)?;
            Some(Instruction::Const {
                dst: *dst,
                value: ConstValue::Int(v),
            })
        }
        Instruction::IntDiv { dst, a, b } => {
            let (a, b) = (read_int(env, *a)?, read_int(env, *b)?);
            if b == 0 {
                return None;
            }
            let v = a.checked_div(b)?;
            Some(Instruction::Const {
                dst: *dst,
                value: ConstValue::Int(v),
            })
        }
        Instruction::IntMod { dst, a, b } => {
            let (a, b) = (read_int(env, *a)?, read_int(env, *b)?);
            if b == 0 {
                return None;
            }
            let v = a.checked_rem(b)?;
            Some(Instruction::Const {
                dst: *dst,
                value: ConstValue::Int(v),
            })
        }

        Instruction::IntLt { dst, a, b } => Some(Instruction::Const {
            dst: *dst,
            value: ConstValue::Bool(read_int(env, *a)? < read_int(env, *b)?),
        }),
        Instruction::IntLe { dst, a, b } => Some(Instruction::Const {
            dst: *dst,
            value: ConstValue::Bool(read_int(env, *a)? <= read_int(env, *b)?),
        }),
        Instruction::IntGt { dst, a, b } => Some(Instruction::Const {
            dst: *dst,
            value: ConstValue::Bool(read_int(env, *a)? > read_int(env, *b)?),
        }),
        Instruction::IntGe { dst, a, b } => Some(Instruction::Const {
            dst: *dst,
            value: ConstValue::Bool(read_int(env, *a)? >= read_int(env, *b)?),
        }),
        Instruction::IntEq { dst, a, b } => Some(Instruction::Const {
            dst: *dst,
            value: ConstValue::Bool(read_int(env, *a)? == read_int(env, *b)?),
        }),
        Instruction::IntNe { dst, a, b } => Some(Instruction::Const {
            dst: *dst,
            value: ConstValue::Bool(read_int(env, *a)? != read_int(env, *b)?),
        }),

        Instruction::BoolNot { dst, v } => Some(Instruction::Const {
            dst: *dst,
            value: ConstValue::Bool(!read_bool(env, *v)?),
        }),
        Instruction::BoolEq { dst, a, b } => Some(Instruction::Const {
            dst: *dst,
            value: ConstValue::Bool(read_bool(env, *a)? == read_bool(env, *b)?),
        }),
        Instruction::BoolNe { dst, a, b } => Some(Instruction::Const {
            dst: *dst,
            value: ConstValue::Bool(read_bool(env, *a)? != read_bool(env, *b)?),
        }),

        _ => None,
    }
}

fn update_const_env(env: &mut BTreeMap<Reg, ScalarConst>, instr: &Instruction) {
    match instr {
        Instruction::Const { dst, value } => match scalar_const_from_value(value) {
            Some(v) => {
                env.insert(*dst, v);
            }
            None => {
                env.remove(dst);
            }
        },
        Instruction::Copy { dst, src } => match env.get(src).copied() {
            Some(v) => {
                env.insert(*dst, v);
            }
            None => {
                env.remove(dst);
            }
        },
        Instruction::Move { dst, src } => {
            let v = env.get(src).copied();
            env.remove(src);
            match v {
                Some(v) => {
                    env.insert(*dst, v);
                }
                None => {
                    env.remove(dst);
                }
            }
        }
        // Any other write to `dst` invalidates our scalar const view.
        Instruction::AsReadonly { dst, .. }
        | Instruction::IsType { dst, .. }
        | Instruction::CheckedCast { dst, .. }
        | Instruction::MakeTypeRep { dst, .. }
        | Instruction::MakeStruct { dst, .. }
        | Instruction::MakeArray { dst, .. }
        | Instruction::MakeTuple { dst, .. }
        | Instruction::MakeEnum { dst, .. }
        | Instruction::GetField { dst, .. }
        | Instruction::StructGet { dst, .. }
        | Instruction::TupleGet { dst, .. }
        | Instruction::IndexGet { dst, .. }
        | Instruction::Len { dst, .. }
        | Instruction::IntAdd { dst, .. }
        | Instruction::IntSub { dst, .. }
        | Instruction::IntMul { dst, .. }
        | Instruction::IntDiv { dst, .. }
        | Instruction::IntMod { dst, .. }
        | Instruction::IntLt { dst, .. }
        | Instruction::IntLe { dst, .. }
        | Instruction::IntGt { dst, .. }
        | Instruction::IntGe { dst, .. }
        | Instruction::IntEq { dst, .. }
        | Instruction::IntNe { dst, .. }
        | Instruction::BoolNot { dst, .. }
        | Instruction::BoolEq { dst, .. }
        | Instruction::BoolNe { dst, .. }
        | Instruction::Call { dst: Some(dst), .. }
        | Instruction::ICall { dst: Some(dst), .. }
        | Instruction::VCall { dst: Some(dst), .. }
        | Instruction::Perform { dst: Some(dst), .. }
        | Instruction::Resume { dst: Some(dst), .. } => {
            env.remove(dst);
        }
        _ => {}
    }
}

fn scalar_const_from_value(value: &ConstValue) -> Option<ScalarConst> {
    match value {
        ConstValue::Bool(v) => Some(ScalarConst::Bool(*v)),
        ConstValue::Int(v) => Some(ScalarConst::Int(*v)),
        _ => None,
    }
}

fn read_int(env: &BTreeMap<Reg, ScalarConst>, reg: Reg) -> Option<i64> {
    match env.get(&reg).copied()? {
        ScalarConst::Int(v) => Some(v),
        ScalarConst::Bool(_) => None,
    }
}

fn read_bool(env: &BTreeMap<Reg, ScalarConst>, reg: Reg) -> Option<bool> {
    match env.get(&reg).copied()? {
        ScalarConst::Bool(v) => Some(v),
        ScalarConst::Int(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{EffectSpec, HandlerClause, SwitchCase, verify_module};

    fn module_with_function(code: Vec<Instruction>, reg_count: u32) -> ExecutableModule {
        let func = Function {
            name: "main".to_string(),
            reg_count,
            param_count: 0,
            code,
        };
        let mut module = ExecutableModule::default();
        module.add_function(func).unwrap();
        module.entry = crate::FunctionId(0);
        module
    }

    #[test]
    fn jump_threading_folds_trampoline_edges() {
        // pc0: JumpIf -> pc1 / pc2
        // pc1: Jump -> pc3 (trampoline)
        // pc2: Jump -> pc4 (trampoline)
        // pc3: Return
        // pc4: Return
        let mut module = module_with_function(
            vec![
                Instruction::JumpIf {
                    cond: 0,
                    then_pc: 1,
                    else_pc: 2,
                },
                Instruction::Jump { target_pc: 3 },
                Instruction::Jump { target_pc: 4 },
                Instruction::Return { value: 0 },
                Instruction::Return { value: 0 },
            ],
            1,
        );

        peephole_optimize_module(&mut module, OptLevel::O1).expect("optimize");
        let main = module.function(module.entry).unwrap();

        let Instruction::JumpIf {
            then_pc, else_pc, ..
        } = main.code[0]
        else {
            panic!("expected jumpif at pc0, got {:?}", main.code[0]);
        };
        assert_eq!(then_pc, 3, "then_pc should skip trampoline at pc1");
        assert_eq!(else_pc, 4, "else_pc should skip trampoline at pc2");
    }

    #[test]
    fn relocation_updates_switch_and_handler_targets() {
        // Delete pc0 (Copy 0<-0), ensure all target PCs are remapped.
        let mut module = module_with_function(
            vec![
                Instruction::Copy { dst: 0, src: 0 }, // deleted
                Instruction::PushHandler {
                    clauses: vec![HandlerClause {
                        effect: EffectSpec {
                            interface: "E".to_string(),
                            interface_args: vec![],
                            method: "m".to_string(),
                        },
                        arg_patterns: vec![],
                        target_pc: 4,
                        param_regs: vec![0],
                    }],
                },
                Instruction::Switch {
                    value: 0,
                    cases: vec![SwitchCase {
                        pattern: rusk_mir::Pattern::Wildcard,
                        target_pc: 4,
                        param_regs: vec![],
                    }],
                    default_pc: 3,
                },
                Instruction::Jump { target_pc: 4 },
                Instruction::Return { value: 0 },
            ],
            1,
        );

        peephole_optimize_module(&mut module, OptLevel::O1).expect("optimize");
        verify_module(&module).expect("verify");

        let main = module.function(module.entry).unwrap();
        // After deletion, old pc4 becomes new pc3.
        let Instruction::PushHandler { clauses } = &main.code[0] else {
            panic!("expected push_handler at pc0, got {:?}", main.code[0]);
        };
        assert_eq!(clauses[0].target_pc, 3);

        let Instruction::Switch {
            cases, default_pc, ..
        } = &main.code[1]
        else {
            panic!("expected switch at pc1, got {:?}", main.code[1]);
        };
        assert_eq!(cases[0].target_pc, 3);
        assert_eq!(*default_pc, 2);
    }

    #[test]
    fn constant_folding_rewrites_int_add_to_const() {
        let mut module = module_with_function(
            vec![
                Instruction::Const {
                    dst: 0,
                    value: ConstValue::Int(1),
                },
                Instruction::Const {
                    dst: 0,
                    value: ConstValue::Int(2),
                },
                Instruction::IntAdd { dst: 0, a: 0, b: 0 },
                Instruction::Return { value: 0 },
            ],
            1,
        );

        peephole_optimize_module(&mut module, OptLevel::O1).expect("optimize");
        let main = module.function(module.entry).unwrap();
        assert!(
            matches!(
                &main.code[2],
                Instruction::Const {
                    value: ConstValue::Int(4),
                    ..
                }
            ),
            "expected folded const at pc2, got {:?}",
            main.code[2]
        );
    }

    #[test]
    fn constant_folding_does_not_fold_div_by_zero() {
        let mut module = module_with_function(
            vec![
                Instruction::Const {
                    dst: 0,
                    value: ConstValue::Int(1),
                },
                Instruction::Const {
                    dst: 0,
                    value: ConstValue::Int(0),
                },
                Instruction::IntDiv { dst: 0, a: 0, b: 0 },
                Instruction::Return { value: 0 },
            ],
            1,
        );

        peephole_optimize_module(&mut module, OptLevel::O1).expect("optimize");
        let main = module.function(module.entry).unwrap();
        assert!(
            matches!(&main.code[2], Instruction::IntDiv { .. }),
            "expected IntDiv to remain (no fold), got {:?}",
            main.code[2]
        );
    }

    #[test]
    fn o2_eliminates_copy_sink_by_rewriting_dst() {
        let mut module = module_with_function(
            vec![
                Instruction::Const {
                    dst: 1,
                    value: ConstValue::Int(123),
                },
                Instruction::Copy { dst: 0, src: 1 },
                Instruction::Return { value: 0 },
            ],
            2,
        );

        peephole_optimize_module(&mut module, OptLevel::O2).expect("optimize");
        verify_module(&module).expect("verify");

        let main = module.function(module.entry).unwrap();
        assert!(
            main.code
                .iter()
                .all(|i| !matches!(i, Instruction::Copy { .. })),
            "expected copy to be eliminated, got {:?}",
            main.code
        );
        assert!(
            matches!(
                &main.code[0],
                Instruction::Const {
                    dst: 0,
                    value: ConstValue::Int(123),
                }
            ),
            "expected const to write directly to dst=0, got {:?}",
            main.code[0]
        );
    }

    #[test]
    fn o2_eliminates_transitive_copy_chain() {
        let mut module = module_with_function(
            vec![
                Instruction::Const {
                    dst: 0,
                    value: ConstValue::Int(1),
                },
                Instruction::Copy { dst: 1, src: 0 },
                Instruction::Copy { dst: 2, src: 1 },
                Instruction::Return { value: 2 },
            ],
            3,
        );

        peephole_optimize_module(&mut module, OptLevel::O2).expect("optimize");
        verify_module(&module).expect("verify");

        let main = module.function(module.entry).unwrap();
        let copy_count = main
            .code
            .iter()
            .filter(|i| matches!(i, Instruction::Copy { .. }))
            .count();
        assert_eq!(
            copy_count, 0,
            "expected copy chain to be eliminated, got {:?}",
            main.code
        );
    }
}
