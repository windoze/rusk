/// Compile-time measurements for the VM execution loop.
///
/// Metrics collection is best-effort and meant for profiling/benchmarking. The VM can run with
/// metrics disabled to reduce overhead.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct VmMetrics {
    /// Total number of executed bytecode instructions.
    pub executed_instructions: u64,

    pub const_instructions: u64,
    pub copy_instructions: u64,
    pub move_instructions: u64,
    pub as_readonly_instructions: u64,

    pub int_binop_instructions: u64,
    pub int_cmp_instructions: u64,
    pub bool_op_instructions: u64,

    pub call_instructions: u64,
    pub icall_instructions: u64,
    pub vcall_instructions: u64,
    /// Number of `VCall`s handled by a VM fast path rather than the module dispatch table.
    ///
    /// Best-effort; requires metrics enabled.
    pub vcall_fast_path_hits: u64,

    pub push_handler_instructions: u64,
    pub pop_handler_instructions: u64,
    pub perform_instructions: u64,
    pub resume_instructions: u64,
    pub resume_tail_instructions: u64,

    pub jump_instructions: u64,
    pub jumpif_instructions: u64,
    pub switch_instructions: u64,

    pub return_instructions: u64,
    pub trap_instructions: u64,
    pub other_instructions: u64,

    /// Maximum observed VM frame stack size during execution (best-effort; requires metrics enabled).
    pub max_frames_len: u64,
    /// Maximum observed handler stack size during execution (best-effort; requires metrics enabled).
    pub max_handlers_len: u64,

    /// Number of successful handler-cache hits in `Perform` (best-effort; requires metrics enabled).
    pub handler_cache_hits: u64,
    /// Number of handler-cache misses/fallbacks in `Perform` (best-effort; requires metrics enabled).
    pub handler_cache_misses: u64,

    /// Number of captured continuations created by in-VM `perform` (best-effort; requires metrics enabled).
    pub continuations_captured: u64,
    /// Number of `perform`s where continuation capture was skipped due to an abortive handler clause.
    pub continuations_skipped_abortive: u64,
}

impl VmMetrics {
    pub(crate) fn record(&mut self, instr: &rusk_bytecode::Instruction) {
        use rusk_bytecode::Instruction as I;

        self.executed_instructions = self.executed_instructions.saturating_add(1);

        match instr {
            I::Const { .. } => self.const_instructions += 1,
            I::Copy { .. } => self.copy_instructions += 1,
            I::Move { .. } => self.move_instructions += 1,
            I::AsReadonly { .. } => self.as_readonly_instructions += 1,

            I::IntAdd { .. }
            | I::IntSub { .. }
            | I::IntMul { .. }
            | I::IntDiv { .. }
            | I::IntMod { .. }
            | I::IntAnd { .. }
            | I::IntOr { .. }
            | I::IntXor { .. }
            | I::IntShl { .. }
            | I::IntShr { .. }
            | I::IntUShr { .. }
            | I::IntNot { .. } => self.int_binop_instructions += 1,

            I::IntLt { .. }
            | I::IntLe { .. }
            | I::IntGt { .. }
            | I::IntGe { .. }
            | I::IntEq { .. }
            | I::IntNe { .. } => self.int_cmp_instructions += 1,

            I::BoolNot { .. } | I::BoolEq { .. } | I::BoolNe { .. } => {
                self.bool_op_instructions += 1
            }

            I::Call { .. } | I::CallMulti { .. } => self.call_instructions += 1,
            I::ICall { .. } => self.icall_instructions += 1,
            I::VCall { .. } => self.vcall_instructions += 1,

            I::PushHandler { .. } => self.push_handler_instructions += 1,
            I::PopHandler => self.pop_handler_instructions += 1,
            I::Perform { .. } => self.perform_instructions += 1,
            I::Resume { .. } => self.resume_instructions += 1,
            I::ResumeTail { .. } => self.resume_tail_instructions += 1,

            I::Jump { .. } => self.jump_instructions += 1,
            I::JumpIf { .. } => self.jumpif_instructions += 1,
            I::Switch { .. } => self.switch_instructions += 1,

            I::Return { .. } | I::ReturnMulti { .. } => self.return_instructions += 1,
            I::Trap { .. } => self.trap_instructions += 1,

            _ => self.other_instructions += 1,
        }
    }

    /// Adds all counters from `other` into `self` (saturating where appropriate).
    pub fn add_from(&mut self, other: &Self) {
        self.executed_instructions = self
            .executed_instructions
            .saturating_add(other.executed_instructions);

        self.const_instructions = self
            .const_instructions
            .saturating_add(other.const_instructions);
        self.copy_instructions = self
            .copy_instructions
            .saturating_add(other.copy_instructions);
        self.move_instructions = self
            .move_instructions
            .saturating_add(other.move_instructions);
        self.as_readonly_instructions = self
            .as_readonly_instructions
            .saturating_add(other.as_readonly_instructions);

        self.int_binop_instructions = self
            .int_binop_instructions
            .saturating_add(other.int_binop_instructions);
        self.int_cmp_instructions = self
            .int_cmp_instructions
            .saturating_add(other.int_cmp_instructions);
        self.bool_op_instructions = self
            .bool_op_instructions
            .saturating_add(other.bool_op_instructions);

        self.call_instructions = self
            .call_instructions
            .saturating_add(other.call_instructions);
        self.icall_instructions = self
            .icall_instructions
            .saturating_add(other.icall_instructions);
        self.vcall_instructions = self
            .vcall_instructions
            .saturating_add(other.vcall_instructions);
        self.vcall_fast_path_hits = self
            .vcall_fast_path_hits
            .saturating_add(other.vcall_fast_path_hits);

        self.push_handler_instructions = self
            .push_handler_instructions
            .saturating_add(other.push_handler_instructions);
        self.pop_handler_instructions = self
            .pop_handler_instructions
            .saturating_add(other.pop_handler_instructions);
        self.perform_instructions = self
            .perform_instructions
            .saturating_add(other.perform_instructions);
        self.resume_instructions = self
            .resume_instructions
            .saturating_add(other.resume_instructions);
        self.resume_tail_instructions = self
            .resume_tail_instructions
            .saturating_add(other.resume_tail_instructions);

        self.jump_instructions = self
            .jump_instructions
            .saturating_add(other.jump_instructions);
        self.jumpif_instructions = self
            .jumpif_instructions
            .saturating_add(other.jumpif_instructions);
        self.switch_instructions = self
            .switch_instructions
            .saturating_add(other.switch_instructions);

        self.return_instructions = self
            .return_instructions
            .saturating_add(other.return_instructions);
        self.trap_instructions = self
            .trap_instructions
            .saturating_add(other.trap_instructions);
        self.other_instructions = self
            .other_instructions
            .saturating_add(other.other_instructions);

        self.max_frames_len = self.max_frames_len.max(other.max_frames_len);
        self.max_handlers_len = self.max_handlers_len.max(other.max_handlers_len);

        self.handler_cache_hits = self
            .handler_cache_hits
            .saturating_add(other.handler_cache_hits);
        self.handler_cache_misses = self
            .handler_cache_misses
            .saturating_add(other.handler_cache_misses);

        self.continuations_captured = self
            .continuations_captured
            .saturating_add(other.continuations_captured);
        self.continuations_skipped_abortive = self
            .continuations_skipped_abortive
            .saturating_add(other.continuations_skipped_abortive);
    }
}
