#![forbid(unsafe_code)]

use rusk_bytecode::{AbiType, EffectId, ExecutableModule, FunctionId, HostImportId};

#[derive(Clone, Debug, PartialEq)]
pub enum AbiValue {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
}

impl AbiValue {
    pub fn ty(&self) -> AbiType {
        match self {
            Self::Unit => AbiType::Unit,
            Self::Bool(_) => AbiType::Bool,
            Self::Int(_) => AbiType::Int,
            Self::Float(_) => AbiType::Float,
            Self::String(_) => AbiType::String,
            Self::Bytes(_) => AbiType::Bytes,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ContinuationHandle {
    pub index: u32,
    pub generation: u32,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StepResult {
    Done { value: AbiValue },
    Trap { message: String },
    Request {
        effect_id: EffectId,
        args: Vec<AbiValue>,
        k: ContinuationHandle,
    },
    Yield { remaining_fuel: u64 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VmError {
    InvalidState { message: String },
    InvalidContinuation { message: String },
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VmError::InvalidState { message } => write!(f, "invalid vm state: {message}"),
            VmError::InvalidContinuation { message } => write!(f, "invalid continuation: {message}"),
        }
    }
}

impl std::error::Error for VmError {}

#[derive(Debug)]
pub struct HostError {
    pub message: String,
}

impl std::fmt::Display for HostError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "host error: {}", self.message)
    }
}

impl std::error::Error for HostError {}

pub trait HostFn: 'static {
    fn call(&mut self, args: &[AbiValue]) -> Result<AbiValue, HostError>;
}

impl<F> HostFn for F
where
    F: for<'a> FnMut(&'a [AbiValue]) -> Result<AbiValue, HostError> + 'static,
{
    fn call(&mut self, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        self(args)
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
}

impl Value {
    fn from_abi(v: &AbiValue) -> Self {
        match v {
            AbiValue::Unit => Value::Unit,
            AbiValue::Bool(b) => Value::Bool(*b),
            AbiValue::Int(n) => Value::Int(*n),
            AbiValue::Float(x) => Value::Float(*x),
            AbiValue::String(s) => Value::String(s.clone()),
            AbiValue::Bytes(b) => Value::Bytes(b.clone()),
        }
    }

    fn to_abi(&self) -> AbiValue {
        match self {
            Value::Unit => AbiValue::Unit,
            Value::Bool(b) => AbiValue::Bool(*b),
            Value::Int(n) => AbiValue::Int(*n),
            Value::Float(x) => AbiValue::Float(*x),
            Value::String(s) => AbiValue::String(s.clone()),
            Value::Bytes(b) => AbiValue::Bytes(b.clone()),
        }
    }
}

#[derive(Debug)]
struct Frame {
    func: FunctionId,
    pc: usize,
    regs: Vec<Option<Value>>,
    return_dst: Option<rusk_bytecode::Reg>,
}

#[derive(Debug)]
enum VmState {
    Running,
    Suspended {
        k: ContinuationHandle,
        perform_dst: Option<rusk_bytecode::Reg>,
    },
    Done { value: AbiValue },
    Trapped { message: String },
}

pub struct Vm {
    module: ExecutableModule,
    state: VmState,
    frames: Vec<Frame>,
    host_fns: Vec<Option<Box<dyn HostFn>>>,
    in_host_call: bool,
    continuation_generation: u32,
}

impl Vm {
    pub fn new(module: ExecutableModule) -> Result<Self, VmError> {
        let entry = module.entry;
        let Some(entry_fn) = module.function(entry) else {
            return Err(VmError::InvalidState {
                message: format!("invalid entry function id {}", entry.0),
            });
        };

        let reg_count: usize = entry_fn
            .reg_count
            .try_into()
            .map_err(|_| VmError::InvalidState {
                message: "entry reg_count overflow".to_string(),
            })?;

        let mut regs = Vec::with_capacity(reg_count);
        regs.resize(reg_count, None);

        Ok(Self {
            host_fns: {
                let mut host_fns: Vec<Option<Box<dyn HostFn>>> =
                    Vec::with_capacity(module.host_imports.len());
                host_fns.resize_with(module.host_imports.len(), || None);
                host_fns
            },
            module,
            state: VmState::Running,
            frames: vec![Frame {
                func: entry,
                pc: 0,
                regs,
                return_dst: None,
            }],
            in_host_call: false,
            continuation_generation: 0,
        })
    }

    pub fn register_host_import(
        &mut self,
        id: HostImportId,
        host_fn: impl HostFn,
    ) -> Result<(), VmError> {
        let idx: usize = id.0 as usize;
        if idx >= self.host_fns.len() {
            return Err(VmError::InvalidState {
                message: format!("host import id {} out of range", id.0),
            });
        }
        self.host_fns[idx] = Some(Box::new(host_fn));
        Ok(())
    }
}

pub fn vm_step(vm: &mut Vm, fuel: Option<u64>) -> StepResult {
    if vm.in_host_call {
        return StepResult::Trap {
            message: "vm re-entered during host call".to_string(),
        };
    }

    match &vm.state {
        VmState::Running => {}
        VmState::Suspended { .. } => {
            return StepResult::Trap {
                message: "vm is suspended; call resume/drop first".to_string(),
            };
        }
        VmState::Done { value } => {
            return StepResult::Done { value: value.clone() };
        }
        VmState::Trapped { message } => {
            return StepResult::Trap {
                message: message.clone(),
            };
        }
    }

    let mut remaining = fuel.unwrap_or(u64::MAX);

    loop {
        if fuel.is_some() && remaining == 0 {
            return StepResult::Yield { remaining_fuel: 0 };
        }

        let Some(frame) = vm.frames.last_mut() else {
            vm.state = VmState::Done {
                value: AbiValue::Unit,
            };
            return StepResult::Done {
                value: AbiValue::Unit,
            };
        };

        let Some(func) = vm.module.function(frame.func) else {
            let message = format!("invalid function id {}", frame.func.0);
            vm.state = VmState::Trapped {
                message: message.clone(),
            };
            return StepResult::Trap { message };
        };

        if frame.pc >= func.code.len() {
            let ret = Value::Unit;
            let return_dst = frame.return_dst;
            vm.frames.pop();
            if let Some(caller) = vm.frames.last_mut() {
                if let Some(dst) = return_dst {
                    let idx: usize = match dst.try_into() {
                        Ok(i) => i,
                        Err(_) => {
                            let message = format!("return dst reg {dst} overflow");
                            vm.state = VmState::Trapped {
                                message: message.clone(),
                            };
                            return StepResult::Trap { message };
                        }
                    };
                    if idx >= caller.regs.len() {
                        let message = format!("return dst reg {dst} out of range");
                        vm.state = VmState::Trapped {
                            message: message.clone(),
                        };
                        return StepResult::Trap { message };
                    }
                    caller.regs[idx] = Some(ret);
                }
                continue;
            }

            let ret = ret.to_abi();
            vm.state = VmState::Done { value: ret.clone() };
            return StepResult::Done { value: ret };
        }

        let instr = &func.code[frame.pc];
        frame.pc += 1;

        if fuel.is_some() {
            remaining = remaining.saturating_sub(1);
        }

        match instr {
            rusk_bytecode::Instruction::Const { dst, value } => {
                let idx: usize = (*dst).try_into().unwrap_or(usize::MAX);
                let Some(slot) = frame.regs.get_mut(idx) else {
                    let message = format!("const dst reg {dst} out of range");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };
                let v = match value {
                    rusk_bytecode::ConstValue::Unit => Value::Unit,
                    rusk_bytecode::ConstValue::Bool(b) => Value::Bool(*b),
                    rusk_bytecode::ConstValue::Int(n) => Value::Int(*n),
                    rusk_bytecode::ConstValue::Float(x) => Value::Float(*x),
                    rusk_bytecode::ConstValue::String(s) => Value::String(s.clone()),
                    rusk_bytecode::ConstValue::Bytes(b) => Value::Bytes(b.clone()),
                };
                *slot = Some(v);
            }
            rusk_bytecode::Instruction::Copy { dst, src } => {
                let dst_idx: usize = (*dst).try_into().unwrap_or(usize::MAX);
                let src_idx: usize = (*src).try_into().unwrap_or(usize::MAX);
                let Some(v) = frame
                    .regs
                    .get(src_idx)
                    .and_then(|v| v.as_ref())
                    .cloned()
                else {
                    let message = format!("copy from uninitialized reg {src}");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };
                let Some(dst_slot) = frame.regs.get_mut(dst_idx) else {
                    let message = format!("copy dst reg {dst} out of range");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };
                *dst_slot = Some(v);
            }
            rusk_bytecode::Instruction::Move { dst, src } => {
                let dst_idx: usize = (*dst).try_into().unwrap_or(usize::MAX);
                let src_idx: usize = (*src).try_into().unwrap_or(usize::MAX);
                let Some(src_slot) = frame.regs.get_mut(src_idx) else {
                    let message = format!("move src reg {src} out of range");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };
                let Some(v) = src_slot.take() else {
                    let message = format!("move from uninitialized reg {src}");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };
                let Some(dst_slot) = frame.regs.get_mut(dst_idx) else {
                    let message = format!("move dst reg {dst} out of range");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };
                *dst_slot = Some(v);
            }

            rusk_bytecode::Instruction::IntAdd { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_add a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_add b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(a + b)) {
                    return trap(vm, format!("int_add dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntSub { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_sub a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_sub b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(a - b)) {
                    return trap(vm, format!("int_sub dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntMul { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_mul a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_mul b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(a * b)) {
                    return trap(vm, format!("int_mul dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntDiv { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_div a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_div b: {msg}")),
                };
                if b == 0 {
                    return trap(vm, "int_div: division by zero".to_string());
                }
                if let Err(msg) = write_value(frame, *dst, Value::Int(a / b)) {
                    return trap(vm, format!("int_div dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntMod { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_mod a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_mod b: {msg}")),
                };
                if b == 0 {
                    return trap(vm, "int_mod: modulo by zero".to_string());
                }
                if let Err(msg) = write_value(frame, *dst, Value::Int(a % b)) {
                    return trap(vm, format!("int_mod dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::IntLt { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_lt a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_lt b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a < b)) {
                    return trap(vm, format!("int_lt dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntLe { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_le a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_le b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a <= b)) {
                    return trap(vm, format!("int_le dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntGt { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_gt a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_gt b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a > b)) {
                    return trap(vm, format!("int_gt dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntGe { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_ge a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_ge b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a >= b)) {
                    return trap(vm, format!("int_ge dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntEq { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_eq a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_eq b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a == b)) {
                    return trap(vm, format!("int_eq dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntNe { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_ne a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_ne b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a != b)) {
                    return trap(vm, format!("int_ne dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::BoolNot { dst, v } => {
                let v = match read_bool(frame, *v) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("bool_not v: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(!v)) {
                    return trap(vm, format!("bool_not dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::BoolEq { dst, a, b } => {
                let a = match read_bool(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("bool_eq a: {msg}")),
                };
                let b = match read_bool(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("bool_eq b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a == b)) {
                    return trap(vm, format!("bool_eq dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::BoolNe { dst, a, b } => {
                let a = match read_bool(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("bool_ne a: {msg}")),
                };
                let b = match read_bool(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("bool_ne b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a != b)) {
                    return trap(vm, format!("bool_ne dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::Call { dst, func, args } => match func {
                rusk_bytecode::CallTarget::Bc(fid) => {
                    let Some(callee) = vm.module.function(*fid) else {
                        let message = format!("invalid function id {}", fid.0);
                        vm.state = VmState::Trapped {
                            message: message.clone(),
                        };
                        return StepResult::Trap { message };
                    };
                    if args.len() != callee.param_count as usize {
                        let message = format!(
                            "call arity mismatch: expected {} args but got {}",
                            callee.param_count,
                            args.len()
                        );
                        vm.state = VmState::Trapped {
                            message: message.clone(),
                        };
                        return StepResult::Trap { message };
                    }
                    let mut regs: Vec<Option<Value>> = Vec::with_capacity(callee.reg_count as usize);
                    regs.resize(callee.reg_count as usize, None);
                    for (idx, arg_reg) in args.iter().enumerate() {
                        let src_idx: usize = (*arg_reg).try_into().unwrap_or(usize::MAX);
                        let Some(v) = frame
                            .regs
                            .get(src_idx)
                            .and_then(|v| v.as_ref())
                            .cloned()
                        else {
                            let message = format!("call arg reg {arg_reg} is uninitialized");
                            vm.state = VmState::Trapped {
                                message: message.clone(),
                            };
                            return StepResult::Trap { message };
                        };
                        regs[idx] = Some(v);
                    }

                    vm.frames.push(Frame {
                        func: *fid,
                        pc: 0,
                        regs,
                        return_dst: *dst,
                    });
                }
                rusk_bytecode::CallTarget::Host(hid) => {
                    let Some(import) = vm.module.host_import(*hid) else {
                        return trap(vm, format!("invalid host import id {}", hid.0));
                    };

                    if args.len() != import.sig.params.len() {
                        return trap(
                            vm,
                            format!(
                                "host call `{}` arity mismatch: expected {} args but got {}",
                                import.name,
                                import.sig.params.len(),
                                args.len()
                            ),
                        );
                    }

                    let idx: usize = hid.0 as usize;
                    let Some(host_fn) = vm.host_fns.get_mut(idx).and_then(|v| v.as_mut()) else {
                        return trap(
                            vm,
                            format!("missing host import implementation: `{}`", import.name),
                        );
                    };

                    let mut abi_args = Vec::with_capacity(args.len());
                    for (arg_reg, expected) in args.iter().zip(import.sig.params.iter()) {
                        let src_idx: usize = (*arg_reg).try_into().unwrap_or(usize::MAX);
                        let Some(v) = frame
                            .regs
                            .get(src_idx)
                            .and_then(|v| v.as_ref())
                            .cloned()
                        else {
                            return trap(
                                vm,
                                format!(
                                    "host call `{}` read from uninitialized reg {arg_reg}",
                                    import.name
                                ),
                            );
                        };
                        let abi = v.to_abi();
                        if abi.ty() != *expected {
                            return trap(
                                vm,
                                format!(
                                    "host call `{}` arg type mismatch: expected {:?}, got {:?}",
                                    import.name,
                                    expected,
                                    abi.ty()
                                ),
                            );
                        }
                        abi_args.push(abi);
                    }

                    vm.in_host_call = true;
                    let call_result = host_fn.call(&abi_args);
                    vm.in_host_call = false;

                    let abi_ret = match call_result {
                        Ok(v) => v,
                        Err(e) => {
                            return trap(
                                vm,
                                format!("host call `{}` failed: {e}", import.name),
                            );
                        }
                    };

                    if abi_ret.ty() != import.sig.ret {
                        return trap(
                            vm,
                            format!(
                                "host call `{}` return type mismatch: expected {:?}, got {:?}",
                                import.name,
                                import.sig.ret,
                                abi_ret.ty()
                            ),
                        );
                    }

                    if let Some(dst) = dst {
                        if let Err(msg) = write_value(frame, *dst, Value::from_abi(&abi_ret)) {
                            return trap(vm, format!("host call `{}` dst: {msg}", import.name));
                        }
                    }
                }
            },

            rusk_bytecode::Instruction::Perform {
                dst,
                effect_id,
                args,
            } => {
                let Some(effect) = vm.module.external_effect(*effect_id) else {
                    return trap(vm, format!("invalid effect id {}", effect_id.0));
                };
                if args.len() != effect.sig.params.len() {
                    return trap(
                        vm,
                        format!(
                            "external effect `{}.{}` arity mismatch: expected {} args but got {}",
                            effect.interface,
                            effect.method,
                            effect.sig.params.len(),
                            args.len()
                        ),
                    );
                }

                let mut abi_args = Vec::with_capacity(args.len());
                for (arg_reg, expected) in args.iter().zip(effect.sig.params.iter()) {
                    let src_idx: usize = (*arg_reg).try_into().unwrap_or(usize::MAX);
                    let Some(v) = frame
                        .regs
                        .get(src_idx)
                        .and_then(|v| v.as_ref())
                        .cloned()
                    else {
                        return trap(
                            vm,
                            format!(
                                "external effect `{}.{}` read from uninitialized reg {arg_reg}",
                                effect.interface, effect.method
                            ),
                        );
                    };
                    let abi = v.to_abi();
                    if abi.ty() != *expected {
                        return trap(
                            vm,
                            format!(
                                "external effect `{}.{}` arg type mismatch: expected {:?}, got {:?}",
                                effect.interface,
                                effect.method,
                                expected,
                                abi.ty()
                            ),
                        );
                    }
                    abi_args.push(abi);
                }

                let k = ContinuationHandle {
                    index: 0,
                    generation: vm.continuation_generation,
                };
                vm.state = VmState::Suspended {
                    k: k.clone(),
                    perform_dst: *dst,
                };
                return StepResult::Request {
                    effect_id: *effect_id,
                    args: abi_args,
                    k,
                };
            }

            rusk_bytecode::Instruction::Jump { target_pc } => {
                frame.pc = (*target_pc) as usize;
            }
            rusk_bytecode::Instruction::JumpIf {
                cond,
                then_pc,
                else_pc,
            } => {
                let cond = match read_bool(frame, *cond) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("jump_if cond: {msg}")),
                };
                frame.pc = if cond { *then_pc } else { *else_pc } as usize;
            }
            rusk_bytecode::Instruction::Return { value } => {
                let idx: usize = (*value).try_into().unwrap_or(usize::MAX);
                let Some(v) = frame
                    .regs
                    .get(idx)
                    .and_then(|v| v.as_ref())
                    .cloned()
                else {
                    let message = format!("return from uninitialized reg {value}");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };

                let return_dst = frame.return_dst;
                vm.frames.pop();
                if let Some(caller) = vm.frames.last_mut() {
                    if let Some(dst) = return_dst {
                        let idx: usize = match dst.try_into() {
                            Ok(i) => i,
                            Err(_) => {
                                let message = format!("return dst reg {dst} overflow");
                                vm.state = VmState::Trapped {
                                    message: message.clone(),
                                };
                                return StepResult::Trap { message };
                            }
                        };
                        if idx >= caller.regs.len() {
                            let message = format!("return dst reg {dst} out of range");
                            vm.state = VmState::Trapped {
                                message: message.clone(),
                            };
                            return StepResult::Trap { message };
                        }
                        caller.regs[idx] = Some(v);
                    }
                    continue;
                }

                let ret = v.to_abi();
                vm.state = VmState::Done { value: ret.clone() };
                return StepResult::Done { value: ret };
            }
            rusk_bytecode::Instruction::Trap { message } => {
                vm.state = VmState::Trapped {
                    message: message.clone(),
                };
                return StepResult::Trap {
                    message: message.clone(),
                };
            }
            _ => {
                let message = format!("unimplemented opcode: {instr:?}");
                vm.state = VmState::Trapped {
                    message: message.clone(),
                };
                return StepResult::Trap { message };
            }
        }
    }
}

fn trap(vm: &mut Vm, message: String) -> StepResult {
    vm.state = VmState::Trapped {
        message: message.clone(),
    };
    StepResult::Trap { message }
}

fn read_int(frame: &Frame, reg: rusk_bytecode::Reg) -> Result<i64, String> {
    let idx: usize = reg.try_into().unwrap_or(usize::MAX);
    let Some(v) = frame.regs.get(idx).and_then(|v| v.as_ref()) else {
        return Err(format!("read from uninitialized reg {reg}"));
    };
    match v {
        Value::Int(n) => Ok(*n),
        other => Err(format!("expected int, got {other:?}")),
    }
}

fn read_bool(frame: &Frame, reg: rusk_bytecode::Reg) -> Result<bool, String> {
    let idx: usize = reg.try_into().unwrap_or(usize::MAX);
    let Some(v) = frame.regs.get(idx).and_then(|v| v.as_ref()) else {
        return Err(format!("read from uninitialized reg {reg}"));
    };
    match v {
        Value::Bool(b) => Ok(*b),
        other => Err(format!("expected bool, got {other:?}")),
    }
}

fn write_value(frame: &mut Frame, reg: rusk_bytecode::Reg, value: Value) -> Result<(), String> {
    let idx: usize = reg.try_into().unwrap_or(usize::MAX);
    let Some(slot) = frame.regs.get_mut(idx) else {
        return Err(format!("write to out-of-range reg {reg}"));
    };
    *slot = Some(value);
    Ok(())
}

pub fn vm_resume(vm: &mut Vm, k: ContinuationHandle, value: AbiValue) -> Result<(), VmError> {
    if vm.in_host_call {
        return Err(VmError::InvalidState {
            message: "vm re-entered during host call".to_string(),
        });
    }
    match &vm.state {
        VmState::Suspended {
            k: want,
            perform_dst,
        } => {
            if want != &k {
                return Err(VmError::InvalidContinuation {
                    message: "continuation handle mismatch".to_string(),
                });
            }

            let dst = *perform_dst;
            vm.state = VmState::Running;
            vm.continuation_generation = vm.continuation_generation.wrapping_add(1);

            if let Some(dst) = dst {
                let Some(frame) = vm.frames.last_mut() else {
                    return Err(VmError::InvalidState {
                        message: "resume with empty stack".to_string(),
                    });
                };
                write_value(frame, dst, Value::from_abi(&value)).map_err(|message| {
                    VmError::InvalidState {
                        message: format!("resume dst write failed: {message}"),
                    }
                })?;
            }

            Ok(())
        }
        VmState::Running | VmState::Done { .. } | VmState::Trapped { .. } => Err(VmError::InvalidState {
            message: "vm is not suspended".to_string(),
        }),
    }
}

pub fn vm_drop_continuation(vm: &mut Vm, k: ContinuationHandle) -> Result<(), VmError> {
    if vm.in_host_call {
        return Err(VmError::InvalidState {
            message: "vm re-entered during host call".to_string(),
        });
    }

    match &vm.state {
        VmState::Suspended { k: want, .. } => {
            if want != &k {
                return Err(VmError::InvalidContinuation {
                    message: "continuation handle mismatch".to_string(),
                });
            }
            vm.continuation_generation = vm.continuation_generation.wrapping_add(1);
            vm.frames.clear();
            vm.state = VmState::Trapped {
                message: "cancelled".to_string(),
            };
            Ok(())
        }
        VmState::Running | VmState::Done { .. } | VmState::Trapped { .. } => Err(VmError::InvalidState {
            message: "vm is not suspended".to_string(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rusk_bytecode::{AbiType, CallTarget, ExecutableModule, Function, HostFnSig, HostImport, Instruction};

    #[test]
    fn step_done_for_trivial_program() {
        let mut module = ExecutableModule::default();
        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 0,
                param_count: 0,
                code: vec![],
            })
            .unwrap();
        module.entry = main;

        let mut vm = Vm::new(module).unwrap();
        let got = vm_step(&mut vm, None);
        assert_eq!(got, StepResult::Done { value: AbiValue::Unit });
    }

    #[test]
    fn step_done_is_idempotent() {
        let mut module = ExecutableModule::default();
        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 0,
                param_count: 0,
                code: vec![],
            })
            .unwrap();
        module.entry = main;

        let mut vm = Vm::new(module).unwrap();
        assert_eq!(
            vm_step(&mut vm, None),
            StepResult::Done { value: AbiValue::Unit }
        );
        assert_eq!(
            vm_step(&mut vm, None),
            StepResult::Done { value: AbiValue::Unit }
        );
    }

    #[test]
    fn resume_fails_when_not_suspended() {
        let mut module = ExecutableModule::default();
        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 0,
                param_count: 0,
                code: vec![],
            })
            .unwrap();
        module.entry = main;

        let mut vm = Vm::new(module).unwrap();
        let err = vm_resume(
            &mut vm,
            ContinuationHandle {
                index: 0,
                generation: 0,
            },
            AbiValue::Unit,
        )
        .expect_err("expected error");
        assert!(err.to_string().contains("not suspended"));
    }

    #[test]
    fn drop_fails_when_not_suspended() {
        let mut module = ExecutableModule::default();
        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 0,
                param_count: 0,
                code: vec![],
            })
            .unwrap();
        module.entry = main;

        let mut vm = Vm::new(module).unwrap();
        let err = vm_drop_continuation(
            &mut vm,
            ContinuationHandle {
                index: 0,
                generation: 0,
            },
        )
        .expect_err("expected error");
        assert!(err.to_string().contains("not suspended"));
    }

    #[test]
    fn call_host_int_add_ok() {
        let mut module = ExecutableModule::default();
        let add_id = module
            .add_host_import(HostImport {
                name: "test::add_int".to_string(),
                sig: HostFnSig {
                    params: vec![AbiType::Int, AbiType::Int],
                    ret: AbiType::Int,
                },
            })
            .unwrap();
        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 3,
                param_count: 0,
                code: vec![
                    Instruction::Const {
                        dst: 0,
                        value: rusk_bytecode::ConstValue::Int(1),
                    },
                    Instruction::Const {
                        dst: 1,
                        value: rusk_bytecode::ConstValue::Int(2),
                    },
                    Instruction::Call {
                        dst: Some(2),
                        func: CallTarget::Host(add_id),
                        args: vec![0, 1],
                    },
                    Instruction::Return { value: 2 },
                ],
            })
            .unwrap();
        module.entry = main;

        let mut vm = Vm::new(module).unwrap();
        vm.register_host_import(add_id, |args: &[AbiValue]| match args {
            [AbiValue::Int(a), AbiValue::Int(b)] => Ok(AbiValue::Int(a + b)),
            other => Err(HostError {
                message: format!("bad args: {other:?}"),
            }),
        })
        .unwrap();

        assert_eq!(
            vm_step(&mut vm, None),
            StepResult::Done {
                value: AbiValue::Int(3)
            }
        );
    }

    #[test]
    fn call_host_type_mismatch_traps() {
        let mut module = ExecutableModule::default();
        let add_id = module
            .add_host_import(HostImport {
                name: "test::add_int".to_string(),
                sig: HostFnSig {
                    params: vec![AbiType::Int],
                    ret: AbiType::Int,
                },
            })
            .unwrap();
        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 2,
                param_count: 0,
                code: vec![
                    Instruction::Const {
                        dst: 0,
                        value: rusk_bytecode::ConstValue::Bool(true),
                    },
                    Instruction::Call {
                        dst: Some(1),
                        func: CallTarget::Host(add_id),
                        args: vec![0],
                    },
                    Instruction::Return { value: 1 },
                ],
            })
            .unwrap();
        module.entry = main;

        let mut vm = Vm::new(module).unwrap();
        vm.register_host_import(add_id, |_args: &[AbiValue]| Ok(AbiValue::Int(0)))
            .unwrap();

        let got = vm_step(&mut vm, None);
        let StepResult::Trap { message } = got else {
            panic!("expected trap, got {got:?}");
        };
        assert!(message.contains("arg type mismatch"), "{message}");
    }

    #[test]
    fn call_host_arity_mismatch_traps() {
        let mut module = ExecutableModule::default();
        let add_id = module
            .add_host_import(HostImport {
                name: "test::add_int".to_string(),
                sig: HostFnSig {
                    params: vec![AbiType::Int, AbiType::Int],
                    ret: AbiType::Int,
                },
            })
            .unwrap();
        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 2,
                param_count: 0,
                code: vec![
                    Instruction::Const {
                        dst: 0,
                        value: rusk_bytecode::ConstValue::Int(1),
                    },
                    Instruction::Call {
                        dst: Some(1),
                        func: CallTarget::Host(add_id),
                        args: vec![0],
                    },
                    Instruction::Return { value: 1 },
                ],
            })
            .unwrap();
        module.entry = main;

        let mut vm = Vm::new(module).unwrap();
        vm.register_host_import(add_id, |_args: &[AbiValue]| Ok(AbiValue::Int(0)))
            .unwrap();

        let got = vm_step(&mut vm, None);
        let StepResult::Trap { message } = got else {
            panic!("expected trap, got {got:?}");
        };
        assert!(message.contains("arity mismatch"), "{message}");
    }

    #[test]
    fn call_host_non_reentrant_guard_traps() {
        let mut module = ExecutableModule::default();
        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 0,
                param_count: 0,
                code: vec![],
            })
            .unwrap();
        module.entry = main;

        let mut vm = Vm::new(module).unwrap();
        vm.in_host_call = true;
        let got = vm_step(&mut vm, None);
        let StepResult::Trap { message } = got else {
            panic!("expected trap, got {got:?}");
        };
        assert!(message.contains("re-entered"), "{message}");
    }
}
