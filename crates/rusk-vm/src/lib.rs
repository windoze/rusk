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
    F: FnMut(&[AbiValue]) -> Result<AbiValue, HostError> + 'static,
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
    regs: Vec<Value>,
}

#[derive(Debug)]
enum VmState {
    Running,
    Suspended { k: ContinuationHandle },
    Done { value: AbiValue },
    Trapped { message: String },
}

pub struct Vm {
    module: ExecutableModule,
    state: VmState,
    frames: Vec<Frame>,
    host_fns: Vec<Option<Box<dyn HostFn>>>,
    in_host_call: bool,
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
        regs.resize(reg_count, Value::Unit);

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
            }],
            in_host_call: false,
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
            // v0 behavior: empty/no-op function returns `unit`.
            let ret = AbiValue::Unit;
            vm.frames.pop();
            if vm.frames.is_empty() {
                vm.state = VmState::Done { value: ret.clone() };
                return StepResult::Done { value: ret };
            }

            // No CALL op yet in milestone A: treat returning into a caller as a trap.
            let message = "return to caller is not supported yet".to_string();
            vm.state = VmState::Trapped {
                message: message.clone(),
            };
            return StepResult::Trap { message };
        }

        let instr = &func.code[frame.pc];
        frame.pc += 1;

        if fuel.is_some() {
            remaining = remaining.saturating_sub(1);
        }

        match instr {
            rusk_bytecode::Instruction::Return { value } => {
                let idx: usize = (*value).try_into().unwrap_or(usize::MAX);
                let Some(v) = frame.regs.get(idx) else {
                    let message = format!("return reg {value} out of range");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };
                let ret = v.to_abi();
                vm.frames.pop();
                if vm.frames.is_empty() {
                    vm.state = VmState::Done { value: ret.clone() };
                    return StepResult::Done { value: ret };
                }
                let message = "return to caller is not supported yet".to_string();
                vm.state = VmState::Trapped {
                    message: message.clone(),
                };
                return StepResult::Trap { message };
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

pub fn vm_resume(vm: &mut Vm, k: ContinuationHandle, value: AbiValue) -> Result<(), VmError> {
    if vm.in_host_call {
        return Err(VmError::InvalidState {
            message: "vm re-entered during host call".to_string(),
        });
    }

    let _ = value;
    match &vm.state {
        VmState::Suspended { k: want } => {
            if want != &k {
                return Err(VmError::InvalidContinuation {
                    message: "continuation handle mismatch".to_string(),
                });
            }
            Err(VmError::InvalidState {
                message: "resume is not implemented yet".to_string(),
            })
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
        VmState::Suspended { k: want } => {
            if want != &k {
                return Err(VmError::InvalidContinuation {
                    message: "continuation handle mismatch".to_string(),
                });
            }
            Err(VmError::InvalidState {
                message: "drop is not implemented yet".to_string(),
            })
        }
        VmState::Running | VmState::Done { .. } | VmState::Trapped { .. } => Err(VmError::InvalidState {
            message: "vm is not suspended".to_string(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rusk_bytecode::{ExecutableModule, Function};

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
}
