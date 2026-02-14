#![forbid(unsafe_code)]

use rusk_bytecode::{AbiType, EffectId, ExecutableModule, FunctionId, HostImportId};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
    Done {
        value: AbiValue,
    },
    Trap {
        message: String,
    },
    Request {
        effect_id: EffectId,
        args: Vec<AbiValue>,
        k: ContinuationHandle,
    },
    Yield {
        remaining_fuel: u64,
    },
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
            VmError::InvalidContinuation { message } => {
                write!(f, "invalid continuation: {message}")
            }
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeRepId(pub u32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum TypeCtor {
    Unit,
    Bool,
    Int,
    Float,
    String,
    Bytes,
    Array,
    Tuple(usize),
    Struct(String),
    Enum(String),
    Interface(String),
    Fn,
    Cont,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct TypeRepNode {
    ctor: TypeCtor,
    args: Vec<TypeRepId>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct GenericSpecializationKey {
    func: FunctionId,
    type_args: Vec<TypeRepId>,
}

#[derive(Debug, Default)]
struct TypeReps {
    nodes: Vec<TypeRepNode>,
    intern: HashMap<TypeRepNode, TypeRepId>,
}

impl TypeReps {
    fn intern(&mut self, node: TypeRepNode) -> TypeRepId {
        if let Some(id) = self.intern.get(&node).copied() {
            return id;
        }
        let id_u32: u32 = self
            .nodes
            .len()
            .try_into()
            .expect("type rep table overflow");
        let id = TypeRepId(id_u32);
        self.nodes.push(node.clone());
        self.intern.insert(node, id);
        id
    }

    fn node(&self, id: TypeRepId) -> Option<&TypeRepNode> {
        self.nodes.get(id.0 as usize)
    }

    fn ctor_from_lit(lit: &rusk_mir::TypeRepLit) -> TypeCtor {
        match lit {
            rusk_mir::TypeRepLit::Unit => TypeCtor::Unit,
            rusk_mir::TypeRepLit::Bool => TypeCtor::Bool,
            rusk_mir::TypeRepLit::Int => TypeCtor::Int,
            rusk_mir::TypeRepLit::Float => TypeCtor::Float,
            rusk_mir::TypeRepLit::String => TypeCtor::String,
            rusk_mir::TypeRepLit::Bytes => TypeCtor::Bytes,
            rusk_mir::TypeRepLit::Array => TypeCtor::Array,
            rusk_mir::TypeRepLit::Tuple(arity) => TypeCtor::Tuple(*arity),
            rusk_mir::TypeRepLit::Struct(name) => TypeCtor::Struct(name.clone()),
            rusk_mir::TypeRepLit::Enum(name) => TypeCtor::Enum(name.clone()),
            rusk_mir::TypeRepLit::Interface(name) => TypeCtor::Interface(name.clone()),
            rusk_mir::TypeRepLit::Fn => TypeCtor::Fn,
            rusk_mir::TypeRepLit::Cont => TypeCtor::Cont,
        }
    }
}

#[derive(Clone, Debug)]
struct RefValue {
    readonly: bool,
    obj: Rc<RefCell<HeapValue>>,
}

impl RefValue {
    fn new(obj: Rc<RefCell<HeapValue>>) -> Self {
        Self {
            readonly: false,
            obj,
        }
    }

    fn as_readonly(&self) -> Self {
        Self {
            readonly: true,
            obj: Rc::clone(&self.obj),
        }
    }

    fn is_readonly(&self) -> bool {
        self.readonly
    }

    fn ptr_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.obj, &other.obj)
    }
}

#[derive(Clone, Debug)]
struct ContinuationToken {
    state: Rc<RefCell<Option<ContinuationState>>>,
}

impl ContinuationToken {
    fn new(state: ContinuationState) -> Self {
        Self {
            state: Rc::new(RefCell::new(Some(state))),
        }
    }

    fn take_state(&self) -> Option<ContinuationState> {
        self.state.borrow_mut().take()
    }
}

#[derive(Clone, Debug)]
struct ContinuationState {
    frames: Vec<Frame>,
    handlers: Vec<HandlerEntry>,
    perform_dst: Option<rusk_bytecode::Reg>,
}

#[derive(Clone, Debug)]
enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
    TypeRep(TypeRepId),
    Ref(RefValue),
    Function(FunctionId),
    Continuation(ContinuationToken),
}

impl Value {
    fn kind(&self) -> &'static str {
        match self {
            Value::Unit => "unit",
            Value::Bool(_) => "bool",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Bytes(_) => "bytes",
            Value::TypeRep(_) => "typerep",
            Value::Ref(_) => "ref",
            Value::Function(_) => "function",
            Value::Continuation(_) => "continuation",
        }
    }

    fn into_readonly_view(self) -> Self {
        match self {
            Value::Ref(r) => Value::Ref(r.as_readonly()),
            other => other,
        }
    }

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

    fn to_abi(&self) -> Option<AbiValue> {
        Some(match self {
            Value::Unit => AbiValue::Unit,
            Value::Bool(b) => AbiValue::Bool(*b),
            Value::Int(n) => AbiValue::Int(*n),
            Value::Float(x) => AbiValue::Float(*x),
            Value::String(s) => AbiValue::String(s.clone()),
            Value::Bytes(b) => AbiValue::Bytes(b.clone()),
            Value::TypeRep(_) | Value::Ref(_) | Value::Function(_) | Value::Continuation(_) => {
                return None;
            }
        })
    }
}

#[derive(Clone, Debug)]
enum HeapValue {
    Struct {
        type_name: String,
        type_args: Vec<TypeRepId>,
        fields: Vec<Value>,
    },
    Array(Vec<Value>),
    Tuple(Vec<Value>),
    Enum {
        enum_name: String,
        type_args: Vec<TypeRepId>,
        variant: String,
        fields: Vec<Value>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct RuntimeEffectId {
    interface: String,
    interface_args: Vec<TypeRepId>,
    method: String,
}

#[derive(Clone, Debug)]
struct RuntimeHandlerClause {
    effect: RuntimeEffectId,
    arg_patterns: Vec<rusk_mir::Pattern>,
    target_pc: u32,
    param_regs: Vec<rusk_bytecode::Reg>,
}

#[derive(Clone, Debug)]
struct HandlerEntry {
    owner_depth: usize,
    clauses: Vec<RuntimeHandlerClause>,
}

#[derive(Debug, Clone)]
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
    Done {
        value: AbiValue,
    },
    Trapped {
        message: String,
    },
}

pub struct Vm {
    module: ExecutableModule,
    state: VmState,
    frames: Vec<Frame>,
    handlers: Vec<HandlerEntry>,
    generic_specializations: HashMap<GenericSpecializationKey, HostImportId>,
    host_fns: Vec<Option<Box<dyn HostFn>>>,
    in_host_call: bool,
    continuation_generation: u32,
    type_reps: TypeReps,
}

impl Vm {
    pub fn new(module: ExecutableModule) -> Result<Self, VmError> {
        let entry = module.entry;
        let Some(entry_fn) = module.function(entry) else {
            return Err(VmError::InvalidState {
                message: format!("invalid entry function id {}", entry.0),
            });
        };

        let reg_count: usize =
            entry_fn
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
            handlers: Vec::new(),
            generic_specializations: HashMap::new(),
            in_host_call: false,
            continuation_generation: 0,
            type_reps: TypeReps::default(),
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

    pub fn intern_type_rep(
        &mut self,
        base: &rusk_mir::TypeRepLit,
        args: &[TypeRepId],
    ) -> TypeRepId {
        self.type_reps.intern(TypeRepNode {
            ctor: TypeReps::ctor_from_lit(base),
            args: args.to_vec(),
        })
    }

    pub fn register_generic_specialization(
        &mut self,
        fn_id: FunctionId,
        type_args: Vec<TypeRepId>,
        host_import_id: HostImportId,
    ) -> Result<(), VmError> {
        let Some(func) = self.module.function(fn_id) else {
            return Err(VmError::InvalidState {
                message: format!("invalid function id {}", fn_id.0),
            });
        };
        let Some(import) = self.module.host_import(host_import_id) else {
            return Err(VmError::InvalidState {
                message: format!("invalid host import id {}", host_import_id.0),
            });
        };

        let generic_params = self.module.function_generic_param_count(fn_id).unwrap_or(0);
        if generic_params == 0 {
            return Err(VmError::InvalidState {
                message: "cannot register specialization for non-generic function".to_string(),
            });
        }

        if type_args.len() != generic_params as usize {
            return Err(VmError::InvalidState {
                message: format!(
                    "generic specialization arity mismatch: expected {} type args but got {}",
                    generic_params,
                    type_args.len()
                ),
            });
        }

        let value_params = func.param_count.saturating_sub(generic_params) as usize;
        if import.sig.params.len() != value_params {
            return Err(VmError::InvalidState {
                message: format!(
                    "specialized host import `{}` arity mismatch: expected {} params but got {}",
                    import.name,
                    value_params,
                    import.sig.params.len()
                ),
            });
        }

        self.generic_specializations.insert(
            GenericSpecializationKey {
                func: fn_id,
                type_args,
            },
            host_import_id,
        );
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
            return StepResult::Done {
                value: value.clone(),
            };
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

        let frame_index = match vm.frames.len() {
            0 => {
                vm.state = VmState::Done {
                    value: AbiValue::Unit,
                };
                return StepResult::Done {
                    value: AbiValue::Unit,
                };
            }
            len => len - 1,
        };

        let frame = &mut vm.frames[frame_index];

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
            unwind_handlers_to_stack_len(&mut vm.handlers, vm.frames.len());
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

            let Some(ret) = ret.to_abi() else {
                return trap(vm, "non-ABI-safe return value".to_string());
            };
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
                    rusk_bytecode::ConstValue::TypeRep(lit) => {
                        let id = vm.type_reps.intern(TypeRepNode {
                            ctor: TypeReps::ctor_from_lit(lit),
                            args: Vec::new(),
                        });
                        Value::TypeRep(id)
                    }
                    rusk_bytecode::ConstValue::Function(id) => Value::Function(*id),
                };
                *slot = Some(v);
            }
            rusk_bytecode::Instruction::Copy { dst, src } => {
                let dst_idx: usize = (*dst).try_into().unwrap_or(usize::MAX);
                let src_idx: usize = (*src).try_into().unwrap_or(usize::MAX);
                let Some(v) = frame.regs.get(src_idx).and_then(|v| v.as_ref()).cloned() else {
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

            rusk_bytecode::Instruction::AsReadonly { dst, src } => {
                let v = match read_value(frame, *src) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("as_readonly src: {msg}")),
                };
                let v = v.into_readonly_view();
                if let Err(msg) = write_value(frame, *dst, v) {
                    return trap(vm, format!("as_readonly dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IsType { dst, value, ty } => {
                let v = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("is_type value: {msg}")),
                };
                let target = match read_type_rep(frame, *ty) {
                    Ok(id) => id,
                    Err(msg) => return trap(vm, format!("is_type ty: {msg}")),
                };
                let ok = match type_test(&vm.module, &vm.type_reps, &v, target) {
                    Ok(ok) => ok,
                    Err(msg) => return trap(vm, msg),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(ok)) {
                    return trap(vm, format!("is_type dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::CheckedCast { dst, value, ty } => {
                let v = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("checked_cast value: {msg}")),
                };
                let target = match read_type_rep(frame, *ty) {
                    Ok(id) => id,
                    Err(msg) => return trap(vm, format!("checked_cast ty: {msg}")),
                };
                let ok = match type_test(&vm.module, &vm.type_reps, &v, target) {
                    Ok(ok) => ok,
                    Err(msg) => return trap(vm, msg),
                };
                let (variant, fields) = if ok {
                    ("Some".to_string(), vec![v])
                } else {
                    ("None".to_string(), Vec::new())
                };
                let option = alloc_ref(HeapValue::Enum {
                    enum_name: "Option".to_string(),
                    type_args: vec![target],
                    variant,
                    fields,
                });
                if let Err(msg) = write_value(frame, *dst, option) {
                    return trap(vm, format!("checked_cast dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::MakeTypeRep { dst, base, args } => {
                let mut arg_ids = Vec::with_capacity(args.len());
                for reg in args {
                    let id = match read_type_rep(frame, *reg) {
                        Ok(id) => id,
                        Err(msg) => return trap(vm, format!("make_typerep arg: {msg}")),
                    };
                    arg_ids.push(id);
                }
                let id = vm.type_reps.intern(TypeRepNode {
                    ctor: TypeReps::ctor_from_lit(base),
                    args: arg_ids,
                });
                if let Err(msg) = write_value(frame, *dst, Value::TypeRep(id)) {
                    return trap(vm, format!("make_typerep dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::MakeStruct {
                dst,
                type_name,
                type_args,
                fields,
            } => {
                let mut arg_ids = Vec::with_capacity(type_args.len());
                for reg in type_args {
                    let id = match read_type_rep(frame, *reg) {
                        Ok(id) => id,
                        Err(msg) => return trap(vm, format!("make_struct type_args: {msg}")),
                    };
                    arg_ids.push(id);
                }

                let layout = match vm.module.struct_layouts.get(type_name.as_str()) {
                    Some(l) => l,
                    None => {
                        return trap(vm, format!("missing struct layout for `{type_name}`"));
                    }
                };

                let mut out: Vec<Option<Value>> = vec![None; layout.len()];
                for (field_name, reg) in fields {
                    let value = match read_value(frame, *reg) {
                        Ok(v) => v,
                        Err(msg) => {
                            return trap(vm, format!("make_struct field `{field_name}`: {msg}"));
                        }
                    };
                    let Some(idx) = layout.iter().position(|name| name == field_name) else {
                        return trap(vm, format!("missing field: {field_name}"));
                    };
                    if out[idx].is_some() {
                        return trap(
                            vm,
                            format!("duplicate field `{field_name}` in `{type_name}` literal"),
                        );
                    }
                    out[idx] = Some(value);
                }

                let mut values = Vec::with_capacity(layout.len());
                for (idx, slot) in out.into_iter().enumerate() {
                    let Some(v) = slot else {
                        return trap(vm, format!("missing field: {}", layout[idx]));
                    };
                    values.push(v);
                }

                let obj = HeapValue::Struct {
                    type_name: type_name.clone(),
                    type_args: arg_ids,
                    fields: values,
                };
                if let Err(msg) = write_value(frame, *dst, alloc_ref(obj)) {
                    return trap(vm, format!("make_struct dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::MakeArray { dst, items } => {
                let mut values = Vec::with_capacity(items.len());
                for reg in items {
                    let v = match read_value(frame, *reg) {
                        Ok(v) => v,
                        Err(msg) => return trap(vm, format!("make_array item: {msg}")),
                    };
                    values.push(v);
                }
                if let Err(msg) = write_value(frame, *dst, alloc_ref(HeapValue::Array(values))) {
                    return trap(vm, format!("make_array dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::MakeTuple { dst, items } => {
                if items.is_empty() {
                    if let Err(msg) = write_value(frame, *dst, Value::Unit) {
                        return trap(vm, format!("make_tuple dst: {msg}"));
                    }
                    continue;
                }
                let mut values = Vec::with_capacity(items.len());
                for reg in items {
                    let v = match read_value(frame, *reg) {
                        Ok(v) => v,
                        Err(msg) => return trap(vm, format!("make_tuple item: {msg}")),
                    };
                    values.push(v);
                }
                if let Err(msg) = write_value(frame, *dst, alloc_ref(HeapValue::Tuple(values))) {
                    return trap(vm, format!("make_tuple dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::MakeEnum {
                dst,
                enum_name,
                type_args,
                variant,
                fields,
            } => {
                let mut arg_ids = Vec::with_capacity(type_args.len());
                for reg in type_args {
                    let id = match read_type_rep(frame, *reg) {
                        Ok(id) => id,
                        Err(msg) => return trap(vm, format!("make_enum type_args: {msg}")),
                    };
                    arg_ids.push(id);
                }
                let mut values = Vec::with_capacity(fields.len());
                for reg in fields {
                    let v = match read_value(frame, *reg) {
                        Ok(v) => v,
                        Err(msg) => return trap(vm, format!("make_enum field: {msg}")),
                    };
                    values.push(v);
                }
                let obj = HeapValue::Enum {
                    enum_name: enum_name.clone(),
                    type_args: arg_ids,
                    variant: variant.clone(),
                    fields: values,
                };
                if let Err(msg) = write_value(frame, *dst, alloc_ref(obj)) {
                    return trap(vm, format!("make_enum dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::GetField { dst, obj, field } => {
                let obj_v = match read_value(frame, *obj) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("get_field obj: {msg}")),
                };
                let Value::Ref(r) = obj_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in get_field: expected ref(struct/tuple), got {}",
                            obj_v.kind()
                        ),
                    );
                };

                let value = match &*r.obj.borrow() {
                    HeapValue::Struct {
                        type_name, fields, ..
                    } => {
                        let idx = match struct_field_index(
                            &vm.module,
                            type_name.as_str(),
                            field.as_str(),
                        ) {
                            Ok(i) => i,
                            Err(msg) => return trap(vm, msg),
                        };
                        let Some(v) = fields.get(idx) else {
                            return trap(vm, format!("missing field: {field}"));
                        };
                        if r.readonly {
                            v.clone().into_readonly_view()
                        } else {
                            v.clone()
                        }
                    }
                    HeapValue::Tuple(items) => {
                        let Some(idx) = tuple_field_index(field.as_str()) else {
                            return trap(vm, format!("missing field: {field}"));
                        };
                        let Some(v) = items.get(idx) else {
                            return trap(vm, format!("missing field: {field}"));
                        };
                        if r.readonly {
                            v.clone().into_readonly_view()
                        } else {
                            v.clone()
                        }
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in get_field: expected struct/tuple, got ref".to_string(),
                        );
                    }
                };

                if let Err(msg) = write_value(frame, *dst, value) {
                    return trap(vm, format!("get_field dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::SetField { obj, field, value } => {
                let obj_v = match read_value(frame, *obj) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("set_field obj: {msg}")),
                };
                let Value::Ref(r) = obj_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in set_field: expected ref(struct/tuple), got {}",
                            obj_v.kind()
                        ),
                    );
                };
                if r.is_readonly() {
                    return trap(vm, "illegal write through readonly reference".to_string());
                }
                let val = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("set_field value: {msg}")),
                };

                match &mut *r.obj.borrow_mut() {
                    HeapValue::Struct {
                        type_name, fields, ..
                    } => {
                        let idx = match struct_field_index(
                            &vm.module,
                            type_name.as_str(),
                            field.as_str(),
                        ) {
                            Ok(i) => i,
                            Err(msg) => return trap(vm, msg),
                        };
                        let Some(slot) = fields.get_mut(idx) else {
                            return trap(vm, format!("missing field: {field}"));
                        };
                        *slot = val;
                    }
                    HeapValue::Tuple(items) => {
                        let Some(idx) = tuple_field_index(field.as_str()) else {
                            return trap(vm, format!("missing field: {field}"));
                        };
                        let Some(slot) = items.get_mut(idx) else {
                            return trap(vm, format!("missing field: {field}"));
                        };
                        *slot = val;
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in set_field: expected struct/tuple, got ref".to_string(),
                        );
                    }
                }
            }

            rusk_bytecode::Instruction::StructGet { dst, obj, idx } => {
                let obj_v = match read_value(frame, *obj) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("struct_get obj: {msg}")),
                };
                let Value::Ref(r) = obj_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in struct_get: expected ref(struct), got {}",
                            obj_v.kind()
                        ),
                    );
                };
                let value = match &*r.obj.borrow() {
                    HeapValue::Struct {
                        type_name, fields, ..
                    } => {
                        let Some(v) = fields.get(*idx) else {
                            let field_name = vm
                                .module
                                .struct_layouts
                                .get(type_name.as_str())
                                .and_then(|names| names.get(*idx))
                                .cloned()
                                .unwrap_or_else(|| format!("#{idx}"));
                            return trap(vm, format!("missing field: {field_name}"));
                        };
                        if r.readonly {
                            v.clone().into_readonly_view()
                        } else {
                            v.clone()
                        }
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in struct_get: expected struct, got ref".to_string(),
                        );
                    }
                };
                if let Err(msg) = write_value(frame, *dst, value) {
                    return trap(vm, format!("struct_get dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::StructSet { obj, idx, value } => {
                let obj_v = match read_value(frame, *obj) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("struct_set obj: {msg}")),
                };
                let Value::Ref(r) = obj_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in struct_set: expected ref(struct), got {}",
                            obj_v.kind()
                        ),
                    );
                };
                if r.is_readonly() {
                    return trap(vm, "illegal write through readonly reference".to_string());
                }
                let val = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("struct_set value: {msg}")),
                };
                match &mut *r.obj.borrow_mut() {
                    HeapValue::Struct { fields, .. } => {
                        let Some(slot) = fields.get_mut(*idx) else {
                            return trap(vm, format!("missing field: #{idx}"));
                        };
                        *slot = val;
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in struct_set: expected struct, got ref".to_string(),
                        );
                    }
                }
            }

            rusk_bytecode::Instruction::TupleGet { dst, tup, idx } => {
                let tup_v = match read_value(frame, *tup) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("tuple_get tup: {msg}")),
                };
                let Value::Ref(r) = tup_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in tuple_get: expected ref(tuple), got {}",
                            tup_v.kind()
                        ),
                    );
                };
                let value = match &*r.obj.borrow() {
                    HeapValue::Tuple(items) => {
                        let Some(v) = items.get(*idx) else {
                            return trap(vm, format!("missing field: .{idx}"));
                        };
                        if r.readonly {
                            v.clone().into_readonly_view()
                        } else {
                            v.clone()
                        }
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in tuple_get: expected tuple, got ref".to_string(),
                        );
                    }
                };
                if let Err(msg) = write_value(frame, *dst, value) {
                    return trap(vm, format!("tuple_get dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::TupleSet { tup, idx, value } => {
                let tup_v = match read_value(frame, *tup) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("tuple_set tup: {msg}")),
                };
                let Value::Ref(r) = tup_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in tuple_set: expected ref(tuple), got {}",
                            tup_v.kind()
                        ),
                    );
                };
                if r.is_readonly() {
                    return trap(vm, "illegal write through readonly reference".to_string());
                }
                let val = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("tuple_set value: {msg}")),
                };
                match &mut *r.obj.borrow_mut() {
                    HeapValue::Tuple(items) => {
                        let Some(slot) = items.get_mut(*idx) else {
                            return trap(vm, format!("missing field: .{idx}"));
                        };
                        *slot = val;
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in tuple_set: expected tuple, got ref".to_string(),
                        );
                    }
                }
            }

            rusk_bytecode::Instruction::IndexGet { dst, arr, idx } => {
                let arr_v = match read_value(frame, *arr) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("index_get arr: {msg}")),
                };
                let Value::Ref(r) = arr_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in index_get: expected ref(array), got {}",
                            arr_v.kind()
                        ),
                    );
                };
                let i = match read_int(frame, *idx) {
                    Ok(i) => i,
                    Err(msg) => {
                        return trap(
                            vm,
                            format!("type error in index_get: expected int index, got {msg}"),
                        );
                    }
                };

                let element = match &*r.obj.borrow() {
                    HeapValue::Array(items) => {
                        let idx_usize: usize = match i.try_into() {
                            Ok(u) => u,
                            Err(_) => {
                                return trap(
                                    vm,
                                    format!("index out of bounds: index={i}, len={}", items.len()),
                                );
                            }
                        };
                        let Some(v) = items.get(idx_usize) else {
                            return trap(
                                vm,
                                format!("index out of bounds: index={i}, len={}", items.len()),
                            );
                        };
                        if r.readonly {
                            v.clone().into_readonly_view()
                        } else {
                            v.clone()
                        }
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in index_get: expected array, got ref".to_string(),
                        );
                    }
                };
                if let Err(msg) = write_value(frame, *dst, element) {
                    return trap(vm, format!("index_get dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IndexSet { arr, idx, value } => {
                let arr_v = match read_value(frame, *arr) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("index_set arr: {msg}")),
                };
                let Value::Ref(r) = arr_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in index_set: expected ref(array), got {}",
                            arr_v.kind()
                        ),
                    );
                };
                if r.is_readonly() {
                    return trap(vm, "illegal write through readonly reference".to_string());
                }
                let i = match read_int(frame, *idx) {
                    Ok(i) => i,
                    Err(msg) => {
                        return trap(
                            vm,
                            format!("type error in index_set: expected int index, got {msg}"),
                        );
                    }
                };
                let val = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("index_set value: {msg}")),
                };

                match &mut *r.obj.borrow_mut() {
                    HeapValue::Array(items) => {
                        let idx_usize: usize = match i.try_into() {
                            Ok(u) => u,
                            Err(_) => {
                                return trap(
                                    vm,
                                    format!("index out of bounds: index={i}, len={}", items.len()),
                                );
                            }
                        };
                        if idx_usize >= items.len() {
                            return trap(
                                vm,
                                format!("index out of bounds: index={i}, len={}", items.len()),
                            );
                        }
                        items[idx_usize] = val;
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in index_set: expected array, got ref".to_string(),
                        );
                    }
                }
            }
            rusk_bytecode::Instruction::Len { dst, arr } => {
                let arr_v = match read_value(frame, *arr) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("len arr: {msg}")),
                };
                let Value::Ref(r) = arr_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in len: expected ref(array), got {}",
                            arr_v.kind()
                        ),
                    );
                };
                let len = match &*r.obj.borrow() {
                    HeapValue::Array(items) => items.len(),
                    _ => {
                        return trap(vm, "type error in len: expected array, got ref".to_string());
                    }
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(len as i64)) {
                    return trap(vm, format!("len dst: {msg}"));
                }
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

                    let generic_params =
                        vm.module.function_generic_param_count(*fid).unwrap_or(0) as usize;
                    if generic_params > 0 && generic_params <= args.len() {
                        let mut type_args = Vec::with_capacity(generic_params);
                        let mut ok = true;
                        for arg_reg in args.iter().take(generic_params) {
                            let src_idx: usize = (*arg_reg).try_into().unwrap_or(usize::MAX);
                            let Some(v) = frame.regs.get(src_idx).and_then(|v| v.as_ref()) else {
                                ok = false;
                                break;
                            };
                            let Value::TypeRep(id) = v else {
                                ok = false;
                                break;
                            };
                            type_args.push(*id);
                        }

                        if ok {
                            let key = GenericSpecializationKey {
                                func: *fid,
                                type_args,
                            };
                            if let Some(hid) = vm.generic_specializations.get(&key).copied() {
                                if let Err(msg) = call_host_import(
                                    &vm.module,
                                    &mut vm.host_fns,
                                    &mut vm.in_host_call,
                                    frame,
                                    *dst,
                                    hid,
                                    &args[generic_params..],
                                ) {
                                    return trap(vm, msg);
                                }
                                continue;
                            }
                        }
                    }
                    let mut regs: Vec<Option<Value>> =
                        Vec::with_capacity(callee.reg_count as usize);
                    regs.resize(callee.reg_count as usize, None);
                    for (idx, arg_reg) in args.iter().enumerate() {
                        let src_idx: usize = (*arg_reg).try_into().unwrap_or(usize::MAX);
                        let Some(v) = frame.regs.get(src_idx).and_then(|v| v.as_ref()).cloned()
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
                    if let Err(msg) = call_host_import(
                        &vm.module,
                        &mut vm.host_fns,
                        &mut vm.in_host_call,
                        frame,
                        *dst,
                        *hid,
                        args.as_slice(),
                    ) {
                        return trap(vm, msg);
                    }
                }
                rusk_bytecode::CallTarget::Intrinsic(intr) => {
                    let out = match eval_core_intrinsic(&vm.module, frame, *intr, args.as_slice()) {
                        Ok(v) => v,
                        Err(msg) => return trap(vm, msg),
                    };
                    if let Some(dst) = dst {
                        if let Err(msg) = write_value(frame, *dst, out) {
                            return trap(vm, format!("intrinsic call dst: {msg}"));
                        }
                    }
                }
            },

            rusk_bytecode::Instruction::ICall { dst, fnptr, args } => {
                let fn_value = match read_value(frame, *fnptr) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("icall fnptr: {msg}")),
                };
                let Value::Function(id) = fn_value else {
                    return trap(
                        vm,
                        format!(
                            "type error in icall: expected fn reference, got {}",
                            fn_value.kind()
                        ),
                    );
                };

                let Some(callee) = vm.module.function(id) else {
                    return trap(vm, format!("invalid function id {}", id.0));
                };
                if args.len() != callee.param_count as usize {
                    return trap(
                        vm,
                        format!(
                            "icall arity mismatch: expected {} args but got {}",
                            callee.param_count,
                            args.len()
                        ),
                    );
                }

                let mut regs: Vec<Option<Value>> = Vec::with_capacity(callee.reg_count as usize);
                regs.resize(callee.reg_count as usize, None);
                for (idx, arg_reg) in args.iter().enumerate() {
                    let src_idx: usize = (*arg_reg).try_into().unwrap_or(usize::MAX);
                    let Some(v) = frame.regs.get(src_idx).and_then(|v| v.as_ref()).cloned() else {
                        return trap(vm, format!("icall arg reg {arg_reg} is uninitialized"));
                    };
                    regs[idx] = Some(v);
                }

                vm.frames.push(Frame {
                    func: id,
                    pc: 0,
                    regs,
                    return_dst: *dst,
                });
            }

            rusk_bytecode::Instruction::VCall {
                dst,
                obj,
                method,
                method_type_args,
                args,
            } => {
                let recv = match read_value(frame, *obj) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("vcall obj: {msg}")),
                };
                let Value::Ref(r) = &recv else {
                    return trap(
                        vm,
                        format!(
                            "type error in vcall: expected ref(struct|enum), got {}",
                            recv.kind()
                        ),
                    );
                };

                let (type_name, type_args) = match &*r.obj.borrow() {
                    HeapValue::Struct {
                        type_name,
                        type_args,
                        ..
                    } => (type_name.clone(), type_args.clone()),
                    HeapValue::Enum {
                        enum_name,
                        type_args,
                        ..
                    } => (enum_name.clone(), type_args.clone()),
                    HeapValue::Array(_) | HeapValue::Tuple(_) => {
                        return trap(
                            vm,
                            format!(
                                "type error in vcall: expected struct|enum, got {}",
                                recv.kind()
                            ),
                        );
                    }
                };

                let lookup_key = (type_name.clone(), method.clone());
                let Some(fn_id) = vm.module.methods.get(&lookup_key).copied() else {
                    return trap(
                        vm,
                        format!("unresolved vcall method: {method} on {type_name}"),
                    );
                };

                let Some(callee) = vm.module.function(fn_id) else {
                    return trap(vm, format!("invalid function id {}", fn_id.0));
                };

                let mut arg_values =
                    Vec::with_capacity(type_args.len() + method_type_args.len() + args.len() + 1);
                for id in type_args {
                    arg_values.push(Value::TypeRep(id));
                }
                for reg in method_type_args {
                    let id = match read_type_rep(frame, *reg) {
                        Ok(id) => id,
                        Err(msg) => return trap(vm, format!("vcall method_type_args: {msg}")),
                    };
                    arg_values.push(Value::TypeRep(id));
                }
                arg_values.push(recv);
                for reg in args {
                    let v = match read_value(frame, *reg) {
                        Ok(v) => v,
                        Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                    };
                    arg_values.push(v);
                }

                if arg_values.len() != callee.param_count as usize {
                    return trap(
                        vm,
                        format!(
                            "vcall arity mismatch: expected {} args but got {}",
                            callee.param_count,
                            arg_values.len()
                        ),
                    );
                }

                let mut regs: Vec<Option<Value>> = Vec::with_capacity(callee.reg_count as usize);
                regs.resize(callee.reg_count as usize, None);
                for (idx, value) in arg_values.into_iter().enumerate() {
                    regs[idx] = Some(value);
                }

                vm.frames.push(Frame {
                    func: fn_id,
                    pc: 0,
                    regs,
                    return_dst: *dst,
                });
            }

            rusk_bytecode::Instruction::PushHandler { clauses } => {
                let owner_depth = frame_index;
                let mut runtime_clauses = Vec::with_capacity(clauses.len());
                for clause in clauses {
                    let mut interface_args = Vec::with_capacity(clause.effect.interface_args.len());
                    for reg in &clause.effect.interface_args {
                        let id = match read_type_rep(frame, *reg) {
                            Ok(id) => id,
                            Err(msg) => {
                                return trap(vm, format!("push_handler interface args: {msg}"));
                            }
                        };
                        interface_args.push(id);
                    }
                    runtime_clauses.push(RuntimeHandlerClause {
                        effect: RuntimeEffectId {
                            interface: clause.effect.interface.clone(),
                            interface_args,
                            method: clause.effect.method.clone(),
                        },
                        arg_patterns: clause.arg_patterns.clone(),
                        target_pc: clause.target_pc,
                        param_regs: clause.param_regs.clone(),
                    });
                }
                vm.handlers.push(HandlerEntry {
                    owner_depth,
                    clauses: runtime_clauses,
                });
            }
            rusk_bytecode::Instruction::PopHandler => {
                let Some(top) = vm.handlers.last() else {
                    return trap(vm, "mismatched pop_handler".to_string());
                };
                if top.owner_depth != frame_index {
                    return trap(vm, "mismatched pop_handler".to_string());
                }
                vm.handlers.pop();
            }

            rusk_bytecode::Instruction::Perform { dst, effect, args } => {
                let mut interface_args = Vec::with_capacity(effect.interface_args.len());
                for reg in &effect.interface_args {
                    let id = match read_type_rep(frame, *reg) {
                        Ok(id) => id,
                        Err(msg) => return trap(vm, format!("perform interface args: {msg}")),
                    };
                    interface_args.push(id);
                }
                let effect_id = RuntimeEffectId {
                    interface: effect.interface.clone(),
                    interface_args,
                    method: effect.method.clone(),
                };

                let mut arg_values = Vec::with_capacity(args.len());
                for reg in args {
                    let v = match read_value(frame, *reg) {
                        Ok(v) => v,
                        Err(msg) => return trap(vm, format!("perform arg: {msg}")),
                    };
                    arg_values.push(v);
                }

                match find_handler_for_effect(
                    &vm.module,
                    vm.handlers.as_slice(),
                    &effect_id,
                    &arg_values,
                ) {
                    Ok(Some((handler_index, clause_index, binds))) => {
                        let handler_owner_depth = vm.handlers[handler_index].owner_depth;

                        let mut captured_frames = vm.frames[handler_owner_depth..].to_vec();
                        let captured_handlers = vm
                            .handlers
                            .iter()
                            .filter_map(|entry| {
                                let owner_depth =
                                    entry.owner_depth.checked_sub(handler_owner_depth)?;
                                Some(HandlerEntry {
                                    owner_depth,
                                    clauses: entry.clauses.clone(),
                                })
                            })
                            .collect::<Vec<_>>();

                        if let Some(dst_reg) = dst {
                            let Some(top) = captured_frames.last_mut() else {
                                return trap(
                                    vm,
                                    format!(
                                        "unhandled effect: {}.{}",
                                        effect_id.interface, effect_id.method
                                    ),
                                );
                            };
                            let idx: usize = (*dst_reg).try_into().unwrap_or(usize::MAX);
                            let Some(slot) = top.regs.get_mut(idx) else {
                                return trap(vm, format!("perform dst reg {dst_reg} out of range"));
                            };
                            *slot = None;
                        }

                        let token = ContinuationToken::new(ContinuationState {
                            frames: captured_frames,
                            handlers: captured_handlers,
                            perform_dst: *dst,
                        });

                        let clause = vm.handlers[handler_index]
                            .clauses
                            .get(clause_index)
                            .cloned()
                            .expect("handler clause index is valid");

                        vm.frames.truncate(handler_owner_depth + 1);
                        vm.handlers.truncate(handler_index + 1);

                        let Some(handler_frame) = vm.frames.get_mut(handler_owner_depth) else {
                            return trap(vm, "invalid handler owner frame".to_string());
                        };

                        let expected_params = binds.len() + 1;
                        if clause.param_regs.len() != expected_params {
                            return trap(
                                vm,
                                format!(
                                    "invalid handler params for {}.{}: expected {expected_params}, got {}",
                                    effect_id.interface,
                                    effect_id.method,
                                    clause.param_regs.len()
                                ),
                            );
                        }

                        for (dst_reg, value) in clause
                            .param_regs
                            .iter()
                            .copied()
                            .take(binds.len())
                            .zip(binds.into_iter())
                        {
                            if let Err(msg) = write_value(handler_frame, dst_reg, value) {
                                return trap(vm, format!("handler bind dst: {msg}"));
                            }
                        }

                        let k_reg = clause.param_regs[expected_params - 1];
                        if let Err(msg) =
                            write_value(handler_frame, k_reg, Value::Continuation(token))
                        {
                            return trap(vm, format!("handler k dst: {msg}"));
                        }

                        handler_frame.pc = clause.target_pc as usize;
                    }
                    Ok(None) => {
                        let external_id = if effect_id.interface_args.is_empty() {
                            vm.module.external_effect_id(
                                effect_id.interface.as_str(),
                                effect_id.method.as_str(),
                            )
                        } else {
                            None
                        };

                        let Some(effect_id_u32) = external_id else {
                            return trap(
                                vm,
                                format!(
                                    "unhandled effect: {}.{}",
                                    effect_id.interface, effect_id.method
                                ),
                            );
                        };

                        let Some(effect) = vm.module.external_effect(effect_id_u32) else {
                            return trap(vm, format!("invalid effect id {}", effect_id_u32.0));
                        };
                        if arg_values.len() != effect.sig.params.len() {
                            return trap(
                                vm,
                                format!(
                                    "external effect `{}.{}` arity mismatch: expected {} args but got {}",
                                    effect.interface,
                                    effect.method,
                                    effect.sig.params.len(),
                                    arg_values.len()
                                ),
                            );
                        }

                        let mut abi_args = Vec::with_capacity(arg_values.len());
                        for (v, expected) in arg_values.iter().zip(effect.sig.params.iter()) {
                            let Some(abi) = v.to_abi() else {
                                return trap(
                                    vm,
                                    format!(
                                        "external effect `{}.{}` arg type mismatch: expected {:?}, got {}",
                                        effect.interface,
                                        effect.method,
                                        expected,
                                        v.kind()
                                    ),
                                );
                            };
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
                            effect_id: effect_id_u32,
                            args: abi_args,
                            k,
                        };
                    }
                    Err(msg) => return trap(vm, msg),
                }
            }

            rusk_bytecode::Instruction::Resume { dst, k, value } => {
                let k_value = match read_value(frame, *k) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("resume k: {msg}")),
                };
                let Value::Continuation(token) = k_value else {
                    return trap(vm, "invalid resume".to_string());
                };

                let v = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("resume value: {msg}")),
                };
                let Some(mut cont) = token.take_state() else {
                    return trap(vm, "invalid resume".to_string());
                };

                if let Some(perform_dst) = cont.perform_dst {
                    let top_index = cont.frames.len().saturating_sub(1);
                    let Some(top_frame) = cont.frames.get_mut(top_index) else {
                        return trap(vm, "invalid resume".to_string());
                    };
                    if let Err(msg) = write_value(top_frame, perform_dst, v) {
                        return trap(vm, format!("resume inject: {msg}"));
                    }
                }

                let base_depth = vm.frames.len();
                let Some(bottom) = cont.frames.first_mut() else {
                    return trap(vm, "invalid resume".to_string());
                };
                bottom.return_dst = *dst;

                for handler in &mut cont.handlers {
                    handler.owner_depth = handler.owner_depth.saturating_add(base_depth);
                }

                vm.frames.extend(cont.frames);
                vm.handlers.extend(cont.handlers);
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
            rusk_bytecode::Instruction::Switch {
                value,
                cases,
                default_pc,
            } => {
                let scrutinee = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("switch value: {msg}")),
                };

                let mut matched = false;
                for case in cases {
                    let mut binds = Vec::new();
                    let ok = match match_pattern(&vm.module, &case.pattern, &scrutinee, &mut binds)
                    {
                        Ok(ok) => ok,
                        Err(msg) => return trap(vm, msg),
                    };
                    if !ok {
                        continue;
                    }
                    if binds.len() != case.param_regs.len() {
                        return trap(
                            vm,
                            format!(
                                "invalid switch binds: expected {}, got {}",
                                case.param_regs.len(),
                                binds.len()
                            ),
                        );
                    }
                    for (dst, value) in case.param_regs.iter().copied().zip(binds.into_iter()) {
                        if let Err(msg) = write_value(frame, dst, value) {
                            return trap(vm, format!("switch bind dst: {msg}"));
                        }
                    }
                    frame.pc = case.target_pc as usize;
                    matched = true;
                    break;
                }

                if !matched {
                    frame.pc = (*default_pc) as usize;
                }
            }
            rusk_bytecode::Instruction::Return { value } => {
                let idx: usize = (*value).try_into().unwrap_or(usize::MAX);
                let Some(v) = frame.regs.get(idx).and_then(|v| v.as_ref()).cloned() else {
                    let message = format!("return from uninitialized reg {value}");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };

                let return_dst = frame.return_dst;
                vm.frames.pop();
                unwind_handlers_to_stack_len(&mut vm.handlers, vm.frames.len());
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

                let Some(ret) = v.to_abi() else {
                    return trap(vm, "non-ABI-safe return value".to_string());
                };
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

fn unwind_handlers_to_stack_len(handlers: &mut Vec<HandlerEntry>, stack_len: usize) {
    while let Some(top) = handlers.last() {
        if top.owner_depth < stack_len {
            break;
        }
        handlers.pop();
    }
}

fn find_handler_for_effect(
    module: &ExecutableModule,
    handlers: &[HandlerEntry],
    effect: &RuntimeEffectId,
    args: &[Value],
) -> Result<Option<(usize, usize, Vec<Value>)>, String> {
    for (handler_index, handler) in handlers.iter().enumerate().rev() {
        for (clause_index, clause) in handler.clauses.iter().enumerate() {
            if &clause.effect != effect {
                continue;
            }
            if clause.arg_patterns.len() != args.len() {
                continue;
            }

            let mut binds = Vec::new();
            let mut ok = true;
            for (pat, arg) in clause.arg_patterns.iter().zip(args.iter()) {
                if !match_pattern(module, pat, arg, &mut binds)? {
                    ok = false;
                    break;
                }
            }

            if ok {
                return Ok(Some((handler_index, clause_index, binds)));
            }
        }
    }

    Ok(None)
}

fn alloc_ref(obj: HeapValue) -> Value {
    Value::Ref(RefValue::new(Rc::new(RefCell::new(obj))))
}

fn read_value(frame: &Frame, reg: rusk_bytecode::Reg) -> Result<Value, String> {
    let idx: usize = reg.try_into().unwrap_or(usize::MAX);
    let Some(v) = frame.regs.get(idx).and_then(|v| v.as_ref()).cloned() else {
        return Err(format!("read from uninitialized reg {reg}"));
    };
    Ok(v)
}

fn read_type_rep(frame: &Frame, reg: rusk_bytecode::Reg) -> Result<TypeRepId, String> {
    let v = read_value(frame, reg)?;
    match v {
        Value::TypeRep(id) => Ok(id),
        other => Err(format!(
            "type error in typerep: expected typerep, got {}",
            other.kind()
        )),
    }
}

fn tuple_field_index(field: &str) -> Option<usize> {
    field
        .strip_prefix('.')
        .and_then(|s| s.parse::<usize>().ok())
}

fn struct_field_index(
    module: &ExecutableModule,
    type_name: &str,
    field: &str,
) -> Result<usize, String> {
    let Some(layout) = module.struct_layouts.get(type_name) else {
        return Err(format!("missing struct layout for `{type_name}`"));
    };
    layout
        .iter()
        .position(|name| name == field)
        .ok_or_else(|| format!("missing field: {field}"))
}

fn type_test(
    module: &ExecutableModule,
    type_reps: &TypeReps,
    value: &Value,
    target: TypeRepId,
) -> Result<bool, String> {
    let Some(target) = type_reps.node(target) else {
        return Err(format!("invalid typerep({})", target.0));
    };

    Ok(match &target.ctor {
        TypeCtor::Unit => matches!(value, Value::Unit),
        TypeCtor::Bool => matches!(value, Value::Bool(_)),
        TypeCtor::Int => matches!(value, Value::Int(_)),
        TypeCtor::Float => matches!(value, Value::Float(_)),
        TypeCtor::String => matches!(value, Value::String(_)),
        TypeCtor::Bytes => matches!(value, Value::Bytes(_)),
        TypeCtor::Array => match value {
            Value::Ref(r) => matches!(&*r.obj.borrow(), HeapValue::Array(_)),
            _ => false,
        },
        TypeCtor::Tuple(arity) => match value {
            Value::Unit => *arity == 0,
            Value::Ref(r) => match &*r.obj.borrow() {
                HeapValue::Tuple(items) => items.len() == *arity,
                _ => false,
            },
            _ => false,
        },
        TypeCtor::Struct(name) => match value {
            Value::Ref(r) => match &*r.obj.borrow() {
                HeapValue::Struct {
                    type_name,
                    type_args,
                    ..
                } => type_name == name && type_args.as_slice() == target.args.as_slice(),
                _ => false,
            },
            _ => false,
        },
        TypeCtor::Enum(name) => match value {
            Value::Ref(r) => match &*r.obj.borrow() {
                HeapValue::Enum {
                    enum_name,
                    type_args,
                    ..
                } => enum_name == name && type_args.as_slice() == target.args.as_slice(),
                _ => false,
            },
            _ => false,
        },
        TypeCtor::Interface(iface) => {
            let Value::Ref(r) = value else {
                return Ok(false);
            };

            let borrowed = r.obj.borrow();
            let (dyn_type, dyn_type_args) = match &*borrowed {
                HeapValue::Struct {
                    type_name,
                    type_args,
                    ..
                } => (type_name.as_str(), type_args.as_slice()),
                HeapValue::Enum {
                    enum_name,
                    type_args,
                    ..
                } => (enum_name.as_str(), type_args.as_slice()),
                _ => return Ok(false),
            };

            let implements_iface = module
                .interface_impls
                .get(dyn_type)
                .is_some_and(|ifaces| ifaces.contains(iface.as_str()));
            if !implements_iface {
                return Ok(false);
            }

            if target.args.is_empty() {
                return Ok(true);
            }
            if dyn_type_args.len() < target.args.len() {
                return Ok(false);
            }
            dyn_type_args[..target.args.len()] == target.args
        }
        TypeCtor::Fn | TypeCtor::Cont => false,
    })
}

const ARRAY_ITER_TYPE: &str = "core::intrinsics::ArrayIter";
const ARRAY_ITER_FIELD_ARRAY: &str = "arr";
const ARRAY_ITER_FIELD_INDEX: &str = "idx";

fn eval_core_intrinsic(
    module: &ExecutableModule,
    frame: &Frame,
    intr: rusk_bytecode::Intrinsic,
    arg_regs: &[rusk_bytecode::Reg],
) -> Result<Value, String> {
    use rusk_bytecode::Intrinsic as I;

    let mut args = Vec::with_capacity(arg_regs.len());
    for reg in arg_regs {
        args.push(read_value(frame, *reg)?);
    }

    let bad_args = |name: &str| -> String { format!("{name}: bad args: {args:?}") };

    match intr {
        I::StringConcat => match args.as_slice() {
            [Value::String(a), Value::String(b)] => Ok(Value::String(format!("{a}{b}"))),
            _ => Err(bad_args("core::intrinsics::string_concat")),
        },
        I::ToString => match args.as_slice() {
            [Value::TypeRep(_), Value::Unit] => Ok(Value::String("()".to_string())),
            [Value::TypeRep(_), Value::Bool(v)] => Ok(Value::String(v.to_string())),
            [Value::TypeRep(_), Value::Int(v)] => Ok(Value::String(v.to_string())),
            [Value::TypeRep(_), Value::Float(v)] => Ok(Value::String(v.to_string())),
            [Value::TypeRep(_), Value::String(v)] => Ok(Value::String(v.clone())),
            [Value::TypeRep(_), Value::Bytes(v)] => {
                Ok(Value::String(format!("bytes(len={})", v.len())))
            }
            [Value::TypeRep(_), Value::Ref(r)] => {
                Ok(Value::String(format!("{:?}", Value::Ref(r.clone()))))
            }
            [Value::TypeRep(_), Value::Function(id)] => Ok(Value::String(format!("fn#{}", id.0))),
            [Value::TypeRep(_), Value::TypeRep(id)] => {
                Ok(Value::String(format!("typerep({})", id.0)))
            }
            _ => Err(bad_args("core::intrinsics::to_string")),
        },
        I::Panic => match args.as_slice() {
            [Value::TypeRep(_), Value::String(msg)] => Err(format!("panic: {msg}")),
            _ => Err(bad_args("core::intrinsics::panic")),
        },

        I::BoolNot => match args.as_slice() {
            [Value::Bool(v)] => Ok(Value::Bool(!v)),
            _ => Err(bad_args("core::intrinsics::bool_not")),
        },
        I::BoolEq => match args.as_slice() {
            [Value::Bool(a), Value::Bool(b)] => Ok(Value::Bool(a == b)),
            _ => Err(bad_args("core::intrinsics::bool_eq")),
        },
        I::BoolNe => match args.as_slice() {
            [Value::Bool(a), Value::Bool(b)] => Ok(Value::Bool(a != b)),
            _ => Err(bad_args("core::intrinsics::bool_ne")),
        },

        I::IntAdd => match args.as_slice() {
            [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a + b)),
            _ => Err(bad_args("core::intrinsics::int_add")),
        },
        I::IntSub => match args.as_slice() {
            [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a - b)),
            _ => Err(bad_args("core::intrinsics::int_sub")),
        },
        I::IntMul => match args.as_slice() {
            [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a * b)),
            _ => Err(bad_args("core::intrinsics::int_mul")),
        },
        I::IntDiv => match args.as_slice() {
            [Value::Int(_), Value::Int(0)] => {
                Err("core::intrinsics::int_div: division by zero".to_string())
            }
            [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a / b)),
            _ => Err(bad_args("core::intrinsics::int_div")),
        },
        I::IntMod => match args.as_slice() {
            [Value::Int(_), Value::Int(0)] => {
                Err("core::intrinsics::int_mod: modulo by zero".to_string())
            }
            [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a % b)),
            _ => Err(bad_args("core::intrinsics::int_mod")),
        },
        I::IntEq => match args.as_slice() {
            [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a == b)),
            _ => Err(bad_args("core::intrinsics::int_eq")),
        },
        I::IntNe => match args.as_slice() {
            [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a != b)),
            _ => Err(bad_args("core::intrinsics::int_ne")),
        },
        I::IntLt => match args.as_slice() {
            [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a < b)),
            _ => Err(bad_args("core::intrinsics::int_lt")),
        },
        I::IntLe => match args.as_slice() {
            [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a <= b)),
            _ => Err(bad_args("core::intrinsics::int_le")),
        },
        I::IntGt => match args.as_slice() {
            [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a > b)),
            _ => Err(bad_args("core::intrinsics::int_gt")),
        },
        I::IntGe => match args.as_slice() {
            [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a >= b)),
            _ => Err(bad_args("core::intrinsics::int_ge")),
        },

        I::FloatAdd => match args.as_slice() {
            [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a + b)),
            _ => Err(bad_args("core::intrinsics::float_add")),
        },
        I::FloatSub => match args.as_slice() {
            [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a - b)),
            _ => Err(bad_args("core::intrinsics::float_sub")),
        },
        I::FloatMul => match args.as_slice() {
            [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a * b)),
            _ => Err(bad_args("core::intrinsics::float_mul")),
        },
        I::FloatDiv => match args.as_slice() {
            [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a / b)),
            _ => Err(bad_args("core::intrinsics::float_div")),
        },
        I::FloatMod => match args.as_slice() {
            [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a % b)),
            _ => Err(bad_args("core::intrinsics::float_mod")),
        },
        I::FloatEq => match args.as_slice() {
            [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a == b)),
            _ => Err(bad_args("core::intrinsics::float_eq")),
        },
        I::FloatNe => match args.as_slice() {
            [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a != b)),
            _ => Err(bad_args("core::intrinsics::float_ne")),
        },
        I::FloatLt => match args.as_slice() {
            [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a < b)),
            _ => Err(bad_args("core::intrinsics::float_lt")),
        },
        I::FloatLe => match args.as_slice() {
            [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a <= b)),
            _ => Err(bad_args("core::intrinsics::float_le")),
        },
        I::FloatGt => match args.as_slice() {
            [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a > b)),
            _ => Err(bad_args("core::intrinsics::float_gt")),
        },
        I::FloatGe => match args.as_slice() {
            [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a >= b)),
            _ => Err(bad_args("core::intrinsics::float_ge")),
        },

        I::StringEq => match args.as_slice() {
            [Value::String(a), Value::String(b)] => Ok(Value::Bool(a == b)),
            _ => Err(bad_args("core::intrinsics::string_eq")),
        },
        I::StringNe => match args.as_slice() {
            [Value::String(a), Value::String(b)] => Ok(Value::Bool(a != b)),
            _ => Err(bad_args("core::intrinsics::string_ne")),
        },
        I::BytesEq => match args.as_slice() {
            [Value::Bytes(a), Value::Bytes(b)] => Ok(Value::Bool(a == b)),
            _ => Err(bad_args("core::intrinsics::bytes_eq")),
        },
        I::BytesNe => match args.as_slice() {
            [Value::Bytes(a), Value::Bytes(b)] => Ok(Value::Bool(a != b)),
            _ => Err(bad_args("core::intrinsics::bytes_ne")),
        },
        I::UnitEq => match args.as_slice() {
            [Value::Unit, Value::Unit] => Ok(Value::Bool(true)),
            _ => Err(bad_args("core::intrinsics::unit_eq")),
        },
        I::UnitNe => match args.as_slice() {
            [Value::Unit, Value::Unit] => Ok(Value::Bool(false)),
            _ => Err(bad_args("core::intrinsics::unit_ne")),
        },

        I::ArrayLen => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(arr)] => match &*arr.obj.borrow() {
                HeapValue::Array(items) => Ok(Value::Int(items.len() as i64)),
                _ => Err("core::intrinsics::array_len: expected an array".to_string()),
            },
            _ => Err(bad_args("core::intrinsics::array_len")),
        },
        I::ArrayLenRo => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(arr)] => match &*arr.obj.borrow() {
                HeapValue::Array(items) => Ok(Value::Int(items.len() as i64)),
                _ => Err("core::intrinsics::array_len_ro: expected an array".to_string()),
            },
            _ => Err(bad_args("core::intrinsics::array_len_ro")),
        },
        I::ArrayPush => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(arr), value] => {
                if arr.is_readonly() {
                    return Err("illegal write through readonly reference".to_string());
                }
                let HeapValue::Array(items) = &mut *arr.obj.borrow_mut() else {
                    return Err("core::intrinsics::array_push: expected an array".to_string());
                };
                items.push(value.clone());
                Ok(Value::Unit)
            }
            _ => Err(bad_args("core::intrinsics::array_push")),
        },
        I::ArrayPop => match args.as_slice() {
            [Value::TypeRep(elem_rep), Value::Ref(arr)] => {
                if arr.is_readonly() {
                    return Err("illegal write through readonly reference".to_string());
                }
                let HeapValue::Array(items) = &mut *arr.obj.borrow_mut() else {
                    return Err("core::intrinsics::array_pop: expected an array".to_string());
                };
                match items.pop() {
                    Some(v) => Ok(alloc_ref(HeapValue::Enum {
                        enum_name: "Option".to_string(),
                        type_args: vec![*elem_rep],
                        variant: "Some".to_string(),
                        fields: vec![v],
                    })),
                    None => Ok(alloc_ref(HeapValue::Enum {
                        enum_name: "Option".to_string(),
                        type_args: vec![*elem_rep],
                        variant: "None".to_string(),
                        fields: Vec::new(),
                    })),
                }
            }
            _ => Err(bad_args("core::intrinsics::array_pop")),
        },
        I::ArrayClear => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(arr)] => {
                if arr.is_readonly() {
                    return Err("illegal write through readonly reference".to_string());
                }
                let HeapValue::Array(items) = &mut *arr.obj.borrow_mut() else {
                    return Err("core::intrinsics::array_clear: expected an array".to_string());
                };
                items.clear();
                Ok(Value::Unit)
            }
            _ => Err(bad_args("core::intrinsics::array_clear")),
        },
        I::ArrayResize => match args.as_slice() {
            [
                Value::TypeRep(_),
                Value::Ref(arr),
                Value::Int(new_len),
                fill,
            ] => {
                if arr.is_readonly() {
                    return Err("illegal write through readonly reference".to_string());
                }
                if *new_len < 0 {
                    return Err("core::intrinsics::array_resize: new_len must be >= 0".to_string());
                }
                let new_len_usize: usize = *new_len as usize;

                let HeapValue::Array(items) = &mut *arr.obj.borrow_mut() else {
                    return Err("core::intrinsics::array_resize: expected an array".to_string());
                };

                if new_len_usize <= items.len() {
                    items.truncate(new_len_usize);
                    return Ok(Value::Unit);
                }

                let extra = new_len_usize - items.len();
                items.reserve(extra);
                for _ in 0..extra {
                    items.push(fill.clone());
                }
                Ok(Value::Unit)
            }
            _ => Err(bad_args("core::intrinsics::array_resize")),
        },
        I::ArrayInsert => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(arr), Value::Int(idx), value] => {
                if arr.is_readonly() {
                    return Err("illegal write through readonly reference".to_string());
                }
                let HeapValue::Array(items) = &mut *arr.obj.borrow_mut() else {
                    return Err("core::intrinsics::array_insert: expected an array".to_string());
                };
                let idx_usize: usize = (*idx).try_into().map_err(|_| {
                    format!("index out of bounds: index={idx}, len={}", items.len())
                })?;
                if idx_usize > items.len() {
                    return Err(format!(
                        "index out of bounds: index={idx}, len={}",
                        items.len()
                    ));
                }
                items.insert(idx_usize, value.clone());
                Ok(Value::Unit)
            }
            _ => Err(bad_args("core::intrinsics::array_insert")),
        },
        I::ArrayRemove => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(arr), Value::Int(idx)] => {
                if arr.is_readonly() {
                    return Err("illegal write through readonly reference".to_string());
                }
                let HeapValue::Array(items) = &mut *arr.obj.borrow_mut() else {
                    return Err("core::intrinsics::array_remove: expected an array".to_string());
                };
                let idx_usize: usize = (*idx).try_into().map_err(|_| {
                    format!("index out of bounds: index={idx}, len={}", items.len())
                })?;
                if idx_usize >= items.len() {
                    return Err(format!(
                        "index out of bounds: index={idx}, len={}",
                        items.len()
                    ));
                }
                Ok(items.remove(idx_usize))
            }
            _ => Err(bad_args("core::intrinsics::array_remove")),
        },
        I::ArrayExtend => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(arr), Value::Ref(other)] => {
                if arr.is_readonly() {
                    return Err("illegal write through readonly reference".to_string());
                }
                let other_items = match &*other.obj.borrow() {
                    HeapValue::Array(items) => items.clone(),
                    _ => {
                        return Err(
                            "core::intrinsics::array_extend: expected an array for `other`"
                                .to_string(),
                        );
                    }
                };
                let HeapValue::Array(items) = &mut *arr.obj.borrow_mut() else {
                    return Err(
                        "core::intrinsics::array_extend: expected an array for `arr`".to_string(),
                    );
                };
                items.extend(other_items);
                Ok(Value::Unit)
            }
            _ => Err(bad_args("core::intrinsics::array_extend")),
        },
        I::ArrayConcat => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(a), Value::Ref(b)] => {
                let a_items = match &*a.obj.borrow() {
                    HeapValue::Array(items) => items.clone(),
                    _ => {
                        return Err(
                            "core::intrinsics::array_concat: expected an array for `a`".to_string()
                        );
                    }
                };
                let b_items = match &*b.obj.borrow() {
                    HeapValue::Array(items) => items.clone(),
                    _ => {
                        return Err(
                            "core::intrinsics::array_concat: expected an array for `b`".to_string()
                        );
                    }
                };
                let mut items = Vec::with_capacity(a_items.len() + b_items.len());
                items.extend(a_items);
                items.extend(b_items);
                Ok(alloc_ref(HeapValue::Array(items)))
            }
            _ => Err(bad_args("core::intrinsics::array_concat")),
        },
        I::ArrayConcatRo => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(a), Value::Ref(b)] => {
                let a_items = match &*a.obj.borrow() {
                    HeapValue::Array(items) => items.clone(),
                    _ => {
                        return Err(
                            "core::intrinsics::array_concat_ro: expected an array for `a`"
                                .to_string(),
                        );
                    }
                };
                let b_items = match &*b.obj.borrow() {
                    HeapValue::Array(items) => items.clone(),
                    _ => {
                        return Err(
                            "core::intrinsics::array_concat_ro: expected an array for `b`"
                                .to_string(),
                        );
                    }
                };
                let mut items = Vec::with_capacity(a_items.len() + b_items.len());
                items.extend(a_items.into_iter().map(Value::into_readonly_view));
                items.extend(b_items.into_iter().map(Value::into_readonly_view));
                Ok(alloc_ref(HeapValue::Array(items)))
            }
            _ => Err(bad_args("core::intrinsics::array_concat_ro")),
        },
        I::ArraySlice => match args.as_slice() {
            [
                Value::TypeRep(_),
                Value::Ref(arr),
                Value::Int(start),
                Value::Int(end),
            ] => {
                let borrowed = arr.obj.borrow();
                let items = match &*borrowed {
                    HeapValue::Array(items) => items,
                    _ => {
                        return Err("core::intrinsics::array_slice: expected an array".to_string());
                    }
                };
                let len = items.len();

                let start_usize: usize = (*start)
                    .try_into()
                    .map_err(|_| format!("index out of bounds: index={start}, len={len}"))?;
                let end_usize: usize = (*end)
                    .try_into()
                    .map_err(|_| format!("index out of bounds: index={end}, len={len}"))?;
                if start_usize > end_usize {
                    return Err("core::intrinsics::array_slice: start must be <= end".to_string());
                }
                if end_usize > len {
                    return Err(format!("index out of bounds: index={end}, len={len}"));
                }
                Ok(alloc_ref(HeapValue::Array(
                    items[start_usize..end_usize].to_vec(),
                )))
            }
            _ => Err(bad_args("core::intrinsics::array_slice")),
        },
        I::ArraySliceRo => match args.as_slice() {
            [
                Value::TypeRep(_),
                Value::Ref(arr),
                Value::Int(start),
                Value::Int(end),
            ] => {
                let borrowed = arr.obj.borrow();
                let items = match &*borrowed {
                    HeapValue::Array(items) => items,
                    _ => {
                        return Err(
                            "core::intrinsics::array_slice_ro: expected an array".to_string()
                        );
                    }
                };
                let len = items.len();

                let start_usize: usize = (*start)
                    .try_into()
                    .map_err(|_| format!("index out of bounds: index={start}, len={len}"))?;
                let end_usize: usize = (*end)
                    .try_into()
                    .map_err(|_| format!("index out of bounds: index={end}, len={len}"))?;
                if start_usize > end_usize {
                    return Err(
                        "core::intrinsics::array_slice_ro: start must be <= end".to_string()
                    );
                }
                if end_usize > len {
                    return Err(format!("index out of bounds: index={end}, len={len}"));
                }
                Ok(alloc_ref(HeapValue::Array(
                    items[start_usize..end_usize]
                        .iter()
                        .cloned()
                        .map(Value::into_readonly_view)
                        .collect(),
                )))
            }
            _ => Err(bad_args("core::intrinsics::array_slice_ro")),
        },

        I::IntoIter => match args.as_slice() {
            [Value::TypeRep(elem_rep), Value::Ref(arr)] => {
                match &*arr.obj.borrow() {
                    HeapValue::Array(_) => {}
                    _ => {
                        return Err("core::intrinsics::into_iter: expected an array".to_string());
                    }
                }

                let layout = module
                    .struct_layouts
                    .get(ARRAY_ITER_TYPE)
                    .ok_or_else(|| format!("missing struct layout for `{ARRAY_ITER_TYPE}`"))?;
                let mut fields = vec![Value::Unit; layout.len()];

                let arr_idx = struct_field_index(module, ARRAY_ITER_TYPE, ARRAY_ITER_FIELD_ARRAY)?;
                let idx_idx = struct_field_index(module, ARRAY_ITER_TYPE, ARRAY_ITER_FIELD_INDEX)?;

                fields[arr_idx] = Value::Ref(arr.clone());
                fields[idx_idx] = Value::Int(0);

                Ok(alloc_ref(HeapValue::Struct {
                    type_name: ARRAY_ITER_TYPE.to_string(),
                    type_args: vec![*elem_rep],
                    fields,
                }))
            }
            _ => Err(bad_args("core::intrinsics::into_iter")),
        },
        I::Next => match args.as_slice() {
            [Value::TypeRep(elem_rep), Value::Ref(iter)] => {
                if iter.is_readonly() {
                    return Err("illegal write through readonly reference".to_string());
                }

                let arr_idx = struct_field_index(module, ARRAY_ITER_TYPE, ARRAY_ITER_FIELD_ARRAY)?;
                let idx_idx = struct_field_index(module, ARRAY_ITER_TYPE, ARRAY_ITER_FIELD_INDEX)?;

                let (arr_ref, idx) = {
                    let borrowed = iter.obj.borrow();
                    let HeapValue::Struct {
                        type_name, fields, ..
                    } = &*borrowed
                    else {
                        return Err("core::intrinsics::next: expected iterator struct".to_string());
                    };
                    if type_name != ARRAY_ITER_TYPE {
                        return Err(format!(
                            "core::intrinsics::next: expected `{ARRAY_ITER_TYPE}`, got `{type_name}`"
                        ));
                    }

                    let Some(Value::Ref(arr_ref)) = fields.get(arr_idx).cloned() else {
                        return Err(
                            "core::intrinsics::next: iterator missing `arr` field".to_string()
                        );
                    };
                    let Some(Value::Int(idx)) = fields.get(idx_idx).cloned() else {
                        return Err(
                            "core::intrinsics::next: iterator missing `idx` field".to_string()
                        );
                    };
                    (arr_ref, idx)
                };

                if idx < 0 {
                    return Err("core::intrinsics::next: negative iterator index".to_string());
                }
                let idx_usize: usize = idx as usize;

                let out = {
                    let borrowed = arr_ref.obj.borrow();
                    let HeapValue::Array(items) = &*borrowed else {
                        return Err(
                            "core::intrinsics::next: iterator `arr` is not an array".to_string()
                        );
                    };

                    if idx_usize < items.len() {
                        let mut item = items[idx_usize].clone();
                        if arr_ref.is_readonly() {
                            item = item.into_readonly_view();
                        }
                        alloc_ref(HeapValue::Enum {
                            enum_name: "Option".to_string(),
                            type_args: vec![*elem_rep],
                            variant: "Some".to_string(),
                            fields: vec![item],
                        })
                    } else {
                        alloc_ref(HeapValue::Enum {
                            enum_name: "Option".to_string(),
                            type_args: vec![*elem_rep],
                            variant: "None".to_string(),
                            fields: Vec::new(),
                        })
                    }
                };

                {
                    let mut borrowed = iter.obj.borrow_mut();
                    let HeapValue::Struct { fields, .. } = &mut *borrowed else {
                        return Err("core::intrinsics::next: expected iterator struct".to_string());
                    };
                    let Some(slot) = fields.get_mut(idx_idx) else {
                        return Err(
                            "core::intrinsics::next: iterator missing `idx` field".to_string()
                        );
                    };
                    *slot = Value::Int(idx + 1);
                }

                Ok(out)
            }
            _ => Err(bad_args("core::intrinsics::next")),
        },
    }
}

fn match_pattern(
    module: &ExecutableModule,
    pat: &rusk_mir::Pattern,
    value: &Value,
    binds: &mut Vec<Value>,
) -> Result<bool, String> {
    use rusk_mir::{ConstValue, Pattern};

    match pat {
        Pattern::Wildcard => Ok(true),
        Pattern::Bind => {
            binds.push(value.clone());
            Ok(true)
        }
        Pattern::Literal(lit) => Ok(match (lit, value) {
            (ConstValue::Unit, Value::Unit) => true,
            (ConstValue::Bool(a), Value::Bool(b)) => a == b,
            (ConstValue::Int(a), Value::Int(b)) => a == b,
            (ConstValue::Float(a), Value::Float(b)) => a == b,
            (ConstValue::String(a), Value::String(b)) => a == b,
            (ConstValue::Bytes(a), Value::Bytes(b)) => a == b,
            (ConstValue::FunctionId(a), Value::Function(b)) => a.0 == b.0,
            _ => false,
        }),
        Pattern::Enum {
            enum_name,
            variant,
            fields,
        } => {
            let Value::Ref(r) = value else {
                return Ok(false);
            };
            let (e, v, actual_fields) = match &*r.obj.borrow() {
                HeapValue::Enum {
                    enum_name,
                    variant,
                    fields,
                    ..
                } => (enum_name.clone(), variant.clone(), fields.clone()),
                _ => return Ok(false),
            };
            if e != *enum_name || v != *variant || actual_fields.len() != fields.len() {
                return Ok(false);
            }
            for (p, actual) in fields.iter().zip(actual_fields.iter()) {
                let actual = if r.readonly {
                    actual.clone().into_readonly_view()
                } else {
                    actual.clone()
                };
                if !match_pattern(module, p, &actual, binds)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        Pattern::Tuple {
            prefix,
            rest,
            suffix,
        } => {
            let Value::Ref(r) = value else {
                return Ok(false);
            };
            let actual_items = match &*r.obj.borrow() {
                HeapValue::Tuple(items) => items.clone(),
                _ => return Ok(false),
            };
            let min_len = prefix.len() + suffix.len();
            if rest.is_some() {
                if actual_items.len() < min_len {
                    return Ok(false);
                }
            } else if actual_items.len() != min_len {
                return Ok(false);
            }

            let readonly = r.readonly;

            for (p, actual) in prefix.iter().zip(actual_items.iter()) {
                let actual = if readonly {
                    actual.clone().into_readonly_view()
                } else {
                    actual.clone()
                };
                if !match_pattern(module, p, &actual, binds)? {
                    return Ok(false);
                }
            }

            if let Some(rest_pat) = rest {
                let start = prefix.len();
                let end = actual_items.len().saturating_sub(suffix.len());
                let slice: Vec<Value> = actual_items[start..end]
                    .iter()
                    .cloned()
                    .map(|v| if readonly { v.into_readonly_view() } else { v })
                    .collect();
                match rest_pat.as_ref() {
                    Pattern::Wildcard => {}
                    Pattern::Bind => {
                        let rest_value = if slice.is_empty() {
                            Value::Unit
                        } else {
                            alloc_ref(HeapValue::Tuple(slice))
                        };
                        binds.push(rest_value);
                    }
                    _ => return Ok(false),
                }
            }

            for (p, actual) in suffix
                .iter()
                .zip(actual_items.iter().rev().take(suffix.len()).rev())
            {
                let actual = if readonly {
                    actual.clone().into_readonly_view()
                } else {
                    actual.clone()
                };
                if !match_pattern(module, p, &actual, binds)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        Pattern::Struct { type_name, fields } => {
            let Value::Ref(r) = value else {
                return Ok(false);
            };
            let (actual_ty, actual_fields) = match &*r.obj.borrow() {
                HeapValue::Struct {
                    type_name, fields, ..
                } => (type_name.clone(), fields.clone()),
                _ => return Ok(false),
            };
            if actual_ty != *type_name {
                return Ok(false);
            }
            let Some(layout) = module.struct_layouts.get(actual_ty.as_str()) else {
                return Err(format!("missing struct layout for `{actual_ty}`"));
            };
            for (field_name, field_pat) in fields.iter() {
                let Some(idx) = layout.iter().position(|f| f == field_name) else {
                    return Err(format!("missing field: {field_name}"));
                };
                let Some(field_value) = actual_fields.get(idx) else {
                    return Err(format!("missing field: {field_name}"));
                };
                let field_value = if r.readonly {
                    field_value.clone().into_readonly_view()
                } else {
                    field_value.clone()
                };
                if !match_pattern(module, field_pat, &field_value, binds)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        Pattern::Array {
            prefix,
            rest,
            suffix,
        } => {
            let Value::Ref(r) = value else {
                return Ok(false);
            };
            let actual_items = match &*r.obj.borrow() {
                HeapValue::Array(items) => items.clone(),
                _ => return Ok(false),
            };
            let min_len = prefix.len() + suffix.len();
            if rest.is_some() {
                if actual_items.len() < min_len {
                    return Ok(false);
                }
            } else if actual_items.len() != min_len {
                return Ok(false);
            }

            let readonly = r.readonly;

            for (p, actual) in prefix.iter().zip(actual_items.iter()) {
                let actual = if readonly {
                    actual.clone().into_readonly_view()
                } else {
                    actual.clone()
                };
                if !match_pattern(module, p, &actual, binds)? {
                    return Ok(false);
                }
            }

            if let Some(rest_pat) = rest {
                let start = prefix.len();
                let end = actual_items.len().saturating_sub(suffix.len());
                let slice: Vec<Value> = actual_items[start..end]
                    .iter()
                    .cloned()
                    .map(|v| if readonly { v.into_readonly_view() } else { v })
                    .collect();
                match rest_pat.as_ref() {
                    Pattern::Wildcard => {}
                    Pattern::Bind => {
                        binds.push(alloc_ref(HeapValue::Array(slice)));
                    }
                    _ => return Ok(false),
                }
            }

            for (p, actual) in suffix
                .iter()
                .zip(actual_items.iter().rev().take(suffix.len()).rev())
            {
                let actual = if readonly {
                    actual.clone().into_readonly_view()
                } else {
                    actual.clone()
                };
                if !match_pattern(module, p, &actual, binds)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
    }
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

fn call_host_import(
    module: &ExecutableModule,
    host_fns: &mut Vec<Option<Box<dyn HostFn>>>,
    in_host_call: &mut bool,
    frame: &mut Frame,
    dst: Option<rusk_bytecode::Reg>,
    hid: HostImportId,
    args: &[rusk_bytecode::Reg],
) -> Result<(), String> {
    let Some(import) = module.host_import(hid) else {
        return Err(format!("invalid host import id {}", hid.0));
    };

    if args.len() != import.sig.params.len() {
        return Err(format!(
            "host call `{}` arity mismatch: expected {} args but got {}",
            import.name,
            import.sig.params.len(),
            args.len()
        ));
    }

    let idx: usize = hid.0 as usize;
    let Some(host_fn) = host_fns.get_mut(idx).and_then(|v| v.as_mut()) else {
        return Err(format!(
            "missing host import implementation: `{}`",
            import.name
        ));
    };

    let mut abi_args = Vec::with_capacity(args.len());
    for (arg_reg, expected) in args.iter().zip(import.sig.params.iter()) {
        let src_idx: usize = (*arg_reg).try_into().unwrap_or(usize::MAX);
        let Some(v) = frame.regs.get(src_idx).and_then(|v| v.as_ref()).cloned() else {
            return Err(format!(
                "host call `{}` read from uninitialized reg {arg_reg}",
                import.name
            ));
        };
        let Some(abi) = v.to_abi() else {
            return Err(format!(
                "host call `{}` arg type mismatch: expected {:?}, got {}",
                import.name,
                expected,
                v.kind()
            ));
        };
        if abi.ty() != *expected {
            return Err(format!(
                "host call `{}` arg type mismatch: expected {:?}, got {:?}",
                import.name,
                expected,
                abi.ty()
            ));
        }
        abi_args.push(abi);
    }

    *in_host_call = true;
    let call_result = host_fn.call(&abi_args);
    *in_host_call = false;

    let abi_ret = match call_result {
        Ok(v) => v,
        Err(e) => {
            return Err(format!("host call `{}` failed: {e}", import.name));
        }
    };

    if abi_ret.ty() != import.sig.ret {
        return Err(format!(
            "host call `{}` return type mismatch: expected {:?}, got {:?}",
            import.name,
            import.sig.ret,
            abi_ret.ty()
        ));
    }

    if let Some(dst) = dst {
        write_value(frame, dst, Value::from_abi(&abi_ret))
            .map_err(|msg| format!("host call `{}` dst: {msg}", import.name))?;
    }

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
        VmState::Running | VmState::Done { .. } | VmState::Trapped { .. } => {
            Err(VmError::InvalidState {
                message: "vm is not suspended".to_string(),
            })
        }
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
            vm.handlers.clear();
            vm.state = VmState::Trapped {
                message: "cancelled".to_string(),
            };
            Ok(())
        }
        VmState::Running | VmState::Done { .. } | VmState::Trapped { .. } => {
            Err(VmError::InvalidState {
                message: "vm is not suspended".to_string(),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rusk_bytecode::{
        AbiType, CallTarget, ExecutableModule, Function, HostFnSig, HostImport, Instruction,
    };

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
        assert_eq!(
            got,
            StepResult::Done {
                value: AbiValue::Unit
            }
        );
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
            StepResult::Done {
                value: AbiValue::Unit
            }
        );
        assert_eq!(
            vm_step(&mut vm, None),
            StepResult::Done {
                value: AbiValue::Unit
            }
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
    fn vcall_dispatch_reads_receiver_type_args_and_calls_method() {
        let mut module = ExecutableModule::default();
        module
            .struct_layouts
            .insert("S".to_string(), vec!["x".to_string()]);

        let foo = module
            .add_function(Function {
                name: "S::foo".to_string(),
                reg_count: 6,
                param_count: 3,
                code: vec![
                    Instruction::MakeTypeRep {
                        dst: 3,
                        base: rusk_mir::TypeRepLit::Array,
                        args: vec![0],
                    },
                    Instruction::MakeTypeRep {
                        dst: 4,
                        base: rusk_mir::TypeRepLit::Array,
                        args: vec![1],
                    },
                    Instruction::StructGet {
                        dst: 5,
                        obj: 2,
                        idx: 0,
                    },
                    Instruction::Return { value: 5 },
                ],
            })
            .unwrap();

        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 5,
                param_count: 0,
                code: vec![
                    Instruction::Const {
                        dst: 0,
                        value: rusk_bytecode::ConstValue::TypeRep(rusk_mir::TypeRepLit::Int),
                    },
                    Instruction::Const {
                        dst: 1,
                        value: rusk_bytecode::ConstValue::TypeRep(rusk_mir::TypeRepLit::Bool),
                    },
                    Instruction::Const {
                        dst: 2,
                        value: rusk_bytecode::ConstValue::Int(42),
                    },
                    Instruction::MakeStruct {
                        dst: 3,
                        type_name: "S".to_string(),
                        type_args: vec![0],
                        fields: vec![("x".to_string(), 2)],
                    },
                    Instruction::VCall {
                        dst: Some(4),
                        obj: 3,
                        method: "foo".to_string(),
                        method_type_args: vec![1],
                        args: vec![],
                    },
                    Instruction::Return { value: 4 },
                ],
            })
            .unwrap();

        module.entry = main;
        module
            .methods
            .insert(("S".to_string(), "foo".to_string()), foo);

        let mut vm = Vm::new(module).unwrap();
        assert_eq!(
            vm_step(&mut vm, None),
            StepResult::Done {
                value: AbiValue::Int(42)
            }
        );
    }

    #[test]
    fn in_vm_effect_handler_resume_splices_continuation() {
        let mut module = ExecutableModule::default();
        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 5,
                param_count: 0,
                code: vec![
                    Instruction::PushHandler {
                        clauses: vec![rusk_bytecode::HandlerClause {
                            effect: rusk_bytecode::EffectSpec {
                                interface: "Test".to_string(),
                                interface_args: vec![],
                                method: "boom".to_string(),
                            },
                            arg_patterns: vec![rusk_mir::Pattern::Bind],
                            target_pc: 6,
                            param_regs: vec![0, 1],
                        }],
                    },
                    Instruction::Const {
                        dst: 2,
                        value: rusk_bytecode::ConstValue::Int(41),
                    },
                    Instruction::Perform {
                        dst: Some(3),
                        effect: rusk_bytecode::EffectSpec {
                            interface: "Test".to_string(),
                            interface_args: vec![],
                            method: "boom".to_string(),
                        },
                        args: vec![2],
                    },
                    Instruction::Const {
                        dst: 4,
                        value: rusk_bytecode::ConstValue::Int(1),
                    },
                    Instruction::IntAdd { dst: 4, a: 3, b: 4 },
                    Instruction::Return { value: 4 },
                    Instruction::Resume {
                        dst: Some(2),
                        k: 1,
                        value: 0,
                    },
                    Instruction::PopHandler,
                    Instruction::Return { value: 2 },
                ],
            })
            .unwrap();
        module.entry = main;

        let mut vm = Vm::new(module).unwrap();
        assert_eq!(
            vm_step(&mut vm, None),
            StepResult::Done {
                value: AbiValue::Int(42)
            }
        );
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
