use alloc::rc::Rc;
use core::cell::RefCell;
use rusk_bytecode::{EffectId, ExecutableModule, FunctionId, HostImportId, MethodId, TypeId};
use rusk_gc::{GcHeap, GcRef, ImmixHeap, Trace, Tracer};
use std::collections::HashMap;

use crate::{AbiValue, HostFn, VmError, VmMetrics};

const VM_GC_TRIGGER_ALLOCATIONS: usize = 50_000;

mod resume;
mod step;

pub use resume::vm_resume_pinned_continuation_tail;
pub use resume::{vm_drop_continuation, vm_drop_pinned_continuation, vm_resume};
pub use step::vm_step;

/// A handle to a captured continuation produced by `perform`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ContinuationHandle {
    /// A stable handle index.
    ///
    /// - `0` is reserved for the single outstanding external effect suspension handle returned in
    ///   [`StepResult::Request`].
    /// - Non-zero values refer to entries in the per-VM pinned continuation table used for
    ///   host-storable continuation values (`AbiValue::Continuation`).
    pub index: u32,
    /// A generation counter used to invalidate stale handles.
    pub generation: u32,
}

#[derive(Debug, Default)]
struct PinnedContinuations {
    slots: Vec<PinnedContinuationSlot>,
    free: Vec<u32>,
}

#[derive(Debug)]
struct PinnedContinuationSlot {
    generation: u32,
    token: Option<ContinuationToken>,
}

impl PinnedContinuations {
    fn pin(&mut self, token: ContinuationToken) -> Result<ContinuationHandle, String> {
        let slot_index: u32 = match self.free.pop() {
            Some(idx) => idx,
            None => {
                let idx: u32 = self.slots.len().try_into().map_err(|_| {
                    "pinned continuation table overflow (too many continuations)".to_string()
                })?;
                self.slots.push(PinnedContinuationSlot {
                    generation: 0,
                    token: None,
                });
                idx
            }
        };
        let slot_index_usize: usize = slot_index as usize;
        let Some(slot) = self.slots.get_mut(slot_index_usize) else {
            return Err("internal error: pinned continuation slot out of range".to_string());
        };
        slot.token = Some(token);

        // Reserve `index == 0` for the step API suspended continuation handle.
        let index = slot_index
            .checked_add(1)
            .ok_or_else(|| "pinned continuation handle overflow".to_string())?;

        Ok(ContinuationHandle {
            index,
            generation: slot.generation,
        })
    }

    fn resolve(&self, handle: ContinuationHandle) -> Result<ContinuationToken, String> {
        if handle.index == 0 {
            return Err("invalid pinned continuation handle: index 0 is reserved".to_string());
        }
        let slot_index = handle.index - 1;
        let slot_index_usize: usize = slot_index as usize;
        let Some(slot) = self.slots.get(slot_index_usize) else {
            return Err("invalid pinned continuation handle: out of range".to_string());
        };
        if slot.generation != handle.generation {
            return Err("invalid pinned continuation handle: generation mismatch".to_string());
        }
        let Some(token) = slot.token.as_ref() else {
            return Err("invalid pinned continuation handle: dropped".to_string());
        };
        if token.state.borrow().is_none() {
            return Err("invalid pinned continuation handle: already consumed".to_string());
        }
        Ok(token.clone())
    }

    fn drop_pinned(&mut self, handle: ContinuationHandle) -> Result<(), VmError> {
        if handle.index == 0 {
            return Err(VmError::InvalidContinuation {
                message: "invalid pinned continuation handle: index 0 is reserved".to_string(),
            });
        }
        let slot_index = handle.index - 1;
        let slot_index_usize: usize = slot_index as usize;
        let Some(slot) = self.slots.get_mut(slot_index_usize) else {
            return Err(VmError::InvalidContinuation {
                message: "invalid pinned continuation handle: out of range".to_string(),
            });
        };
        if slot.generation != handle.generation {
            return Err(VmError::InvalidContinuation {
                message: "invalid pinned continuation handle: generation mismatch".to_string(),
            });
        }
        if slot.token.is_none() {
            return Err(VmError::InvalidContinuation {
                message: "invalid pinned continuation handle: already dropped".to_string(),
            });
        }

        slot.token = None;
        slot.generation = slot.generation.wrapping_add(1);
        self.free.push(slot_index);
        Ok(())
    }
}

impl Trace for PinnedContinuations {
    fn trace(&self, tracer: &mut dyn Tracer) {
        for slot in &self.slots {
            if let Some(token) = &slot.token {
                token.trace(tracer);
            }
        }
    }
}

/// Result of a single VM stepping operation.
#[derive(Clone, Debug, PartialEq)]
pub enum StepResult {
    /// The program completed successfully with a final ABI value.
    Done { value: AbiValue },
    /// The VM trapped (a runtime error occurred).
    Trap { message: String },
    /// The VM requested an external effect operation and suspended execution.
    Request {
        effect_id: EffectId,
        args: Vec<AbiValue>,
        k: ContinuationHandle,
    },
    /// The VM yielded due to fuel exhaustion.
    Yield { remaining_fuel: u64 },
}

/// A stable identifier for an interned runtime type representation.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeRepId(pub u32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum TypeCtor {
    Unit,
    Never,
    Bool,
    Int,
    Float,
    Byte,
    Char,
    String,
    Bytes,
    Array,
    Tuple(usize),
    Struct(TypeId),
    Enum(TypeId),
    Interface(TypeId),
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

    fn ctor_from_lit(
        module: &ExecutableModule,
        lit: &rusk_bytecode::TypeRepLit,
    ) -> Result<TypeCtor, String> {
        Ok(match lit {
            rusk_bytecode::TypeRepLit::Unit => TypeCtor::Unit,
            rusk_bytecode::TypeRepLit::Never => TypeCtor::Never,
            rusk_bytecode::TypeRepLit::Bool => TypeCtor::Bool,
            rusk_bytecode::TypeRepLit::Int => TypeCtor::Int,
            rusk_bytecode::TypeRepLit::Float => TypeCtor::Float,
            rusk_bytecode::TypeRepLit::Byte => TypeCtor::Byte,
            rusk_bytecode::TypeRepLit::Char => TypeCtor::Char,
            rusk_bytecode::TypeRepLit::String => TypeCtor::String,
            rusk_bytecode::TypeRepLit::Bytes => TypeCtor::Bytes,
            rusk_bytecode::TypeRepLit::Array => TypeCtor::Array,
            rusk_bytecode::TypeRepLit::Tuple(arity) => TypeCtor::Tuple(*arity),
            rusk_bytecode::TypeRepLit::Struct(name) => TypeCtor::Struct(
                module
                    .type_id(name.as_str())
                    .ok_or_else(|| format!("unknown struct type `{name}`"))?,
            ),
            rusk_bytecode::TypeRepLit::Enum(name) => TypeCtor::Enum(
                module
                    .type_id(name.as_str())
                    .ok_or_else(|| format!("unknown enum type `{name}`"))?,
            ),
            rusk_bytecode::TypeRepLit::Interface(name) => TypeCtor::Interface(
                module
                    .type_id(name.as_str())
                    .ok_or_else(|| format!("unknown interface type `{name}`"))?,
            ),
            rusk_bytecode::TypeRepLit::Fn => TypeCtor::Fn,
            rusk_bytecode::TypeRepLit::Cont => TypeCtor::Cont,
        })
    }
}

#[derive(Clone, Debug)]
struct RefValue {
    readonly: bool,
    handle: GcRef,
}

impl RefValue {
    fn new(handle: GcRef) -> Self {
        Self {
            readonly: false,
            handle,
        }
    }

    fn as_readonly(&self) -> Self {
        Self {
            readonly: true,
            handle: self.handle,
        }
    }

    fn is_readonly(&self) -> bool {
        self.readonly
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

#[derive(Clone, Copy, Debug)]
struct BytesView {
    buf: GcRef,
    start: u32,
    len: u32,
}

#[derive(Clone, Copy, Debug)]
struct StringView {
    buf: GcRef,
    start: u32,
    len: u32,
}

impl BytesView {
    fn as_slice<'a>(&self, heap: &'a ImmixHeap<HeapValue>) -> Result<&'a [u8], String> {
        let Some(obj) = heap.get(self.buf) else {
            return Err("bytes view: dangling buffer".to_string());
        };
        let HeapValue::BytesBuf { data } = obj else {
            return Err("bytes view: expected bytes buffer".to_string());
        };

        let start: usize = self.start as usize;
        let len: usize = self.len as usize;
        let end = start
            .checked_add(len)
            .ok_or_else(|| "bytes view: index overflow".to_string())?;
        data.get(start..end)
            .ok_or_else(|| "bytes view: out of bounds".to_string())
    }

    fn len_usize(&self) -> usize {
        self.len as usize
    }
}

impl StringView {
    fn as_str<'a>(&self, heap: &'a ImmixHeap<HeapValue>) -> Result<&'a str, String> {
        let Some(obj) = heap.get(self.buf) else {
            return Err("string view: dangling buffer".to_string());
        };
        let HeapValue::StringBuf { data } = obj else {
            return Err("string view: expected string buffer".to_string());
        };

        let start: usize = self.start as usize;
        let len: usize = self.len as usize;
        let end = start
            .checked_add(len)
            .ok_or_else(|| "string view: index overflow".to_string())?;
        let s: &str = data.as_ref();
        if start > s.len() || end > s.len() || start > end {
            return Err("string view: out of bounds".to_string());
        }
        if !s.is_char_boundary(start) || !s.is_char_boundary(end) {
            return Err("string view: invalid UTF-8 boundary".to_string());
        }
        Ok(&s[start..end])
    }

    fn len_usize(&self) -> usize {
        self.len as usize
    }
}

#[derive(Clone, Debug)]
enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    Byte(u8),
    Char(char),
    String(StringView),
    Bytes(BytesView),
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
            Value::Byte(_) => "byte",
            Value::Char(_) => "char",
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

    fn from_abi(
        heap: &mut ImmixHeap<HeapValue>,
        gc_allocations_since_collect: &mut usize,
        pinned_continuations: &PinnedContinuations,
        v: &AbiValue,
    ) -> Result<Self, String> {
        Ok(match v {
            AbiValue::Unit => Value::Unit,
            AbiValue::Bool(b) => Value::Bool(*b),
            AbiValue::Int(n) => Value::Int(*n),
            AbiValue::Float(x) => Value::Float(*x),
            AbiValue::String(s) => alloc_string(heap, gc_allocations_since_collect, s.clone())?,
            AbiValue::Bytes(b) => alloc_bytes(heap, gc_allocations_since_collect, b.clone())?,
            AbiValue::Continuation(h) => {
                Value::Continuation(pinned_continuations.resolve(h.clone())?)
            }
        })
    }

    fn try_to_abi(
        &self,
        heap: &ImmixHeap<HeapValue>,
        pinned_continuations: &mut PinnedContinuations,
    ) -> Result<Option<AbiValue>, String> {
        Ok(Some(match self {
            Value::Unit => AbiValue::Unit,
            Value::Bool(b) => AbiValue::Bool(*b),
            Value::Int(n) => AbiValue::Int(*n),
            Value::Float(x) => AbiValue::Float(*x),
            Value::String(s) => AbiValue::String(s.as_str(heap)?.to_string()),
            Value::Bytes(b) => AbiValue::Bytes(b.as_slice(heap)?.to_vec()),
            Value::Continuation(k) => AbiValue::Continuation(pinned_continuations.pin(k.clone())?),
            Value::Byte(_)
            | Value::Char(_)
            | Value::TypeRep(_)
            | Value::Ref(_)
            | Value::Function(_) => {
                return Ok(None);
            }
        }))
    }
}

#[derive(Clone, Debug)]
enum HeapValue {
    BytesBuf {
        data: Box<[u8]>,
    },
    StringBuf {
        data: Box<str>,
    },
    Struct {
        type_id: TypeId,
        type_args: Vec<TypeRepId>,
        fields: Vec<Value>,
    },
    Array(Vec<Value>),
    Tuple(Vec<Value>),
    Enum {
        type_id: TypeId,
        type_args: Vec<TypeRepId>,
        variant: String,
        fields: Vec<Value>,
    },
}

impl Trace for Value {
    fn trace(&self, tracer: &mut dyn Tracer) {
        match self {
            Value::Ref(r) => tracer.mark(r.handle),
            Value::String(s) => tracer.mark(s.buf),
            Value::Bytes(b) => tracer.mark(b.buf),
            Value::Continuation(k) => k.trace(tracer),
            Value::Unit
            | Value::Bool(_)
            | Value::Int(_)
            | Value::Float(_)
            | Value::Byte(_)
            | Value::Char(_)
            | Value::TypeRep(_)
            | Value::Function(_) => {}
        }
    }
}

impl Trace for HeapValue {
    fn trace(&self, tracer: &mut dyn Tracer) {
        match self {
            HeapValue::BytesBuf { .. } | HeapValue::StringBuf { .. } => {}
            HeapValue::Struct { fields, .. } => {
                for v in fields {
                    v.trace(tracer);
                }
            }
            HeapValue::Array(items) | HeapValue::Tuple(items) => {
                for v in items {
                    v.trace(tracer);
                }
            }
            HeapValue::Enum { fields, .. } => {
                for v in fields {
                    v.trace(tracer);
                }
            }
        }
    }
}

impl Trace for ContinuationToken {
    fn trace(&self, tracer: &mut dyn Tracer) {
        let borrowed = self.state.borrow();
        let Some(state) = borrowed.as_ref() else {
            return;
        };

        for frame in &state.frames {
            for v in frame.regs.iter().flatten() {
                v.trace(tracer);
            }
        }
    }
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
    arg_patterns: Vec<rusk_bytecode::Pattern>,
    target_pc: u32,
    param_regs: Vec<rusk_bytecode::Reg>,
}

#[derive(Clone, Debug)]
struct HandlerEntry {
    owner_depth: usize,
    clauses: Vec<RuntimeHandlerClause>,
}

#[derive(Clone, Copy, Debug)]
struct HandlerLookupCacheEntry {
    generation: u32,
    handlers_len: usize,
    effect_hash: u64,
    handler_index: usize,
    clause_index: usize,
}

#[derive(Debug, Clone)]
enum ReturnDsts {
    None,
    One(rusk_bytecode::Reg),
    Multi(Vec<rusk_bytecode::Reg>),
}

impl ReturnDsts {
    fn from_option(dst: Option<rusk_bytecode::Reg>) -> Self {
        match dst {
            Some(r) => ReturnDsts::One(r),
            None => ReturnDsts::None,
        }
    }
}

#[derive(Debug, Clone)]
struct Frame {
    func: FunctionId,
    pc: usize,
    regs: Vec<Option<Value>>,
    return_dsts: ReturnDsts,
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

#[derive(Clone, Copy, Debug)]
struct PrimitiveTypeIds {
    unit: TypeId,
    bool: TypeId,
    int: TypeId,
    float: TypeId,
    byte: TypeId,
    char: TypeId,
    string: TypeId,
    bytes: TypeId,
}

impl PrimitiveTypeIds {
    fn from_module(module: &ExecutableModule) -> Result<Self, VmError> {
        let lookup = |name: &str| {
            module.type_id(name).ok_or_else(|| VmError::InvalidState {
                message: format!(
                    "missing required primitive type name `{name}` in module type table"
                ),
            })
        };

        Ok(Self {
            unit: lookup("unit")?,
            bool: lookup("bool")?,
            int: lookup("int")?,
            float: lookup("float")?,
            byte: lookup("byte")?,
            char: lookup("char")?,
            string: lookup("string")?,
            bytes: lookup("bytes")?,
        })
    }
}

#[derive(Clone, Copy, Debug)]
struct VCallFastPathIds {
    hash: Option<MethodId>,
    eq: Option<MethodId>,
    ne: Option<MethodId>,
}

impl VCallFastPathIds {
    fn from_module(module: &ExecutableModule) -> Self {
        Self {
            hash: module.method_id("core::hash::Hash::hash"),
            eq: module.method_id("core::ops::Eq::eq"),
            ne: module.method_id("core::ops::Ne::ne"),
        }
    }
}

/// A bytecode virtual machine for executing an [`ExecutableModule`].
///
/// Execution is performed incrementally via [`vm_step`]. Host imports declared by the module can
/// be backed by user-provided [`HostFn`] implementations registered with
/// [`Vm::register_host_import`].
pub struct Vm {
    heap: ImmixHeap<HeapValue>,
    gc_allocations_since_collect: usize,
    module: ExecutableModule,
    primitive_type_ids: PrimitiveTypeIds,
    vcall_fast_path_ids: VCallFastPathIds,
    state: VmState,
    frames: Vec<Frame>,
    handlers: Vec<HandlerEntry>,
    handler_stack_generation: u32,
    handler_lookup_cache: Option<HandlerLookupCacheEntry>,
    generic_specializations: HashMap<GenericSpecializationKey, HostImportId>,
    host_fns: Vec<Option<Box<dyn HostFn>>>,
    in_host_call: bool,
    continuation_generation: u32,
    pinned_continuations: PinnedContinuations,
    type_reps: TypeReps,

    metrics: VmMetrics,
    collect_metrics: bool,
}

impl Vm {
    /// Creates a new VM instance for `module` and prepares execution at the module entrypoint.
    ///
    /// The entry function must take 0 parameters.
    pub fn new(module: ExecutableModule) -> Result<Self, VmError> {
        Self::new_with_entry_argv(module, None)
    }

    /// Creates a new VM instance for `module`, passing `argv` to the entry function.
    ///
    /// The entry function must take exactly 1 parameter (`argv: [string]`).
    pub fn new_with_argv(module: ExecutableModule, argv: Vec<String>) -> Result<Self, VmError> {
        Self::new_with_entry_argv(module, Some(argv))
    }

    fn new_with_entry_argv(
        module: ExecutableModule,
        argv: Option<Vec<String>>,
    ) -> Result<Self, VmError> {
        let entry = module.entry;
        let Some(entry_fn) = module.function(entry) else {
            return Err(VmError::InvalidState {
                message: format!("invalid entry function id {}", entry.0),
            });
        };
        let entry_param_count = entry_fn.param_count;

        let reg_count: usize =
            entry_fn
                .reg_count
                .try_into()
                .map_err(|_| VmError::InvalidState {
                    message: "entry reg_count overflow".to_string(),
                })?;

        if (entry_param_count as usize) > reg_count {
            return Err(VmError::InvalidState {
                message: format!(
                    "entry param_count {} exceeds reg_count {}",
                    entry_param_count, reg_count
                ),
            });
        }

        let mut regs = Vec::with_capacity(reg_count);
        regs.resize(reg_count, None);

        let primitive_type_ids = PrimitiveTypeIds::from_module(&module)?;
        let vcall_fast_path_ids = VCallFastPathIds::from_module(&module);

        let mut vm = Self {
            heap: ImmixHeap::new(),
            gc_allocations_since_collect: 0,
            host_fns: {
                let mut host_fns: Vec<Option<Box<dyn HostFn>>> =
                    Vec::with_capacity(module.host_imports.len());
                host_fns.resize_with(module.host_imports.len(), || None);
                host_fns
            },
            module,
            primitive_type_ids,
            vcall_fast_path_ids,
            state: VmState::Running,
            frames: vec![Frame {
                func: entry,
                pc: 0,
                regs,
                return_dsts: ReturnDsts::None,
            }],
            handlers: Vec::new(),
            handler_stack_generation: 0,
            handler_lookup_cache: None,
            generic_specializations: HashMap::new(),
            in_host_call: false,
            continuation_generation: 0,
            pinned_continuations: PinnedContinuations::default(),
            type_reps: TypeReps::default(),
            metrics: VmMetrics::default(),
            collect_metrics: false,
        };

        let entry_generic_params = vm.module.function_generic_param_count(entry).unwrap_or(0);
        if entry_generic_params != 0 {
            return Err(VmError::InvalidState {
                message: "entry function cannot be generic".to_string(),
            });
        }

        match argv {
            None => {
                if entry_param_count != 0 {
                    return Err(VmError::InvalidState {
                        message: format!(
                            "entry function expects {entry_param_count} parameter(s); use Vm::new_with_argv"
                        ),
                    });
                }
            }
            Some(argv) => {
                if entry_param_count != 1 {
                    return Err(VmError::InvalidState {
                        message: format!(
                            "entry function expects {entry_param_count} parameter(s); cannot pass argv"
                        ),
                    });
                }

                let mut argv = argv;
                if argv.is_empty() {
                    argv.push(String::new());
                }

                let mut items = Vec::with_capacity(argv.len());
                for s in argv {
                    let v = alloc_string(&mut vm.heap, &mut vm.gc_allocations_since_collect, s)
                        .map_err(|message| VmError::InvalidState { message })?;
                    items.push(v);
                }
                let argv = alloc_ref(
                    &mut vm.heap,
                    &mut vm.gc_allocations_since_collect,
                    HeapValue::Array(items),
                );

                let Some(entry_frame) = vm.frames.first_mut() else {
                    return Err(VmError::InvalidState {
                        message: "missing entry frame".to_string(),
                    });
                };
                let Some(param0) = entry_frame.regs.first_mut() else {
                    return Err(VmError::InvalidState {
                        message: "entry param reg 0 out of range".to_string(),
                    });
                };
                *param0 = Some(argv);
            }
        }

        Ok(vm)
    }

    /// Enables or disables collection of [`VmMetrics`].
    pub fn enable_metrics(&mut self, enabled: bool) {
        self.collect_metrics = enabled;
    }

    /// Resets all collected [`VmMetrics`] counters to zero.
    pub fn reset_metrics(&mut self) {
        self.metrics = VmMetrics::default();
    }

    /// Takes the current [`VmMetrics`] and resets metrics to zero.
    pub fn take_metrics(&mut self) -> VmMetrics {
        std::mem::take(&mut self.metrics)
    }

    /// Returns a reference to the current [`VmMetrics`].
    pub fn metrics(&self) -> &VmMetrics {
        &self.metrics
    }

    /// Registers an implementation for a host import declared in the loaded module.
    ///
    /// The host function is invoked by the VM when executing a `CallTarget::Host(...)`.
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

    /// Interns (deduplicates) a runtime type representation and returns its [`TypeRepId`].
    pub fn intern_type_rep(
        &mut self,
        base: &rusk_bytecode::TypeRepLit,
        args: &[TypeRepId],
    ) -> Result<TypeRepId, VmError> {
        let ctor = TypeReps::ctor_from_lit(&self.module, base).map_err(|message| {
            VmError::InvalidState {
                message: format!("invalid typerep literal: {message}"),
            }
        })?;
        Ok(self.type_reps.intern(TypeRepNode {
            ctor,
            args: args.to_vec(),
        }))
    }

    /// Registers a specialization mapping for a generic bytecode function.
    ///
    /// This allows the VM to dispatch a generic bytecode function call to a concrete host import
    /// when the call site provides runtime `typerep` arguments matching `type_args`.
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

    /// Returns the number of currently live heap objects (best-effort; implementation-defined).
    pub fn heap_live_objects(&self) -> usize {
        self.heap.live_objects()
    }

    /// Forces an immediate garbage collection cycle.
    pub fn collect_garbage_now(&mut self) {
        let roots = VmRoots {
            frames: &self.frames,
            pinned_continuations: &self.pinned_continuations,
        };
        self.heap.collect(&roots);
        self.gc_allocations_since_collect = 0;
    }
}

struct VmRoots<'a> {
    frames: &'a [Frame],
    pinned_continuations: &'a PinnedContinuations,
}

impl Trace for VmRoots<'_> {
    fn trace(&self, tracer: &mut dyn Tracer) {
        for frame in self.frames {
            for v in frame.regs.iter().flatten() {
                v.trace(tracer);
            }
        }
        self.pinned_continuations.trace(tracer);
    }
}

fn effect_hash_spec(interface: &str, method: &str, interface_args: &[TypeRepId]) -> u64 {
    use std::hash::{Hash, Hasher};

    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    interface.hash(&mut hasher);
    interface_args.hash(&mut hasher);
    method.hash(&mut hasher);
    hasher.finish()
}

#[allow(clippy::too_many_arguments)]
fn match_cached_handler_for_effect(
    module: &ExecutableModule,
    heap: &mut ImmixHeap<HeapValue>,
    gc_allocations_since_collect: &mut usize,
    type_reps: &TypeReps,
    handlers: &[HandlerEntry],
    handler_index: usize,
    clause_index: usize,
    effect_interface: &str,
    interface_args: &[TypeRepId],
    effect_method: &str,
    args: &[Value],
) -> Result<Option<Vec<Value>>, String> {
    let Some(handler) = handlers.get(handler_index) else {
        return Ok(None);
    };
    let Some(clause) = handler.clauses.get(clause_index) else {
        return Ok(None);
    };

    if clause.effect.interface != effect_interface
        || clause.effect.method != effect_method
        || clause.effect.interface_args.as_slice() != interface_args
    {
        return Ok(None);
    }
    if clause.arg_patterns.len() != args.len() {
        return Ok(None);
    }

    let mut binds = Vec::new();
    for (pat, arg) in clause.arg_patterns.iter().zip(args.iter()) {
        if !match_pattern(
            module,
            heap,
            gc_allocations_since_collect,
            type_reps,
            pat,
            arg,
            &mut binds,
        )? {
            return Ok(None);
        }
    }

    Ok(Some(binds))
}

#[allow(clippy::too_many_arguments)]
fn find_handler_for_effect_spec(
    module: &ExecutableModule,
    heap: &mut ImmixHeap<HeapValue>,
    gc_allocations_since_collect: &mut usize,
    type_reps: &TypeReps,
    handlers: &[HandlerEntry],
    effect_interface: &str,
    interface_args: &[TypeRepId],
    effect_method: &str,
    args: &[Value],
) -> Result<Option<(usize, usize, Vec<Value>)>, String> {
    for (handler_index, handler) in handlers.iter().enumerate().rev() {
        for (clause_index, clause) in handler.clauses.iter().enumerate() {
            if clause.effect.interface != effect_interface
                || clause.effect.method != effect_method
                || clause.effect.interface_args.as_slice() != interface_args
            {
                continue;
            }
            if clause.arg_patterns.len() != args.len() {
                continue;
            }

            let mut binds = Vec::new();
            let mut ok = true;
            for (pat, arg) in clause.arg_patterns.iter().zip(args.iter()) {
                if !match_pattern(
                    module,
                    heap,
                    gc_allocations_since_collect,
                    type_reps,
                    pat,
                    arg,
                    &mut binds,
                )? {
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

fn alloc_ref(
    heap: &mut ImmixHeap<HeapValue>,
    gc_allocations_since_collect: &mut usize,
    obj: HeapValue,
) -> Value {
    *gc_allocations_since_collect = gc_allocations_since_collect.saturating_add(1);
    Value::Ref(RefValue::new(heap.alloc(obj)))
}

fn alloc_bytes(
    heap: &mut ImmixHeap<HeapValue>,
    gc_allocations_since_collect: &mut usize,
    bytes: Vec<u8>,
) -> Result<Value, String> {
    let data = bytes.into_boxed_slice();
    let len: u32 = data
        .len()
        .try_into()
        .map_err(|_| "bytes length overflow".to_string())?;
    *gc_allocations_since_collect = gc_allocations_since_collect.saturating_add(1);
    let buf = heap.alloc(HeapValue::BytesBuf { data });
    Ok(Value::Bytes(BytesView { buf, start: 0, len }))
}

fn alloc_string(
    heap: &mut ImmixHeap<HeapValue>,
    gc_allocations_since_collect: &mut usize,
    s: String,
) -> Result<Value, String> {
    let data = s.into_boxed_str();
    let len: u32 = data
        .len()
        .try_into()
        .map_err(|_| "string length overflow".to_string())?;
    *gc_allocations_since_collect = gc_allocations_since_collect.saturating_add(1);
    let buf = heap.alloc(HeapValue::StringBuf { data });
    Ok(Value::String(StringView { buf, start: 0, len }))
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
    type_id: TypeId,
    field: &str,
) -> Result<usize, String> {
    let Some(layout) = module
        .struct_layouts
        .get(type_id.0 as usize)
        .and_then(|v| v.as_ref())
    else {
        let ty = module.type_name(type_id).unwrap_or("<unknown>");
        return Err(format!("missing struct layout for `{ty}`"));
    };
    layout
        .iter()
        .position(|name| name == field)
        .ok_or_else(|| format!("missing field: {field}"))
}

fn type_test(
    module: &ExecutableModule,
    type_reps: &TypeReps,
    heap: &ImmixHeap<HeapValue>,
    primitive_type_ids: &PrimitiveTypeIds,
    value: &Value,
    target: TypeRepId,
) -> Result<bool, String> {
    let Some(target) = type_reps.node(target) else {
        return Err(format!("invalid typerep({})", target.0));
    };

    Ok(match &target.ctor {
        TypeCtor::Unit => matches!(value, Value::Unit),
        TypeCtor::Never => false,
        TypeCtor::Bool => matches!(value, Value::Bool(_)),
        TypeCtor::Int => matches!(value, Value::Int(_)),
        TypeCtor::Float => matches!(value, Value::Float(_)),
        TypeCtor::Byte => matches!(value, Value::Byte(_)),
        TypeCtor::Char => matches!(value, Value::Char(_)),
        TypeCtor::String => matches!(value, Value::String(_)),
        TypeCtor::Bytes => matches!(value, Value::Bytes(_)),
        TypeCtor::Array => match value {
            Value::Ref(r) => match heap.get(r.handle) {
                Some(HeapValue::Array(_)) => true,
                Some(_) => false,
                None => return Err("dangling reference in type test".to_string()),
            },
            _ => false,
        },
        TypeCtor::Tuple(arity) => match value {
            Value::Unit => *arity == 0,
            Value::Ref(r) => match heap.get(r.handle) {
                Some(HeapValue::Tuple(items)) => items.len() == *arity,
                Some(_) => false,
                None => return Err("dangling reference in type test".to_string()),
            },
            _ => false,
        },
        TypeCtor::Struct(type_id) => match value {
            Value::Ref(r) => match heap.get(r.handle) {
                Some(HeapValue::Struct {
                    type_id: dyn_type_id,
                    type_args,
                    ..
                }) => dyn_type_id == type_id && type_args.as_slice() == target.args.as_slice(),
                Some(_) => false,
                None => return Err("dangling reference in type test".to_string()),
            },
            _ => false,
        },
        TypeCtor::Enum(type_id) => match value {
            Value::Ref(r) => match heap.get(r.handle) {
                Some(HeapValue::Enum {
                    type_id: dyn_type_id,
                    type_args,
                    ..
                }) => dyn_type_id == type_id && type_args.as_slice() == target.args.as_slice(),
                Some(_) => false,
                None => return Err("dangling reference in type test".to_string()),
            },
            _ => false,
        },
        TypeCtor::Interface(iface_id) => {
            let (dyn_type_id, dyn_type_args): (TypeId, &[TypeRepId]) = match value {
                Value::Unit => (primitive_type_ids.unit, &[]),
                Value::Bool(_) => (primitive_type_ids.bool, &[]),
                Value::Int(_) => (primitive_type_ids.int, &[]),
                Value::Float(_) => (primitive_type_ids.float, &[]),
                Value::Byte(_) => (primitive_type_ids.byte, &[]),
                Value::Char(_) => (primitive_type_ids.char, &[]),
                Value::String(_) => (primitive_type_ids.string, &[]),
                Value::Bytes(_) => (primitive_type_ids.bytes, &[]),
                Value::Ref(r) => {
                    let Some(obj) = heap.get(r.handle) else {
                        return Err("dangling reference in type test".to_string());
                    };
                    match obj {
                        HeapValue::Struct {
                            type_id, type_args, ..
                        } => (*type_id, type_args.as_slice()),
                        HeapValue::Enum {
                            type_id, type_args, ..
                        } => (*type_id, type_args.as_slice()),
                        _ => return Ok(false),
                    }
                }
                Value::TypeRep(_) | Value::Function(_) | Value::Continuation(_) => {
                    return Ok(false);
                }
            };

            let Some(ifaces) = module.interface_impls.get(dyn_type_id.0 as usize) else {
                return Err(format!(
                    "invalid TypeId {} in interface_impls lookup",
                    dyn_type_id.0
                ));
            };
            let implements_iface = ifaces.binary_search(iface_id).is_ok();
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

// Hashing: deterministic (non-cryptographic) 64-bit FNV-1a, returned as `int`.
const FNV1A_OFFSET_BASIS: u64 = 0xcbf29ce484222325;
const FNV1A_PRIME: u64 = 0x100000001b3;

fn fnv1a_hash<I: IntoIterator<Item = u8>>(bytes: I) -> i64 {
    let mut hash = FNV1A_OFFSET_BASIS;
    for b in bytes {
        hash ^= b as u64;
        hash = hash.wrapping_mul(FNV1A_PRIME);
    }
    hash as i64
}

fn hash_int_fnv(v: i64) -> i64 {
    fnv1a_hash((v as u64).to_le_bytes())
}

fn eval_core_intrinsic(
    module: &ExecutableModule,
    type_reps: &mut TypeReps,
    heap: &mut ImmixHeap<HeapValue>,
    gc_allocations_since_collect: &mut usize,
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

    fn alloc_option(
        module: &ExecutableModule,
        heap: &mut ImmixHeap<HeapValue>,
        gc_allocations_since_collect: &mut usize,
        type_arg: TypeRepId,
        variant: &str,
        fields: Vec<Value>,
    ) -> Result<Value, String> {
        let Some(option_type_id) = module.type_id("Option") else {
            return Err("missing required enum type `Option`".to_string());
        };
        Ok(alloc_ref(
            heap,
            gc_allocations_since_collect,
            HeapValue::Enum {
                type_id: option_type_id,
                type_args: vec![type_arg],
                variant: variant.to_string(),
                fields,
            },
        ))
    }

    fn read_option_int(
        module: &ExecutableModule,
        heap: &ImmixHeap<HeapValue>,
        v: &Value,
    ) -> Result<Option<i64>, String> {
        let Some(option_type_id) = module.type_id("Option") else {
            return Err("missing required enum type `Option`".to_string());
        };
        let Value::Ref(r) = v else {
            return Err("expected `Option<int>` value".to_string());
        };
        let Some(obj) = heap.get(r.handle) else {
            return Err("dangling reference in `Option<int>` value".to_string());
        };
        let HeapValue::Enum {
            type_id,
            variant,
            fields,
            ..
        } = obj
        else {
            return Err("expected `Option<int>` enum value".to_string());
        };
        if *type_id != option_type_id {
            let got = module.type_name(*type_id).unwrap_or("<unknown>");
            return Err(format!("expected `Option`, got `{got}`"));
        }
        match variant.as_str() {
            "None" => Ok(None),
            "Some" => match fields.as_slice() {
                [Value::Int(n)] => Ok(Some(*n)),
                _ => Err("malformed `Option::Some(int)` value".to_string()),
            },
            other => Err(format!("invalid `Option` variant `{other}`")),
        }
    }

    match intr {
        I::StringConcat => match args.as_slice() {
            [Value::String(a), Value::String(b)] => {
                let out = {
                    let a_str = a.as_str(heap)?;
                    let b_str = b.as_str(heap)?;
                    let mut out = String::with_capacity(a_str.len().saturating_add(b_str.len()));
                    out.push_str(a_str);
                    out.push_str(b_str);
                    out
                };
                alloc_string(heap, gc_allocations_since_collect, out)
            }
            _ => Err(bad_args("core::intrinsics::string_concat")),
        },
        I::ToString => match args.as_slice() {
            [Value::TypeRep(_), Value::Unit] => {
                alloc_string(heap, gc_allocations_since_collect, "()".to_string())
            }
            [Value::TypeRep(_), Value::Bool(v)] => {
                alloc_string(heap, gc_allocations_since_collect, v.to_string())
            }
            [Value::TypeRep(_), Value::Int(v)] => {
                alloc_string(heap, gc_allocations_since_collect, v.to_string())
            }
            [Value::TypeRep(_), Value::Float(v)] => {
                alloc_string(heap, gc_allocations_since_collect, v.to_string())
            }
            [Value::TypeRep(_), Value::Byte(v)] => {
                alloc_string(heap, gc_allocations_since_collect, v.to_string())
            }
            [Value::TypeRep(_), Value::Char(v)] => {
                alloc_string(heap, gc_allocations_since_collect, v.to_string())
            }
            [Value::TypeRep(_), Value::String(v)] => Ok(Value::String(*v)),
            [Value::TypeRep(_), Value::Bytes(v)] => alloc_string(
                heap,
                gc_allocations_since_collect,
                format!("bytes(len={})", v.len_usize()),
            ),
            [Value::TypeRep(_), Value::Ref(r)] => alloc_string(
                heap,
                gc_allocations_since_collect,
                format!("{:?}", Value::Ref(r.clone())),
            ),
            [Value::TypeRep(_), Value::Function(id)] => {
                alloc_string(heap, gc_allocations_since_collect, format!("fn#{}", id.0))
            }
            [Value::TypeRep(_), Value::TypeRep(id)] => alloc_string(
                heap,
                gc_allocations_since_collect,
                format!("typerep({})", id.0),
            ),
            _ => Err(bad_args("core::intrinsics::to_string")),
        },
        I::Panic => match args.as_slice() {
            [Value::String(msg)] => Err(format!("panic: {}", msg.as_str(heap)?)),
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
        I::IntAnd => match args.as_slice() {
            [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a & b)),
            _ => Err(bad_args("core::intrinsics::int_and")),
        },
        I::IntOr => match args.as_slice() {
            [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a | b)),
            _ => Err(bad_args("core::intrinsics::int_or")),
        },
        I::IntXor => match args.as_slice() {
            [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a ^ b)),
            _ => Err(bad_args("core::intrinsics::int_xor")),
        },
        I::IntNot => match args.as_slice() {
            [Value::Int(v)] => Ok(Value::Int(!v)),
            _ => Err(bad_args("core::intrinsics::int_not")),
        },
        I::IntShl => match args.as_slice() {
            [Value::Int(a), Value::Int(sh)] => {
                let Ok(sh) = u32::try_from(*sh) else {
                    return Err("core::intrinsics::int_shl: shift amount out of range".to_string());
                };
                if sh >= 64 {
                    return Err("core::intrinsics::int_shl: shift amount out of range".to_string());
                }
                Ok(Value::Int(a.wrapping_shl(sh)))
            }
            _ => Err(bad_args("core::intrinsics::int_shl")),
        },
        I::IntShr => match args.as_slice() {
            [Value::Int(a), Value::Int(sh)] => {
                let Ok(sh) = u32::try_from(*sh) else {
                    return Err("core::intrinsics::int_shr: shift amount out of range".to_string());
                };
                if sh >= 64 {
                    return Err("core::intrinsics::int_shr: shift amount out of range".to_string());
                }
                Ok(Value::Int(a.wrapping_shr(sh)))
            }
            _ => Err(bad_args("core::intrinsics::int_shr")),
        },
        I::IntUShr => match args.as_slice() {
            [Value::Int(a), Value::Int(sh)] => {
                let Ok(sh) = u32::try_from(*sh) else {
                    return Err("core::intrinsics::int_ushr: shift amount out of range".to_string());
                };
                if sh >= 64 {
                    return Err("core::intrinsics::int_ushr: shift amount out of range".to_string());
                }
                Ok(Value::Int(((*a as u64) >> sh) as i64))
            }
            _ => Err(bad_args("core::intrinsics::int_ushr")),
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
            [Value::String(a), Value::String(b)] => {
                Ok(Value::Bool(a.as_str(heap)? == b.as_str(heap)?))
            }
            _ => Err(bad_args("core::intrinsics::string_eq")),
        },
        I::StringNe => match args.as_slice() {
            [Value::String(a), Value::String(b)] => {
                Ok(Value::Bool(a.as_str(heap)? != b.as_str(heap)?))
            }
            _ => Err(bad_args("core::intrinsics::string_ne")),
        },
        I::BytesEq => match args.as_slice() {
            [Value::Bytes(a), Value::Bytes(b)] => {
                Ok(Value::Bool(a.as_slice(heap)? == b.as_slice(heap)?))
            }
            _ => Err(bad_args("core::intrinsics::bytes_eq")),
        },
        I::BytesNe => match args.as_slice() {
            [Value::Bytes(a), Value::Bytes(b)] => {
                Ok(Value::Bool(a.as_slice(heap)? != b.as_slice(heap)?))
            }
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

        I::IntToByte => match args.as_slice() {
            [Value::Int(v)] => Ok(Value::Byte(*v as u8)),
            _ => Err(bad_args("core::intrinsics::int_to_byte")),
        },
        I::IntTryByte => match args.as_slice() {
            [Value::Int(v)] => {
                let byte_rep = type_reps.intern(TypeRepNode {
                    ctor: TypeCtor::Byte,
                    args: Vec::new(),
                });
                let out = if (0..=255).contains(v) {
                    alloc_option(
                        module,
                        heap,
                        gc_allocations_since_collect,
                        byte_rep,
                        "Some",
                        vec![Value::Byte(*v as u8)],
                    )?
                } else {
                    alloc_option(
                        module,
                        heap,
                        gc_allocations_since_collect,
                        byte_rep,
                        "None",
                        Vec::new(),
                    )?
                };
                Ok(out)
            }
            _ => Err(bad_args("core::intrinsics::int_try_byte")),
        },
        I::ByteToInt => match args.as_slice() {
            [Value::Byte(v)] => Ok(Value::Int((*v).into())),
            _ => Err(bad_args("core::intrinsics::byte_to_int")),
        },
        I::ByteAnd => match args.as_slice() {
            [Value::Byte(a), Value::Byte(b)] => Ok(Value::Byte(a & b)),
            _ => Err(bad_args("core::intrinsics::byte_and")),
        },
        I::ByteOr => match args.as_slice() {
            [Value::Byte(a), Value::Byte(b)] => Ok(Value::Byte(a | b)),
            _ => Err(bad_args("core::intrinsics::byte_or")),
        },
        I::ByteXor => match args.as_slice() {
            [Value::Byte(a), Value::Byte(b)] => Ok(Value::Byte(a ^ b)),
            _ => Err(bad_args("core::intrinsics::byte_xor")),
        },
        I::ByteNot => match args.as_slice() {
            [Value::Byte(v)] => Ok(Value::Byte(!v)),
            _ => Err(bad_args("core::intrinsics::byte_not")),
        },
        I::ByteShl => match args.as_slice() {
            [Value::Byte(a), Value::Int(sh)] => {
                let Ok(sh) = u32::try_from(*sh) else {
                    return Err("core::intrinsics::byte_shl: shift amount out of range".to_string());
                };
                if sh >= 8 {
                    return Err("core::intrinsics::byte_shl: shift amount out of range".to_string());
                }
                Ok(Value::Byte(a.wrapping_shl(sh)))
            }
            _ => Err(bad_args("core::intrinsics::byte_shl")),
        },
        I::ByteShr => match args.as_slice() {
            [Value::Byte(a), Value::Int(sh)] => {
                let Ok(sh) = u32::try_from(*sh) else {
                    return Err("core::intrinsics::byte_shr: shift amount out of range".to_string());
                };
                if sh >= 8 {
                    return Err("core::intrinsics::byte_shr: shift amount out of range".to_string());
                }
                Ok(Value::Byte(a.wrapping_shr(sh)))
            }
            _ => Err(bad_args("core::intrinsics::byte_shr")),
        },
        I::ByteUShr => match args.as_slice() {
            [Value::Byte(a), Value::Int(sh)] => {
                let Ok(sh) = u32::try_from(*sh) else {
                    return Err(
                        "core::intrinsics::byte_ushr: shift amount out of range".to_string()
                    );
                };
                if sh >= 8 {
                    return Err(
                        "core::intrinsics::byte_ushr: shift amount out of range".to_string()
                    );
                }
                Ok(Value::Byte(a.wrapping_shr(sh)))
            }
            _ => Err(bad_args("core::intrinsics::byte_ushr")),
        },

        I::IntToChar => match args.as_slice() {
            [Value::Int(v)] => {
                let Some(u) = u32::try_from(*v).ok() else {
                    return Err("core::intrinsics::int_to_char: value out of range".to_string());
                };
                if (0xD800..=0xDFFF).contains(&u) {
                    return Err("core::intrinsics::int_to_char: surrogate code point".to_string());
                }
                let Some(ch) = char::from_u32(u) else {
                    return Err("core::intrinsics::int_to_char: invalid scalar value".to_string());
                };
                Ok(Value::Char(ch))
            }
            _ => Err(bad_args("core::intrinsics::int_to_char")),
        },
        I::IntTryChar => match args.as_slice() {
            [Value::Int(v)] => {
                let char_rep = type_reps.intern(TypeRepNode {
                    ctor: TypeCtor::Char,
                    args: Vec::new(),
                });
                let out = if let Ok(u) = u32::try_from(*v)
                    && !(0xD800..=0xDFFF).contains(&u)
                    && let Some(ch) = char::from_u32(u)
                {
                    alloc_option(
                        module,
                        heap,
                        gc_allocations_since_collect,
                        char_rep,
                        "Some",
                        vec![Value::Char(ch)],
                    )?
                } else {
                    alloc_option(
                        module,
                        heap,
                        gc_allocations_since_collect,
                        char_rep,
                        "None",
                        Vec::new(),
                    )?
                };
                Ok(out)
            }
            _ => Err(bad_args("core::intrinsics::int_try_char")),
        },
        I::CharToInt => match args.as_slice() {
            [Value::Char(v)] => Ok(Value::Int(*v as u32 as i64)),
            _ => Err(bad_args("core::intrinsics::char_to_int")),
        },

        I::BytesGet => match args.as_slice() {
            [Value::Bytes(b), Value::Int(idx)] => {
                let byte_rep = type_reps.intern(TypeRepNode {
                    ctor: TypeCtor::Byte,
                    args: Vec::new(),
                });
                let Some(i) = usize::try_from(*idx).ok() else {
                    return alloc_option(
                        module,
                        heap,
                        gc_allocations_since_collect,
                        byte_rep,
                        "None",
                        Vec::new(),
                    );
                };
                let byte = {
                    let bytes = b.as_slice(heap)?;
                    bytes.get(i).copied()
                };
                match byte {
                    Some(v) => alloc_option(
                        module,
                        heap,
                        gc_allocations_since_collect,
                        byte_rep,
                        "Some",
                        vec![Value::Byte(v)],
                    ),
                    None => alloc_option(
                        module,
                        heap,
                        gc_allocations_since_collect,
                        byte_rep,
                        "None",
                        Vec::new(),
                    ),
                }
            }
            _ => Err(bad_args("core::intrinsics::bytes_get")),
        },
        I::BytesLen => match args.as_slice() {
            [Value::Bytes(b)] => {
                let len_i64: i64 = b
                    .len_usize()
                    .try_into()
                    .map_err(|_| "core::intrinsics::bytes_len: len overflow".to_string())?;
                Ok(Value::Int(len_i64))
            }
            _ => Err(bad_args("core::intrinsics::bytes_len")),
        },
        I::BytesSlice => match args.as_slice() {
            [Value::Bytes(b), Value::Int(from), to] => {
                let len_i64: i64 = b
                    .len_usize()
                    .try_into()
                    .map_err(|_| "core::intrinsics::bytes_slice: len overflow".to_string())?;
                let to = read_option_int(module, heap, to)?.unwrap_or(len_i64);

                if *from < 0 {
                    return Err("core::intrinsics::bytes_slice: from must be >= 0".to_string());
                }
                if to < 0 {
                    return Err("core::intrinsics::bytes_slice: to must be >= 0".to_string());
                }
                if *from > to {
                    return Err("core::intrinsics::bytes_slice: from > to".to_string());
                }
                if to > len_i64 {
                    return Err("core::intrinsics::bytes_slice: to out of bounds".to_string());
                }

                let from_u32: u32 = (*from)
                    .try_into()
                    .map_err(|_| "core::intrinsics::bytes_slice: from overflow".to_string())?;
                let to_u32: u32 = to
                    .try_into()
                    .map_err(|_| "core::intrinsics::bytes_slice: to overflow".to_string())?;
                let len_u32 = to_u32
                    .checked_sub(from_u32)
                    .ok_or_else(|| "core::intrinsics::bytes_slice: from > to".to_string())?;
                let start = b.start.checked_add(from_u32).ok_or_else(|| {
                    "core::intrinsics::bytes_slice: bytes view index overflow".to_string()
                })?;
                Ok(Value::Bytes(BytesView {
                    buf: b.buf,
                    start,
                    len: len_u32,
                }))
            }
            _ => Err(bad_args("core::intrinsics::bytes_slice")),
        },
        I::BytesToArray => match args.as_slice() {
            [Value::Bytes(b)] => {
                let items = {
                    let bytes = b.as_slice(heap)?;
                    bytes.iter().copied().map(Value::Byte).collect()
                };
                Ok(alloc_ref(
                    heap,
                    gc_allocations_since_collect,
                    HeapValue::Array(items),
                ))
            }
            _ => Err(bad_args("core::intrinsics::bytes_to_array")),
        },
        I::BytesFromArray => match args.as_slice() {
            [Value::Ref(arr)] => {
                let Some(obj) = heap.get(arr.handle) else {
                    return Err(
                        "core::intrinsics::bytes_from_array: dangling reference".to_string()
                    );
                };
                let HeapValue::Array(items) = obj else {
                    return Err("core::intrinsics::bytes_from_array: expected an array".to_string());
                };
                let out = {
                    let mut out = Vec::with_capacity(items.len());
                    for item in items {
                        let Value::Byte(b) = item else {
                            return Err(
                                "core::intrinsics::bytes_from_array: expected `[byte]`".to_string()
                            );
                        };
                        out.push(*b);
                    }
                    out
                };
                alloc_bytes(heap, gc_allocations_since_collect, out)
            }
            _ => Err(bad_args("core::intrinsics::bytes_from_array")),
        },

        I::StringSlice => match args.as_slice() {
            [Value::String(s), Value::Int(from), to] => {
                let len_i64: i64 = s
                    .len_usize()
                    .try_into()
                    .map_err(|_| "core::intrinsics::string_slice: len overflow".to_string())?;
                let to = read_option_int(module, heap, to)?.unwrap_or(len_i64);
                let s_str = s.as_str(heap)?;

                if *from < 0 {
                    return Err("core::intrinsics::string_slice: from must be >= 0".to_string());
                }
                if to < 0 {
                    return Err("core::intrinsics::string_slice: to must be >= 0".to_string());
                }
                if *from > to {
                    return Err("core::intrinsics::string_slice: from > to".to_string());
                }
                if to > len_i64 {
                    return Err("core::intrinsics::string_slice: to out of bounds".to_string());
                }

                let from_usize: usize = (*from)
                    .try_into()
                    .map_err(|_| "core::intrinsics::string_slice: from overflow".to_string())?;
                let to_usize: usize = to
                    .try_into()
                    .map_err(|_| "core::intrinsics::string_slice: to overflow".to_string())?;
                if !s_str.is_char_boundary(from_usize) || !s_str.is_char_boundary(to_usize) {
                    return Err(
                        "core::intrinsics::string_slice: invalid UTF-8 boundary".to_string()
                    );
                }
                let from_u32: u32 = (*from)
                    .try_into()
                    .map_err(|_| "core::intrinsics::string_slice: from overflow".to_string())?;
                let to_u32: u32 = to
                    .try_into()
                    .map_err(|_| "core::intrinsics::string_slice: to overflow".to_string())?;
                let len_u32 = to_u32
                    .checked_sub(from_u32)
                    .ok_or_else(|| "core::intrinsics::string_slice: from > to".to_string())?;
                let start = s.start.checked_add(from_u32).ok_or_else(|| {
                    "core::intrinsics::string_slice: string view index overflow".to_string()
                })?;
                Ok(Value::String(StringView {
                    buf: s.buf,
                    start,
                    len: len_u32,
                }))
            }
            _ => Err(bad_args("core::intrinsics::string_slice")),
        },
        I::StringNextIndex => match args.as_slice() {
            [Value::String(s), Value::Int(idx)] => {
                if *idx < 0 {
                    return Err("core::intrinsics::string_next_index: idx must be >= 0".to_string());
                }
                let idx_usize: usize = (*idx)
                    .try_into()
                    .map_err(|_| "core::intrinsics::string_next_index: idx overflow".to_string())?;

                let s_str = s.as_str(heap)?;
                if idx_usize > s_str.len() {
                    return Err(
                        "core::intrinsics::string_next_index: idx out of bounds".to_string()
                    );
                }
                if idx_usize == s_str.len() {
                    return Ok(Value::Int(-1));
                }
                if !s_str.is_char_boundary(idx_usize) {
                    return Err(
                        "core::intrinsics::string_next_index: invalid UTF-8 boundary".to_string(),
                    );
                }
                let ch = s_str[idx_usize..].chars().next().ok_or_else(|| {
                    "core::intrinsics::string_next_index: invalid UTF-8".to_string()
                })?;
                let next_idx = idx_usize.checked_add(ch.len_utf8()).ok_or_else(|| {
                    "core::intrinsics::string_next_index: index overflow".to_string()
                })?;
                Ok(Value::Int(next_idx as i64))
            }
            _ => Err(bad_args("core::intrinsics::string_next_index")),
        },
        I::StringCodepointAt => match args.as_slice() {
            [Value::String(s), Value::Int(idx)] => {
                if *idx < 0 {
                    return Err(
                        "core::intrinsics::string_codepoint_at: idx must be >= 0".to_string()
                    );
                }
                let idx_usize: usize = (*idx).try_into().map_err(|_| {
                    "core::intrinsics::string_codepoint_at: idx overflow".to_string()
                })?;

                let s_str = s.as_str(heap)?;
                if idx_usize >= s_str.len() {
                    return Err(
                        "core::intrinsics::string_codepoint_at: idx out of bounds".to_string()
                    );
                }
                if !s_str.is_char_boundary(idx_usize) {
                    return Err(
                        "core::intrinsics::string_codepoint_at: invalid UTF-8 boundary".to_string(),
                    );
                }
                let ch = s_str[idx_usize..].chars().next().ok_or_else(|| {
                    "core::intrinsics::string_codepoint_at: invalid UTF-8".to_string()
                })?;
                Ok(Value::Int(ch as u32 as i64))
            }
            _ => Err(bad_args("core::intrinsics::string_codepoint_at")),
        },
        I::StringFromChars => match args.as_slice() {
            [Value::Ref(arr)] => {
                let out = {
                    let Some(obj) = heap.get(arr.handle) else {
                        return Err(
                            "core::intrinsics::string_from_chars: dangling reference".to_string()
                        );
                    };
                    let HeapValue::Array(items) = obj else {
                        return Err(
                            "core::intrinsics::string_from_chars: expected an array".to_string()
                        );
                    };
                    let mut out = String::new();
                    for item in items {
                        let Value::Char(c) = item else {
                            return Err("core::intrinsics::string_from_chars: expected `[char]`"
                                .to_string());
                        };
                        out.push(*c);
                    }
                    out
                };
                alloc_string(heap, gc_allocations_since_collect, out)
            }
            _ => Err(bad_args("core::intrinsics::string_from_chars")),
        },
        I::StringFromUtf8 => match args.as_slice() {
            [Value::Bytes(b)] => {
                let out = {
                    let bytes = b.as_slice(heap)?;
                    String::from_utf8_lossy(bytes).into_owned()
                };
                alloc_string(heap, gc_allocations_since_collect, out)
            }
            _ => Err(bad_args("core::intrinsics::string_from_utf8")),
        },
        I::StringFromUtf8Strict => match args.as_slice() {
            [Value::Bytes(b)] => {
                let string_rep = type_reps.intern(TypeRepNode {
                    ctor: TypeCtor::String,
                    args: Vec::new(),
                });
                let bytes = b.as_slice(heap)?;
                match String::from_utf8(bytes.to_vec()) {
                    Ok(s) => {
                        let s = alloc_string(heap, gc_allocations_since_collect, s)?;
                        alloc_option(
                            module,
                            heap,
                            gc_allocations_since_collect,
                            string_rep,
                            "Some",
                            vec![s],
                        )
                    }
                    Err(_) => alloc_option(
                        module,
                        heap,
                        gc_allocations_since_collect,
                        string_rep,
                        "None",
                        Vec::new(),
                    ),
                }
            }
            _ => Err(bad_args("core::intrinsics::string_from_utf8_strict")),
        },
        I::StringFromUtf16Le | I::StringFromUtf16Be => match args.as_slice() {
            [Value::Ref(arr)] => {
                let out =
                    {
                        let Some(obj) = heap.get(arr.handle) else {
                            return Err("core::intrinsics::string_from_utf16: dangling reference"
                                .to_string());
                        };
                        let HeapValue::Array(items) = obj else {
                            return Err("core::intrinsics::string_from_utf16: expected an array"
                                .to_string());
                        };
                        let mut units = Vec::with_capacity(items.len());
                        for item in items {
                            let Value::Int(n) = item else {
                                // lossy: invalid code unit
                                units.push(0xFFFD);
                                continue;
                            };
                            if *n < 0 || *n > 0xFFFF {
                                units.push(0xFFFD);
                                continue;
                            }
                            units.push(*n as u16);
                        }

                        let mut out = String::new();
                        for r in char::decode_utf16(units.into_iter()) {
                            match r {
                                Ok(c) => out.push(c),
                                Err(_) => out.push('\u{FFFD}'),
                            }
                        }
                        out
                    };
                alloc_string(heap, gc_allocations_since_collect, out)
            }
            _ => Err(bad_args("core::intrinsics::string_from_utf16")),
        },
        I::StringFromUtf16LeStrict | I::StringFromUtf16BeStrict => match args.as_slice() {
            [Value::Ref(arr)] => {
                let string_rep = type_reps.intern(TypeRepNode {
                    ctor: TypeCtor::String,
                    args: Vec::new(),
                });
                let maybe = {
                    let Some(obj) = heap.get(arr.handle) else {
                        return Err(
                            "core::intrinsics::string_from_utf16_strict: dangling reference"
                                .to_string(),
                        );
                    };
                    let HeapValue::Array(items) = obj else {
                        return Err(
                            "core::intrinsics::string_from_utf16_strict: expected an array"
                                .to_string(),
                        );
                    };
                    let mut units = Vec::with_capacity(items.len());
                    for item in items {
                        let Value::Int(n) = item else {
                            return alloc_option(
                                module,
                                heap,
                                gc_allocations_since_collect,
                                string_rep,
                                "None",
                                Vec::new(),
                            );
                        };
                        if *n < 0 || *n > 0xFFFF {
                            return alloc_option(
                                module,
                                heap,
                                gc_allocations_since_collect,
                                string_rep,
                                "None",
                                Vec::new(),
                            );
                        }
                        units.push(*n as u16);
                    }

                    let mut out = String::new();
                    for r in char::decode_utf16(units.into_iter()) {
                        match r {
                            Ok(c) => out.push(c),
                            Err(_) => {
                                return alloc_option(
                                    module,
                                    heap,
                                    gc_allocations_since_collect,
                                    string_rep,
                                    "None",
                                    Vec::new(),
                                );
                            }
                        }
                    }
                    Some(out)
                };
                match maybe {
                    Some(out) => {
                        let s = alloc_string(heap, gc_allocations_since_collect, out)?;
                        alloc_option(
                            module,
                            heap,
                            gc_allocations_since_collect,
                            string_rep,
                            "Some",
                            vec![s],
                        )
                    }
                    None => alloc_option(
                        module,
                        heap,
                        gc_allocations_since_collect,
                        string_rep,
                        "None",
                        Vec::new(),
                    ),
                }
            }
            _ => Err(bad_args("core::intrinsics::string_from_utf16_strict")),
        },

        // Hashing: deterministic (non-cryptographic) 64-bit FNV-1a, returned as `int`.
        I::HashInt => match args.as_slice() {
            [Value::Int(v)] => {
                let mut hash: u64 = 0xcbf29ce484222325;
                for b in (*v as u64).to_le_bytes() {
                    hash ^= b as u64;
                    hash = hash.wrapping_mul(0x100000001b3);
                }
                Ok(Value::Int(hash as i64))
            }
            _ => Err(bad_args("core::intrinsics::hash_int")),
        },
        I::HashString => match args.as_slice() {
            [Value::String(s)] => {
                let mut hash: u64 = 0xcbf29ce484222325;
                for &b in s.as_str(heap)?.as_bytes() {
                    hash ^= b as u64;
                    hash = hash.wrapping_mul(0x100000001b3);
                }
                Ok(Value::Int(hash as i64))
            }
            _ => Err(bad_args("core::intrinsics::hash_string")),
        },
        I::HashBytes => match args.as_slice() {
            [Value::Bytes(b)] => {
                let mut hash: u64 = 0xcbf29ce484222325;
                for &byte in b.as_slice(heap)? {
                    hash ^= byte as u64;
                    hash = hash.wrapping_mul(0x100000001b3);
                }
                Ok(Value::Int(hash as i64))
            }
            _ => Err(bad_args("core::intrinsics::hash_bytes")),
        },
        I::HashCombine => match args.as_slice() {
            [Value::Int(a), Value::Int(b)] => {
                let mut hash: u64 = 0xcbf29ce484222325;
                for byte in (*a as u64).to_le_bytes() {
                    hash ^= byte as u64;
                    hash = hash.wrapping_mul(0x100000001b3);
                }
                for byte in (*b as u64).to_le_bytes() {
                    hash ^= byte as u64;
                    hash = hash.wrapping_mul(0x100000001b3);
                }
                Ok(Value::Int(hash as i64))
            }
            _ => Err(bad_args("core::intrinsics::hash_combine")),
        },

        I::ArrayLen => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(arr)] => {
                let Some(obj) = heap.get(arr.handle) else {
                    return Err("core::intrinsics::array_len: dangling reference".to_string());
                };
                match obj {
                    HeapValue::Array(items) => Ok(Value::Int(items.len() as i64)),
                    _ => Err("core::intrinsics::array_len: expected an array".to_string()),
                }
            }
            _ => Err(bad_args("core::intrinsics::array_len")),
        },
        I::ArrayLenRo => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(arr)] => {
                let Some(obj) = heap.get(arr.handle) else {
                    return Err("core::intrinsics::array_len_ro: dangling reference".to_string());
                };
                match obj {
                    HeapValue::Array(items) => Ok(Value::Int(items.len() as i64)),
                    _ => Err("core::intrinsics::array_len_ro: expected an array".to_string()),
                }
            }
            _ => Err(bad_args("core::intrinsics::array_len_ro")),
        },
        I::ArrayPush => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(arr), value] => {
                if arr.is_readonly() {
                    return Err("illegal write through readonly reference".to_string());
                }
                let Some(obj) = heap.get_mut(arr.handle) else {
                    return Err("core::intrinsics::array_push: dangling reference".to_string());
                };
                let HeapValue::Array(items) = obj else {
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
                let popped = {
                    let Some(obj) = heap.get_mut(arr.handle) else {
                        return Err("core::intrinsics::array_pop: dangling reference".to_string());
                    };
                    let HeapValue::Array(items) = obj else {
                        return Err("core::intrinsics::array_pop: expected an array".to_string());
                    };
                    items.pop()
                };
                match popped {
                    Some(v) => alloc_option(
                        module,
                        heap,
                        gc_allocations_since_collect,
                        *elem_rep,
                        "Some",
                        vec![v],
                    ),
                    None => alloc_option(
                        module,
                        heap,
                        gc_allocations_since_collect,
                        *elem_rep,
                        "None",
                        Vec::new(),
                    ),
                }
            }
            _ => Err(bad_args("core::intrinsics::array_pop")),
        },
        I::ArrayClear => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(arr)] => {
                if arr.is_readonly() {
                    return Err("illegal write through readonly reference".to_string());
                }
                let Some(obj) = heap.get_mut(arr.handle) else {
                    return Err("core::intrinsics::array_clear: dangling reference".to_string());
                };
                let HeapValue::Array(items) = obj else {
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

                let Some(obj) = heap.get_mut(arr.handle) else {
                    return Err("core::intrinsics::array_resize: dangling reference".to_string());
                };
                let HeapValue::Array(items) = obj else {
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
                let Some(obj) = heap.get_mut(arr.handle) else {
                    return Err("core::intrinsics::array_insert: dangling reference".to_string());
                };
                let HeapValue::Array(items) = obj else {
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
                let Some(obj) = heap.get_mut(arr.handle) else {
                    return Err("core::intrinsics::array_remove: dangling reference".to_string());
                };
                let HeapValue::Array(items) = obj else {
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
                let other_items = match heap.get(other.handle) {
                    Some(HeapValue::Array(items)) => items.clone(),
                    Some(_) => {
                        return Err(
                            "core::intrinsics::array_extend: expected an array for `other`"
                                .to_string(),
                        );
                    }
                    None => {
                        return Err(
                            "core::intrinsics::array_extend: dangling reference for `other`"
                                .to_string(),
                        );
                    }
                };
                let Some(obj) = heap.get_mut(arr.handle) else {
                    return Err(
                        "core::intrinsics::array_extend: dangling reference for `arr`".to_string(),
                    );
                };
                let HeapValue::Array(items) = obj else {
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
                let a_items = match heap.get(a.handle) {
                    Some(HeapValue::Array(items)) => items.clone(),
                    Some(_) => {
                        return Err(
                            "core::intrinsics::array_concat: expected an array for `a`".to_string()
                        );
                    }
                    None => {
                        return Err("core::intrinsics::array_concat: dangling reference for `a`"
                            .to_string());
                    }
                };
                let b_items = match heap.get(b.handle) {
                    Some(HeapValue::Array(items)) => items.clone(),
                    Some(_) => {
                        return Err(
                            "core::intrinsics::array_concat: expected an array for `b`".to_string()
                        );
                    }
                    None => {
                        return Err("core::intrinsics::array_concat: dangling reference for `b`"
                            .to_string());
                    }
                };
                let mut items = Vec::with_capacity(a_items.len() + b_items.len());
                items.extend(a_items);
                items.extend(b_items);
                Ok(alloc_ref(
                    heap,
                    gc_allocations_since_collect,
                    HeapValue::Array(items),
                ))
            }
            _ => Err(bad_args("core::intrinsics::array_concat")),
        },
        I::ArrayConcatRo => match args.as_slice() {
            [Value::TypeRep(_), Value::Ref(a), Value::Ref(b)] => {
                let a_items = match heap.get(a.handle) {
                    Some(HeapValue::Array(items)) => items.clone(),
                    Some(_) => {
                        return Err(
                            "core::intrinsics::array_concat_ro: expected an array for `a`"
                                .to_string(),
                        );
                    }
                    None => {
                        return Err(
                            "core::intrinsics::array_concat_ro: dangling reference for `a`"
                                .to_string(),
                        );
                    }
                };
                let b_items = match heap.get(b.handle) {
                    Some(HeapValue::Array(items)) => items.clone(),
                    Some(_) => {
                        return Err(
                            "core::intrinsics::array_concat_ro: expected an array for `b`"
                                .to_string(),
                        );
                    }
                    None => {
                        return Err(
                            "core::intrinsics::array_concat_ro: dangling reference for `b`"
                                .to_string(),
                        );
                    }
                };
                let mut items = Vec::with_capacity(a_items.len() + b_items.len());
                items.extend(a_items.into_iter().map(Value::into_readonly_view));
                items.extend(b_items.into_iter().map(Value::into_readonly_view));
                Ok(alloc_ref(
                    heap,
                    gc_allocations_since_collect,
                    HeapValue::Array(items),
                ))
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
                let slice_items = {
                    let Some(obj) = heap.get(arr.handle) else {
                        return Err("core::intrinsics::array_slice: dangling reference".to_string());
                    };
                    let HeapValue::Array(items) = obj else {
                        return Err("core::intrinsics::array_slice: expected an array".to_string());
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
                            "core::intrinsics::array_slice: start must be <= end".to_string()
                        );
                    }
                    if end_usize > len {
                        return Err(format!("index out of bounds: index={end}, len={len}"));
                    }
                    items[start_usize..end_usize].to_vec()
                };
                Ok(alloc_ref(
                    heap,
                    gc_allocations_since_collect,
                    HeapValue::Array(slice_items),
                ))
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
                let slice_items = {
                    let Some(obj) = heap.get(arr.handle) else {
                        return Err(
                            "core::intrinsics::array_slice_ro: dangling reference".to_string()
                        );
                    };
                    let HeapValue::Array(items) = obj else {
                        return Err(
                            "core::intrinsics::array_slice_ro: expected an array".to_string()
                        );
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
                    items[start_usize..end_usize]
                        .iter()
                        .cloned()
                        .map(Value::into_readonly_view)
                        .collect::<Vec<_>>()
                };
                Ok(alloc_ref(
                    heap,
                    gc_allocations_since_collect,
                    HeapValue::Array(slice_items),
                ))
            }
            _ => Err(bad_args("core::intrinsics::array_slice_ro")),
        },
    }
}

fn match_pattern(
    module: &ExecutableModule,
    heap: &mut ImmixHeap<HeapValue>,
    gc_allocations_since_collect: &mut usize,
    type_reps: &TypeReps,
    pat: &rusk_bytecode::Pattern,
    value: &Value,
    binds: &mut Vec<Value>,
) -> Result<bool, String> {
    use rusk_bytecode::{ConstValue, Pattern};

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
            (ConstValue::String(a), Value::String(b)) => a == b.as_str(heap)?,
            (ConstValue::Bytes(a), Value::Bytes(b)) => a.as_slice() == b.as_slice(heap)?,
            (ConstValue::TypeRep(lit), Value::TypeRep(id)) => {
                match (type_reps.node(*id), TypeReps::ctor_from_lit(module, lit)) {
                    (Some(n), Ok(ctor)) => n.ctor == ctor && n.args.is_empty(),
                    _ => false,
                }
            }
            (ConstValue::Function(a), Value::Function(b)) => a == b,
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
            let Some(obj) = heap.get(r.handle) else {
                return Err("dangling reference in pattern match".to_string());
            };
            let Some(pat_enum_id) = module.type_id(enum_name.as_str()) else {
                return Ok(false);
            };
            let (actual_enum_id, v, actual_fields) = match obj {
                HeapValue::Enum {
                    type_id,
                    variant,
                    fields,
                    ..
                } => (*type_id, variant.clone(), fields.clone()),
                _ => return Ok(false),
            };
            if actual_enum_id != pat_enum_id || v != *variant || actual_fields.len() != fields.len()
            {
                return Ok(false);
            }
            for (p, actual) in fields.iter().zip(actual_fields.iter()) {
                let actual = if r.readonly {
                    actual.clone().into_readonly_view()
                } else {
                    actual.clone()
                };
                if !match_pattern(
                    module,
                    heap,
                    gc_allocations_since_collect,
                    type_reps,
                    p,
                    &actual,
                    binds,
                )? {
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
            let Some(obj) = heap.get(r.handle) else {
                return Err("dangling reference in pattern match".to_string());
            };
            let actual_items = match obj {
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
                if !match_pattern(
                    module,
                    heap,
                    gc_allocations_since_collect,
                    type_reps,
                    p,
                    &actual,
                    binds,
                )? {
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
                            alloc_ref(heap, gc_allocations_since_collect, HeapValue::Tuple(slice))
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
                if !match_pattern(
                    module,
                    heap,
                    gc_allocations_since_collect,
                    type_reps,
                    p,
                    &actual,
                    binds,
                )? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        Pattern::Struct { type_name, fields } => {
            let Value::Ref(r) = value else {
                return Ok(false);
            };
            let Some(obj) = heap.get(r.handle) else {
                return Err("dangling reference in pattern match".to_string());
            };
            let Some(pat_type_id) = module.type_id(type_name.as_str()) else {
                return Ok(false);
            };
            let (actual_type_id, actual_fields) = match obj {
                HeapValue::Struct {
                    type_id, fields, ..
                } => (*type_id, fields.clone()),
                _ => return Ok(false),
            };
            if actual_type_id != pat_type_id {
                return Ok(false);
            }
            for (field_name, field_pat) in fields.iter() {
                let idx = struct_field_index(module, actual_type_id, field_name.as_str())?;
                let Some(field_value) = actual_fields.get(idx) else {
                    return Err(format!("missing field: {field_name}"));
                };
                let field_value = if r.readonly {
                    field_value.clone().into_readonly_view()
                } else {
                    field_value.clone()
                };
                if !match_pattern(
                    module,
                    heap,
                    gc_allocations_since_collect,
                    type_reps,
                    field_pat,
                    &field_value,
                    binds,
                )? {
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
            let Some(obj) = heap.get(r.handle) else {
                return Err("dangling reference in pattern match".to_string());
            };
            let actual_items = match obj {
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
                if !match_pattern(
                    module,
                    heap,
                    gc_allocations_since_collect,
                    type_reps,
                    p,
                    &actual,
                    binds,
                )? {
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
                        binds.push(alloc_ref(
                            heap,
                            gc_allocations_since_collect,
                            HeapValue::Array(slice),
                        ));
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
                if !match_pattern(
                    module,
                    heap,
                    gc_allocations_since_collect,
                    type_reps,
                    p,
                    &actual,
                    binds,
                )? {
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

fn read_byte(frame: &Frame, reg: rusk_bytecode::Reg) -> Result<u8, String> {
    let idx: usize = reg.try_into().unwrap_or(usize::MAX);
    let Some(v) = frame.regs.get(idx).and_then(|v| v.as_ref()) else {
        return Err(format!("read from uninitialized reg {reg}"));
    };
    match v {
        Value::Byte(n) => Ok(*n),
        other => Err(format!("expected byte, got {other:?}")),
    }
}

fn read_shift_amount(frame: &Frame, reg: rusk_bytecode::Reg, width: u32) -> Result<u32, String> {
    let shift = read_int(frame, reg)?;
    let Ok(shift) = u32::try_from(shift) else {
        return Err(format!("shift amount out of range: {shift}"));
    };
    if shift >= width {
        return Err(format!("shift amount out of range: {shift}"));
    }
    Ok(shift)
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

#[allow(clippy::too_many_arguments)]
fn call_host_import(
    module: &ExecutableModule,
    heap: &mut ImmixHeap<HeapValue>,
    gc_allocations_since_collect: &mut usize,
    pinned_continuations: &mut PinnedContinuations,
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
        let Some(abi) = v
            .try_to_abi(heap, pinned_continuations)
            .map_err(|msg| format!("host call `{}` arg conversion: {msg}", import.name))?
        else {
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
        let ret = Value::from_abi(
            heap,
            gc_allocations_since_collect,
            pinned_continuations,
            &abi_ret,
        )
        .map_err(|msg| format!("host call `{}` return conversion: {msg}", import.name))?;
        write_value(frame, dst, ret)
            .map_err(|msg| format!("host call `{}` dst: {msg}", import.name))?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rusk_bytecode::{
        AbiType, CallTarget, ExecutableModule, Function, HostFnSig, HostImport, Instruction,
    };
    use rusk_gc::GcHeap;

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
            other => Err(crate::HostError {
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
        let s_type_id = module.intern_type("S".to_string()).unwrap();
        module
            .set_struct_layout(s_type_id, vec!["x".to_string()])
            .unwrap();
        let foo_method = module.intern_method("foo".to_string()).unwrap();

        let foo = module
            .add_function(Function {
                name: "S::foo".to_string(),
                reg_count: 6,
                param_count: 3,
                code: vec![
                    Instruction::MakeTypeRep {
                        dst: 3,
                        base: rusk_bytecode::TypeRepLit::Array,
                        args: vec![0],
                    },
                    Instruction::MakeTypeRep {
                        dst: 4,
                        base: rusk_bytecode::TypeRepLit::Array,
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
                        value: rusk_bytecode::ConstValue::TypeRep(rusk_bytecode::TypeRepLit::Int),
                    },
                    Instruction::Const {
                        dst: 1,
                        value: rusk_bytecode::ConstValue::TypeRep(rusk_bytecode::TypeRepLit::Bool),
                    },
                    Instruction::Const {
                        dst: 2,
                        value: rusk_bytecode::ConstValue::Int(42),
                    },
                    Instruction::MakeStruct {
                        dst: 3,
                        type_id: s_type_id,
                        type_args: vec![0],
                        fields: vec![("x".to_string(), 2)],
                    },
                    Instruction::VCall {
                        dst: Some(4),
                        obj: 3,
                        method: foo_method,
                        method_type_args: vec![1],
                        args: vec![],
                    },
                    Instruction::Return { value: 4 },
                ],
            })
            .unwrap();

        module.entry = main;
        module.add_vcall_entry(s_type_id, foo_method, foo).unwrap();

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
                            arg_patterns: vec![rusk_bytecode::Pattern::Bind],
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

    #[test]
    fn gc_collects_unreachable_objects() {
        let mut module = ExecutableModule::default();
        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 1,
                param_count: 0,
                code: vec![],
            })
            .unwrap();
        module.entry = main;

        let mut vm = Vm::new(module).unwrap();

        let root = alloc_ref(
            &mut vm.heap,
            &mut vm.gc_allocations_since_collect,
            HeapValue::Array(vec![Value::Int(1), Value::Int(2)]),
        );
        vm.frames[0].regs[0] = Some(root);

        alloc_ref(
            &mut vm.heap,
            &mut vm.gc_allocations_since_collect,
            HeapValue::Array(vec![Value::Int(9)]),
        );
        assert_eq!(vm.heap_live_objects(), 2);

        vm.collect_garbage_now();
        assert_eq!(vm.heap_live_objects(), 1);

        vm.frames[0].regs[0] = None;
        vm.collect_garbage_now();
        assert_eq!(vm.heap_live_objects(), 0);
    }

    #[test]
    fn gc_prevents_stale_handle_use_after_reuse() {
        let mut module = ExecutableModule::default();
        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 2,
                param_count: 0,
                code: vec![Instruction::Len { dst: 1, arr: 0 }],
            })
            .unwrap();
        module.entry = main;

        let mut vm = Vm::new(module).unwrap();

        let stale = alloc_ref(
            &mut vm.heap,
            &mut vm.gc_allocations_since_collect,
            HeapValue::Array(vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
        );
        let Value::Ref(stale_ref) = &stale else {
            panic!("expected ref");
        };
        let old_handle = stale_ref.handle;
        vm.frames[0].regs[0] = Some(stale.clone());

        vm.collect_garbage_now();
        assert_eq!(vm.heap_live_objects(), 1);

        vm.frames[0].regs[0] = None;
        vm.collect_garbage_now();
        assert_eq!(vm.heap_live_objects(), 0);

        let new = alloc_ref(
            &mut vm.heap,
            &mut vm.gc_allocations_since_collect,
            HeapValue::Array(vec![Value::Int(0)]),
        );
        let Value::Ref(new_ref) = &new else {
            panic!("expected ref");
        };
        assert_eq!(new_ref.handle.index, old_handle.index);
        assert_ne!(new_ref.handle.generation, old_handle.generation);
        assert!(vm.heap.get(old_handle).is_none());

        vm.frames[0].regs[0] = Some(stale);
        let got = vm_step(&mut vm, None);
        let StepResult::Trap { message } = got else {
            panic!("expected trap, got {got:?}");
        };
        assert!(message.contains("dangling"), "{message}");
    }

    #[test]
    fn readonly_view_propagates_through_tuple_get() {
        let mut module = ExecutableModule::default();
        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 2,
                param_count: 0,
                code: vec![Instruction::TupleGet {
                    dst: 1,
                    tup: 0,
                    idx: 0,
                }],
            })
            .unwrap();
        module.entry = main;

        let mut vm = Vm::new(module).unwrap();

        let arr = alloc_ref(
            &mut vm.heap,
            &mut vm.gc_allocations_since_collect,
            HeapValue::Array(vec![Value::Int(7)]),
        );
        let Value::Ref(arr_ref) = &arr else {
            panic!("expected array ref");
        };
        let arr_handle = arr_ref.handle;

        let tup = alloc_ref(
            &mut vm.heap,
            &mut vm.gc_allocations_since_collect,
            HeapValue::Tuple(vec![arr]),
        );
        vm.frames[0].regs[0] = Some(tup.into_readonly_view());

        let got = vm_step(&mut vm, Some(1));
        assert_eq!(got, StepResult::Yield { remaining_fuel: 0 });

        let frame = vm.frames.last().unwrap();
        let got = frame.regs[1].as_ref().unwrap();
        let Value::Ref(got_ref) = got else {
            panic!("expected ref, got {got:?}");
        };
        assert!(got_ref.is_readonly());
        assert_eq!(got_ref.handle, arr_handle);
    }

    #[test]
    fn readonly_write_traps() {
        let mut module = ExecutableModule::default();
        let main = module
            .add_function(Function {
                name: "main".to_string(),
                reg_count: 3,
                param_count: 0,
                code: vec![Instruction::IndexSet {
                    arr: 0,
                    idx: 1,
                    value: 2,
                }],
            })
            .unwrap();
        module.entry = main;

        let mut vm = Vm::new(module).unwrap();

        let arr = alloc_ref(
            &mut vm.heap,
            &mut vm.gc_allocations_since_collect,
            HeapValue::Array(vec![Value::Int(1)]),
        );
        vm.frames[0].regs[0] = Some(arr.into_readonly_view());
        vm.frames[0].regs[1] = Some(Value::Int(0));
        vm.frames[0].regs[2] = Some(Value::Int(99));

        let got = vm_step(&mut vm, None);
        let StepResult::Trap { message } = got else {
            panic!("expected trap, got {got:?}");
        };
        assert!(message.contains("readonly"), "{message}");
    }
}
