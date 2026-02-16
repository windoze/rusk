extern crate alloc;

pub mod hashmap {
    #[cfg(feature = "std")]
    pub use std::collections::HashMap;

    #[cfg(all(not(feature = "std"), feature = "hashbrown"))]
    pub use hashbrown::HashMap;

    #[cfg(not(any(feature = "std", feature = "hashbrown")))]
    compile_error!(
        "You must enable either the 'std' feature or the 'hashbrown' feature for rusk-interpreter."
    );
}

use hashmap::HashMap;

use alloc::collections::BTreeMap;
use alloc::fmt;
use alloc::format;
use alloc::rc::Rc;
use alloc::string::{String, ToString};
use alloc::vec;
use alloc::vec::Vec;
use core::{cell::RefCell, mem};
#[cfg(feature = "std")]
use std::time::Instant;

use rusk_gc::{GcHeap, GcRef, Trace, Tracer};
use rusk_mir::{
    BasicBlock, BlockId, CallTarget, ConstValue, EffectSpec, Function, FunctionId, HostImportId,
    Instruction, Local, Module, Mutability, Operand, Pattern, SwitchCase, Terminator, TypeRepLit,
};

pub type MarkSweepHeap = rusk_gc::MarkSweepHeap<HeapValue>;

/// An internal runtime type representation identifier.
///
/// This is intentionally a small, comparable token. The actual structure of a `TypeRep` is kept
/// inside the interpreter.
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

/// A runtime value produced by the MIR interpreter.
#[derive(Clone)]
pub enum Value {
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
    fn kind(&self) -> ValueKind {
        match self {
            Value::Unit => ValueKind::Unit,
            Value::Bool(_) => ValueKind::Bool,
            Value::Int(_) => ValueKind::Int,
            Value::Float(_) => ValueKind::Float,
            Value::String(_) => ValueKind::String,
            Value::Bytes(_) => ValueKind::Bytes,
            Value::TypeRep(_) => ValueKind::TypeRep,
            Value::Ref(_) => ValueKind::Ref,
            Value::Function(_) => ValueKind::Function,
            Value::Continuation(_) => ValueKind::Continuation,
        }
    }

    pub fn into_readonly_view(self) -> Self {
        match self {
            Value::Ref(r) => Value::Ref(r.as_readonly()),
            other => other,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "unit"),
            Value::Bool(v) => write!(f, "{v}"),
            Value::Int(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::String(v) => write!(f, "{v:?}"),
            Value::Bytes(v) => write!(f, "bytes(len={})", v.len()),
            Value::TypeRep(id) => write!(f, "typerep({})", id.0),
            Value::Ref(r) => {
                let ro = if r.readonly { ", readonly" } else { "" };
                write!(
                    f,
                    "ref(index={}, gen={}{})",
                    r.handle.index, r.handle.generation, ro
                )
            }
            Value::Function(id) => write!(f, "fn#{}", id.0),
            Value::Continuation(_) => write!(f, "continuation(..)"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Unit, Value::Unit) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bytes(a), Value::Bytes(b)) => a == b,
            (Value::TypeRep(a), Value::TypeRep(b)) => a == b,
            (Value::Function(a), Value::Function(b)) => a == b,
            (Value::Continuation(a), Value::Continuation(b)) => a.ptr_eq(b),
            (Value::Ref(a), Value::Ref(b)) => a.ptr_eq(b) && a.readonly == b.readonly,
            _ => false,
        }
    }
}

impl Eq for Value {}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueKind {
    Unit,
    Bool,
    Int,
    Float,
    String,
    Bytes,
    TypeRep,
    Ref,
    Function,
    Continuation,
}

/// A reference value (struct/enum/array) with an optional readonly view tag.
#[derive(Clone)]
pub struct RefValue {
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

    pub fn is_readonly(&self) -> bool {
        self.readonly
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.handle == other.handle
    }
}

/// A heap-allocated object.
#[derive(Clone, Debug)]
pub enum HeapValue {
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

/// A one-shot continuation token.
#[derive(Clone)]
pub struct ContinuationToken(Rc<RefCell<ContinuationInner>>);

impl ContinuationToken {
    fn new(state: ContinuationState) -> Self {
        Self(Rc::new(RefCell::new(ContinuationInner {
            state: Some(state),
        })))
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    fn take_state(&self) -> Option<ContinuationState> {
        self.0.borrow_mut().state.take()
    }
}

#[derive(Clone)]
struct ContinuationInner {
    state: Option<ContinuationState>,
}

#[derive(Clone)]
struct ContinuationState {
    stack: Vec<Frame>,
    handlers: Vec<HandlerEntry>,
    perform_dst: Option<Local>,
}

/// A runtime error raised by the MIR interpreter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeError {
    Trap {
        message: String,
    },
    UnknownFunction {
        name: String,
    },
    MissingHostFunctions {
        names: Vec<String>,
    },
    InvalidLocal {
        local: Local,
        locals: usize,
    },
    UninitializedLocal {
        local: Local,
    },
    InvalidBlock {
        block: BlockId,
        blocks: usize,
    },
    InvalidBlockArgs {
        target: BlockId,
        expected: usize,
        got: usize,
    },
    TypeError {
        op: &'static str,
        expected: &'static str,
        got: ValueKind,
    },
    MissingField {
        field: String,
    },
    DanglingRef {
        handle: GcRef,
    },
    ReadonlyWrite,
    IndexOutOfBounds {
        index: i64,
        len: usize,
    },
    UnhandledEffect {
        interface: String,
        method: String,
    },
    MismatchedPopHandler,
    InvalidResume,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::Trap { message } => write!(f, "trap: {message}"),
            RuntimeError::UnknownFunction { name } => write!(f, "unknown function: {name}"),
            RuntimeError::MissingHostFunctions { names } => {
                write!(f, "missing host functions: ")?;
                for (idx, name) in names.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{name}")?;
                }
                Ok(())
            }
            RuntimeError::InvalidLocal { local, locals } => {
                write!(f, "invalid local {:?} (locals={locals})", local.0)
            }
            RuntimeError::UninitializedLocal { local } => {
                write!(f, "uninitialized local %{id}", id = local.0)
            }
            RuntimeError::InvalidBlock { block, blocks } => {
                write!(f, "invalid block {:?} (blocks={blocks})", block.0)
            }
            RuntimeError::InvalidBlockArgs {
                target,
                expected,
                got,
            } => write!(
                f,
                "invalid block args for block {:?}: expected {expected}, got {got}",
                target.0
            ),
            RuntimeError::TypeError { op, expected, got } => {
                write!(f, "type error in {op}: expected {expected}, got {got:?}")
            }
            RuntimeError::MissingField { field } => write!(f, "missing field: {field}"),
            RuntimeError::DanglingRef { handle } => write!(
                f,
                "dangling heap reference: index={}, gen={}",
                handle.index, handle.generation
            ),
            RuntimeError::ReadonlyWrite => write!(f, "illegal write through readonly reference"),
            RuntimeError::IndexOutOfBounds { index, len } => {
                write!(f, "index out of bounds: index={index}, len={len}")
            }
            RuntimeError::UnhandledEffect { interface, method } => {
                write!(f, "unhandled effect: {interface}.{method}")
            }
            RuntimeError::MismatchedPopHandler => write!(f, "mismatched pop_handler"),
            RuntimeError::InvalidResume => write!(f, "invalid resume"),
        }
    }
}

impl core::error::Error for RuntimeError {}

#[derive(Clone, Debug)]
struct StructLayout {
    fields: Vec<String>,
    index_by_name: HashMap<String, usize>,
}

impl StructLayout {
    fn new(fields: Vec<String>) -> Self {
        let mut index_by_name = HashMap::new();
        for (idx, name) in fields.iter().enumerate() {
            index_by_name.insert(name.clone(), idx);
        }
        Self {
            fields,
            index_by_name,
        }
    }

    fn field_index(&self, name: &str) -> Option<usize> {
        self.index_by_name.get(name).copied()
    }
}

type HostFunction<GC> =
    Rc<dyn Fn(&mut InterpreterImpl<GC>, &[Value]) -> Result<Value, RuntimeError> + 'static>;

/// A handle to a value rooted in an [`InterpreterImpl`] instance.
///
/// Rooted values are treated as GC roots, which allows host code to keep
/// heap references alive across interpreter execution and garbage collection.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RootHandle {
    index: u32,
    generation: u32,
}

impl RootHandle {
    fn new(index: u32, generation: u32) -> Self {
        Self { index, generation }
    }
}

#[derive(Debug)]
struct RootSlot {
    generation: u32,
    value: Option<Value>,
}

/// A small, single-threaded interpreter for executing Rusk MIR.
pub type Interpreter = InterpreterImpl<MarkSweepHeap>;

/// A small, single-threaded interpreter for executing Rusk MIR.
///
/// This type is generic over the GC heap implementation to allow swapping
/// collectors. Most users should use the `Interpreter` type alias, which uses
/// `MarkSweepHeap`.
pub struct InterpreterImpl<GC: GcHeap<HeapValue>> {
    module: Rc<Module>,
    type_reps: Vec<TypeRepNode>,
    type_rep_intern: HashMap<TypeRepNode, TypeRepId>,
    struct_layouts: HashMap<String, StructLayout>,
    host_functions: BTreeMap<String, HostFunction<GC>>,
    host_import_functions: Vec<Option<HostFunction<GC>>>,
    value_buffers: Vec<Vec<Value>>,
    core_intrinsic_ids: CoreIntrinsicIds,
    stack: Vec<Frame>,
    handlers: Vec<HandlerEntry>,
    heap: GC,
    roots: Vec<RootSlot>,
    free_roots: Vec<u32>,
    gc_threshold: usize,
    allocations_since_gc: usize,
    metrics: InterpreterMetrics,
}

/// Runtime execution counters for profiling and benchmarking.
///
/// These counters are intended to be cheap to maintain and work in both `std` and `no_std`
/// builds. Time-based fields are best-effort: `gc_nanos` is only populated when the `std` feature
/// is enabled.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct InterpreterMetrics {
    pub executed_instructions: u64,
    pub executed_terminators: u64,
    pub block_entries: u64,

    pub allocations: u64,
    pub gc_cycles: u64,
    pub gc_nanos: u128,

    pub call_instructions: u64,
    pub icall_instructions: u64,
    pub vcall_instructions: u64,
    pub host_calls: u64,
    pub mir_calls: u64,

    pub br_terminators: u64,
    pub cond_br_terminators: u64,
    pub switch_terminators: u64,
    pub return_terminators: u64,
    pub trap_terminators: u64,
}

#[derive(Clone, Copy, Debug, Default)]
struct CoreIntrinsicIds {
    bool_not: Option<HostImportId>,
    bool_eq: Option<HostImportId>,
    bool_ne: Option<HostImportId>,

    int_add: Option<HostImportId>,
    int_sub: Option<HostImportId>,
    int_mul: Option<HostImportId>,
    int_div: Option<HostImportId>,
    int_mod: Option<HostImportId>,

    int_eq: Option<HostImportId>,
    int_ne: Option<HostImportId>,
    int_lt: Option<HostImportId>,
    int_le: Option<HostImportId>,
    int_gt: Option<HostImportId>,
    int_ge: Option<HostImportId>,
}

impl<GC: GcHeap<HeapValue> + Default> InterpreterImpl<GC> {
    /// Creates a new interpreter instance for the given MIR module.
    pub fn new(module: Module) -> Self {
        Self::with_heap(module, GC::default())
    }

    /// Creates a new interpreter instance for a shared MIR module.
    ///
    /// This avoids cloning large `Module` values when repeatedly creating interpreter instances
    /// for benchmarking or embedding.
    pub fn new_shared(module: Rc<Module>) -> Self {
        Self::with_heap_shared(module, GC::default())
    }
}

impl<GC: GcHeap<HeapValue>> InterpreterImpl<GC> {
    const DEFAULT_GC_THRESHOLD: usize = 10_000;

    /// Creates a new interpreter instance using an explicit GC heap implementation.
    pub fn with_heap(module: Module, heap: GC) -> Self {
        Self::with_heap_shared(Rc::new(module), heap)
    }

    /// Creates a new interpreter instance using an explicit GC heap implementation and a shared
    /// MIR module.
    pub fn with_heap_shared(module: Rc<Module>, heap: GC) -> Self {
        let mut struct_layouts = HashMap::new();
        for (type_name, fields) in module.struct_layouts.iter() {
            struct_layouts.insert(type_name.clone(), StructLayout::new(fields.clone()));
        }

        let core_intrinsic_ids = CoreIntrinsicIds {
            bool_not: module.host_import_id("core::intrinsics::bool_not"),
            bool_eq: module.host_import_id("core::intrinsics::bool_eq"),
            bool_ne: module.host_import_id("core::intrinsics::bool_ne"),

            int_add: module.host_import_id("core::intrinsics::int_add"),
            int_sub: module.host_import_id("core::intrinsics::int_sub"),
            int_mul: module.host_import_id("core::intrinsics::int_mul"),
            int_div: module.host_import_id("core::intrinsics::int_div"),
            int_mod: module.host_import_id("core::intrinsics::int_mod"),

            int_eq: module.host_import_id("core::intrinsics::int_eq"),
            int_ne: module.host_import_id("core::intrinsics::int_ne"),
            int_lt: module.host_import_id("core::intrinsics::int_lt"),
            int_le: module.host_import_id("core::intrinsics::int_le"),
            int_gt: module.host_import_id("core::intrinsics::int_gt"),
            int_ge: module.host_import_id("core::intrinsics::int_ge"),
        };

        let host_import_functions = vec![None; module.host_imports.len()];

        Self {
            module,
            type_reps: Vec::new(),
            type_rep_intern: HashMap::new(),
            struct_layouts,
            host_functions: BTreeMap::new(),
            host_import_functions,
            value_buffers: Vec::new(),
            core_intrinsic_ids,
            stack: Vec::new(),
            handlers: Vec::new(),
            heap,
            roots: Vec::new(),
            free_roots: Vec::new(),
            gc_threshold: Self::DEFAULT_GC_THRESHOLD,
            allocations_since_gc: 0,
            metrics: InterpreterMetrics::default(),
        }
    }

    /// Returns a snapshot of the current interpreter metrics.
    pub fn metrics(&self) -> &InterpreterMetrics {
        &self.metrics
    }

    /// Resets all interpreter metrics to zero.
    pub fn reset_metrics(&mut self) {
        self.metrics = InterpreterMetrics::default();
    }

    /// Takes the current metrics, resetting them to zero.
    pub fn take_metrics(&mut self) -> InterpreterMetrics {
        mem::take(&mut self.metrics)
    }

    /// Pins `value` as an external GC root.
    ///
    /// This is useful when embedding the interpreter, because `Value::Ref`
    /// handles can become dangling if GC runs while the host still holds them.
    pub fn root_value(&mut self, value: Value) -> RootHandle {
        if let Some(index) = self.free_roots.pop() {
            let slot = &mut self.roots[index as usize];
            debug_assert!(slot.value.is_none(), "free list points at live root slot");
            slot.value = Some(value);
            return RootHandle::new(index, slot.generation);
        }

        let index: u32 = self
            .roots
            .len()
            .try_into()
            .expect("root table index overflow");
        self.roots.push(RootSlot {
            generation: 0,
            value: Some(value),
        });
        RootHandle::new(index, 0)
    }

    /// Removes a previously-rooted value, returning it if `handle` is still valid.
    pub fn unroot_value(&mut self, handle: RootHandle) -> Option<Value> {
        let slot = self.roots.get_mut(handle.index as usize)?;
        if slot.generation != handle.generation {
            return None;
        }
        let value = slot.value.take()?;
        slot.generation = slot.generation.wrapping_add(1);
        self.free_roots.push(handle.index);
        Some(value)
    }

    /// Returns the number of currently live heap objects.
    pub fn heap_live_objects(&self) -> usize {
        self.heap.live_objects()
    }

    /// Sets the allocation threshold (in number of heap allocations) between
    /// GC runs.
    pub fn set_gc_threshold(&mut self, threshold: usize) {
        self.gc_threshold = threshold;
    }

    /// Forces a garbage collection using the current interpreter roots.
    pub fn collect_garbage_now(&mut self) {
        self.allocations_since_gc = 0;
        self.metrics.gc_cycles = self.metrics.gc_cycles.saturating_add(1);
        #[cfg(feature = "std")]
        let gc_start = Instant::now();
        let roots = InterpreterRoots {
            stack: &self.stack,
            roots: &self.roots,
        };
        self.heap.collect(&roots);
        #[cfg(feature = "std")]
        {
            self.metrics.gc_nanos = self
                .metrics
                .gc_nanos
                .saturating_add(gc_start.elapsed().as_nanos());
        }
    }

    /// Registers a host function callable via `call <name>(...)`.
    pub fn register_host_fn<F>(&mut self, name: impl Into<String>, f: F)
    where
        F: Fn(&mut Self, &[Value]) -> Result<Value, RuntimeError> + 'static,
    {
        let name: String = name.into();
        let f: HostFunction<GC> = Rc::new(f);
        self.host_functions.insert(name.clone(), Rc::clone(&f));

        if let Some(id) = self.module.host_import_id(name.as_str()) {
            let slot = self
                .host_import_functions
                .get_mut(id.0 as usize)
                .expect("host import id in bounds");
            *slot = Some(f);
        }
    }

    /// Returns the names of all currently registered host functions.
    pub fn host_function_names(&self) -> impl Iterator<Item = &str> {
        self.host_functions.keys().map(|name| name.as_str())
    }

    pub(crate) fn function_name(&self, id: FunctionId) -> Option<&str> {
        self.module.function(id).map(|f| f.name.as_str())
    }

    fn validate_declared_host_functions(&self) -> Result<(), RuntimeError> {
        let mut missing = Vec::new();
        for (idx, import) in self.module.host_imports.iter().enumerate() {
            if self
                .host_import_functions
                .get(idx)
                .and_then(|slot| slot.as_ref())
                .is_none()
            {
                missing.push(import.name.clone());
            }
        }
        if missing.is_empty() {
            Ok(())
        } else {
            Err(RuntimeError::MissingHostFunctions { names: missing })
        }
    }

    /// Allocates a heap array and returns it as a value.
    pub fn alloc_array(&mut self, items: Vec<Value>) -> Value {
        let handle = self.alloc_heap(HeapValue::Array(items));
        Value::Ref(RefValue::new(handle))
    }

    /// Interns a runtime `TypeRep` literal and returns its id.
    pub fn intern_type_rep_lit(&mut self, lit: rusk_mir::TypeRepLit) -> TypeRepId {
        self.eval_type_rep_lit(&lit)
    }

    /// Allocates a heap struct with named fields and returns it as a value.
    pub fn alloc_struct(
        &mut self,
        type_name: impl Into<String>,
        fields: BTreeMap<String, Value>,
    ) -> Value {
        self.alloc_struct_typed(type_name, Vec::new(), fields)
    }

    /// Allocates a heap struct with instantiated type arguments and returns it as a value.
    pub fn alloc_struct_typed(
        &mut self,
        type_name: impl Into<String>,
        type_args: Vec<TypeRepId>,
        fields: BTreeMap<String, Value>,
    ) -> Value {
        let type_name: String = type_name.into();
        let fields: Vec<(String, Value)> = fields.into_iter().collect();
        let observed_fields: Vec<&str> = fields.iter().map(|(k, _)| k.as_str()).collect();
        let module = Rc::clone(&self.module);
        let (field_count, field_indices, in_layout_order) = self
            .struct_layout_indices(module.as_ref(), type_name.as_str(), &observed_fields)
            .expect("alloc_struct_typed: inconsistent struct layout");

        let values = if in_layout_order {
            fields.into_iter().map(|(_, v)| v).collect()
        } else {
            let mut out: Vec<Option<Value>> = vec![None; field_count];
            for ((field_name, value), idx) in fields.into_iter().zip(field_indices.iter().copied())
            {
                if out[idx].is_some() {
                    panic!("alloc_struct_typed: duplicate field `{field_name}`");
                }
                out[idx] = Some(value);
            }
            let layout = self
                .struct_layouts
                .get(type_name.as_str())
                .expect("struct layout inserted");
            out.into_iter()
                .enumerate()
                .map(|(idx, v)| {
                    v.unwrap_or_else(|| {
                        panic!("alloc_struct_typed: missing `{}`", layout.fields[idx])
                    })
                })
                .collect()
        };

        let handle = self.alloc_heap(HeapValue::Struct {
            type_name,
            type_args,
            fields: values,
        });
        Value::Ref(RefValue::new(handle))
    }

    pub(crate) fn read_struct_field(
        &mut self,
        r: &RefValue,
        field: &str,
    ) -> Result<Value, RuntimeError> {
        let HeapValue::Struct {
            type_name, fields, ..
        } = self.heap_get(r.handle)?
        else {
            return Err(RuntimeError::TypeError {
                op: "struct_get",
                expected: "struct",
                got: ValueKind::Ref,
            });
        };
        let idx = self.struct_field_index_by_type(type_name.as_str(), field)?;
        fields
            .get(idx)
            .cloned()
            .ok_or_else(|| RuntimeError::MissingField {
                field: field.to_string(),
            })
    }

    pub(crate) fn write_struct_field(
        &mut self,
        r: &RefValue,
        field: &str,
        value: Value,
    ) -> Result<(), RuntimeError> {
        if r.readonly {
            return Err(RuntimeError::ReadonlyWrite);
        }
        let idx = match self.heap_get(r.handle)? {
            HeapValue::Struct { type_name, .. } => {
                self.struct_field_index_by_type(type_name.as_str(), field)?
            }
            _ => {
                return Err(RuntimeError::TypeError {
                    op: "struct_set",
                    expected: "struct",
                    got: ValueKind::Ref,
                });
            }
        };
        let HeapValue::Struct { fields, .. } = self.heap_get_mut(r.handle)? else {
            unreachable!("struct arm checked above");
        };
        let Some(slot) = fields.get_mut(idx) else {
            return Err(RuntimeError::MissingField {
                field: field.to_string(),
            });
        };
        *slot = value;
        Ok(())
    }

    /// Allocates a heap enum value and returns it as a value.
    pub fn alloc_enum(
        &mut self,
        enum_name: impl Into<String>,
        variant: impl Into<String>,
        fields: Vec<Value>,
    ) -> Value {
        self.alloc_enum_typed(enum_name, Vec::new(), variant, fields)
    }

    /// Allocates a heap enum value with instantiated type arguments and returns it as a value.
    pub fn alloc_enum_typed(
        &mut self,
        enum_name: impl Into<String>,
        type_args: Vec<TypeRepId>,
        variant: impl Into<String>,
        fields: Vec<Value>,
    ) -> Value {
        let handle = self.alloc_heap(HeapValue::Enum {
            enum_name: enum_name.into(),
            type_args,
            variant: variant.into(),
            fields,
        });
        Value::Ref(RefValue::new(handle))
    }

    /// Returns the heap value referenced by `r`.
    pub fn heap_value(&self, r: &RefValue) -> Result<&HeapValue, RuntimeError> {
        self.heap_get(r.handle)
    }

    /// Returns the heap value referenced by `r` mutably.
    pub fn heap_value_mut(&mut self, r: &RefValue) -> Result<&mut HeapValue, RuntimeError> {
        self.heap_get_mut(r.handle)
    }

    /// Executes `fn_name` as an entry function.
    pub fn run_function(&mut self, fn_name: &str, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.stack.clear();
        self.handlers.clear();

        self.validate_declared_host_functions()?;

        let module = Rc::clone(&self.module);
        let func_id = module
            .function_id(fn_name)
            .ok_or_else(|| RuntimeError::UnknownFunction {
                name: fn_name.to_string(),
            })?;
        let function = module.function(func_id).ok_or_else(|| RuntimeError::Trap {
            message: format!("invalid function id: {}", func_id.0),
        })?;

        let mut frame = Frame::new(func_id, function.locals, None);
        self.init_params(&mut frame, function, &args)?;
        self.stack.push(frame);
        self.run_loop_with_module(module.as_ref())
    }

    /// Resumes a previously captured continuation token, injecting `value` as the result of the
    /// suspended `perform`.
    ///
    /// This is the host-side equivalent of the MIR `resume` instruction and is useful for tests
    /// and embedding (FFI).
    pub fn resume_continuation(
        &mut self,
        token: ContinuationToken,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        self.validate_declared_host_functions()?;

        let Some(mut cont) = token.take_state() else {
            return Err(RuntimeError::InvalidResume);
        };

        if let Some(perform_dst) = cont.perform_dst {
            let top_index = cont.stack.len().saturating_sub(1);
            let top_frame = cont
                .stack
                .get_mut(top_index)
                .ok_or(RuntimeError::InvalidResume)?;
            top_frame.write_local(perform_dst, value)?;
        }

        let saved_stack = mem::take(&mut self.stack);
        let saved_handlers = mem::take(&mut self.handlers);

        self.stack = cont.stack;
        self.handlers = cont.handlers;
        let result = self.run_loop();

        self.stack = saved_stack;
        self.handlers = saved_handlers;

        result
    }

    /// Returns an immutable view of the loaded MIR module.
    pub fn module(&self) -> &Module {
        self.module.as_ref()
    }

    fn init_params(
        &self,
        frame: &mut Frame,
        function: &Function,
        args: &[Value],
    ) -> Result<(), RuntimeError> {
        if args.len() != function.params.len() {
            return Err(RuntimeError::Trap {
                message: format!(
                    "argument arity mismatch for {}: expected {}, got {}",
                    function.name,
                    function.params.len(),
                    args.len()
                ),
            });
        }

        for (param, arg) in function.params.iter().zip(args.iter()) {
            let value = match param.mutability {
                Mutability::Mutable => arg.clone(),
                Mutability::Readonly => arg.clone().into_readonly_view(),
            };
            frame.write_local(param.local, value)?;
        }
        Ok(())
    }

    fn run_loop(&mut self) -> Result<Value, RuntimeError> {
        let module = Rc::clone(&self.module);
        self.run_loop_with_module(module.as_ref())
    }

    fn run_loop_with_module(&mut self, module: &Module) -> Result<Value, RuntimeError> {
        loop {
            let Some(frame_index) = self.stack.len().checked_sub(1) else {
                return Err(RuntimeError::Trap {
                    message: "empty stack without return value".to_string(),
                });
            };

            let (func_id, block_id, ip) = {
                let frame = &self.stack[frame_index];
                (frame.func, frame.block, frame.ip)
            };

            let function = Self::function(module, func_id)?;
            let block = Self::block(function, block_id)?;

            if ip < block.instructions.len() {
                let instr = &block.instructions[ip];
                // Advance before executing so calls/perform capture the "after" PC.
                self.stack[frame_index].ip += 1;
                self.metrics.executed_instructions =
                    self.metrics.executed_instructions.saturating_add(1);
                self.execute_instruction(module, frame_index, instr)?;
                self.maybe_collect_garbage();
                continue;
            }

            let term = &block.terminator;
            self.metrics.executed_terminators = self.metrics.executed_terminators.saturating_add(1);
            if let Some(final_value) = self.execute_terminator(module, frame_index, term)? {
                return Ok(final_value);
            }
        }
    }

    fn function(module: &Module, id: FunctionId) -> Result<&Function, RuntimeError> {
        module.function(id).ok_or_else(|| RuntimeError::Trap {
            message: format!("invalid function id: {}", id.0),
        })
    }

    fn block(function: &Function, block: BlockId) -> Result<&BasicBlock, RuntimeError> {
        function
            .blocks
            .get(block.0)
            .ok_or(RuntimeError::InvalidBlock {
                block,
                blocks: function.blocks.len(),
            })
    }

    fn heap_get(&self, handle: GcRef) -> Result<&HeapValue, RuntimeError> {
        self.heap
            .get(handle)
            .ok_or(RuntimeError::DanglingRef { handle })
    }

    fn heap_get_mut(&mut self, handle: GcRef) -> Result<&mut HeapValue, RuntimeError> {
        self.heap
            .get_mut(handle)
            .ok_or(RuntimeError::DanglingRef { handle })
    }

    fn alloc_heap(&mut self, value: HeapValue) -> GcRef {
        self.allocations_since_gc = self.allocations_since_gc.saturating_add(1);
        self.metrics.allocations = self.metrics.allocations.saturating_add(1);
        self.heap.alloc(value)
    }

    fn maybe_collect_garbage(&mut self) {
        if self.allocations_since_gc < self.gc_threshold {
            return;
        }
        self.collect_garbage_now();
    }

    fn eval_operand(&mut self, frame_index: usize, op: &Operand) -> Result<Value, RuntimeError> {
        match op {
            Operand::Local(local) => self.stack[frame_index].read_local(*local),
            Operand::Literal(lit) => self.eval_const(lit),
        }
    }

    fn eval_type_rep_lit(&mut self, lit: &TypeRepLit) -> TypeRepId {
        let ctor = match lit {
            TypeRepLit::Unit => TypeCtor::Unit,
            TypeRepLit::Bool => TypeCtor::Bool,
            TypeRepLit::Int => TypeCtor::Int,
            TypeRepLit::Float => TypeCtor::Float,
            TypeRepLit::String => TypeCtor::String,
            TypeRepLit::Bytes => TypeCtor::Bytes,
            TypeRepLit::Array => TypeCtor::Array,
            TypeRepLit::Tuple(arity) => TypeCtor::Tuple(*arity),
            TypeRepLit::Struct(name) => TypeCtor::Struct(name.clone()),
            TypeRepLit::Enum(name) => TypeCtor::Enum(name.clone()),
            TypeRepLit::Interface(name) => TypeCtor::Interface(name.clone()),
            TypeRepLit::Fn => TypeCtor::Fn,
            TypeRepLit::Cont => TypeCtor::Cont,
        };

        self.intern_type_rep(TypeRepNode {
            ctor,
            args: Vec::new(),
        })
    }

    fn intern_type_rep(&mut self, node: TypeRepNode) -> TypeRepId {
        if let Some(id) = self.type_rep_intern.get(&node).copied() {
            return id;
        }

        let id: u32 = self
            .type_reps
            .len()
            .try_into()
            .expect("TypeRep table overflow");
        let id = TypeRepId(id);
        self.type_reps.push(node.clone());
        self.type_rep_intern.insert(node, id);
        id
    }

    fn type_rep_node(&self, id: TypeRepId) -> &TypeRepNode {
        self.type_reps
            .get(id.0 as usize)
            .unwrap_or_else(|| panic!("invalid TypeRepId: {}", id.0))
    }

    fn eval_type_rep_operand(
        &mut self,
        frame_index: usize,
        op: &Operand,
    ) -> Result<TypeRepId, RuntimeError> {
        let v = self.eval_operand(frame_index, op)?;
        match v {
            Value::TypeRep(id) => Ok(id),
            other => Err(RuntimeError::TypeError {
                op: "typerep",
                expected: "typerep",
                got: other.kind(),
            }),
        }
    }

    fn ensure_struct_layout(
        &mut self,
        module: &Module,
        type_name: &str,
        observed_fields: Option<&[&str]>,
    ) -> Result<(), RuntimeError> {
        if self.struct_layouts.contains_key(type_name) {
            return Ok(());
        }

        let fields = if let Some(fields) = module.struct_layouts.get(type_name) {
            fields.clone()
        } else if let Some(observed) = observed_fields {
            observed.iter().map(|name| (*name).to_string()).collect()
        } else {
            return Err(RuntimeError::Trap {
                message: format!("missing struct layout for `{type_name}`"),
            });
        };

        self.struct_layouts
            .insert(type_name.to_string(), StructLayout::new(fields));
        Ok(())
    }

    fn struct_layout_indices(
        &mut self,
        module: &Module,
        type_name: &str,
        observed_fields: &[&str],
    ) -> Result<(usize, Vec<usize>, bool), RuntimeError> {
        self.ensure_struct_layout(module, type_name, Some(observed_fields))?;
        let layout = self
            .struct_layouts
            .get(type_name)
            .expect("struct layout inserted");

        if layout.fields.len() != observed_fields.len() {
            return Err(RuntimeError::Trap {
                message: format!(
                    "struct `{type_name}` layout mismatch: expected {} fields, got {}",
                    layout.fields.len(),
                    observed_fields.len()
                ),
            });
        }

        let in_layout_order = layout
            .fields
            .iter()
            .map(|name| name.as_str())
            .eq(observed_fields.iter().copied());

        let mut indices = Vec::with_capacity(observed_fields.len());
        for field in observed_fields {
            let Some(idx) = layout.field_index(field) else {
                return Err(RuntimeError::MissingField {
                    field: (*field).to_string(),
                });
            };
            indices.push(idx);
        }

        Ok((layout.fields.len(), indices, in_layout_order))
    }

    fn struct_field_index_by_type(
        &self,
        type_name: &str,
        field: &str,
    ) -> Result<usize, RuntimeError> {
        let Some(layout) = self.struct_layouts.get(type_name) else {
            return Err(RuntimeError::Trap {
                message: format!("missing struct layout for `{type_name}`"),
            });
        };
        layout
            .field_index(field)
            .ok_or_else(|| RuntimeError::MissingField {
                field: field.to_string(),
            })
    }

    fn eval_const(&mut self, lit: &ConstValue) -> Result<Value, RuntimeError> {
        match lit {
            ConstValue::Unit => Ok(Value::Unit),
            ConstValue::Bool(v) => Ok(Value::Bool(*v)),
            ConstValue::Int(v) => Ok(Value::Int(*v)),
            ConstValue::Float(v) => Ok(Value::Float(*v)),
            ConstValue::String(v) => Ok(Value::String(v.clone())),
            ConstValue::Bytes(v) => Ok(Value::Bytes(v.clone())),
            ConstValue::TypeRep(lit) => Ok(Value::TypeRep(self.eval_type_rep_lit(lit))),
            ConstValue::FunctionId(id) => Ok(Value::Function(*id)),
            ConstValue::Function(name) => {
                let module = Rc::clone(&self.module);
                let Some(id) = module.function_id(name.as_str()) else {
                    return Err(RuntimeError::UnknownFunction { name: name.clone() });
                };
                Ok(Value::Function(id))
            }
            ConstValue::Array(items) => {
                let values = items
                    .iter()
                    .map(|x| self.eval_const(x))
                    .collect::<Result<Vec<_>, _>>()?;
                let handle = self.alloc_heap(HeapValue::Array(values));
                Ok(Value::Ref(RefValue::new(handle)))
            }
            ConstValue::Tuple(items) => {
                if items.is_empty() {
                    return Ok(Value::Unit);
                }
                let values = items
                    .iter()
                    .map(|x| self.eval_const(x))
                    .collect::<Result<Vec<_>, _>>()?;
                let handle = self.alloc_heap(HeapValue::Tuple(values));
                Ok(Value::Ref(RefValue::new(handle)))
            }
            ConstValue::Struct { type_name, fields } => {
                let observed_fields: Vec<&str> = fields.iter().map(|(k, _)| k.as_str()).collect();
                let module = Rc::clone(&self.module);
                let (field_count, field_indices, in_layout_order) = self
                    .struct_layout_indices(module.as_ref(), type_name.as_str(), &observed_fields)
                    .expect("ConstValue::Struct produced inconsistent layout");

                let values = if in_layout_order {
                    fields
                        .iter()
                        .map(|(_, v)| self.eval_const(v))
                        .collect::<Result<Vec<_>, _>>()?
                } else {
                    let mut out: Vec<Option<Value>> = vec![None; field_count];
                    for ((_, v), idx) in fields.iter().zip(field_indices.iter().copied()) {
                        out[idx] = Some(self.eval_const(v)?);
                    }
                    let layout = self
                        .struct_layouts
                        .get(type_name.as_str())
                        .expect("struct layout inserted");
                    out.into_iter()
                        .enumerate()
                        .map(|(idx, v)| {
                            v.unwrap_or_else(|| {
                                panic!("ConstValue::Struct missing field `{}`", layout.fields[idx])
                            })
                        })
                        .collect()
                };

                let handle = self.alloc_heap(HeapValue::Struct {
                    type_name: type_name.clone(),
                    type_args: Vec::new(),
                    fields: values,
                });
                Ok(Value::Ref(RefValue::new(handle)))
            }
            ConstValue::Enum {
                enum_name,
                variant,
                fields,
            } => {
                let vals = fields
                    .iter()
                    .map(|x| self.eval_const(x))
                    .collect::<Result<Vec<_>, _>>()?;
                let handle = self.alloc_heap(HeapValue::Enum {
                    enum_name: enum_name.clone(),
                    type_args: Vec::new(),
                    variant: variant.clone(),
                    fields: vals,
                });
                Ok(Value::Ref(RefValue::new(handle)))
            }
        }
    }

    fn type_test(
        &mut self,
        module: &Module,
        value: &Value,
        target: TypeRepId,
    ) -> Result<bool, RuntimeError> {
        let target = self.type_rep_node(target);

        Ok(match &target.ctor {
            TypeCtor::Unit => matches!(value, Value::Unit),
            TypeCtor::Bool => matches!(value, Value::Bool(_)),
            TypeCtor::Int => matches!(value, Value::Int(_)),
            TypeCtor::Float => matches!(value, Value::Float(_)),
            TypeCtor::String => matches!(value, Value::String(_)),
            TypeCtor::Bytes => matches!(value, Value::Bytes(_)),
            TypeCtor::Array => match value {
                Value::Ref(r) => matches!(self.heap_get(r.handle)?, HeapValue::Array(_)),
                _ => false,
            },
            TypeCtor::Tuple(arity) => match value {
                Value::Unit => *arity == 0,
                Value::Ref(r) => match self.heap_get(r.handle)? {
                    HeapValue::Tuple(items) => items.len() == *arity,
                    _ => false,
                },
                _ => false,
            },
            TypeCtor::Struct(name) => match value {
                Value::Ref(r) => match self.heap_get(r.handle)? {
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
                Value::Ref(r) => match self.heap_get(r.handle)? {
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
                let (dyn_type, dyn_type_args) = match self.heap_get(r.handle)? {
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

    fn execute_instruction(
        &mut self,
        module: &Module,
        frame_index: usize,
        instr: &Instruction,
    ) -> Result<(), RuntimeError> {
        match instr {
            Instruction::Const { dst, value } => {
                let v = self.eval_const(value)?;
                self.stack[frame_index].write_local(*dst, v)?;
            }
            Instruction::Copy { dst, src } => {
                let v = self.stack[frame_index].read_local(*src)?;
                self.stack[frame_index].write_local(*dst, v)?;
            }
            Instruction::Move { dst, src } => {
                let v = self.stack[frame_index].take_local(*src)?;
                self.stack[frame_index].write_local(*dst, v)?;
            }
            Instruction::AsReadonly { dst, src } => {
                let v = self.stack[frame_index]
                    .read_local(*src)?
                    .into_readonly_view();
                self.stack[frame_index].write_local(*dst, v)?;
            }
            Instruction::IsType { dst, value, ty } => {
                let v = self.eval_operand(frame_index, value)?;
                let target = self.eval_type_rep_operand(frame_index, ty)?;
                let ok = self.type_test(module, &v, target)?;
                self.stack[frame_index].write_local(*dst, Value::Bool(ok))?;
            }
            Instruction::CheckedCast { dst, value, ty } => {
                let v = self.eval_operand(frame_index, value)?;
                let target = self.eval_type_rep_operand(frame_index, ty)?;
                let ok = self.type_test(module, &v, target)?;
                let (variant, fields) = if ok {
                    ("Some", vec![v])
                } else {
                    ("None", Vec::new())
                };
                let handle = self.alloc_heap(HeapValue::Enum {
                    enum_name: "Option".to_string(),
                    type_args: vec![target],
                    variant: variant.to_string(),
                    fields,
                });
                self.stack[frame_index].write_local(*dst, Value::Ref(RefValue::new(handle)))?;
            }

            Instruction::MakeTypeRep { dst, base, args } => {
                let arg_ids = args
                    .iter()
                    .map(|op| self.eval_type_rep_operand(frame_index, op))
                    .collect::<Result<Vec<_>, _>>()?;
                let ctor = match base {
                    TypeRepLit::Unit => TypeCtor::Unit,
                    TypeRepLit::Bool => TypeCtor::Bool,
                    TypeRepLit::Int => TypeCtor::Int,
                    TypeRepLit::Float => TypeCtor::Float,
                    TypeRepLit::String => TypeCtor::String,
                    TypeRepLit::Bytes => TypeCtor::Bytes,
                    TypeRepLit::Array => TypeCtor::Array,
                    TypeRepLit::Tuple(arity) => TypeCtor::Tuple(*arity),
                    TypeRepLit::Struct(name) => TypeCtor::Struct(name.clone()),
                    TypeRepLit::Enum(name) => TypeCtor::Enum(name.clone()),
                    TypeRepLit::Interface(name) => TypeCtor::Interface(name.clone()),
                    TypeRepLit::Fn => TypeCtor::Fn,
                    TypeRepLit::Cont => TypeCtor::Cont,
                };
                let id = self.intern_type_rep(TypeRepNode {
                    ctor,
                    args: arg_ids,
                });
                self.stack[frame_index].write_local(*dst, Value::TypeRep(id))?;
            }

            Instruction::MakeStruct {
                dst,
                type_name,
                type_args,
                fields,
            } => {
                let type_args = type_args
                    .iter()
                    .map(|op| self.eval_type_rep_operand(frame_index, op))
                    .collect::<Result<Vec<_>, _>>()?;
                let observed_fields: Vec<&str> = fields.iter().map(|(k, _)| k.as_str()).collect();
                let (field_count, field_indices, in_layout_order) =
                    self.struct_layout_indices(module, type_name, &observed_fields)?;

                let values = if in_layout_order {
                    let mut out = Vec::with_capacity(field_count);
                    for (_, op) in fields.iter() {
                        out.push(self.eval_operand(frame_index, op)?);
                    }
                    out
                } else {
                    let mut out: Vec<Option<Value>> = vec![None; field_count];
                    for ((field_name, op), idx) in fields.iter().zip(field_indices.iter().copied())
                    {
                        if out[idx].is_some() {
                            return Err(RuntimeError::Trap {
                                message: format!(
                                    "duplicate field `{}` in `{}` literal",
                                    field_name, type_name
                                ),
                            });
                        }
                        out[idx] = Some(self.eval_operand(frame_index, op)?);
                    }
                    let layout = self
                        .struct_layouts
                        .get(type_name.as_str())
                        .expect("struct layout inserted");
                    out.into_iter()
                        .enumerate()
                        .map(|(idx, v)| {
                            v.ok_or_else(|| RuntimeError::MissingField {
                                field: layout.fields[idx].clone(),
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?
                };

                let obj = HeapValue::Struct {
                    type_name: type_name.clone(),
                    type_args,
                    fields: values,
                };
                let handle = self.alloc_heap(obj);
                self.stack[frame_index].write_local(*dst, Value::Ref(RefValue::new(handle)))?;
            }
            Instruction::MakeArray { dst, items } => {
                let mut values = Vec::with_capacity(items.len());
                for op in items {
                    values.push(self.eval_operand(frame_index, op)?);
                }
                let handle = self.alloc_heap(HeapValue::Array(values));
                self.stack[frame_index].write_local(*dst, Value::Ref(RefValue::new(handle)))?;
            }
            Instruction::MakeTuple { dst, items } => {
                if items.is_empty() {
                    self.stack[frame_index].write_local(*dst, Value::Unit)?;
                    return Ok(());
                }
                let mut values = Vec::with_capacity(items.len());
                for op in items {
                    values.push(self.eval_operand(frame_index, op)?);
                }
                let handle = self.alloc_heap(HeapValue::Tuple(values));
                self.stack[frame_index].write_local(*dst, Value::Ref(RefValue::new(handle)))?;
            }
            Instruction::MakeEnum {
                dst,
                enum_name,
                type_args,
                variant,
                fields,
            } => {
                let type_args = type_args
                    .iter()
                    .map(|op| self.eval_type_rep_operand(frame_index, op))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut values = Vec::with_capacity(fields.len());
                for op in fields {
                    values.push(self.eval_operand(frame_index, op)?);
                }
                let handle = self.alloc_heap(HeapValue::Enum {
                    enum_name: enum_name.clone(),
                    type_args,
                    variant: variant.clone(),
                    fields: values,
                });
                self.stack[frame_index].write_local(*dst, Value::Ref(RefValue::new(handle)))?;
            }
            Instruction::GetField { dst, obj, field } => {
                let v = self.eval_operand(frame_index, obj)?;
                let Value::Ref(r) = v else {
                    return Err(RuntimeError::TypeError {
                        op: "get_field",
                        expected: "ref(struct/tuple)",
                        got: v.kind(),
                    });
                };

                let value = match self.heap_get(r.handle)? {
                    HeapValue::Struct {
                        type_name, fields, ..
                    } => {
                        let idx = self.struct_field_index_by_type(type_name.as_str(), field)?;
                        fields
                            .get(idx)
                            .cloned()
                            .ok_or_else(|| RuntimeError::MissingField {
                                field: field.clone(),
                            })?
                    }
                    HeapValue::Tuple(items) => {
                        let Some(idx) = field
                            .strip_prefix('.')
                            .and_then(|s| s.parse::<usize>().ok())
                        else {
                            return Err(RuntimeError::MissingField {
                                field: field.clone(),
                            });
                        };
                        items
                            .get(idx)
                            .cloned()
                            .ok_or_else(|| RuntimeError::MissingField {
                                field: field.clone(),
                            })?
                    }
                    _ => {
                        return Err(RuntimeError::TypeError {
                            op: "get_field",
                            expected: "struct/tuple",
                            got: ValueKind::Ref,
                        });
                    }
                };
                self.stack[frame_index].write_local(*dst, value)?;
            }
            Instruction::SetField { obj, field, value } => {
                let obj_v = self.eval_operand(frame_index, obj)?;
                let Value::Ref(r) = obj_v else {
                    return Err(RuntimeError::TypeError {
                        op: "set_field",
                        expected: "ref(struct/tuple)",
                        got: obj_v.kind(),
                    });
                };
                if r.readonly {
                    return Err(RuntimeError::ReadonlyWrite);
                }
                let val = self.eval_operand(frame_index, value)?;
                let struct_field_idx = match self.heap_get(r.handle)? {
                    HeapValue::Struct { type_name, .. } => {
                        Some(self.struct_field_index_by_type(type_name.as_str(), field)?)
                    }
                    HeapValue::Tuple(_) => None,
                    _ => {
                        return Err(RuntimeError::TypeError {
                            op: "set_field",
                            expected: "struct/tuple",
                            got: ValueKind::Ref,
                        });
                    }
                };

                if let Some(idx) = struct_field_idx {
                    let HeapValue::Struct { fields, .. } = self.heap_get_mut(r.handle)? else {
                        unreachable!("struct arm checked above");
                    };
                    let Some(slot) = fields.get_mut(idx) else {
                        return Err(RuntimeError::MissingField {
                            field: field.clone(),
                        });
                    };
                    *slot = val;
                } else {
                    let Some(idx) = field
                        .strip_prefix('.')
                        .and_then(|s| s.parse::<usize>().ok())
                    else {
                        return Err(RuntimeError::MissingField {
                            field: field.clone(),
                        });
                    };
                    let HeapValue::Tuple(items) = self.heap_get_mut(r.handle)? else {
                        unreachable!("tuple arm checked above");
                    };
                    let Some(slot) = items.get_mut(idx) else {
                        return Err(RuntimeError::MissingField {
                            field: field.clone(),
                        });
                    };
                    *slot = val;
                }
            }

            Instruction::StructGet { dst, obj, idx } => {
                let v = self.eval_operand(frame_index, obj)?;
                let Value::Ref(r) = v else {
                    return Err(RuntimeError::TypeError {
                        op: "struct_get",
                        expected: "ref(struct)",
                        got: v.kind(),
                    });
                };
                let value = match self.heap_get(r.handle)? {
                    HeapValue::Struct {
                        type_name, fields, ..
                    } => {
                        let Some(value) = fields.get(*idx) else {
                            let field = module
                                .struct_layouts
                                .get(type_name.as_str())
                                .and_then(|names| names.get(*idx))
                                .cloned()
                                .unwrap_or_else(|| format!("#{idx}"));
                            return Err(RuntimeError::MissingField { field });
                        };
                        value.clone()
                    }
                    _ => {
                        return Err(RuntimeError::TypeError {
                            op: "struct_get",
                            expected: "struct",
                            got: ValueKind::Ref,
                        });
                    }
                };
                self.stack[frame_index].write_local(*dst, value)?;
            }
            Instruction::StructSet { obj, idx, value } => {
                let obj_v = self.eval_operand(frame_index, obj)?;
                let Value::Ref(r) = obj_v else {
                    return Err(RuntimeError::TypeError {
                        op: "struct_set",
                        expected: "ref(struct)",
                        got: obj_v.kind(),
                    });
                };
                if r.readonly {
                    return Err(RuntimeError::ReadonlyWrite);
                }
                let val = self.eval_operand(frame_index, value)?;
                let HeapValue::Struct { fields, .. } = self.heap_get_mut(r.handle)? else {
                    return Err(RuntimeError::TypeError {
                        op: "struct_set",
                        expected: "struct",
                        got: ValueKind::Ref,
                    });
                };
                let Some(slot) = fields.get_mut(*idx) else {
                    return Err(RuntimeError::MissingField {
                        field: format!("#{idx}"),
                    });
                };
                *slot = val;
            }

            Instruction::TupleGet { dst, tup, idx } => {
                let v = self.eval_operand(frame_index, tup)?;
                let Value::Ref(r) = v else {
                    return Err(RuntimeError::TypeError {
                        op: "tuple_get",
                        expected: "ref(tuple)",
                        got: v.kind(),
                    });
                };
                let value = match self.heap_get(r.handle)? {
                    HeapValue::Tuple(items) => {
                        items
                            .get(*idx)
                            .cloned()
                            .ok_or_else(|| RuntimeError::MissingField {
                                field: format!(".{idx}"),
                            })?
                    }
                    _ => {
                        return Err(RuntimeError::TypeError {
                            op: "tuple_get",
                            expected: "tuple",
                            got: ValueKind::Ref,
                        });
                    }
                };
                self.stack[frame_index].write_local(*dst, value)?;
            }
            Instruction::TupleSet { tup, idx, value } => {
                let tup_v = self.eval_operand(frame_index, tup)?;
                let Value::Ref(r) = tup_v else {
                    return Err(RuntimeError::TypeError {
                        op: "tuple_set",
                        expected: "ref(tuple)",
                        got: tup_v.kind(),
                    });
                };
                if r.readonly {
                    return Err(RuntimeError::ReadonlyWrite);
                }
                let val = self.eval_operand(frame_index, value)?;
                match self.heap_get_mut(r.handle)? {
                    HeapValue::Tuple(items) => {
                        let Some(slot) = items.get_mut(*idx) else {
                            return Err(RuntimeError::MissingField {
                                field: format!(".{idx}"),
                            });
                        };
                        *slot = val;
                    }
                    _ => {
                        return Err(RuntimeError::TypeError {
                            op: "tuple_set",
                            expected: "tuple",
                            got: ValueKind::Ref,
                        });
                    }
                }
            }

            Instruction::IndexGet { dst, arr, idx } => {
                let arr_v = self.eval_operand(frame_index, arr)?;
                let Value::Ref(r) = arr_v else {
                    return Err(RuntimeError::TypeError {
                        op: "index_get",
                        expected: "ref(array)",
                        got: arr_v.kind(),
                    });
                };
                let idx_v = self.eval_operand(frame_index, idx)?;
                let Value::Int(i) = idx_v else {
                    return Err(RuntimeError::TypeError {
                        op: "index_get",
                        expected: "int index",
                        got: idx_v.kind(),
                    });
                };
                let element = match self.heap_get(r.handle)? {
                    HeapValue::Array(items) => {
                        let Some(v) = items.get(i as usize) else {
                            return Err(RuntimeError::IndexOutOfBounds {
                                index: i,
                                len: items.len(),
                            });
                        };
                        v.clone()
                    }
                    _ => {
                        return Err(RuntimeError::TypeError {
                            op: "index_get",
                            expected: "array",
                            got: ValueKind::Ref,
                        });
                    }
                };
                self.stack[frame_index].write_local(*dst, element)?;
            }
            Instruction::IndexSet { arr, idx, value } => {
                let arr_v = self.eval_operand(frame_index, arr)?;
                let Value::Ref(r) = arr_v else {
                    return Err(RuntimeError::TypeError {
                        op: "index_set",
                        expected: "ref(array)",
                        got: arr_v.kind(),
                    });
                };
                if r.readonly {
                    return Err(RuntimeError::ReadonlyWrite);
                }
                let idx_v = self.eval_operand(frame_index, idx)?;
                let Value::Int(i) = idx_v else {
                    return Err(RuntimeError::TypeError {
                        op: "index_set",
                        expected: "int index",
                        got: idx_v.kind(),
                    });
                };
                let val = self.eval_operand(frame_index, value)?;
                let HeapValue::Array(items) = self.heap_get_mut(r.handle)? else {
                    return Err(RuntimeError::TypeError {
                        op: "index_set",
                        expected: "array",
                        got: ValueKind::Ref,
                    });
                };
                let idx_usize: usize =
                    i.try_into().map_err(|_| RuntimeError::IndexOutOfBounds {
                        index: i,
                        len: items.len(),
                    })?;
                if idx_usize >= items.len() {
                    return Err(RuntimeError::IndexOutOfBounds {
                        index: i,
                        len: items.len(),
                    });
                }
                items[idx_usize] = val;
            }
            Instruction::Len { dst, arr } => {
                let arr_v = self.eval_operand(frame_index, arr)?;
                let Value::Ref(r) = arr_v else {
                    return Err(RuntimeError::TypeError {
                        op: "len",
                        expected: "ref(array)",
                        got: arr_v.kind(),
                    });
                };
                let len = match self.heap_get(r.handle)? {
                    HeapValue::Array(items) => items.len(),
                    _ => {
                        return Err(RuntimeError::TypeError {
                            op: "len",
                            expected: "array",
                            got: ValueKind::Ref,
                        });
                    }
                };
                self.stack[frame_index].write_local(*dst, Value::Int(len as i64))?;
            }

            Instruction::IntAdd { dst, a, b } => {
                let a_v = self.eval_operand(frame_index, a)?;
                let Value::Int(a_i) = a_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_add",
                        expected: "int",
                        got: a_v.kind(),
                    });
                };
                let b_v = self.eval_operand(frame_index, b)?;
                let Value::Int(b_i) = b_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_add",
                        expected: "int",
                        got: b_v.kind(),
                    });
                };
                self.stack[frame_index].write_local(*dst, Value::Int(a_i + b_i))?;
            }
            Instruction::IntSub { dst, a, b } => {
                let a_v = self.eval_operand(frame_index, a)?;
                let Value::Int(a_i) = a_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_sub",
                        expected: "int",
                        got: a_v.kind(),
                    });
                };
                let b_v = self.eval_operand(frame_index, b)?;
                let Value::Int(b_i) = b_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_sub",
                        expected: "int",
                        got: b_v.kind(),
                    });
                };
                self.stack[frame_index].write_local(*dst, Value::Int(a_i - b_i))?;
            }
            Instruction::IntMul { dst, a, b } => {
                let a_v = self.eval_operand(frame_index, a)?;
                let Value::Int(a_i) = a_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_mul",
                        expected: "int",
                        got: a_v.kind(),
                    });
                };
                let b_v = self.eval_operand(frame_index, b)?;
                let Value::Int(b_i) = b_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_mul",
                        expected: "int",
                        got: b_v.kind(),
                    });
                };
                self.stack[frame_index].write_local(*dst, Value::Int(a_i * b_i))?;
            }
            Instruction::IntDiv { dst, a, b } => {
                let a_v = self.eval_operand(frame_index, a)?;
                let Value::Int(a_i) = a_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_div",
                        expected: "int",
                        got: a_v.kind(),
                    });
                };
                let b_v = self.eval_operand(frame_index, b)?;
                let Value::Int(b_i) = b_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_div",
                        expected: "int",
                        got: b_v.kind(),
                    });
                };
                if b_i == 0 {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::int_div: division by zero".to_string(),
                    });
                }
                self.stack[frame_index].write_local(*dst, Value::Int(a_i / b_i))?;
            }
            Instruction::IntMod { dst, a, b } => {
                let a_v = self.eval_operand(frame_index, a)?;
                let Value::Int(a_i) = a_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_mod",
                        expected: "int",
                        got: a_v.kind(),
                    });
                };
                let b_v = self.eval_operand(frame_index, b)?;
                let Value::Int(b_i) = b_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_mod",
                        expected: "int",
                        got: b_v.kind(),
                    });
                };
                if b_i == 0 {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::int_mod: modulo by zero".to_string(),
                    });
                }
                self.stack[frame_index].write_local(*dst, Value::Int(a_i % b_i))?;
            }

            Instruction::IntLt { dst, a, b } => {
                let a_v = self.eval_operand(frame_index, a)?;
                let Value::Int(a_i) = a_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_lt",
                        expected: "int",
                        got: a_v.kind(),
                    });
                };
                let b_v = self.eval_operand(frame_index, b)?;
                let Value::Int(b_i) = b_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_lt",
                        expected: "int",
                        got: b_v.kind(),
                    });
                };
                self.stack[frame_index].write_local(*dst, Value::Bool(a_i < b_i))?;
            }
            Instruction::IntLe { dst, a, b } => {
                let a_v = self.eval_operand(frame_index, a)?;
                let Value::Int(a_i) = a_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_le",
                        expected: "int",
                        got: a_v.kind(),
                    });
                };
                let b_v = self.eval_operand(frame_index, b)?;
                let Value::Int(b_i) = b_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_le",
                        expected: "int",
                        got: b_v.kind(),
                    });
                };
                self.stack[frame_index].write_local(*dst, Value::Bool(a_i <= b_i))?;
            }
            Instruction::IntGt { dst, a, b } => {
                let a_v = self.eval_operand(frame_index, a)?;
                let Value::Int(a_i) = a_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_gt",
                        expected: "int",
                        got: a_v.kind(),
                    });
                };
                let b_v = self.eval_operand(frame_index, b)?;
                let Value::Int(b_i) = b_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_gt",
                        expected: "int",
                        got: b_v.kind(),
                    });
                };
                self.stack[frame_index].write_local(*dst, Value::Bool(a_i > b_i))?;
            }
            Instruction::IntGe { dst, a, b } => {
                let a_v = self.eval_operand(frame_index, a)?;
                let Value::Int(a_i) = a_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_ge",
                        expected: "int",
                        got: a_v.kind(),
                    });
                };
                let b_v = self.eval_operand(frame_index, b)?;
                let Value::Int(b_i) = b_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_ge",
                        expected: "int",
                        got: b_v.kind(),
                    });
                };
                self.stack[frame_index].write_local(*dst, Value::Bool(a_i >= b_i))?;
            }
            Instruction::IntEq { dst, a, b } => {
                let a_v = self.eval_operand(frame_index, a)?;
                let Value::Int(a_i) = a_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_eq",
                        expected: "int",
                        got: a_v.kind(),
                    });
                };
                let b_v = self.eval_operand(frame_index, b)?;
                let Value::Int(b_i) = b_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_eq",
                        expected: "int",
                        got: b_v.kind(),
                    });
                };
                self.stack[frame_index].write_local(*dst, Value::Bool(a_i == b_i))?;
            }
            Instruction::IntNe { dst, a, b } => {
                let a_v = self.eval_operand(frame_index, a)?;
                let Value::Int(a_i) = a_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_ne",
                        expected: "int",
                        got: a_v.kind(),
                    });
                };
                let b_v = self.eval_operand(frame_index, b)?;
                let Value::Int(b_i) = b_v else {
                    return Err(RuntimeError::TypeError {
                        op: "int_ne",
                        expected: "int",
                        got: b_v.kind(),
                    });
                };
                self.stack[frame_index].write_local(*dst, Value::Bool(a_i != b_i))?;
            }

            Instruction::BoolNot { dst, v } => {
                let v = self.eval_operand(frame_index, v)?;
                let Value::Bool(flag) = v else {
                    return Err(RuntimeError::TypeError {
                        op: "bool_not",
                        expected: "bool",
                        got: v.kind(),
                    });
                };
                self.stack[frame_index].write_local(*dst, Value::Bool(!flag))?;
            }
            Instruction::BoolEq { dst, a, b } => {
                let a_v = self.eval_operand(frame_index, a)?;
                let Value::Bool(a_b) = a_v else {
                    return Err(RuntimeError::TypeError {
                        op: "bool_eq",
                        expected: "bool",
                        got: a_v.kind(),
                    });
                };
                let b_v = self.eval_operand(frame_index, b)?;
                let Value::Bool(b_b) = b_v else {
                    return Err(RuntimeError::TypeError {
                        op: "bool_eq",
                        expected: "bool",
                        got: b_v.kind(),
                    });
                };
                self.stack[frame_index].write_local(*dst, Value::Bool(a_b == b_b))?;
            }
            Instruction::BoolNe { dst, a, b } => {
                let a_v = self.eval_operand(frame_index, a)?;
                let Value::Bool(a_b) = a_v else {
                    return Err(RuntimeError::TypeError {
                        op: "bool_ne",
                        expected: "bool",
                        got: a_v.kind(),
                    });
                };
                let b_v = self.eval_operand(frame_index, b)?;
                let Value::Bool(b_b) = b_v else {
                    return Err(RuntimeError::TypeError {
                        op: "bool_ne",
                        expected: "bool",
                        got: b_v.kind(),
                    });
                };
                self.stack[frame_index].write_local(*dst, Value::Bool(a_b != b_b))?;
            }

            Instruction::Call { dst, func, args } => {
                self.metrics.call_instructions = self.metrics.call_instructions.saturating_add(1);
                self.with_evaluated_args(frame_index, args, |interp, arg_values| {
                    interp.call_function_by_name(
                        module,
                        frame_index,
                        *dst,
                        func.as_str(),
                        arg_values,
                    )
                })?;
            }
            Instruction::CallId { dst, func, args } => {
                self.metrics.call_instructions = self.metrics.call_instructions.saturating_add(1);
                self.with_evaluated_args(frame_index, args, |interp, arg_values| {
                    interp.call_target(module, frame_index, *dst, *func, arg_values)
                })?;
            }
            Instruction::ICall { dst, fnptr, args } => {
                self.metrics.icall_instructions = self.metrics.icall_instructions.saturating_add(1);
                let fn_value = self.eval_operand(frame_index, fnptr)?;
                let Value::Function(id) = fn_value else {
                    return Err(RuntimeError::TypeError {
                        op: "icall",
                        expected: "fn reference",
                        got: fn_value.kind(),
                    });
                };
                self.with_evaluated_args(frame_index, args, |interp, arg_values| {
                    interp.call_mir_function(module, frame_index, *dst, id, arg_values)
                })?;
            }
            Instruction::ICallTypeArgs {
                dst,
                fnptr,
                recv,
                method_type_args,
                dict_args,
                args,
            } => {
                self.metrics.icall_instructions = self.metrics.icall_instructions.saturating_add(1);
                let fn_value = self.eval_operand(frame_index, fnptr)?;
                let Value::Function(id) = fn_value else {
                    return Err(RuntimeError::TypeError {
                        op: "icall_type_args",
                        expected: "fn reference",
                        got: fn_value.kind(),
                    });
                };

                let recv = self.eval_operand(frame_index, recv)?;
                let Value::Ref(r) = &recv else {
                    return Err(RuntimeError::TypeError {
                        op: "icall_type_args",
                        expected: "ref(struct|enum)",
                        got: recv.kind(),
                    });
                };
                let type_args = match self.heap_get(r.handle)? {
                    HeapValue::Struct { type_args, .. } => type_args.clone(),
                    HeapValue::Enum { type_args, .. } => type_args.clone(),
                    _ => {
                        return Err(RuntimeError::TypeError {
                            op: "icall_type_args",
                            expected: "ref(struct|enum)",
                            got: ValueKind::Ref,
                        });
                    }
                };

                self.with_value_buffer(|interp, arg_values| {
                    arg_values.clear();
                    arg_values.reserve(
                        type_args.len() + method_type_args.len() + dict_args.len() + args.len() + 1,
                    );

                    for id in type_args {
                        arg_values.push(Value::TypeRep(id));
                    }
                    for op in method_type_args {
                        let id = interp.eval_type_rep_operand(frame_index, op)?;
                        arg_values.push(Value::TypeRep(id));
                    }
                    interp.eval_args_extend(frame_index, dict_args, arg_values)?;
                    arg_values.push(recv);
                    interp.eval_args_extend(frame_index, args, arg_values)?;

                    interp.call_mir_function(module, frame_index, *dst, id, arg_values.as_slice())
                })?;
            }
            Instruction::VCall {
                dst,
                obj,
                method,
                method_type_args,
                dict_args,
                args,
            } => {
                self.metrics.vcall_instructions = self.metrics.vcall_instructions.saturating_add(1);
                let recv = self.eval_operand(frame_index, obj)?;
                let Value::Ref(r) = &recv else {
                    return Err(RuntimeError::TypeError {
                        op: "vcall",
                        expected: "ref(struct|enum)",
                        got: recv.kind(),
                    });
                };
                let (type_name, type_args) = match self.heap_get(r.handle)? {
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
                    _ => {
                        return Err(RuntimeError::TypeError {
                            op: "vcall",
                            expected: "struct|enum",
                            got: ValueKind::Ref,
                        });
                    }
                };
                let lookup_key = (type_name.clone(), method.clone());
                let Some(fn_id) = module.methods.get(&lookup_key).copied() else {
                    return Err(RuntimeError::Trap {
                        message: format!("unresolved vcall method: {method} on {type_name}"),
                    });
                };
                self.with_value_buffer(|interp, arg_values| {
                    arg_values.clear();
                    arg_values.reserve(
                        type_args.len() + method_type_args.len() + dict_args.len() + args.len() + 1,
                    );

                    for id in type_args {
                        arg_values.push(Value::TypeRep(id));
                    }
                    for op in method_type_args {
                        let id = interp.eval_type_rep_operand(frame_index, op)?;
                        arg_values.push(Value::TypeRep(id));
                    }
                    interp.eval_args_extend(frame_index, dict_args, arg_values)?;
                    arg_values.push(recv);
                    interp.eval_args_extend(frame_index, args, arg_values)?;

                    interp.call_mir_function(
                        module,
                        frame_index,
                        *dst,
                        fn_id,
                        arg_values.as_slice(),
                    )
                })?;
            }

            Instruction::PushHandler {
                handler_id: _,
                clauses,
            } => {
                let owner_depth = frame_index;
                // Validate handler blocks have `binds + 1` params and materialize runtime effect ids.
                let func_id = self.stack[frame_index].func;
                let function = Self::function(module, func_id)?;
                let mut runtime_clauses = Vec::with_capacity(clauses.len());
                for clause in clauses {
                    let bind_count = count_binds_in_patterns(&clause.arg_patterns);
                    let block = Self::block(function, clause.target)?;
                    let got = block.params.len();
                    let expected_min = bind_count;
                    let expected_max = bind_count + 1;
                    if got != expected_min && got != expected_max {
                        return Err(RuntimeError::Trap {
                            message: format!(
                                "invalid handler target params for {}.{}: expected {expected_min} or {expected_max}, got {got}",
                                clause.effect.interface, clause.effect.method
                            ),
                        });
                    }

                    let interface_args = clause
                        .effect
                        .interface_args
                        .iter()
                        .map(|op| self.eval_type_rep_operand(frame_index, op))
                        .collect::<Result<Vec<_>, _>>()?;
                    runtime_clauses.push(RuntimeHandlerClause {
                        effect: RuntimeEffectId {
                            interface: clause.effect.interface.clone(),
                            interface_args,
                            method: clause.effect.method.clone(),
                        },
                        arg_patterns: clause.arg_patterns.clone(),
                        target: clause.target,
                    });
                }
                self.handlers.push(HandlerEntry {
                    owner_depth,
                    clauses: runtime_clauses,
                });
            }
            Instruction::PopHandler => {
                let Some(top) = self.handlers.last() else {
                    return Err(RuntimeError::MismatchedPopHandler);
                };
                if top.owner_depth != frame_index {
                    return Err(RuntimeError::MismatchedPopHandler);
                }
                self.handlers.pop();
            }

            Instruction::Perform { dst, effect, args } => {
                self.with_evaluated_args(frame_index, args, |interp, arg_values| {
                    interp.perform_effect(module, frame_index, *dst, effect, arg_values)
                })?;
            }
            Instruction::Resume { dst, k, value } => {
                let k_value = self.eval_operand(frame_index, k)?;
                let Value::Continuation(token) = k_value else {
                    return Err(RuntimeError::InvalidResume);
                };
                let v = self.eval_operand(frame_index, value)?;
                let Some(mut cont) = token.take_state() else {
                    return Err(RuntimeError::InvalidResume);
                };

                if let Some(perform_dst) = cont.perform_dst {
                    let top_index = cont.stack.len().saturating_sub(1);
                    let top_frame = cont
                        .stack
                        .get_mut(top_index)
                        .ok_or(RuntimeError::InvalidResume)?;
                    top_frame.write_local(perform_dst, v)?;
                }

                // Resume by *splicing* the captured stack segment onto the current stack, rather
                // than replacing the entire interpreter state.
                //
                // This is essential for nested effect handlers: an inner continuation may perform
                // effects that are handled by outer handlers, which live below the captured
                // segment. Replacing the whole stack would drop those handlers.
                //
                // We treat the continuation like a function call:
                // - push the captured frames on top of the current stack
                // - shift captured handler owner depths accordingly
                // - patch the bottom-most captured frame to return into the caller's `dst` local
                //
                // When the bottom-most frame returns, the interpreter's normal `return` logic
                // unwinds the captured segment and writes the final value into `dst`.
                let base_depth = self.stack.len();
                debug_assert_eq!(
                    base_depth,
                    frame_index + 1,
                    "resume must execute in the top frame"
                );

                let bottom = cont.stack.first_mut().ok_or(RuntimeError::InvalidResume)?;
                bottom.return_dst = *dst;

                for handler in &mut cont.handlers {
                    handler.owner_depth = handler.owner_depth.saturating_add(base_depth);
                }

                self.stack.extend(cont.stack);
                self.handlers.extend(cont.handlers);
            }
        }
        Ok(())
    }

    fn with_value_buffer<R>(
        &mut self,
        f: impl FnOnce(&mut Self, &mut Vec<Value>) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        let mut buf = self.value_buffers.pop().unwrap_or_default();
        buf.clear();
        let result = f(self, &mut buf);
        buf.clear();
        self.value_buffers.push(buf);
        result
    }

    fn with_evaluated_args<R>(
        &mut self,
        frame_index: usize,
        args: &[Operand],
        mut f: impl FnMut(&mut Self, &[Value]) -> Result<R, RuntimeError>,
    ) -> Result<R, RuntimeError> {
        match args {
            [] => f(self, &[]),
            [a] => {
                let a = self.eval_operand(frame_index, a)?;
                let args = [a];
                f(self, &args)
            }
            [a, b] => {
                let a = self.eval_operand(frame_index, a)?;
                let b = self.eval_operand(frame_index, b)?;
                let args = [a, b];
                f(self, &args)
            }
            _ => self.with_value_buffer(|interp, arg_values| {
                interp.eval_args_into(frame_index, args, arg_values)?;
                f(interp, arg_values.as_slice())
            }),
        }
    }

    fn eval_args_extend(
        &mut self,
        frame_index: usize,
        args: &[Operand],
        out: &mut Vec<Value>,
    ) -> Result<(), RuntimeError> {
        out.reserve(args.len());
        for op in args {
            out.push(self.eval_operand(frame_index, op)?);
        }
        Ok(())
    }

    fn eval_args_into(
        &mut self,
        frame_index: usize,
        args: &[Operand],
        out: &mut Vec<Value>,
    ) -> Result<(), RuntimeError> {
        out.clear();
        self.eval_args_extend(frame_index, args, out)
    }

    fn eval_args(
        &mut self,
        frame_index: usize,
        args: &[Operand],
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut out = Vec::with_capacity(args.len());
        self.eval_args_extend(frame_index, args, &mut out)?;
        Ok(out)
    }

    fn call_target(
        &mut self,
        module: &Module,
        frame_index: usize,
        dst: Option<Local>,
        func: CallTarget,
        args: &[Value],
    ) -> Result<(), RuntimeError> {
        match func {
            CallTarget::Host(id) => {
                if let Some(result) = self.call_core_intrinsic(id, args) {
                    let value = result?;
                    if let Some(dst_local) = dst {
                        self.stack[frame_index].write_local(dst_local, value)?;
                    }
                    return Ok(());
                }

                let host = self
                    .host_import_functions
                    .get(id.0 as usize)
                    .and_then(|slot| slot.as_ref())
                    .cloned();
                let Some(host) = host else {
                    let name = module
                        .host_import(id)
                        .map(|import| import.name.clone())
                        .unwrap_or_else(|| format!("#{idx}", idx = id.0));
                    return Err(RuntimeError::MissingHostFunctions { names: vec![name] });
                };

                self.metrics.host_calls = self.metrics.host_calls.saturating_add(1);
                let value = host(self, args)?;
                if let Some(dst_local) = dst {
                    self.stack[frame_index].write_local(dst_local, value)?;
                }
                Ok(())
            }
            CallTarget::Mir(id) => self.call_mir_function(module, frame_index, dst, id, args),
        }
    }

    fn call_core_intrinsic(
        &self,
        id: HostImportId,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let intrinsics = &self.core_intrinsic_ids;

        if intrinsics.bool_not == Some(id) {
            return Some(match args {
                [Value::Bool(v)] => Ok(Value::Bool(!v)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::bool_not: bad args: {other:?}"),
                }),
            });
        }

        if intrinsics.bool_eq == Some(id) {
            return Some(match args {
                [Value::Bool(a), Value::Bool(b)] => Ok(Value::Bool(a == b)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::bool_eq: bad args: {other:?}"),
                }),
            });
        }

        if intrinsics.bool_ne == Some(id) {
            return Some(match args {
                [Value::Bool(a), Value::Bool(b)] => Ok(Value::Bool(a != b)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::bool_ne: bad args: {other:?}"),
                }),
            });
        }

        if intrinsics.int_add == Some(id) {
            return Some(match args {
                [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a + b)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::int_add: bad args: {other:?}"),
                }),
            });
        }

        if intrinsics.int_sub == Some(id) {
            return Some(match args {
                [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a - b)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::int_sub: bad args: {other:?}"),
                }),
            });
        }

        if intrinsics.int_mul == Some(id) {
            return Some(match args {
                [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a * b)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::int_mul: bad args: {other:?}"),
                }),
            });
        }

        if intrinsics.int_div == Some(id) {
            return Some(match args {
                [Value::Int(_), Value::Int(0)] => Err(RuntimeError::Trap {
                    message: "core::intrinsics::int_div: division by zero".to_string(),
                }),
                [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a / b)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::int_div: bad args: {other:?}"),
                }),
            });
        }

        if intrinsics.int_mod == Some(id) {
            return Some(match args {
                [Value::Int(_), Value::Int(0)] => Err(RuntimeError::Trap {
                    message: "core::intrinsics::int_mod: modulo by zero".to_string(),
                }),
                [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a % b)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::int_mod: bad args: {other:?}"),
                }),
            });
        }

        if intrinsics.int_eq == Some(id) {
            return Some(match args {
                [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a == b)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::int_eq: bad args: {other:?}"),
                }),
            });
        }

        if intrinsics.int_ne == Some(id) {
            return Some(match args {
                [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a != b)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::int_ne: bad args: {other:?}"),
                }),
            });
        }

        if intrinsics.int_lt == Some(id) {
            return Some(match args {
                [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a < b)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::int_lt: bad args: {other:?}"),
                }),
            });
        }

        if intrinsics.int_le == Some(id) {
            return Some(match args {
                [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a <= b)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::int_le: bad args: {other:?}"),
                }),
            });
        }

        if intrinsics.int_gt == Some(id) {
            return Some(match args {
                [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a > b)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::int_gt: bad args: {other:?}"),
                }),
            });
        }

        if intrinsics.int_ge == Some(id) {
            return Some(match args {
                [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a >= b)),
                other => Err(RuntimeError::Trap {
                    message: format!("core::intrinsics::int_ge: bad args: {other:?}"),
                }),
            });
        }

        None
    }

    fn call_mir_function(
        &mut self,
        module: &Module,
        frame_index: usize,
        dst: Option<Local>,
        func: FunctionId,
        args: &[Value],
    ) -> Result<(), RuntimeError> {
        self.metrics.mir_calls = self.metrics.mir_calls.saturating_add(1);
        let callee = Self::function(module, func)?;
        let mut new_frame = Frame::new(func, callee.locals, dst);
        self.init_params(&mut new_frame, callee, args)?;
        self.stack.push(new_frame);
        let _ = frame_index;
        Ok(())
    }

    fn call_function_by_name(
        &mut self,
        module: &Module,
        frame_index: usize,
        dst: Option<Local>,
        func: &str,
        args: &[Value],
    ) -> Result<(), RuntimeError> {
        if let Some(host) = self.host_functions.get(func).cloned() {
            self.metrics.host_calls = self.metrics.host_calls.saturating_add(1);
            let value = host(self, args)?;
            if let Some(dst_local) = dst {
                self.stack[frame_index].write_local(dst_local, value)?;
            }
            return Ok(());
        }

        if let Some(id) = module.function_id(func) {
            return self.call_mir_function(module, frame_index, dst, id, args);
        }

        if let Some(id) = module.host_import_id(func) {
            return self.call_target(module, frame_index, dst, CallTarget::Host(id), args);
        }

        Err(RuntimeError::UnknownFunction {
            name: func.to_string(),
        })
    }

    fn perform_effect(
        &mut self,
        module: &Module,
        frame_index: usize,
        dst: Option<Local>,
        effect: &EffectSpec,
        args: &[Value],
    ) -> Result<(), RuntimeError> {
        let interface_args = effect
            .interface_args
            .iter()
            .map(|op| self.eval_type_rep_operand(frame_index, op))
            .collect::<Result<Vec<_>, _>>()?;
        let effect_id = RuntimeEffectId {
            interface: effect.interface.clone(),
            interface_args,
            method: effect.method.clone(),
        };

        let Some((handler_index, clause_index, binds)) =
            self.find_handler_for_effect(&effect_id, args)?
        else {
            return Err(RuntimeError::UnhandledEffect {
                interface: effect_id.interface.clone(),
                method: effect_id.method.clone(),
            });
        };

        let handler_owner_depth = self.handlers[handler_index].owner_depth;

        // Capture a delimited continuation: the owning frame of the selected handler becomes
        // the bottom-most frame of the captured computation. This makes `resume` return when
        // that owning frame returns, rather than when the entire original program returns.
        //
        // This is essential to compile source-level effect handlers that are scoped to an
        // expression (e.g. Rusk `match` effect arms).
        let mut captured_stack = self.stack[handler_owner_depth..].to_vec();
        let captured_handlers = self
            .handlers
            .iter()
            .filter_map(|entry| {
                let owner_depth = entry.owner_depth.checked_sub(handler_owner_depth)?;
                Some(HandlerEntry {
                    owner_depth,
                    clauses: entry.clauses.clone(),
                })
            })
            .collect::<Vec<_>>();

        if let Some(dst_local) = dst {
            // Ensure the destination is uninitialized in the captured state until resume injects it.
            let top = captured_stack
                .last_mut()
                .ok_or(RuntimeError::UnhandledEffect {
                    interface: effect_id.interface.clone(),
                    method: effect_id.method.clone(),
                })?;
            top.clear_local(dst_local)?;
        }

        let token = ContinuationToken::new(ContinuationState {
            stack: captured_stack,
            handlers: captured_handlers,
            perform_dst: dst,
        });

        // Unwind frames to the handler's owning frame.
        self.stack.truncate(handler_owner_depth + 1);
        // Unwind handlers to the selected handler entry.
        self.handlers.truncate(handler_index + 1);

        // Transfer control to handler block in the now-top frame.
        let handler_frame_index = self.stack.len() - 1;
        let handler_func_id = self.stack[handler_frame_index].func;
        let function = Self::function(module, handler_func_id)?;
        let clause = &self.handlers[handler_index].clauses[clause_index];
        let handler_block = Self::block(function, clause.target)?;
        let got_params = handler_block.params.len();
        let expected_min = binds.len();
        let expected_max = binds.len() + 1;
        if got_params != expected_min && got_params != expected_max {
            return Err(RuntimeError::InvalidBlockArgs {
                target: clause.target,
                expected: expected_max,
                got: got_params,
            });
        }

        let mut block_args = binds;
        if got_params == expected_max {
            // Capturing handler clause: pass the continuation token as the last param.
            block_args.push(Value::Continuation(token));
        }
        self.enter_block(module, handler_frame_index, clause.target, block_args)?;
        Ok(())
    }

    fn find_handler_for_effect(
        &mut self,
        effect: &RuntimeEffectId,
        args: &[Value],
    ) -> Result<Option<(usize, usize, Vec<Value>)>, RuntimeError> {
        for (handler_index, handler) in self.handlers.iter().enumerate().rev() {
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
                    if !match_pattern(
                        &mut self.heap,
                        &mut self.allocations_since_gc,
                        &self.struct_layouts,
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

    fn enter_block(
        &mut self,
        module: &Module,
        frame_index: usize,
        target: BlockId,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        self.metrics.block_entries = self.metrics.block_entries.saturating_add(1);
        let func_id = self.stack[frame_index].func;
        let function = Self::function(module, func_id)?;
        let block = Self::block(function, target)?;

        if args.len() != block.params.len() {
            return Err(RuntimeError::InvalidBlockArgs {
                target,
                expected: block.params.len(),
                got: args.len(),
            });
        }

        for (param, value) in block.params.iter().copied().zip(args.into_iter()) {
            self.stack[frame_index].write_local(param, value)?;
        }

        self.stack[frame_index].block = target;
        self.stack[frame_index].ip = 0;
        Ok(())
    }

    fn execute_terminator(
        &mut self,
        module: &Module,
        frame_index: usize,
        term: &Terminator,
    ) -> Result<Option<Value>, RuntimeError> {
        match term {
            Terminator::Br { target, args } => {
                self.metrics.br_terminators = self.metrics.br_terminators.saturating_add(1);
                let vals = self.eval_args(frame_index, args)?;
                self.enter_block(module, frame_index, *target, vals)?;
                Ok(None)
            }
            Terminator::CondBr {
                cond,
                then_target,
                then_args,
                else_target,
                else_args,
            } => {
                self.metrics.cond_br_terminators =
                    self.metrics.cond_br_terminators.saturating_add(1);
                let c = self.eval_operand(frame_index, cond)?;
                let Value::Bool(flag) = c else {
                    return Err(RuntimeError::TypeError {
                        op: "cond_br",
                        expected: "bool",
                        got: c.kind(),
                    });
                };
                let (target, args) = if flag {
                    (*then_target, then_args.as_slice())
                } else {
                    (*else_target, else_args.as_slice())
                };
                let vals = self.eval_args(frame_index, args)?;
                self.enter_block(module, frame_index, target, vals)?;
                Ok(None)
            }
            Terminator::Switch {
                value,
                cases,
                default,
            } => {
                self.metrics.switch_terminators = self.metrics.switch_terminators.saturating_add(1);
                let scrutinee = self.eval_operand(frame_index, value)?;
                for SwitchCase { pattern, target } in cases {
                    let mut binds = Vec::new();
                    if match_pattern(
                        &mut self.heap,
                        &mut self.allocations_since_gc,
                        &self.struct_layouts,
                        pattern,
                        &scrutinee,
                        &mut binds,
                    )? {
                        self.enter_block(module, frame_index, *target, binds)?;
                        return Ok(None);
                    }
                }
                self.enter_block(module, frame_index, *default, Vec::new())?;
                Ok(None)
            }
            Terminator::Return { value } => {
                self.metrics.return_terminators = self.metrics.return_terminators.saturating_add(1);
                let v = self.eval_operand(frame_index, value)?;
                let returning = self
                    .stack
                    .pop()
                    .expect("frame_index points at the top frame");
                self.unwind_handlers_to_stack_len(self.stack.len());

                let Some(caller) = self.stack.last_mut() else {
                    return Ok(Some(v));
                };
                if let Some(dst_local) = returning.return_dst {
                    caller.write_local(dst_local, v)?;
                }
                Ok(None)
            }
            Terminator::Trap { message } => {
                self.metrics.trap_terminators = self.metrics.trap_terminators.saturating_add(1);
                Err(RuntimeError::Trap {
                    message: message.clone(),
                })
            }
        }
    }

    fn unwind_handlers_to_stack_len(&mut self, new_len: usize) {
        while self
            .handlers
            .last()
            .is_some_and(|h| h.owner_depth >= new_len)
        {
            self.handlers.pop();
        }
    }
}

#[derive(Clone)]
struct Frame {
    func: FunctionId,
    block: BlockId,
    ip: usize,
    locals: Vec<Option<Value>>,
    return_dst: Option<Local>,
}

impl Frame {
    fn new(func: FunctionId, locals_count: usize, return_dst: Option<Local>) -> Self {
        let locals = vec![None; locals_count];
        Self {
            func,
            block: BlockId(0),
            ip: 0,
            locals,
            return_dst,
        }
    }

    fn read_local(&self, local: Local) -> Result<Value, RuntimeError> {
        let Some(slot) = self.locals.get(local.0) else {
            return Err(RuntimeError::InvalidLocal {
                local,
                locals: self.locals.len(),
            });
        };
        slot.clone()
            .ok_or(RuntimeError::UninitializedLocal { local })
    }

    fn take_local(&mut self, local: Local) -> Result<Value, RuntimeError> {
        let Some(slot) = self.locals.get_mut(local.0) else {
            return Err(RuntimeError::InvalidLocal {
                local,
                locals: self.locals.len(),
            });
        };
        slot.take()
            .ok_or(RuntimeError::UninitializedLocal { local })
    }

    fn clear_local(&mut self, local: Local) -> Result<(), RuntimeError> {
        let Some(slot) = self.locals.get_mut(local.0) else {
            return Err(RuntimeError::InvalidLocal {
                local,
                locals: self.locals.len(),
            });
        };
        *slot = None;
        Ok(())
    }

    fn write_local(&mut self, local: Local, value: Value) -> Result<(), RuntimeError> {
        let Some(slot) = self.locals.get_mut(local.0) else {
            return Err(RuntimeError::InvalidLocal {
                local,
                locals: self.locals.len(),
            });
        };
        *slot = Some(value);
        Ok(())
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
    arg_patterns: Vec<Pattern>,
    target: BlockId,
}

#[derive(Clone)]
struct HandlerEntry {
    owner_depth: usize,
    clauses: Vec<RuntimeHandlerClause>,
}

struct InterpreterRoots<'a> {
    stack: &'a [Frame],
    roots: &'a [RootSlot],
}

impl Trace for InterpreterRoots<'_> {
    fn trace(&self, tracer: &mut dyn Tracer) {
        for frame in self.stack {
            frame.trace(tracer);
        }
        for slot in self.roots {
            if let Some(value) = slot.value.as_ref() {
                value.trace(tracer);
            }
        }
    }
}

impl Trace for Frame {
    fn trace(&self, tracer: &mut dyn Tracer) {
        for value in self.locals.iter().flatten() {
            value.trace(tracer);
        }
    }
}

impl Trace for Value {
    fn trace(&self, tracer: &mut dyn Tracer) {
        match self {
            Value::Ref(r) => tracer.mark(r.handle),
            Value::Continuation(k) => k.trace(tracer),
            Value::Unit
            | Value::Bool(_)
            | Value::Int(_)
            | Value::Float(_)
            | Value::String(_)
            | Value::Bytes(_)
            | Value::TypeRep(_)
            | Value::Function(_) => {}
        }
    }
}

impl Trace for HeapValue {
    fn trace(&self, tracer: &mut dyn Tracer) {
        match self {
            HeapValue::Struct { fields, .. } => fields.iter().for_each(|value| value.trace(tracer)),
            HeapValue::Array(items) => {
                for value in items {
                    value.trace(tracer);
                }
            }
            HeapValue::Tuple(items) => {
                for value in items {
                    value.trace(tracer);
                }
            }
            HeapValue::Enum { fields, .. } => {
                for value in fields {
                    value.trace(tracer);
                }
            }
        }
    }
}

impl Trace for ContinuationToken {
    fn trace(&self, tracer: &mut dyn Tracer) {
        let guard = self.0.borrow();
        let Some(state) = guard.state.as_ref() else {
            return;
        };
        state.trace(tracer);
    }
}

impl Trace for ContinuationState {
    fn trace(&self, tracer: &mut dyn Tracer) {
        for frame in &self.stack {
            frame.trace(tracer);
        }
    }
}

fn count_binds_in_patterns(patterns: &[Pattern]) -> usize {
    patterns.iter().map(count_binds_in_pattern).sum()
}

fn count_binds_in_pattern(p: &Pattern) -> usize {
    match p {
        Pattern::Wildcard => 0,
        Pattern::Bind => 1,
        Pattern::Literal(_) => 0,
        Pattern::Tuple {
            prefix,
            rest,
            suffix,
        } => {
            prefix.iter().map(count_binds_in_pattern).sum::<usize>()
                + rest.as_deref().map(count_binds_in_pattern).unwrap_or(0)
                + suffix.iter().map(count_binds_in_pattern).sum::<usize>()
        }
        Pattern::Enum { fields, .. } => fields.iter().map(count_binds_in_pattern).sum(),
        Pattern::Struct { fields, .. } => {
            fields.iter().map(|(_, p)| count_binds_in_pattern(p)).sum()
        }
        Pattern::Array {
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

fn match_pattern<GC: GcHeap<HeapValue>>(
    heap: &mut GC,
    allocations_since_gc: &mut usize,
    struct_layouts: &HashMap<String, StructLayout>,
    pat: &Pattern,
    value: &Value,
    binds: &mut Vec<Value>,
) -> Result<bool, RuntimeError> {
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
            (ConstValue::FunctionId(a), Value::Function(b)) => a == b,
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
            let (e, v, actual_fields) = match heap
                .get(r.handle)
                .ok_or(RuntimeError::DanglingRef { handle: r.handle })?
            {
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
                if !match_pattern(
                    heap,
                    allocations_since_gc,
                    struct_layouts,
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
            let actual_items = match heap
                .get(r.handle)
                .ok_or(RuntimeError::DanglingRef { handle: r.handle })?
            {
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
                    heap,
                    allocations_since_gc,
                    struct_layouts,
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
                            *allocations_since_gc = allocations_since_gc.saturating_add(1);
                            let handle = heap.alloc(HeapValue::Tuple(slice));
                            Value::Ref(RefValue::new(handle))
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
                    heap,
                    allocations_since_gc,
                    struct_layouts,
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
            let (actual_ty, actual_fields) = match heap
                .get(r.handle)
                .ok_or(RuntimeError::DanglingRef { handle: r.handle })?
            {
                HeapValue::Struct {
                    type_name, fields, ..
                } => (type_name.clone(), fields.clone()),
                _ => return Ok(false),
            };
            if actual_ty != *type_name {
                return Ok(false);
            }
            let Some(layout) = struct_layouts.get(actual_ty.as_str()) else {
                return Err(RuntimeError::Trap {
                    message: format!("missing struct layout for `{actual_ty}`"),
                });
            };
            for (field_name, field_pat) in fields.iter() {
                let Some(idx) = layout.field_index(field_name.as_str()) else {
                    return Err(RuntimeError::MissingField {
                        field: field_name.clone(),
                    });
                };
                let Some(field_value) = actual_fields.get(idx) else {
                    return Err(RuntimeError::MissingField {
                        field: field_name.clone(),
                    });
                };
                let field_value = if r.readonly {
                    field_value.clone().into_readonly_view()
                } else {
                    field_value.clone()
                };
                if !match_pattern(
                    heap,
                    allocations_since_gc,
                    struct_layouts,
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
            let actual_items = match heap
                .get(r.handle)
                .ok_or(RuntimeError::DanglingRef { handle: r.handle })?
            {
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
                    heap,
                    allocations_since_gc,
                    struct_layouts,
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
                        *allocations_since_gc = allocations_since_gc.saturating_add(1);
                        let handle = heap.alloc(HeapValue::Array(slice));
                        binds.push(Value::Ref(RefValue::new(handle)));
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
                    heap,
                    allocations_since_gc,
                    struct_layouts,
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
