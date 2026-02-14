#![forbid(unsafe_code)]
#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;
use alloc::{
    boxed::Box,
    collections::{BTreeMap, BTreeSet},
    string::String,
    vec::Vec,
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A local slot index within a MIR function.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Local(pub usize);

/// A stable identifier for a MIR function within a [`Module`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FunctionId(pub u32);

/// A basic block index within a MIR function.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BlockId(pub usize);

/// A stable identifier for a declared host import within a [`Module`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct HostImportId(pub u32);

/// A fully-resolved call target.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum CallTarget {
    Mir(FunctionId),
    Host(HostImportId),
}

/// A MIR module: a set of functions plus optional method-resolution metadata.
#[derive(Clone, Debug, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Module {
    /// MIR functions in declaration order.
    ///
    /// Runtime execution uses `FunctionId` indexing into this table rather than string-keyed map
    /// lookups on hot paths.
    pub functions: Vec<Function>,

    /// Name → [`FunctionId`] map for `run_function("name", ...)` and diagnostics.
    #[cfg_attr(feature = "serde", serde(default))]
    pub function_ids: BTreeMap<String, FunctionId>,

    /// Optional virtual-call resolution: `(type_name, method_name) -> FunctionId`.
    pub methods: BTreeMap<(String, String), FunctionId>,

    /// Optional interface implementation metadata: `(type_name -> {interface_name...})`.
    ///
    /// Used by checked casts / runtime type tests for `interface` targets.
    pub interface_impls: BTreeMap<String, BTreeSet<String>>,

    /// Optional struct field layouts: `type_name -> [field_name...]` in storage order.
    ///
    /// The interpreter can use this to allocate and access struct objects via field indices
    /// (backed by `Vec<Value>`) rather than map lookups on hot paths.
    #[cfg_attr(feature = "serde", serde(default))]
    pub struct_layouts: BTreeMap<String, Vec<String>>,

    /// Declared host function imports required by this module.
    ///
    /// Execution uses `HostImportId` indexing into this table (see [`CallTarget::Host`]).
    #[cfg_attr(feature = "serde", serde(default))]
    pub host_imports: Vec<HostImport>,

    /// Name → [`HostImportId`] map for validation and diagnostics.
    #[cfg_attr(feature = "serde", serde(default))]
    pub host_import_ids: BTreeMap<String, HostImportId>,
}

impl Module {
    /// Inserts a function into the module, returning its assigned [`FunctionId`].
    ///
    /// This rejects duplicate names.
    pub fn add_function(&mut self, func: Function) -> Result<FunctionId, String> {
        if self.function_ids.contains_key(func.name.as_str()) {
            return Err(format!("duplicate function `{}`", func.name));
        }
        let id_u32: u32 = self
            .functions
            .len()
            .try_into()
            .map_err(|_| "function table overflow".to_string())?;
        let id = FunctionId(id_u32);
        self.function_ids.insert(func.name.clone(), id);
        self.functions.push(func);
        Ok(id)
    }

    /// Returns the [`FunctionId`] for `name` if the module defines it.
    pub fn function_id(&self, name: &str) -> Option<FunctionId> {
        self.function_ids.get(name).copied()
    }

    /// Returns the function for `id` if it is valid.
    pub fn function(&self, id: FunctionId) -> Option<&Function> {
        self.functions.get(id.0 as usize)
    }

    /// Inserts a host import into the module, returning its assigned [`HostImportId`].
    ///
    /// This rejects duplicate names.
    pub fn add_host_import(&mut self, import: HostImport) -> Result<HostImportId, String> {
        if self.host_import_ids.contains_key(import.name.as_str()) {
            return Err(format!("duplicate host import `{}`", import.name));
        }
        let id_u32: u32 = self
            .host_imports
            .len()
            .try_into()
            .map_err(|_| "host import table overflow".to_string())?;
        let id = HostImportId(id_u32);
        self.host_import_ids.insert(import.name.clone(), id);
        self.host_imports.push(import);
        Ok(id)
    }

    /// Returns the [`HostImportId`] for `name` if the module declares it.
    pub fn host_import_id(&self, name: &str) -> Option<HostImportId> {
        self.host_import_ids.get(name).copied()
    }

    /// Returns the host import for `id` if it is valid.
    pub fn host_import(&self, id: HostImportId) -> Option<&HostImport> {
        self.host_imports.get(id.0 as usize)
    }
}

/// A MIR function body.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub ret_type: Option<Type>,
    /// The number of local slots in this function.
    pub locals: usize,
    /// Basic blocks in textual order; `blocks[0]` is the entry block.
    pub blocks: Vec<BasicBlock>,
}

impl Function {
    /// Returns the entry block id (`blocks[0]`).
    pub fn entry_block(&self) -> BlockId {
        BlockId(0)
    }
}

/// A function parameter.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Param {
    pub local: Local,
    pub mutability: Mutability,
    pub ty: Option<Type>,
}

/// Parameter / binding mutability.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Mutability {
    Mutable,
    Readonly,
}

/// Optional type annotation.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Type {
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
    Fn,
    Cont,
    Interface(String),
    TypeRep,
}

/// A host function signature (parameter and return types).
///
/// This is a *host ABI surface* description for tooling and validation, not the full Rusk type
/// system. It is intentionally small and (in v0.1) monomorphic.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct HostFnSig {
    pub params: Vec<HostType>,
    pub ret: HostType,
}

/// A declared host import entry in a [`Module`].
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct HostImport {
    pub name: String,
    pub sig: HostFnSig,
}

/// A host ABI type used in [`HostFnSig`].
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum HostType {
    /// A dynamically typed value. Used when a more precise type is not available.
    Any,
    Unit,
    Bool,
    Int,
    Float,
    String,
    Bytes,
    /// A runtime type representation (`typerep`), used by some core intrinsics.
    TypeRep,
    Array(Box<HostType>),
    Tuple(Vec<HostType>),
}

/// A compile-time type constructor used to build runtime [`Type::TypeRep`] values.
///
/// Applied types are constructed via [`Instruction::MakeTypeRep`].
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum TypeRepLit {
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

/// A basic block: params, instructions, and a terminator.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BasicBlock {
    pub label: String,
    pub params: Vec<Local>,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

/// An operand: a local or a literal.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Operand {
    Local(Local),
    Literal(ConstValue),
}

/// A literal value embedded in MIR.
///
/// Composite literals allocate fresh runtime objects when evaluated.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ConstValue {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
    /// An internal runtime type representation (used for reified generics).
    TypeRep(TypeRepLit),
    /// A first-class reference to a named MIR function.
    Function(String),
    /// A first-class reference to a MIR function id.
    FunctionId(FunctionId),
    Array(Vec<ConstValue>),
    Tuple(Vec<ConstValue>),
    Struct {
        type_name: String,
        /// Fields in source/textual order.
        fields: Vec<(String, ConstValue)>,
    },
    Enum {
        enum_name: String,
        variant: String,
        fields: Vec<ConstValue>,
    },
}

/// A pattern for `switch` and effect handler clauses.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Pattern {
    /// `_`
    Wildcard,
    /// A binding site (textual MIR typically writes this as a local, e.g. `%msg`).
    Bind,
    /// A primitive literal match (unit/bool/int/float/string/bytes).
    Literal(ConstValue),
    /// Tuple destructuring: `(p1, p2, .., pk)` / `(p1, p2)` / `(..rest)`.
    ///
    /// If `rest` is:
    /// - `None`: arity must match exactly (`prefix.len() + suffix.len()`).
    /// - `Some(Pattern::Wildcard)`: variable-length match, rest ignored.
    /// - `Some(Pattern::Bind)`: variable-length match, rest captured as a tuple.
    Tuple {
        prefix: Vec<Pattern>,
        rest: Option<Box<Pattern>>,
        suffix: Vec<Pattern>,
    },
    /// `Enum::Variant(p1, p2, ...)`
    Enum {
        enum_name: String,
        variant: String,
        fields: Vec<Pattern>,
    },
    /// `Type { field: pat, ... }`
    Struct {
        type_name: String,
        /// Fields in source/textual order.
        fields: Vec<(String, Pattern)>,
    },
    /// Array destructuring: `[p1, p2, .., pk]` / `[p1, p2]` / `[..rest]`.
    ///
    /// If `rest` is:
    /// - `None`: length must match exactly (`prefix.len() + suffix.len()`).
    /// - `Some(Pattern::Wildcard)`: variable-length match, rest ignored.
    /// - `Some(Pattern::Bind)`: variable-length match, rest captured as a new array.
    Array {
        prefix: Vec<Pattern>,
        rest: Option<Box<Pattern>>,
        suffix: Vec<Pattern>,
    },
}

/// An effect operation identifier (interface + method), extended with instantiated interface type
/// arguments.
///
/// The `interface_args` are runtime `TypeRep` values represented as operands; they are evaluated
/// when pushing a handler and when performing an effect.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct EffectSpec {
    pub interface: String,
    pub interface_args: Vec<Operand>,
    pub method: String,
}

/// A handler clause for a single effect.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct HandlerClause {
    pub effect: EffectSpec,
    pub arg_patterns: Vec<Pattern>,
    pub target: BlockId,
}

/// A MIR instruction.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Instruction {
    Const {
        dst: Local,
        value: ConstValue,
    },
    Copy {
        dst: Local,
        src: Local,
    },
    Move {
        dst: Local,
        src: Local,
    },
    AsReadonly {
        dst: Local,
        src: Local,
    },
    IsType {
        dst: Local,
        value: Operand,
        ty: Operand,
    },
    CheckedCast {
        dst: Local,
        value: Operand,
        ty: Operand,
    },

    MakeTypeRep {
        dst: Local,
        base: TypeRepLit,
        args: Vec<Operand>,
    },

    MakeStruct {
        dst: Local,
        type_name: String,
        type_args: Vec<Operand>,
        /// Fields in source/textual order.
        fields: Vec<(String, Operand)>,
    },
    MakeArray {
        dst: Local,
        /// Elements in source/textual order.
        items: Vec<Operand>,
    },
    MakeTuple {
        dst: Local,
        /// Elements in source/textual order.
        items: Vec<Operand>,
    },
    MakeEnum {
        dst: Local,
        enum_name: String,
        type_args: Vec<Operand>,
        variant: String,
        /// Fields in source/textual order.
        fields: Vec<Operand>,
    },
    GetField {
        dst: Local,
        obj: Operand,
        field: String,
    },
    SetField {
        obj: Operand,
        field: String,
        value: Operand,
    },

    StructGet {
        dst: Local,
        obj: Operand,
        idx: usize,
    },
    StructSet {
        obj: Operand,
        idx: usize,
        value: Operand,
    },

    TupleGet {
        dst: Local,
        tup: Operand,
        idx: usize,
    },
    TupleSet {
        tup: Operand,
        idx: usize,
        value: Operand,
    },

    IndexGet {
        dst: Local,
        arr: Operand,
        idx: Operand,
    },
    IndexSet {
        arr: Operand,
        idx: Operand,
        value: Operand,
    },
    Len {
        dst: Local,
        arr: Operand,
    },

    Call {
        dst: Option<Local>,
        func: String,
        args: Vec<Operand>,
    },
    CallId {
        dst: Option<Local>,
        func: CallTarget,
        args: Vec<Operand>,
    },
    VCall {
        dst: Option<Local>,
        obj: Operand,
        method: String,
        /// Runtime `TypeRep` arguments for method-level generics.
        ///
        /// Type arguments coming from the receiver's nominal type (impl/header generics) are
        /// derived from the receiver value at runtime.
        method_type_args: Vec<Operand>,
        args: Vec<Operand>,
    },
    ICall {
        dst: Option<Local>,
        fnptr: Operand,
        args: Vec<Operand>,
    },

    PushHandler {
        handler_id: String,
        clauses: Vec<HandlerClause>,
    },
    PopHandler,

    Perform {
        dst: Option<Local>,
        effect: EffectSpec,
        args: Vec<Operand>,
    },
    Resume {
        dst: Option<Local>,
        k: Operand,
        value: Operand,
    },
}

/// A terminator instruction.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Terminator {
    Br {
        target: BlockId,
        args: Vec<Operand>,
    },
    CondBr {
        cond: Operand,
        then_target: BlockId,
        then_args: Vec<Operand>,
        else_target: BlockId,
        else_args: Vec<Operand>,
    },
    Switch {
        value: Operand,
        cases: Vec<SwitchCase>,
        default: BlockId,
    },
    Return {
        value: Operand,
    },
    Trap {
        message: String,
    },
}

/// A single `switch` case.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct SwitchCase {
    pub pattern: Pattern,
    pub target: BlockId,
}
