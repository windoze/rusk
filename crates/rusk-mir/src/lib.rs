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
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
pub struct Local(pub usize);

/// A basic block index within a MIR function.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
pub struct BlockId(pub usize);

/// A MIR module: a set of functions plus optional method-resolution metadata.
#[derive(Clone, Debug, Default)]
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
pub struct Module {
    /// MIR functions by name.
    pub functions: BTreeMap<String, Function>,

    /// Optional virtual-call resolution: `(type_name, method_name) -> function_name`.
    pub methods: BTreeMap<(String, String), String>,

    /// Optional interface implementation metadata: `(type_name -> {interface_name...})`.
    ///
    /// Used by checked casts / runtime type tests for `interface` targets.
    pub interface_impls: BTreeMap<String, BTreeSet<String>>,
}

/// A MIR function body.
#[derive(Clone, Debug)]
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
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
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
pub struct Param {
    pub local: Local,
    pub mutability: Mutability,
    pub ty: Option<Type>,
}

/// Parameter / binding mutability.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
pub enum Mutability {
    Mutable,
    Readonly,
}

/// Optional type annotation.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
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

/// A compile-time type constructor used to build runtime [`Type::TypeRep`] values.
///
/// Applied types are constructed via [`Instruction::MakeTypeRep`].
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
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
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
pub struct BasicBlock {
    pub label: String,
    pub params: Vec<Local>,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

/// An operand: a local or a literal.
#[derive(Clone, Debug)]
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
pub enum Operand {
    Local(Local),
    Literal(ConstValue),
}

/// A literal value embedded in MIR.
///
/// Composite literals allocate fresh runtime objects when evaluated.
#[derive(Clone, Debug, PartialEq)]
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
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
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
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
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
pub struct EffectSpec {
    pub interface: String,
    pub interface_args: Vec<Operand>,
    pub method: String,
}

/// A handler clause for a single effect.
#[derive(Clone, Debug)]
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
pub struct HandlerClause {
    pub effect: EffectSpec,
    pub arg_patterns: Vec<Pattern>,
    pub target: BlockId,
}

/// A MIR instruction.
#[derive(Clone, Debug)]
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
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
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize)]
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
#[cfg(feature = "serde")]
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct SwitchCase {
    pub pattern: Pattern,
    pub target: BlockId,
}
