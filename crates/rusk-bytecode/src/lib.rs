#![forbid(unsafe_code)]
#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use alloc::boxed::Box;
use alloc::collections::{BTreeMap, BTreeSet};
use alloc::string::String;
use alloc::vec::Vec;

pub mod opt;
pub mod rbc;
pub mod verify;

pub use opt::{OptError, OptLevel, peephole_optimize_module};
pub use rbc::{DecodeError, EncodeError, from_bytes, to_bytes};
pub use verify::{VerifyError, verify_module};

/// A stable identifier for a bytecode function within an [`ExecutableModule`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct FunctionId(pub u32);

/// A stable identifier for a declared host import within an [`ExecutableModule`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct HostImportId(pub u32);

/// A stable identifier for an externalized effect operation within an [`ExecutableModule`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct EffectId(pub u32);

/// A VM/host boundary ABI type (v0): builtin primitives only.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AbiType {
    Unit,
    Bool,
    Int,
    Float,
    String,
    Bytes,
}

/// A host import signature restricted to [`AbiType`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HostFnSig {
    pub params: Vec<AbiType>,
    pub ret: AbiType,
}

/// A declared host import entry required by an [`ExecutableModule`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HostImport {
    pub name: String,
    pub sig: HostFnSig,
}

/// An externalized effect operation declaration (v0: non-generic interface + method).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternalEffectDecl {
    pub interface: String,
    pub method: String,
    pub sig: HostFnSig,
}

/// A compile-time type constructor used to build runtime `typerep` values.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeRepLit {
    Unit,
    Bool,
    Int,
    Float,
    Byte,
    Char,
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

/// A literal value embedded in bytecode (v0: ABI-safe primitives only).
#[derive(Clone, Debug, PartialEq)]
pub enum ConstValue {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
    TypeRep(TypeRepLit),
    Function(FunctionId),
}

/// A bytecode register index.
pub type Reg = u32;

/// A VM-internal core intrinsic (compiler desugaring target).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Intrinsic {
    // f-string helpers.
    StringConcat,
    ToString,
    Panic,

    // Booleans.
    BoolNot,
    BoolEq,
    BoolNe,

    // Int arithmetic & comparisons.
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntMod,
    IntEq,
    IntNe,
    IntLt,
    IntLe,
    IntGt,
    IntGe,

    // Float arithmetic & comparisons.
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    FloatMod,
    FloatEq,
    FloatNe,
    FloatLt,
    FloatLe,
    FloatGt,
    FloatGe,

    // Primitive equality helpers.
    StringEq,
    StringNe,
    BytesEq,
    BytesNe,
    UnitEq,
    UnitNe,

    // Iterator protocol.
    IntoIter,
    Next,
    StringIntoIter,
    StringNext,
    BytesIntoIter,
    BytesNext,

    // Array operations.
    ArrayLen,
    ArrayLenRo,
    ArrayPush,
    ArrayPop,
    ArrayClear,
    ArrayResize,
    ArrayInsert,
    ArrayRemove,
    ArrayExtend,
    ArrayConcat,
    ArrayConcatRo,
    ArraySlice,
    ArraySliceRo,

    // `byte` / `char` primitives.
    IntToByte,
    IntTryByte,
    ByteToInt,
    IntToChar,
    IntTryChar,
    CharToInt,

    // `bytes` operations.
    BytesGet,
    BytesSlice,
    BytesToArray,
    BytesFromArray,

    // `string` operations.
    StringSlice,
}

/// A bytecode call target (internal function vs host import).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallTarget {
    Bc(FunctionId),
    Host(HostImportId),
    Intrinsic(Intrinsic),
}

/// A pattern for `switch` and effect handler clauses.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    /// `_`
    Wildcard,
    /// A binding site (captured into the clause bind list).
    Bind,
    /// A literal match (unit/bool/int/float/string/bytes/typerep/function).
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

/// A lowered `switch` case (pattern + jump target).
#[derive(Clone, Debug, PartialEq)]
pub struct SwitchCase {
    pub pattern: Pattern,
    pub target_pc: u32,
    /// The destination registers for pattern binds (must match `count_binds(pattern)`).
    pub param_regs: Vec<Reg>,
}

/// A performed effect identity (interface + instantiated interface args + method).
#[derive(Clone, Debug, PartialEq)]
pub struct EffectSpec {
    pub interface: String,
    /// Runtime `TypeRep` registers used to instantiate the interface type arguments.
    ///
    /// These are evaluated at runtime when pushing a handler and when performing an effect.
    pub interface_args: Vec<Reg>,
    pub method: String,
}

/// A lowered effect handler clause.
#[derive(Clone, Debug, PartialEq)]
pub struct HandlerClause {
    pub effect: EffectSpec,
    pub arg_patterns: Vec<Pattern>,
    pub target_pc: u32,
    /// Destination registers for pattern binds, optionally followed by the continuation token.
    ///
    /// When the continuation is omitted (`param_regs.len() == bind_count`), the clause is
    /// **abortive** and the VM may skip capturing a continuation at the `perform` site.
    pub param_regs: Vec<Reg>,
}

/// An in-memory bytecode instruction stream.
///
/// This is not yet a stable serialized format.
#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Const {
        dst: Reg,
        value: ConstValue,
    },
    Copy {
        dst: Reg,
        src: Reg,
    },
    Move {
        dst: Reg,
        src: Reg,
    },
    AsReadonly {
        dst: Reg,
        src: Reg,
    },
    IsType {
        dst: Reg,
        value: Reg,
        ty: Reg,
    },
    CheckedCast {
        dst: Reg,
        value: Reg,
        ty: Reg,
    },

    MakeTypeRep {
        dst: Reg,
        base: TypeRepLit,
        args: Vec<Reg>,
    },

    MakeStruct {
        dst: Reg,
        type_name: String,
        type_args: Vec<Reg>,
        fields: Vec<(String, Reg)>,
    },
    MakeArray {
        dst: Reg,
        items: Vec<Reg>,
    },
    MakeTuple {
        dst: Reg,
        items: Vec<Reg>,
    },
    MakeEnum {
        dst: Reg,
        enum_name: String,
        type_args: Vec<Reg>,
        variant: String,
        fields: Vec<Reg>,
    },

    GetField {
        dst: Reg,
        obj: Reg,
        field: String,
    },
    SetField {
        obj: Reg,
        field: String,
        value: Reg,
    },

    StructGet {
        dst: Reg,
        obj: Reg,
        idx: usize,
    },
    StructSet {
        obj: Reg,
        idx: usize,
        value: Reg,
    },

    TupleGet {
        dst: Reg,
        tup: Reg,
        idx: usize,
    },
    TupleSet {
        tup: Reg,
        idx: usize,
        value: Reg,
    },

    IndexGet {
        dst: Reg,
        arr: Reg,
        idx: Reg,
    },
    IndexSet {
        arr: Reg,
        idx: Reg,
        value: Reg,
    },
    Len {
        dst: Reg,
        arr: Reg,
    },

    IntAdd {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    IntSub {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    IntMul {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    IntDiv {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    IntMod {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    IntLt {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    IntLe {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    IntGt {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    IntGe {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    IntEq {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    IntNe {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    BoolNot {
        dst: Reg,
        v: Reg,
    },
    BoolEq {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    BoolNe {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    Call {
        dst: Option<Reg>,
        func: CallTarget,
        args: Vec<Reg>,
    },
    ICall {
        dst: Option<Reg>,
        fnptr: Reg,
        args: Vec<Reg>,
    },
    VCall {
        dst: Option<Reg>,
        obj: Reg,
        method: String,
        method_type_args: Vec<Reg>,
        args: Vec<Reg>,
    },

    PushHandler {
        clauses: Vec<HandlerClause>,
    },
    PopHandler,

    Perform {
        dst: Option<Reg>,
        effect: EffectSpec,
        args: Vec<Reg>,
    },
    Resume {
        dst: Option<Reg>,
        k: Reg,
        value: Reg,
    },
    /// Tail-call variant of [`Instruction::Resume`].
    ///
    /// This consumes the continuation and splices execution to the resumed computation without
    /// returning to the current frame (tail resume).
    ResumeTail {
        k: Reg,
        value: Reg,
    },

    Jump {
        target_pc: u32,
    },
    JumpIf {
        cond: Reg,
        then_pc: u32,
        else_pc: u32,
    },
    Switch {
        value: Reg,
        cases: Vec<SwitchCase>,
        default_pc: u32,
    },

    Return {
        value: Reg,
    },
    Trap {
        message: String,
    },
}

/// A bytecode function body.
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: String,
    /// Total number of registers used by this function.
    pub reg_count: u32,
    /// Parameter registers are `0..param_count`.
    pub param_count: u32,
    pub code: Vec<Instruction>,
}

/// A compact, ID-based executable module derived from MIR.
#[derive(Clone, Debug, Default)]
pub struct ExecutableModule {
    pub functions: Vec<Function>,
    pub function_ids: BTreeMap<String, FunctionId>,
    /// Number of implicit runtime `TypeRep` params per function (generic arity).
    ///
    /// `function_generic_params[id]` is the count of leading parameters that are runtime type args.
    pub function_generic_params: Vec<u32>,

    pub host_imports: Vec<HostImport>,
    pub host_import_ids: BTreeMap<String, HostImportId>,

    pub methods: BTreeMap<(String, String), FunctionId>,
    pub interface_impls: BTreeMap<String, BTreeSet<String>>,
    pub struct_layouts: BTreeMap<String, Vec<String>>,

    pub external_effects: Vec<ExternalEffectDecl>,
    pub external_effect_ids: BTreeMap<(String, String), EffectId>,

    /// Entry function for starting execution (typically `main`).
    pub entry: FunctionId,
}

impl ExecutableModule {
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
        self.function_generic_params.push(0);
        Ok(id)
    }

    pub fn function(&self, id: FunctionId) -> Option<&Function> {
        self.functions.get(id.0 as usize)
    }

    pub fn function_id(&self, name: &str) -> Option<FunctionId> {
        self.function_ids.get(name).copied()
    }

    pub fn function_generic_param_count(&self, id: FunctionId) -> Option<u32> {
        self.function_generic_params.get(id.0 as usize).copied()
    }

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

    pub fn host_import(&self, id: HostImportId) -> Option<&HostImport> {
        self.host_imports.get(id.0 as usize)
    }

    pub fn host_import_id(&self, name: &str) -> Option<HostImportId> {
        self.host_import_ids.get(name).copied()
    }

    pub fn add_external_effect(&mut self, decl: ExternalEffectDecl) -> Result<EffectId, String> {
        let key = (decl.interface.clone(), decl.method.clone());
        if self.external_effect_ids.contains_key(&key) {
            return Err(format!(
                "duplicate external effect `{}.{}`",
                decl.interface, decl.method
            ));
        }
        let id_u32: u32 = self
            .external_effects
            .len()
            .try_into()
            .map_err(|_| "external effect table overflow".to_string())?;
        let id = EffectId(id_u32);
        self.external_effect_ids.insert(key, id);
        self.external_effects.push(decl);
        Ok(id)
    }

    pub fn external_effect(&self, id: EffectId) -> Option<&ExternalEffectDecl> {
        self.external_effects.get(id.0 as usize)
    }

    pub fn external_effect_id(&self, interface: &str, method: &str) -> Option<EffectId> {
        self.external_effect_ids
            .get(&(interface.to_string(), method.to_string()))
            .copied()
    }
}
