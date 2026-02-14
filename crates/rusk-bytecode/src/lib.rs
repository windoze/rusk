#![forbid(unsafe_code)]
#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

/// A stable identifier for a bytecode function within an [`ExecutableModule`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(pub u32);

impl Default for FunctionId {
    fn default() -> Self {
        Self(0)
    }
}

/// A stable identifier for a declared host import within an [`ExecutableModule`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HostImportId(pub u32);

/// A stable identifier for an externalized effect operation within an [`ExecutableModule`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

impl AbiType {
    pub fn from_host_type(ty: &rusk_mir::HostType) -> Option<Self> {
        match ty {
            rusk_mir::HostType::Unit => Some(Self::Unit),
            rusk_mir::HostType::Bool => Some(Self::Bool),
            rusk_mir::HostType::Int => Some(Self::Int),
            rusk_mir::HostType::Float => Some(Self::Float),
            rusk_mir::HostType::String => Some(Self::String),
            rusk_mir::HostType::Bytes => Some(Self::Bytes),
            rusk_mir::HostType::Any
            | rusk_mir::HostType::TypeRep
            | rusk_mir::HostType::Array(_)
            | rusk_mir::HostType::Tuple(_) => None,
        }
    }
}

/// A host import signature restricted to [`AbiType`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HostFnSig {
    pub params: Vec<AbiType>,
    pub ret: AbiType,
}

impl HostFnSig {
    pub fn from_host_sig(sig: &rusk_mir::HostFnSig) -> Option<Self> {
        let mut params = Vec::with_capacity(sig.params.len());
        for ty in &sig.params {
            params.push(AbiType::from_host_type(ty)?);
        }
        let ret = AbiType::from_host_type(&sig.ret)?;
        Some(Self { params, ret })
    }
}

/// A declared host import entry required by an [`ExecutableModule`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HostImport {
    pub name: String,
    pub sig: HostFnSig,
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
}

/// A bytecode register index.
pub type Reg = u32;

/// A bytecode call target (internal function vs host import).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallTarget {
    Bc(FunctionId),
    Host(HostImportId),
}

/// An in-memory bytecode instruction stream.
///
/// This is not yet a stable serialized format.
#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Const { dst: Reg, value: ConstValue },
    Copy { dst: Reg, src: Reg },
    Move { dst: Reg, src: Reg },

    IntAdd { dst: Reg, a: Reg, b: Reg },
    IntSub { dst: Reg, a: Reg, b: Reg },
    IntMul { dst: Reg, a: Reg, b: Reg },
    IntDiv { dst: Reg, a: Reg, b: Reg },
    IntMod { dst: Reg, a: Reg, b: Reg },

    IntLt { dst: Reg, a: Reg, b: Reg },
    IntLe { dst: Reg, a: Reg, b: Reg },
    IntGt { dst: Reg, a: Reg, b: Reg },
    IntGe { dst: Reg, a: Reg, b: Reg },
    IntEq { dst: Reg, a: Reg, b: Reg },
    IntNe { dst: Reg, a: Reg, b: Reg },

    BoolNot { dst: Reg, v: Reg },
    BoolEq { dst: Reg, a: Reg, b: Reg },
    BoolNe { dst: Reg, a: Reg, b: Reg },

    Call {
        dst: Option<Reg>,
        func: CallTarget,
        args: Vec<Reg>,
    },

    Jump { target_pc: u32 },
    JumpIf {
        cond: Reg,
        then_pc: u32,
        else_pc: u32,
    },

    Return { value: Reg },
    Trap { message: String },
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

    pub host_imports: Vec<HostImport>,
    pub host_import_ids: BTreeMap<String, HostImportId>,

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
        Ok(id)
    }

    pub fn function(&self, id: FunctionId) -> Option<&Function> {
        self.functions.get(id.0 as usize)
    }

    pub fn function_id(&self, name: &str) -> Option<FunctionId> {
        self.function_ids.get(name).copied()
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
}
