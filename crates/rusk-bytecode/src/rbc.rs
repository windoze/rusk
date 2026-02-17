//! Stable `.rbc` serialization for [`ExecutableModule`].
//!
//! v0 design goals:
//! - Portable, explicit encoding (little-endian, fixed-width integers).
//! - No reliance on Rust-specific layouts or `serde` formats.
//! - Deterministic (canonical) output so `encode -> decode -> encode` is byte-identical.
//!
//! This is intentionally minimal and currently treated as **internal** stability:
//! we version the format and provide a verifier, but do not yet promise forward
//! compatibility across major versions.

extern crate alloc;

use alloc::boxed::Box;
use alloc::collections::{BTreeMap, BTreeSet};
use alloc::string::{String, ToString};
use alloc::vec::Vec;

use crate::{
    AbiType, CallTarget, ConstValue, EffectId, EffectSpec, ExecutableModule, ExternalEffectDecl,
    Function, FunctionId, HandlerClause, HostFnSig, HostImport, HostImportId, Instruction,
    Intrinsic, Pattern, Reg, SwitchCase, TypeRepLit,
};

const MAGIC: &[u8; 8] = b"RUSKBC0\0";
const VERSION_MAJOR: u16 = 0;
const VERSION_MINOR: u16 = 3;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EncodeError {
    pub message: String,
}

impl core::fmt::Display for EncodeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "encode error: {}", self.message)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for EncodeError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecodeError {
    pub message: String,
    pub offset: usize,
}

impl core::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "decode error at {}: {}", self.offset, self.message)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for DecodeError {}

pub fn to_bytes(module: &ExecutableModule) -> Result<Vec<u8>, EncodeError> {
    let mut enc = Encoder::new();
    enc.write_bytes(MAGIC);
    enc.write_u16(VERSION_MAJOR);
    enc.write_u16(VERSION_MINOR);
    enc.write_module(module)?;
    Ok(enc.finish())
}

pub fn from_bytes(bytes: &[u8]) -> Result<ExecutableModule, DecodeError> {
    let mut dec = Decoder::new(bytes);
    dec.expect_bytes(MAGIC)?;
    let major = dec.read_u16()?;
    let minor = dec.read_u16()?;
    if major != VERSION_MAJOR || minor != VERSION_MINOR {
        return Err(dec.err(format!(
            "unsupported rbc version {major}.{minor} (expected {VERSION_MAJOR}.{VERSION_MINOR})"
        )));
    }

    let module = dec.read_module()?;
    crate::verify::verify_module(&module).map_err(|e| dec.err(e.message))?;
    if dec.remaining() != 0 {
        return Err(dec.err("trailing bytes".to_string()));
    }
    Ok(module)
}

struct Encoder {
    buf: Vec<u8>,
}

impl Encoder {
    fn new() -> Self {
        Self { buf: Vec::new() }
    }

    fn finish(self) -> Vec<u8> {
        self.buf
    }

    fn write_bytes(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
    }

    fn write_u8(&mut self, v: u8) {
        self.buf.push(v);
    }

    fn write_u16(&mut self, v: u16) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }

    fn write_u32(&mut self, v: u32) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }

    fn write_u64(&mut self, v: u64) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }

    fn write_i64(&mut self, v: i64) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }

    fn write_f64(&mut self, v: f64) {
        self.write_u64(v.to_bits());
    }

    fn write_bool(&mut self, v: bool) {
        self.write_u8(if v { 1 } else { 0 });
    }

    fn write_len(&mut self, len: usize) -> Result<(), EncodeError> {
        let len_u32: u32 = len.try_into().map_err(|_| EncodeError {
            message: "length overflow".to_string(),
        })?;
        self.write_u32(len_u32);
        Ok(())
    }

    fn write_string(&mut self, s: &str) -> Result<(), EncodeError> {
        self.write_len(s.len())?;
        self.write_bytes(s.as_bytes());
        Ok(())
    }

    fn write_blob(&mut self, b: &[u8]) -> Result<(), EncodeError> {
        self.write_len(b.len())?;
        self.write_bytes(b);
        Ok(())
    }

    fn write_vec_reg(&mut self, regs: &[Reg]) -> Result<(), EncodeError> {
        self.write_len(regs.len())?;
        for r in regs {
            self.write_u32(*r);
        }
        Ok(())
    }

    fn write_option_reg(&mut self, r: &Option<Reg>) -> Result<(), EncodeError> {
        match r {
            None => self.write_u8(0),
            Some(v) => {
                self.write_u8(1);
                self.write_u32(*v);
            }
        }
        Ok(())
    }

    fn write_module(&mut self, module: &ExecutableModule) -> Result<(), EncodeError> {
        // Functions
        self.write_len(module.functions.len())?;
        for func in &module.functions {
            self.write_function(func)?;
        }

        // Generic params table (parallel to functions).
        self.write_len(module.function_generic_params.len())?;
        for n in &module.function_generic_params {
            self.write_u32(*n);
        }

        // Host imports
        self.write_len(module.host_imports.len())?;
        for import in &module.host_imports {
            self.write_host_import(import)?;
        }

        // Methods (sorted by BTreeMap).
        self.write_len(module.methods.len())?;
        for ((type_name, method_name), fn_id) in &module.methods {
            self.write_string(type_name)?;
            self.write_string(method_name)?;
            self.write_u32(fn_id.0);
        }

        // Interface impls.
        self.write_len(module.interface_impls.len())?;
        for (type_name, ifaces) in &module.interface_impls {
            self.write_string(type_name)?;
            self.write_len(ifaces.len())?;
            for iface in ifaces {
                self.write_string(iface)?;
            }
        }

        // Struct layouts.
        self.write_len(module.struct_layouts.len())?;
        for (type_name, fields) in &module.struct_layouts {
            self.write_string(type_name)?;
            self.write_len(fields.len())?;
            for field in fields {
                self.write_string(field)?;
            }
        }

        // External effects.
        self.write_len(module.external_effects.len())?;
        for decl in &module.external_effects {
            self.write_external_effect_decl(decl)?;
        }

        // Entry.
        self.write_u32(module.entry.0);
        Ok(())
    }

    fn write_function(&mut self, func: &Function) -> Result<(), EncodeError> {
        self.write_string(&func.name)?;
        self.write_u32(func.reg_count);
        self.write_u32(func.param_count);
        self.write_len(func.code.len())?;
        for inst in &func.code {
            self.write_instruction(inst)?;
        }
        Ok(())
    }

    fn write_abi_type(&mut self, ty: &AbiType) {
        let tag: u8 = match ty {
            AbiType::Unit => 0,
            AbiType::Bool => 1,
            AbiType::Int => 2,
            AbiType::Float => 3,
            AbiType::String => 4,
            AbiType::Bytes => 5,
        };
        self.write_u8(tag);
    }

    fn write_host_fn_sig(&mut self, sig: &HostFnSig) -> Result<(), EncodeError> {
        self.write_len(sig.params.len())?;
        for ty in &sig.params {
            self.write_abi_type(ty);
        }
        self.write_abi_type(&sig.ret);
        Ok(())
    }

    fn write_host_import(&mut self, import: &HostImport) -> Result<(), EncodeError> {
        self.write_string(&import.name)?;
        self.write_host_fn_sig(&import.sig)?;
        Ok(())
    }

    fn write_external_effect_decl(&mut self, decl: &ExternalEffectDecl) -> Result<(), EncodeError> {
        self.write_string(&decl.interface)?;
        self.write_string(&decl.method)?;
        self.write_host_fn_sig(&decl.sig)?;
        Ok(())
    }

    fn write_const_value(&mut self, v: &ConstValue) -> Result<(), EncodeError> {
        match v {
            ConstValue::Unit => self.write_u8(0),
            ConstValue::Bool(b) => {
                self.write_u8(1);
                self.write_bool(*b);
            }
            ConstValue::Int(n) => {
                self.write_u8(2);
                self.write_i64(*n);
            }
            ConstValue::Float(x) => {
                self.write_u8(3);
                self.write_f64(*x);
            }
            ConstValue::String(s) => {
                self.write_u8(4);
                self.write_string(s)?;
            }
            ConstValue::Bytes(b) => {
                self.write_u8(5);
                self.write_blob(b)?;
            }
            ConstValue::TypeRep(lit) => {
                self.write_u8(6);
                self.write_type_rep_lit(lit)?;
            }
            ConstValue::Function(fid) => {
                self.write_u8(7);
                self.write_u32(fid.0);
            }
        }
        Ok(())
    }

    fn write_type_rep_lit(&mut self, lit: &TypeRepLit) -> Result<(), EncodeError> {
        match lit {
            TypeRepLit::Unit => self.write_u8(0),
            TypeRepLit::Bool => self.write_u8(1),
            TypeRepLit::Int => self.write_u8(2),
            TypeRepLit::Float => self.write_u8(3),
            TypeRepLit::String => self.write_u8(4),
            TypeRepLit::Bytes => self.write_u8(5),
            TypeRepLit::Array => self.write_u8(6),
            TypeRepLit::Tuple(arity) => {
                self.write_u8(7);
                let a: u32 = (*arity).try_into().map_err(|_| EncodeError {
                    message: "tuple arity overflow".to_string(),
                })?;
                self.write_u32(a);
            }
            TypeRepLit::Struct(name) => {
                self.write_u8(8);
                self.write_string(name)?;
            }
            TypeRepLit::Enum(name) => {
                self.write_u8(9);
                self.write_string(name)?;
            }
            TypeRepLit::Interface(name) => {
                self.write_u8(10);
                self.write_string(name)?;
            }
            TypeRepLit::Fn => self.write_u8(11),
            TypeRepLit::Cont => self.write_u8(12),
        }
        Ok(())
    }

    fn write_pattern(&mut self, pat: &Pattern) -> Result<(), EncodeError> {
        match pat {
            Pattern::Wildcard => self.write_u8(0),
            Pattern::Bind => self.write_u8(1),
            Pattern::Literal(v) => {
                self.write_u8(2);
                self.write_const_value(v)?;
            }
            Pattern::Tuple {
                prefix,
                rest,
                suffix,
            } => {
                self.write_u8(3);
                self.write_len(prefix.len())?;
                for p in prefix {
                    self.write_pattern(p)?;
                }
                match rest {
                    None => self.write_u8(0),
                    Some(p) => {
                        self.write_u8(1);
                        self.write_pattern(p)?;
                    }
                }
                self.write_len(suffix.len())?;
                for p in suffix {
                    self.write_pattern(p)?;
                }
            }
            Pattern::Enum {
                enum_name,
                variant,
                fields,
            } => {
                self.write_u8(4);
                self.write_string(enum_name)?;
                self.write_string(variant)?;
                self.write_len(fields.len())?;
                for p in fields {
                    self.write_pattern(p)?;
                }
            }
            Pattern::Struct { type_name, fields } => {
                self.write_u8(5);
                self.write_string(type_name)?;
                self.write_len(fields.len())?;
                for (name, p) in fields {
                    self.write_string(name)?;
                    self.write_pattern(p)?;
                }
            }
            Pattern::Array {
                prefix,
                rest,
                suffix,
            } => {
                self.write_u8(6);
                self.write_len(prefix.len())?;
                for p in prefix {
                    self.write_pattern(p)?;
                }
                match rest {
                    None => self.write_u8(0),
                    Some(p) => {
                        self.write_u8(1);
                        self.write_pattern(p)?;
                    }
                }
                self.write_len(suffix.len())?;
                for p in suffix {
                    self.write_pattern(p)?;
                }
            }
        }
        Ok(())
    }

    fn write_effect_spec(&mut self, spec: &EffectSpec) -> Result<(), EncodeError> {
        self.write_string(&spec.interface)?;
        self.write_vec_reg(&spec.interface_args)?;
        self.write_string(&spec.method)?;
        Ok(())
    }

    fn write_handler_clause(&mut self, clause: &HandlerClause) -> Result<(), EncodeError> {
        self.write_effect_spec(&clause.effect)?;
        self.write_len(clause.arg_patterns.len())?;
        for pat in &clause.arg_patterns {
            self.write_pattern(pat)?;
        }
        self.write_u32(clause.target_pc);
        self.write_vec_reg(&clause.param_regs)?;
        Ok(())
    }

    fn write_switch_case(&mut self, case: &SwitchCase) -> Result<(), EncodeError> {
        self.write_pattern(&case.pattern)?;
        self.write_u32(case.target_pc);
        self.write_vec_reg(&case.param_regs)?;
        Ok(())
    }

    fn write_intrinsic(&mut self, intr: &Intrinsic) {
        let tag: u16 = match intr {
            Intrinsic::StringConcat => 0,
            Intrinsic::ToString => 1,
            Intrinsic::Panic => 2,
            Intrinsic::BoolNot => 3,
            Intrinsic::BoolEq => 4,
            Intrinsic::BoolNe => 5,
            Intrinsic::IntAdd => 6,
            Intrinsic::IntSub => 7,
            Intrinsic::IntMul => 8,
            Intrinsic::IntDiv => 9,
            Intrinsic::IntMod => 10,
            Intrinsic::IntEq => 11,
            Intrinsic::IntNe => 12,
            Intrinsic::IntLt => 13,
            Intrinsic::IntLe => 14,
            Intrinsic::IntGt => 15,
            Intrinsic::IntGe => 16,
            Intrinsic::FloatAdd => 17,
            Intrinsic::FloatSub => 18,
            Intrinsic::FloatMul => 19,
            Intrinsic::FloatDiv => 20,
            Intrinsic::FloatMod => 21,
            Intrinsic::FloatEq => 22,
            Intrinsic::FloatNe => 23,
            Intrinsic::FloatLt => 24,
            Intrinsic::FloatLe => 25,
            Intrinsic::FloatGt => 26,
            Intrinsic::FloatGe => 27,
            Intrinsic::StringEq => 28,
            Intrinsic::StringNe => 29,
            Intrinsic::BytesEq => 30,
            Intrinsic::BytesNe => 31,
            Intrinsic::UnitEq => 32,
            Intrinsic::UnitNe => 33,
            Intrinsic::IntoIter => 34,
            Intrinsic::Next => 35,
            Intrinsic::ArrayLen => 36,
            Intrinsic::ArrayLenRo => 37,
            Intrinsic::ArrayPush => 38,
            Intrinsic::ArrayPop => 39,
            Intrinsic::ArrayClear => 40,
            Intrinsic::ArrayResize => 41,
            Intrinsic::ArrayInsert => 42,
            Intrinsic::ArrayRemove => 43,
            Intrinsic::ArrayExtend => 44,
            Intrinsic::ArrayConcat => 45,
            Intrinsic::ArrayConcatRo => 46,
            Intrinsic::ArraySlice => 47,
            Intrinsic::ArraySliceRo => 48,
            Intrinsic::StringIntoIter => 49,
            Intrinsic::StringNext => 50,
            Intrinsic::BytesIntoIter => 51,
            Intrinsic::BytesNext => 52,
        };
        self.write_u16(tag);
    }

    fn write_call_target(&mut self, target: &CallTarget) -> Result<(), EncodeError> {
        match target {
            CallTarget::Bc(fid) => {
                self.write_u8(0);
                self.write_u32(fid.0);
            }
            CallTarget::Host(hid) => {
                self.write_u8(1);
                self.write_u32(hid.0);
            }
            CallTarget::Intrinsic(intr) => {
                self.write_u8(2);
                self.write_intrinsic(intr);
            }
        }
        Ok(())
    }

    fn write_instruction(&mut self, inst: &Instruction) -> Result<(), EncodeError> {
        match inst {
            Instruction::Const { dst, value } => {
                self.write_u8(0);
                self.write_u32(*dst);
                self.write_const_value(value)?;
            }
            Instruction::Copy { dst, src } => {
                self.write_u8(1);
                self.write_u32(*dst);
                self.write_u32(*src);
            }
            Instruction::Move { dst, src } => {
                self.write_u8(2);
                self.write_u32(*dst);
                self.write_u32(*src);
            }
            Instruction::AsReadonly { dst, src } => {
                self.write_u8(3);
                self.write_u32(*dst);
                self.write_u32(*src);
            }
            Instruction::IsType { dst, value, ty } => {
                self.write_u8(4);
                self.write_u32(*dst);
                self.write_u32(*value);
                self.write_u32(*ty);
            }
            Instruction::CheckedCast { dst, value, ty } => {
                self.write_u8(5);
                self.write_u32(*dst);
                self.write_u32(*value);
                self.write_u32(*ty);
            }
            Instruction::MakeTypeRep { dst, base, args } => {
                self.write_u8(6);
                self.write_u32(*dst);
                self.write_type_rep_lit(base)?;
                self.write_vec_reg(args)?;
            }
            Instruction::MakeStruct {
                dst,
                type_name,
                type_args,
                fields,
            } => {
                self.write_u8(7);
                self.write_u32(*dst);
                self.write_string(type_name)?;
                self.write_vec_reg(type_args)?;
                self.write_len(fields.len())?;
                for (name, reg) in fields {
                    self.write_string(name)?;
                    self.write_u32(*reg);
                }
            }
            Instruction::MakeArray { dst, items } => {
                self.write_u8(8);
                self.write_u32(*dst);
                self.write_vec_reg(items)?;
            }
            Instruction::MakeTuple { dst, items } => {
                self.write_u8(9);
                self.write_u32(*dst);
                self.write_vec_reg(items)?;
            }
            Instruction::MakeEnum {
                dst,
                enum_name,
                type_args,
                variant,
                fields,
            } => {
                self.write_u8(10);
                self.write_u32(*dst);
                self.write_string(enum_name)?;
                self.write_vec_reg(type_args)?;
                self.write_string(variant)?;
                self.write_vec_reg(fields)?;
            }
            Instruction::GetField { dst, obj, field } => {
                self.write_u8(11);
                self.write_u32(*dst);
                self.write_u32(*obj);
                self.write_string(field)?;
            }
            Instruction::SetField { obj, field, value } => {
                self.write_u8(12);
                self.write_u32(*obj);
                self.write_string(field)?;
                self.write_u32(*value);
            }
            Instruction::StructGet { dst, obj, idx } => {
                self.write_u8(13);
                self.write_u32(*dst);
                self.write_u32(*obj);
                let n: u32 = (*idx).try_into().map_err(|_| EncodeError {
                    message: "struct field index overflow".to_string(),
                })?;
                self.write_u32(n);
            }
            Instruction::StructSet { obj, idx, value } => {
                self.write_u8(14);
                self.write_u32(*obj);
                let n: u32 = (*idx).try_into().map_err(|_| EncodeError {
                    message: "struct field index overflow".to_string(),
                })?;
                self.write_u32(n);
                self.write_u32(*value);
            }
            Instruction::TupleGet { dst, tup, idx } => {
                self.write_u8(15);
                self.write_u32(*dst);
                self.write_u32(*tup);
                let n: u32 = (*idx).try_into().map_err(|_| EncodeError {
                    message: "tuple index overflow".to_string(),
                })?;
                self.write_u32(n);
            }
            Instruction::TupleSet { tup, idx, value } => {
                self.write_u8(16);
                self.write_u32(*tup);
                let n: u32 = (*idx).try_into().map_err(|_| EncodeError {
                    message: "tuple index overflow".to_string(),
                })?;
                self.write_u32(n);
                self.write_u32(*value);
            }
            Instruction::IndexGet { dst, arr, idx } => {
                self.write_u8(17);
                self.write_u32(*dst);
                self.write_u32(*arr);
                self.write_u32(*idx);
            }
            Instruction::IndexSet { arr, idx, value } => {
                self.write_u8(18);
                self.write_u32(*arr);
                self.write_u32(*idx);
                self.write_u32(*value);
            }
            Instruction::Len { dst, arr } => {
                self.write_u8(19);
                self.write_u32(*dst);
                self.write_u32(*arr);
            }
            Instruction::IntAdd { dst, a, b } => {
                self.write_u8(20);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntSub { dst, a, b } => {
                self.write_u8(21);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntMul { dst, a, b } => {
                self.write_u8(22);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntDiv { dst, a, b } => {
                self.write_u8(23);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntMod { dst, a, b } => {
                self.write_u8(24);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntLt { dst, a, b } => {
                self.write_u8(25);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntLe { dst, a, b } => {
                self.write_u8(26);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntGt { dst, a, b } => {
                self.write_u8(27);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntGe { dst, a, b } => {
                self.write_u8(28);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntEq { dst, a, b } => {
                self.write_u8(29);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntNe { dst, a, b } => {
                self.write_u8(30);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::BoolNot { dst, v } => {
                self.write_u8(31);
                self.write_u32(*dst);
                self.write_u32(*v);
            }
            Instruction::BoolEq { dst, a, b } => {
                self.write_u8(32);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::BoolNe { dst, a, b } => {
                self.write_u8(33);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::Call { dst, func, args } => {
                self.write_u8(34);
                self.write_option_reg(dst)?;
                self.write_call_target(func)?;
                self.write_vec_reg(args)?;
            }
            Instruction::ICall { dst, fnptr, args } => {
                self.write_u8(35);
                self.write_option_reg(dst)?;
                self.write_u32(*fnptr);
                self.write_vec_reg(args)?;
            }
            Instruction::VCall {
                dst,
                obj,
                method,
                method_type_args,
                args,
            } => {
                self.write_u8(36);
                self.write_option_reg(dst)?;
                self.write_u32(*obj);
                self.write_string(method)?;
                self.write_vec_reg(method_type_args)?;
                self.write_vec_reg(args)?;
            }
            Instruction::PushHandler { clauses } => {
                self.write_u8(37);
                self.write_len(clauses.len())?;
                for clause in clauses {
                    self.write_handler_clause(clause)?;
                }
            }
            Instruction::PopHandler => {
                self.write_u8(38);
            }
            Instruction::Perform { dst, effect, args } => {
                self.write_u8(39);
                self.write_option_reg(dst)?;
                self.write_effect_spec(effect)?;
                self.write_vec_reg(args)?;
            }
            Instruction::Resume { dst, k, value } => {
                self.write_u8(40);
                self.write_option_reg(dst)?;
                self.write_u32(*k);
                self.write_u32(*value);
            }
            Instruction::ResumeTail { k, value } => {
                self.write_u8(46);
                self.write_u32(*k);
                self.write_u32(*value);
            }
            Instruction::Jump { target_pc } => {
                self.write_u8(41);
                self.write_u32(*target_pc);
            }
            Instruction::JumpIf {
                cond,
                then_pc,
                else_pc,
            } => {
                self.write_u8(42);
                self.write_u32(*cond);
                self.write_u32(*then_pc);
                self.write_u32(*else_pc);
            }
            Instruction::Switch {
                value,
                cases,
                default_pc,
            } => {
                self.write_u8(43);
                self.write_u32(*value);
                self.write_len(cases.len())?;
                for case in cases {
                    self.write_switch_case(case)?;
                }
                self.write_u32(*default_pc);
            }
            Instruction::Return { value } => {
                self.write_u8(44);
                self.write_u32(*value);
            }
            Instruction::Trap { message } => {
                self.write_u8(45);
                self.write_string(message)?;
            }
        }
        Ok(())
    }
}

struct Decoder<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Decoder<'a> {
    fn new(bytes: &'a [u8]) -> Self {
        Self { bytes, pos: 0 }
    }

    fn err(&self, message: String) -> DecodeError {
        DecodeError {
            message,
            offset: self.pos,
        }
    }

    fn remaining(&self) -> usize {
        self.bytes.len().saturating_sub(self.pos)
    }

    fn read_exact(&mut self, n: usize) -> Result<&'a [u8], DecodeError> {
        let end = self
            .pos
            .checked_add(n)
            .ok_or_else(|| self.err("offset overflow".to_string()))?;
        if end > self.bytes.len() {
            return Err(self.err("unexpected EOF".to_string()));
        }
        let slice = &self.bytes[self.pos..end];
        self.pos = end;
        Ok(slice)
    }

    fn expect_bytes(&mut self, expected: &[u8]) -> Result<(), DecodeError> {
        let got = self.read_exact(expected.len())?;
        if got != expected {
            return Err(self.err("bad magic".to_string()));
        }
        Ok(())
    }

    fn read_u8(&mut self) -> Result<u8, DecodeError> {
        Ok(self.read_exact(1)?[0])
    }

    fn read_u16(&mut self) -> Result<u16, DecodeError> {
        let b = self.read_exact(2)?;
        Ok(u16::from_le_bytes([b[0], b[1]]))
    }

    fn read_u32(&mut self) -> Result<u32, DecodeError> {
        let b = self.read_exact(4)?;
        Ok(u32::from_le_bytes([b[0], b[1], b[2], b[3]]))
    }

    fn read_u64(&mut self) -> Result<u64, DecodeError> {
        let b = self.read_exact(8)?;
        Ok(u64::from_le_bytes([
            b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7],
        ]))
    }

    fn read_i64(&mut self) -> Result<i64, DecodeError> {
        let b = self.read_exact(8)?;
        Ok(i64::from_le_bytes([
            b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7],
        ]))
    }

    fn read_f64(&mut self) -> Result<f64, DecodeError> {
        Ok(f64::from_bits(self.read_u64()?))
    }

    fn read_bool(&mut self) -> Result<bool, DecodeError> {
        match self.read_u8()? {
            0 => Ok(false),
            1 => Ok(true),
            other => Err(self.err(format!("invalid bool tag {other}"))),
        }
    }

    fn read_len(&mut self) -> Result<usize, DecodeError> {
        let n = self.read_u32()?;
        usize::try_from(n).map_err(|_| self.err("length overflow".to_string()))
    }

    fn read_string(&mut self) -> Result<String, DecodeError> {
        let n = self.read_len()?;
        let b = self.read_exact(n)?;
        let s = core::str::from_utf8(b).map_err(|e| self.err(format!("invalid utf-8: {e}")))?;
        Ok(s.to_string())
    }

    fn read_blob(&mut self) -> Result<Vec<u8>, DecodeError> {
        let n = self.read_len()?;
        Ok(self.read_exact(n)?.to_vec())
    }

    fn read_vec_reg(&mut self) -> Result<Vec<Reg>, DecodeError> {
        let n = self.read_len()?;
        // Fixed-size: each reg is u32.
        let min_bytes = n
            .checked_mul(4)
            .ok_or_else(|| self.err("length overflow".to_string()))?;
        if min_bytes > self.remaining() {
            return Err(self.err("unexpected EOF".to_string()));
        }
        let mut out = Vec::with_capacity(n);
        for _ in 0..n {
            out.push(self.read_u32()?);
        }
        Ok(out)
    }

    fn read_option_reg(&mut self) -> Result<Option<Reg>, DecodeError> {
        match self.read_u8()? {
            0 => Ok(None),
            1 => Ok(Some(self.read_u32()?)),
            other => Err(self.err(format!("invalid option tag {other}"))),
        }
    }

    fn read_module(&mut self) -> Result<ExecutableModule, DecodeError> {
        // Functions
        let fn_len = self.read_len()?;
        let mut functions = Vec::with_capacity(fn_len);
        for _ in 0..fn_len {
            functions.push(self.read_function()?);
        }

        let gen_len = self.read_len()?;
        let mut function_generic_params = Vec::with_capacity(gen_len);
        for _ in 0..gen_len {
            function_generic_params.push(self.read_u32()?);
        }

        // Host imports
        let host_len = self.read_len()?;
        let mut host_imports = Vec::with_capacity(host_len);
        for _ in 0..host_len {
            host_imports.push(self.read_host_import()?);
        }

        // Methods
        let method_len = self.read_len()?;
        let mut methods = BTreeMap::new();
        for _ in 0..method_len {
            let type_name = self.read_string()?;
            let method_name = self.read_string()?;
            let fn_id = FunctionId(self.read_u32()?);
            methods.insert((type_name, method_name), fn_id);
        }

        // Interface impls
        let impl_len = self.read_len()?;
        let mut interface_impls = BTreeMap::new();
        for _ in 0..impl_len {
            let type_name = self.read_string()?;
            let iface_len = self.read_len()?;
            let mut set = BTreeSet::new();
            for _ in 0..iface_len {
                set.insert(self.read_string()?);
            }
            interface_impls.insert(type_name, set);
        }

        // Struct layouts
        let layout_len = self.read_len()?;
        let mut struct_layouts = BTreeMap::new();
        for _ in 0..layout_len {
            let type_name = self.read_string()?;
            let field_len = self.read_len()?;
            let mut fields = Vec::with_capacity(field_len);
            for _ in 0..field_len {
                fields.push(self.read_string()?);
            }
            struct_layouts.insert(type_name, fields);
        }

        // External effects.
        let eff_len = self.read_len()?;
        let mut external_effects = Vec::with_capacity(eff_len);
        for _ in 0..eff_len {
            external_effects.push(self.read_external_effect_decl()?);
        }

        let entry = FunctionId(self.read_u32()?);

        // Rebuild auxiliary maps.
        let mut function_ids = BTreeMap::new();
        for (idx, func) in functions.iter().enumerate() {
            let id = FunctionId(idx as u32);
            if function_ids.insert(func.name.clone(), id).is_some() {
                return Err(self.err(format!("duplicate function name `{}`", func.name)));
            }
        }

        let mut host_import_ids = BTreeMap::new();
        for (idx, import) in host_imports.iter().enumerate() {
            let id = HostImportId(idx as u32);
            if host_import_ids.insert(import.name.clone(), id).is_some() {
                return Err(self.err(format!("duplicate host import name `{}`", import.name)));
            }
        }

        let mut external_effect_ids = BTreeMap::new();
        for (idx, decl) in external_effects.iter().enumerate() {
            let id = EffectId(idx as u32);
            let key = (decl.interface.clone(), decl.method.clone());
            if external_effect_ids.insert(key, id).is_some() {
                return Err(self.err(format!(
                    "duplicate external effect `{}.{}`",
                    decl.interface, decl.method
                )));
            }
        }

        Ok(ExecutableModule {
            functions,
            function_ids,
            function_generic_params,
            host_imports,
            host_import_ids,
            methods,
            interface_impls,
            struct_layouts,
            external_effects,
            external_effect_ids,
            entry,
        })
    }

    fn read_function(&mut self) -> Result<Function, DecodeError> {
        let name = self.read_string()?;
        let reg_count = self.read_u32()?;
        let param_count = self.read_u32()?;
        let code_len = self.read_len()?;
        let mut code = Vec::with_capacity(code_len);
        for _ in 0..code_len {
            code.push(self.read_instruction()?);
        }
        Ok(Function {
            name,
            reg_count,
            param_count,
            code,
        })
    }

    fn read_abi_type(&mut self) -> Result<AbiType, DecodeError> {
        match self.read_u8()? {
            0 => Ok(AbiType::Unit),
            1 => Ok(AbiType::Bool),
            2 => Ok(AbiType::Int),
            3 => Ok(AbiType::Float),
            4 => Ok(AbiType::String),
            5 => Ok(AbiType::Bytes),
            other => Err(self.err(format!("invalid AbiType tag {other}"))),
        }
    }

    fn read_host_fn_sig(&mut self) -> Result<HostFnSig, DecodeError> {
        let n = self.read_len()?;
        let mut params = Vec::with_capacity(n);
        for _ in 0..n {
            params.push(self.read_abi_type()?);
        }
        let ret = self.read_abi_type()?;
        Ok(HostFnSig { params, ret })
    }

    fn read_host_import(&mut self) -> Result<HostImport, DecodeError> {
        let name = self.read_string()?;
        let sig = self.read_host_fn_sig()?;
        Ok(HostImport { name, sig })
    }

    fn read_external_effect_decl(&mut self) -> Result<ExternalEffectDecl, DecodeError> {
        let interface = self.read_string()?;
        let method = self.read_string()?;
        let sig = self.read_host_fn_sig()?;
        Ok(ExternalEffectDecl {
            interface,
            method,
            sig,
        })
    }

    fn read_const_value(&mut self) -> Result<ConstValue, DecodeError> {
        match self.read_u8()? {
            0 => Ok(ConstValue::Unit),
            1 => Ok(ConstValue::Bool(self.read_bool()?)),
            2 => Ok(ConstValue::Int(self.read_i64()?)),
            3 => Ok(ConstValue::Float(self.read_f64()?)),
            4 => Ok(ConstValue::String(self.read_string()?)),
            5 => Ok(ConstValue::Bytes(self.read_blob()?)),
            6 => Ok(ConstValue::TypeRep(self.read_type_rep_lit()?)),
            7 => Ok(ConstValue::Function(FunctionId(self.read_u32()?))),
            other => Err(self.err(format!("invalid ConstValue tag {other}"))),
        }
    }

    fn read_type_rep_lit(&mut self) -> Result<TypeRepLit, DecodeError> {
        match self.read_u8()? {
            0 => Ok(TypeRepLit::Unit),
            1 => Ok(TypeRepLit::Bool),
            2 => Ok(TypeRepLit::Int),
            3 => Ok(TypeRepLit::Float),
            4 => Ok(TypeRepLit::String),
            5 => Ok(TypeRepLit::Bytes),
            6 => Ok(TypeRepLit::Array),
            7 => {
                let arity = self.read_u32()?;
                let arity_usize: usize = usize::try_from(arity)
                    .map_err(|_| self.err("tuple arity overflow".to_string()))?;
                Ok(TypeRepLit::Tuple(arity_usize))
            }
            8 => Ok(TypeRepLit::Struct(self.read_string()?)),
            9 => Ok(TypeRepLit::Enum(self.read_string()?)),
            10 => Ok(TypeRepLit::Interface(self.read_string()?)),
            11 => Ok(TypeRepLit::Fn),
            12 => Ok(TypeRepLit::Cont),
            other => Err(self.err(format!("invalid TypeRepLit tag {other}"))),
        }
    }

    fn read_pattern(&mut self) -> Result<Pattern, DecodeError> {
        match self.read_u8()? {
            0 => Ok(Pattern::Wildcard),
            1 => Ok(Pattern::Bind),
            2 => Ok(Pattern::Literal(self.read_const_value()?)),
            3 => {
                let pre_n = self.read_len()?;
                let mut prefix = Vec::with_capacity(pre_n);
                for _ in 0..pre_n {
                    prefix.push(self.read_pattern()?);
                }
                let rest = match self.read_u8()? {
                    0 => None,
                    1 => Some(Box::new(self.read_pattern()?)),
                    other => return Err(self.err(format!("invalid option tag {other}"))),
                };
                let suf_n = self.read_len()?;
                let mut suffix = Vec::with_capacity(suf_n);
                for _ in 0..suf_n {
                    suffix.push(self.read_pattern()?);
                }
                Ok(Pattern::Tuple {
                    prefix,
                    rest,
                    suffix,
                })
            }
            4 => {
                let enum_name = self.read_string()?;
                let variant = self.read_string()?;
                let n = self.read_len()?;
                let mut fields = Vec::with_capacity(n);
                for _ in 0..n {
                    fields.push(self.read_pattern()?);
                }
                Ok(Pattern::Enum {
                    enum_name,
                    variant,
                    fields,
                })
            }
            5 => {
                let type_name = self.read_string()?;
                let n = self.read_len()?;
                let mut fields = Vec::with_capacity(n);
                for _ in 0..n {
                    let name = self.read_string()?;
                    let pat = self.read_pattern()?;
                    fields.push((name, pat));
                }
                Ok(Pattern::Struct { type_name, fields })
            }
            6 => {
                let pre_n = self.read_len()?;
                let mut prefix = Vec::with_capacity(pre_n);
                for _ in 0..pre_n {
                    prefix.push(self.read_pattern()?);
                }
                let rest = match self.read_u8()? {
                    0 => None,
                    1 => Some(Box::new(self.read_pattern()?)),
                    other => return Err(self.err(format!("invalid option tag {other}"))),
                };
                let suf_n = self.read_len()?;
                let mut suffix = Vec::with_capacity(suf_n);
                for _ in 0..suf_n {
                    suffix.push(self.read_pattern()?);
                }
                Ok(Pattern::Array {
                    prefix,
                    rest,
                    suffix,
                })
            }
            other => Err(self.err(format!("invalid Pattern tag {other}"))),
        }
    }

    fn read_effect_spec(&mut self) -> Result<EffectSpec, DecodeError> {
        let interface = self.read_string()?;
        let interface_args = self.read_vec_reg()?;
        let method = self.read_string()?;
        Ok(EffectSpec {
            interface,
            interface_args,
            method,
        })
    }

    fn read_handler_clause(&mut self) -> Result<HandlerClause, DecodeError> {
        let effect = self.read_effect_spec()?;
        let n = self.read_len()?;
        let mut arg_patterns = Vec::with_capacity(n);
        for _ in 0..n {
            arg_patterns.push(self.read_pattern()?);
        }
        let target_pc = self.read_u32()?;
        let param_regs = self.read_vec_reg()?;
        Ok(HandlerClause {
            effect,
            arg_patterns,
            target_pc,
            param_regs,
        })
    }

    fn read_switch_case(&mut self) -> Result<SwitchCase, DecodeError> {
        let pattern = self.read_pattern()?;
        let target_pc = self.read_u32()?;
        let param_regs = self.read_vec_reg()?;
        Ok(SwitchCase {
            pattern,
            target_pc,
            param_regs,
        })
    }

    fn read_intrinsic(&mut self) -> Result<Intrinsic, DecodeError> {
        let tag = self.read_u16()?;
        let intr = match tag {
            0 => Intrinsic::StringConcat,
            1 => Intrinsic::ToString,
            2 => Intrinsic::Panic,
            3 => Intrinsic::BoolNot,
            4 => Intrinsic::BoolEq,
            5 => Intrinsic::BoolNe,
            6 => Intrinsic::IntAdd,
            7 => Intrinsic::IntSub,
            8 => Intrinsic::IntMul,
            9 => Intrinsic::IntDiv,
            10 => Intrinsic::IntMod,
            11 => Intrinsic::IntEq,
            12 => Intrinsic::IntNe,
            13 => Intrinsic::IntLt,
            14 => Intrinsic::IntLe,
            15 => Intrinsic::IntGt,
            16 => Intrinsic::IntGe,
            17 => Intrinsic::FloatAdd,
            18 => Intrinsic::FloatSub,
            19 => Intrinsic::FloatMul,
            20 => Intrinsic::FloatDiv,
            21 => Intrinsic::FloatMod,
            22 => Intrinsic::FloatEq,
            23 => Intrinsic::FloatNe,
            24 => Intrinsic::FloatLt,
            25 => Intrinsic::FloatLe,
            26 => Intrinsic::FloatGt,
            27 => Intrinsic::FloatGe,
            28 => Intrinsic::StringEq,
            29 => Intrinsic::StringNe,
            30 => Intrinsic::BytesEq,
            31 => Intrinsic::BytesNe,
            32 => Intrinsic::UnitEq,
            33 => Intrinsic::UnitNe,
            34 => Intrinsic::IntoIter,
            35 => Intrinsic::Next,
            36 => Intrinsic::ArrayLen,
            37 => Intrinsic::ArrayLenRo,
            38 => Intrinsic::ArrayPush,
            39 => Intrinsic::ArrayPop,
            40 => Intrinsic::ArrayClear,
            41 => Intrinsic::ArrayResize,
            42 => Intrinsic::ArrayInsert,
            43 => Intrinsic::ArrayRemove,
            44 => Intrinsic::ArrayExtend,
            45 => Intrinsic::ArrayConcat,
            46 => Intrinsic::ArrayConcatRo,
            47 => Intrinsic::ArraySlice,
            48 => Intrinsic::ArraySliceRo,
            49 => Intrinsic::StringIntoIter,
            50 => Intrinsic::StringNext,
            51 => Intrinsic::BytesIntoIter,
            52 => Intrinsic::BytesNext,
            other => return Err(self.err(format!("invalid Intrinsic tag {other}"))),
        };
        Ok(intr)
    }

    fn read_call_target(&mut self) -> Result<CallTarget, DecodeError> {
        match self.read_u8()? {
            0 => Ok(CallTarget::Bc(FunctionId(self.read_u32()?))),
            1 => Ok(CallTarget::Host(HostImportId(self.read_u32()?))),
            2 => Ok(CallTarget::Intrinsic(self.read_intrinsic()?)),
            other => Err(self.err(format!("invalid CallTarget tag {other}"))),
        }
    }

    fn read_instruction(&mut self) -> Result<Instruction, DecodeError> {
        match self.read_u8()? {
            0 => Ok(Instruction::Const {
                dst: self.read_u32()?,
                value: self.read_const_value()?,
            }),
            1 => Ok(Instruction::Copy {
                dst: self.read_u32()?,
                src: self.read_u32()?,
            }),
            2 => Ok(Instruction::Move {
                dst: self.read_u32()?,
                src: self.read_u32()?,
            }),
            3 => Ok(Instruction::AsReadonly {
                dst: self.read_u32()?,
                src: self.read_u32()?,
            }),
            4 => Ok(Instruction::IsType {
                dst: self.read_u32()?,
                value: self.read_u32()?,
                ty: self.read_u32()?,
            }),
            5 => Ok(Instruction::CheckedCast {
                dst: self.read_u32()?,
                value: self.read_u32()?,
                ty: self.read_u32()?,
            }),
            6 => Ok(Instruction::MakeTypeRep {
                dst: self.read_u32()?,
                base: self.read_type_rep_lit()?,
                args: self.read_vec_reg()?,
            }),
            7 => {
                let dst = self.read_u32()?;
                let type_name = self.read_string()?;
                let type_args = self.read_vec_reg()?;
                let n = self.read_len()?;
                let mut fields = Vec::with_capacity(n);
                for _ in 0..n {
                    let name = self.read_string()?;
                    let reg = self.read_u32()?;
                    fields.push((name, reg));
                }
                Ok(Instruction::MakeStruct {
                    dst,
                    type_name,
                    type_args,
                    fields,
                })
            }
            8 => Ok(Instruction::MakeArray {
                dst: self.read_u32()?,
                items: self.read_vec_reg()?,
            }),
            9 => Ok(Instruction::MakeTuple {
                dst: self.read_u32()?,
                items: self.read_vec_reg()?,
            }),
            10 => Ok(Instruction::MakeEnum {
                dst: self.read_u32()?,
                enum_name: self.read_string()?,
                type_args: self.read_vec_reg()?,
                variant: self.read_string()?,
                fields: self.read_vec_reg()?,
            }),
            11 => Ok(Instruction::GetField {
                dst: self.read_u32()?,
                obj: self.read_u32()?,
                field: self.read_string()?,
            }),
            12 => Ok(Instruction::SetField {
                obj: self.read_u32()?,
                field: self.read_string()?,
                value: self.read_u32()?,
            }),
            13 => {
                let dst = self.read_u32()?;
                let obj = self.read_u32()?;
                let idx = usize::try_from(self.read_u32()?)
                    .map_err(|_| self.err("struct field index overflow".to_string()))?;
                Ok(Instruction::StructGet { dst, obj, idx })
            }
            14 => {
                let obj = self.read_u32()?;
                let idx = usize::try_from(self.read_u32()?)
                    .map_err(|_| self.err("struct field index overflow".to_string()))?;
                let value = self.read_u32()?;
                Ok(Instruction::StructSet { obj, idx, value })
            }
            15 => {
                let dst = self.read_u32()?;
                let tup = self.read_u32()?;
                let idx = usize::try_from(self.read_u32()?)
                    .map_err(|_| self.err("tuple index overflow".to_string()))?;
                Ok(Instruction::TupleGet { dst, tup, idx })
            }
            16 => {
                let tup = self.read_u32()?;
                let idx = usize::try_from(self.read_u32()?)
                    .map_err(|_| self.err("tuple index overflow".to_string()))?;
                let value = self.read_u32()?;
                Ok(Instruction::TupleSet { tup, idx, value })
            }
            17 => Ok(Instruction::IndexGet {
                dst: self.read_u32()?,
                arr: self.read_u32()?,
                idx: self.read_u32()?,
            }),
            18 => Ok(Instruction::IndexSet {
                arr: self.read_u32()?,
                idx: self.read_u32()?,
                value: self.read_u32()?,
            }),
            19 => Ok(Instruction::Len {
                dst: self.read_u32()?,
                arr: self.read_u32()?,
            }),
            20 => Ok(Instruction::IntAdd {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            21 => Ok(Instruction::IntSub {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            22 => Ok(Instruction::IntMul {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            23 => Ok(Instruction::IntDiv {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            24 => Ok(Instruction::IntMod {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            25 => Ok(Instruction::IntLt {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            26 => Ok(Instruction::IntLe {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            27 => Ok(Instruction::IntGt {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            28 => Ok(Instruction::IntGe {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            29 => Ok(Instruction::IntEq {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            30 => Ok(Instruction::IntNe {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            31 => Ok(Instruction::BoolNot {
                dst: self.read_u32()?,
                v: self.read_u32()?,
            }),
            32 => Ok(Instruction::BoolEq {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            33 => Ok(Instruction::BoolNe {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            34 => Ok(Instruction::Call {
                dst: self.read_option_reg()?,
                func: self.read_call_target()?,
                args: self.read_vec_reg()?,
            }),
            35 => Ok(Instruction::ICall {
                dst: self.read_option_reg()?,
                fnptr: self.read_u32()?,
                args: self.read_vec_reg()?,
            }),
            36 => Ok(Instruction::VCall {
                dst: self.read_option_reg()?,
                obj: self.read_u32()?,
                method: self.read_string()?,
                method_type_args: self.read_vec_reg()?,
                args: self.read_vec_reg()?,
            }),
            37 => {
                let n = self.read_len()?;
                let mut clauses = Vec::with_capacity(n);
                for _ in 0..n {
                    clauses.push(self.read_handler_clause()?);
                }
                Ok(Instruction::PushHandler { clauses })
            }
            38 => Ok(Instruction::PopHandler),
            39 => Ok(Instruction::Perform {
                dst: self.read_option_reg()?,
                effect: self.read_effect_spec()?,
                args: self.read_vec_reg()?,
            }),
            40 => Ok(Instruction::Resume {
                dst: self.read_option_reg()?,
                k: self.read_u32()?,
                value: self.read_u32()?,
            }),
            46 => Ok(Instruction::ResumeTail {
                k: self.read_u32()?,
                value: self.read_u32()?,
            }),
            41 => Ok(Instruction::Jump {
                target_pc: self.read_u32()?,
            }),
            42 => Ok(Instruction::JumpIf {
                cond: self.read_u32()?,
                then_pc: self.read_u32()?,
                else_pc: self.read_u32()?,
            }),
            43 => {
                let value = self.read_u32()?;
                let n = self.read_len()?;
                let mut cases = Vec::with_capacity(n);
                for _ in 0..n {
                    cases.push(self.read_switch_case()?);
                }
                let default_pc = self.read_u32()?;
                Ok(Instruction::Switch {
                    value,
                    cases,
                    default_pc,
                })
            }
            44 => Ok(Instruction::Return {
                value: self.read_u32()?,
            }),
            45 => Ok(Instruction::Trap {
                message: self.read_string()?,
            }),
            other => Err(self.err(format!("invalid Instruction opcode {other}"))),
        }
    }
}
