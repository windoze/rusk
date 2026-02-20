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
use alloc::collections::BTreeMap;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

use crate::{
    AbiType, CallTarget, ConstValue, EffectId, EffectSpec, ExecutableModule, ExternalEffectDecl,
    Function, FunctionId, HandlerClause, HostFnSig, HostImport, HostImportId, Instruction,
    Intrinsic, MethodId, Pattern, Reg, SwitchCase, TypeId, TypeRepLit,
};

const MAGIC: &[u8; 8] = b"RUSKBC0\0";
const VERSION_MAJOR: u16 = 0;
const VERSION_MINOR: u16 = 10;

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

        // Interned type names.
        self.write_len(module.type_names.len())?;
        for name in &module.type_names {
            self.write_string(name)?;
        }

        // Interned method names.
        self.write_len(module.method_names.len())?;
        for name in &module.method_names {
            self.write_string(name)?;
        }

        // VCall dispatch table (sparse encoding, deterministic order by type_id then method_id).
        let mut dispatch_len: usize = 0;
        for entries in &module.vcall_dispatch {
            dispatch_len = dispatch_len.saturating_add(entries.len());
        }
        self.write_len(dispatch_len)?;
        for (type_index, entries) in module.vcall_dispatch.iter().enumerate() {
            let type_id = TypeId(type_index as u32);
            for (method_id, fn_id) in entries {
                self.write_u32(type_id.0);
                self.write_u32(method_id.0);
                self.write_u32(fn_id.0);
            }
        }

        // Interface impls (sparse encoding).
        let impl_len = module
            .interface_impls
            .iter()
            .filter(|ifaces| !ifaces.is_empty())
            .count();
        self.write_len(impl_len)?;
        for (type_index, ifaces) in module.interface_impls.iter().enumerate() {
            if ifaces.is_empty() {
                continue;
            }
            let type_id = TypeId(type_index as u32);
            self.write_u32(type_id.0);
            self.write_len(ifaces.len())?;
            for iface_id in ifaces {
                self.write_u32(iface_id.0);
            }
        }

        // Struct layouts (sparse encoding).
        let layout_len = module
            .struct_layouts
            .iter()
            .filter(|layout| layout.is_some())
            .count();
        self.write_len(layout_len)?;
        for (type_index, layout) in module.struct_layouts.iter().enumerate() {
            let Some(fields) = layout.as_ref() else {
                continue;
            };
            let type_id = TypeId(type_index as u32);
            self.write_u32(type_id.0);
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
            AbiType::Continuation => 6,
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
            TypeRepLit::Never => self.write_u8(1),
            TypeRepLit::Bool => self.write_u8(2),
            TypeRepLit::Int => self.write_u8(3),
            TypeRepLit::Float => self.write_u8(4),
            TypeRepLit::Byte => self.write_u8(5),
            TypeRepLit::Char => self.write_u8(6),
            TypeRepLit::String => self.write_u8(7),
            TypeRepLit::Bytes => self.write_u8(8),
            TypeRepLit::Array => self.write_u8(9),
            TypeRepLit::Tuple(arity) => {
                self.write_u8(10);
                let a: u32 = (*arity).try_into().map_err(|_| EncodeError {
                    message: "tuple arity overflow".to_string(),
                })?;
                self.write_u32(a);
            }
            TypeRepLit::Struct(name) => {
                self.write_u8(11);
                self.write_string(name)?;
            }
            TypeRepLit::Enum(name) => {
                self.write_u8(12);
                self.write_string(name)?;
            }
            TypeRepLit::Interface(name) => {
                self.write_u8(13);
                self.write_string(name)?;
            }
            TypeRepLit::Fn => self.write_u8(14),
            TypeRepLit::Cont => self.write_u8(15),
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
            Intrinsic::IntAnd => 72,
            Intrinsic::IntOr => 73,
            Intrinsic::IntXor => 74,
            Intrinsic::IntNot => 75,
            Intrinsic::IntShl => 76,
            Intrinsic::IntShr => 77,
            Intrinsic::IntUShr => 78,
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
            Intrinsic::ArrayLen => 34,
            Intrinsic::ArrayLenRo => 35,
            Intrinsic::ArrayPush => 36,
            Intrinsic::ArrayPop => 37,
            Intrinsic::ArrayClear => 38,
            Intrinsic::ArrayResize => 39,
            Intrinsic::ArrayInsert => 40,
            Intrinsic::ArrayRemove => 41,
            Intrinsic::ArrayExtend => 42,
            Intrinsic::ArrayConcat => 43,
            Intrinsic::ArrayConcatRo => 44,
            Intrinsic::ArraySlice => 45,
            Intrinsic::ArraySliceRo => 46,
            Intrinsic::IntToByte => 47,
            Intrinsic::IntTryByte => 48,
            Intrinsic::ByteToInt => 49,
            Intrinsic::ByteAnd => 79,
            Intrinsic::ByteOr => 80,
            Intrinsic::ByteXor => 81,
            Intrinsic::ByteNot => 82,
            Intrinsic::ByteShl => 83,
            Intrinsic::ByteShr => 84,
            Intrinsic::ByteUShr => 85,
            Intrinsic::IntToChar => 50,
            Intrinsic::IntTryChar => 51,
            Intrinsic::CharToInt => 52,
            Intrinsic::BytesGet => 53,
            Intrinsic::BytesLen => 54,
            Intrinsic::BytesSlice => 55,
            Intrinsic::BytesToArray => 56,
            Intrinsic::BytesFromArray => 57,
            Intrinsic::StringSlice => 58,
            Intrinsic::StringNextIndex => 59,
            Intrinsic::StringCodepointAt => 60,
            Intrinsic::StringFromChars => 61,
            Intrinsic::StringFromUtf8 => 62,
            Intrinsic::StringFromUtf8Strict => 63,
            Intrinsic::StringFromUtf16Le => 64,
            Intrinsic::StringFromUtf16LeStrict => 65,
            Intrinsic::StringFromUtf16Be => 66,
            Intrinsic::StringFromUtf16BeStrict => 67,
            Intrinsic::HashInt => 68,
            Intrinsic::HashString => 69,
            Intrinsic::HashBytes => 70,
            Intrinsic::HashCombine => 71,
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
            Instruction::MakeTypeRep { dst, base, args } => {
                self.write_u8(5);
                self.write_u32(*dst);
                self.write_type_rep_lit(base)?;
                self.write_vec_reg(args)?;
            }
            Instruction::MakeStruct {
                dst,
                type_id,
                type_args,
                fields,
            } => {
                self.write_u8(6);
                self.write_u32(*dst);
                self.write_u32(type_id.0);
                self.write_vec_reg(type_args)?;
                self.write_len(fields.len())?;
                for (name, reg) in fields {
                    self.write_string(name)?;
                    self.write_u32(*reg);
                }
            }
            Instruction::MakeArray { dst, items } => {
                self.write_u8(7);
                self.write_u32(*dst);
                self.write_vec_reg(items)?;
            }
            Instruction::MakeTuple { dst, items } => {
                self.write_u8(8);
                self.write_u32(*dst);
                self.write_vec_reg(items)?;
            }
            Instruction::MakeEnum {
                dst,
                enum_type_id,
                type_args,
                variant,
                fields,
            } => {
                self.write_u8(9);
                self.write_u32(*dst);
                self.write_u32(enum_type_id.0);
                self.write_vec_reg(type_args)?;
                self.write_string(variant)?;
                self.write_vec_reg(fields)?;
            }
            Instruction::GetField { dst, obj, field } => {
                self.write_u8(10);
                self.write_u32(*dst);
                self.write_u32(*obj);
                self.write_string(field)?;
            }
            Instruction::SetField { obj, field, value } => {
                self.write_u8(11);
                self.write_u32(*obj);
                self.write_string(field)?;
                self.write_u32(*value);
            }
            Instruction::StructGet { dst, obj, idx } => {
                self.write_u8(12);
                self.write_u32(*dst);
                self.write_u32(*obj);
                let n: u32 = (*idx).try_into().map_err(|_| EncodeError {
                    message: "struct field index overflow".to_string(),
                })?;
                self.write_u32(n);
            }
            Instruction::StructSet { obj, idx, value } => {
                self.write_u8(13);
                self.write_u32(*obj);
                let n: u32 = (*idx).try_into().map_err(|_| EncodeError {
                    message: "struct field index overflow".to_string(),
                })?;
                self.write_u32(n);
                self.write_u32(*value);
            }
            Instruction::TupleGet { dst, tup, idx } => {
                self.write_u8(14);
                self.write_u32(*dst);
                self.write_u32(*tup);
                let n: u32 = (*idx).try_into().map_err(|_| EncodeError {
                    message: "tuple index overflow".to_string(),
                })?;
                self.write_u32(n);
            }
            Instruction::TupleSet { tup, idx, value } => {
                self.write_u8(15);
                self.write_u32(*tup);
                let n: u32 = (*idx).try_into().map_err(|_| EncodeError {
                    message: "tuple index overflow".to_string(),
                })?;
                self.write_u32(n);
                self.write_u32(*value);
            }
            Instruction::IndexGet { dst, arr, idx } => {
                self.write_u8(16);
                self.write_u32(*dst);
                self.write_u32(*arr);
                self.write_u32(*idx);
            }
            Instruction::IndexSet { arr, idx, value } => {
                self.write_u8(17);
                self.write_u32(*arr);
                self.write_u32(*idx);
                self.write_u32(*value);
            }
            Instruction::Len { dst, arr } => {
                self.write_u8(18);
                self.write_u32(*dst);
                self.write_u32(*arr);
            }
            Instruction::IntAdd { dst, a, b } => {
                self.write_u8(19);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntSub { dst, a, b } => {
                self.write_u8(20);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntMul { dst, a, b } => {
                self.write_u8(21);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntDiv { dst, a, b } => {
                self.write_u8(22);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntMod { dst, a, b } => {
                self.write_u8(23);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntAnd { dst, a, b } => {
                self.write_u8(48);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntOr { dst, a, b } => {
                self.write_u8(49);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntXor { dst, a, b } => {
                self.write_u8(50);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntShl { dst, a, b } => {
                self.write_u8(51);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntShr { dst, a, b } => {
                self.write_u8(52);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntUShr { dst, a, b } => {
                self.write_u8(53);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntNot { dst, v } => {
                self.write_u8(54);
                self.write_u32(*dst);
                self.write_u32(*v);
            }
            Instruction::IntLt { dst, a, b } => {
                self.write_u8(24);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntLe { dst, a, b } => {
                self.write_u8(25);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntGt { dst, a, b } => {
                self.write_u8(26);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntGe { dst, a, b } => {
                self.write_u8(27);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntEq { dst, a, b } => {
                self.write_u8(28);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::IntNe { dst, a, b } => {
                self.write_u8(29);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::ByteAnd { dst, a, b } => {
                self.write_u8(55);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::ByteOr { dst, a, b } => {
                self.write_u8(56);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::ByteXor { dst, a, b } => {
                self.write_u8(57);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::ByteShl { dst, a, b } => {
                self.write_u8(58);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::ByteShr { dst, a, b } => {
                self.write_u8(59);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::ByteUShr { dst, a, b } => {
                self.write_u8(60);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::ByteNot { dst, v } => {
                self.write_u8(61);
                self.write_u32(*dst);
                self.write_u32(*v);
            }
            Instruction::BoolNot { dst, v } => {
                self.write_u8(30);
                self.write_u32(*dst);
                self.write_u32(*v);
            }
            Instruction::BoolEq { dst, a, b } => {
                self.write_u8(31);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::BoolNe { dst, a, b } => {
                self.write_u8(32);
                self.write_u32(*dst);
                self.write_u32(*a);
                self.write_u32(*b);
            }
            Instruction::Call { dst, func, args } => {
                self.write_u8(33);
                self.write_option_reg(dst)?;
                self.write_call_target(func)?;
                self.write_vec_reg(args)?;
            }
            Instruction::CallMulti { dsts, func, args } => {
                self.write_u8(46);
                self.write_vec_reg(dsts)?;
                self.write_call_target(func)?;
                self.write_vec_reg(args)?;
            }
            Instruction::ICall { dst, fnptr, args } => {
                self.write_u8(34);
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
                self.write_u8(35);
                self.write_option_reg(dst)?;
                self.write_u32(*obj);
                self.write_u32(method.0);
                self.write_vec_reg(method_type_args)?;
                self.write_vec_reg(args)?;
            }
            Instruction::PushHandler { clauses } => {
                self.write_u8(36);
                self.write_len(clauses.len())?;
                for clause in clauses {
                    self.write_handler_clause(clause)?;
                }
            }
            Instruction::PopHandler => {
                self.write_u8(37);
            }
            Instruction::Perform { dst, effect, args } => {
                self.write_u8(38);
                self.write_option_reg(dst)?;
                self.write_effect_spec(effect)?;
                self.write_vec_reg(args)?;
            }
            Instruction::Resume { dst, k, value } => {
                self.write_u8(39);
                self.write_option_reg(dst)?;
                self.write_u32(*k);
                self.write_u32(*value);
            }
            Instruction::ResumeTail { k, value } => {
                self.write_u8(45);
                self.write_u32(*k);
                self.write_u32(*value);
            }
            Instruction::Jump { target_pc } => {
                self.write_u8(40);
                self.write_u32(*target_pc);
            }
            Instruction::JumpIf {
                cond,
                then_pc,
                else_pc,
            } => {
                self.write_u8(41);
                self.write_u32(*cond);
                self.write_u32(*then_pc);
                self.write_u32(*else_pc);
            }
            Instruction::Switch {
                value,
                cases,
                default_pc,
            } => {
                self.write_u8(42);
                self.write_u32(*value);
                self.write_len(cases.len())?;
                for case in cases {
                    self.write_switch_case(case)?;
                }
                self.write_u32(*default_pc);
            }
            Instruction::Return { value } => {
                self.write_u8(43);
                self.write_u32(*value);
            }
            Instruction::ReturnMulti { values } => {
                self.write_u8(47);
                self.write_vec_reg(values)?;
            }
            Instruction::Trap { message } => {
                self.write_u8(44);
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

        // Interned type names.
        let type_len = self.read_len()?;
        let mut type_names = Vec::with_capacity(type_len);
        for _ in 0..type_len {
            type_names.push(self.read_string()?);
        }
        let mut type_ids = BTreeMap::new();
        for (idx, name) in type_names.iter().enumerate() {
            let id = TypeId(idx as u32);
            if type_ids.insert(name.clone(), id).is_some() {
                return Err(self.err(format!("duplicate type name `{name}`")));
            }
        }

        // Interned method names.
        let method_len = self.read_len()?;
        let mut method_names = Vec::with_capacity(method_len);
        for _ in 0..method_len {
            method_names.push(self.read_string()?);
        }
        let mut method_ids = BTreeMap::new();
        for (idx, name) in method_names.iter().enumerate() {
            let id = MethodId(idx as u32);
            if method_ids.insert(name.clone(), id).is_some() {
                return Err(self.err(format!("duplicate method name `{name}`")));
            }
        }

        // Dispatch table.
        let mut vcall_dispatch: Vec<Vec<(MethodId, FunctionId)>> = Vec::with_capacity(type_len);
        vcall_dispatch.resize_with(type_len, Vec::new);
        let dispatch_len = self.read_len()?;
        for _ in 0..dispatch_len {
            let type_id = TypeId(self.read_u32()?);
            let method_id = MethodId(self.read_u32()?);
            let fn_id = FunctionId(self.read_u32()?);
            let idx: usize = type_id
                .0
                .try_into()
                .map_err(|_| self.err("type id overflow".to_string()))?;
            let Some(entries) = vcall_dispatch.get_mut(idx) else {
                return Err(self.err(format!("invalid TypeId {} in dispatch entry", type_id.0)));
            };
            entries.push((method_id, fn_id));
        }

        // Interface impls.
        let mut interface_impls: Vec<Vec<TypeId>> = Vec::with_capacity(type_len);
        interface_impls.resize_with(type_len, Vec::new);
        let impl_len = self.read_len()?;
        for _ in 0..impl_len {
            let type_id = TypeId(self.read_u32()?);
            let idx: usize = type_id
                .0
                .try_into()
                .map_err(|_| self.err("type id overflow".to_string()))?;
            let Some(slot) = interface_impls.get_mut(idx) else {
                return Err(self.err(format!("invalid TypeId {} in interface impls", type_id.0)));
            };
            let iface_len = self.read_len()?;
            let mut ifaces = Vec::with_capacity(iface_len);
            for _ in 0..iface_len {
                ifaces.push(TypeId(self.read_u32()?));
            }
            *slot = ifaces;
        }

        // Struct layouts.
        let mut struct_layouts: Vec<Option<Vec<String>>> = Vec::with_capacity(type_len);
        struct_layouts.resize_with(type_len, || None);
        let layout_len = self.read_len()?;
        for _ in 0..layout_len {
            let type_id = TypeId(self.read_u32()?);
            let idx: usize = type_id
                .0
                .try_into()
                .map_err(|_| self.err("type id overflow".to_string()))?;
            let Some(slot) = struct_layouts.get_mut(idx) else {
                return Err(self.err(format!("invalid TypeId {} in struct layouts", type_id.0)));
            };
            if slot.is_some() {
                return Err(self.err(format!(
                    "duplicate struct layout entry for TypeId {}",
                    type_id.0
                )));
            }
            let field_len = self.read_len()?;
            let mut fields = Vec::with_capacity(field_len);
            for _ in 0..field_len {
                fields.push(self.read_string()?);
            }
            *slot = Some(fields);
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
            type_names,
            type_ids,
            method_names,
            method_ids,
            vcall_dispatch,
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
            6 => Ok(AbiType::Continuation),
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
            1 => Ok(TypeRepLit::Never),
            2 => Ok(TypeRepLit::Bool),
            3 => Ok(TypeRepLit::Int),
            4 => Ok(TypeRepLit::Float),
            5 => Ok(TypeRepLit::Byte),
            6 => Ok(TypeRepLit::Char),
            7 => Ok(TypeRepLit::String),
            8 => Ok(TypeRepLit::Bytes),
            9 => Ok(TypeRepLit::Array),
            10 => {
                let arity = self.read_u32()?;
                let arity_usize: usize = usize::try_from(arity)
                    .map_err(|_| self.err("tuple arity overflow".to_string()))?;
                Ok(TypeRepLit::Tuple(arity_usize))
            }
            11 => Ok(TypeRepLit::Struct(self.read_string()?)),
            12 => Ok(TypeRepLit::Enum(self.read_string()?)),
            13 => Ok(TypeRepLit::Interface(self.read_string()?)),
            14 => Ok(TypeRepLit::Fn),
            15 => Ok(TypeRepLit::Cont),
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
            34 => Intrinsic::ArrayLen,
            35 => Intrinsic::ArrayLenRo,
            36 => Intrinsic::ArrayPush,
            37 => Intrinsic::ArrayPop,
            38 => Intrinsic::ArrayClear,
            39 => Intrinsic::ArrayResize,
            40 => Intrinsic::ArrayInsert,
            41 => Intrinsic::ArrayRemove,
            42 => Intrinsic::ArrayExtend,
            43 => Intrinsic::ArrayConcat,
            44 => Intrinsic::ArrayConcatRo,
            45 => Intrinsic::ArraySlice,
            46 => Intrinsic::ArraySliceRo,
            47 => Intrinsic::IntToByte,
            48 => Intrinsic::IntTryByte,
            49 => Intrinsic::ByteToInt,
            50 => Intrinsic::IntToChar,
            51 => Intrinsic::IntTryChar,
            52 => Intrinsic::CharToInt,
            53 => Intrinsic::BytesGet,
            54 => Intrinsic::BytesLen,
            55 => Intrinsic::BytesSlice,
            56 => Intrinsic::BytesToArray,
            57 => Intrinsic::BytesFromArray,
            58 => Intrinsic::StringSlice,
            59 => Intrinsic::StringNextIndex,
            60 => Intrinsic::StringCodepointAt,
            61 => Intrinsic::StringFromChars,
            62 => Intrinsic::StringFromUtf8,
            63 => Intrinsic::StringFromUtf8Strict,
            64 => Intrinsic::StringFromUtf16Le,
            65 => Intrinsic::StringFromUtf16LeStrict,
            66 => Intrinsic::StringFromUtf16Be,
            67 => Intrinsic::StringFromUtf16BeStrict,
            68 => Intrinsic::HashInt,
            69 => Intrinsic::HashString,
            70 => Intrinsic::HashBytes,
            71 => Intrinsic::HashCombine,
            72 => Intrinsic::IntAnd,
            73 => Intrinsic::IntOr,
            74 => Intrinsic::IntXor,
            75 => Intrinsic::IntNot,
            76 => Intrinsic::IntShl,
            77 => Intrinsic::IntShr,
            78 => Intrinsic::IntUShr,
            79 => Intrinsic::ByteAnd,
            80 => Intrinsic::ByteOr,
            81 => Intrinsic::ByteXor,
            82 => Intrinsic::ByteNot,
            83 => Intrinsic::ByteShl,
            84 => Intrinsic::ByteShr,
            85 => Intrinsic::ByteUShr,
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
            5 => Ok(Instruction::MakeTypeRep {
                dst: self.read_u32()?,
                base: self.read_type_rep_lit()?,
                args: self.read_vec_reg()?,
            }),
            6 => {
                let dst = self.read_u32()?;
                let type_id = TypeId(self.read_u32()?);
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
                    type_id,
                    type_args,
                    fields,
                })
            }
            7 => Ok(Instruction::MakeArray {
                dst: self.read_u32()?,
                items: self.read_vec_reg()?,
            }),
            8 => Ok(Instruction::MakeTuple {
                dst: self.read_u32()?,
                items: self.read_vec_reg()?,
            }),
            9 => Ok(Instruction::MakeEnum {
                dst: self.read_u32()?,
                enum_type_id: TypeId(self.read_u32()?),
                type_args: self.read_vec_reg()?,
                variant: self.read_string()?,
                fields: self.read_vec_reg()?,
            }),
            10 => Ok(Instruction::GetField {
                dst: self.read_u32()?,
                obj: self.read_u32()?,
                field: self.read_string()?,
            }),
            11 => Ok(Instruction::SetField {
                obj: self.read_u32()?,
                field: self.read_string()?,
                value: self.read_u32()?,
            }),
            12 => {
                let dst = self.read_u32()?;
                let obj = self.read_u32()?;
                let idx = usize::try_from(self.read_u32()?)
                    .map_err(|_| self.err("struct field index overflow".to_string()))?;
                Ok(Instruction::StructGet { dst, obj, idx })
            }
            13 => {
                let obj = self.read_u32()?;
                let idx = usize::try_from(self.read_u32()?)
                    .map_err(|_| self.err("struct field index overflow".to_string()))?;
                let value = self.read_u32()?;
                Ok(Instruction::StructSet { obj, idx, value })
            }
            14 => {
                let dst = self.read_u32()?;
                let tup = self.read_u32()?;
                let idx = usize::try_from(self.read_u32()?)
                    .map_err(|_| self.err("tuple index overflow".to_string()))?;
                Ok(Instruction::TupleGet { dst, tup, idx })
            }
            15 => {
                let tup = self.read_u32()?;
                let idx = usize::try_from(self.read_u32()?)
                    .map_err(|_| self.err("tuple index overflow".to_string()))?;
                let value = self.read_u32()?;
                Ok(Instruction::TupleSet { tup, idx, value })
            }
            16 => Ok(Instruction::IndexGet {
                dst: self.read_u32()?,
                arr: self.read_u32()?,
                idx: self.read_u32()?,
            }),
            17 => Ok(Instruction::IndexSet {
                arr: self.read_u32()?,
                idx: self.read_u32()?,
                value: self.read_u32()?,
            }),
            18 => Ok(Instruction::Len {
                dst: self.read_u32()?,
                arr: self.read_u32()?,
            }),
            19 => Ok(Instruction::IntAdd {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            20 => Ok(Instruction::IntSub {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            21 => Ok(Instruction::IntMul {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            22 => Ok(Instruction::IntDiv {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            23 => Ok(Instruction::IntMod {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            48 => Ok(Instruction::IntAnd {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            49 => Ok(Instruction::IntOr {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            50 => Ok(Instruction::IntXor {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            51 => Ok(Instruction::IntShl {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            52 => Ok(Instruction::IntShr {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            53 => Ok(Instruction::IntUShr {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            54 => Ok(Instruction::IntNot {
                dst: self.read_u32()?,
                v: self.read_u32()?,
            }),
            24 => Ok(Instruction::IntLt {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            25 => Ok(Instruction::IntLe {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            26 => Ok(Instruction::IntGt {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            27 => Ok(Instruction::IntGe {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            28 => Ok(Instruction::IntEq {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            29 => Ok(Instruction::IntNe {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            55 => Ok(Instruction::ByteAnd {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            56 => Ok(Instruction::ByteOr {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            57 => Ok(Instruction::ByteXor {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            58 => Ok(Instruction::ByteShl {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            59 => Ok(Instruction::ByteShr {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            60 => Ok(Instruction::ByteUShr {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            61 => Ok(Instruction::ByteNot {
                dst: self.read_u32()?,
                v: self.read_u32()?,
            }),
            30 => Ok(Instruction::BoolNot {
                dst: self.read_u32()?,
                v: self.read_u32()?,
            }),
            31 => Ok(Instruction::BoolEq {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            32 => Ok(Instruction::BoolNe {
                dst: self.read_u32()?,
                a: self.read_u32()?,
                b: self.read_u32()?,
            }),
            33 => Ok(Instruction::Call {
                dst: self.read_option_reg()?,
                func: self.read_call_target()?,
                args: self.read_vec_reg()?,
            }),
            46 => Ok(Instruction::CallMulti {
                dsts: self.read_vec_reg()?,
                func: self.read_call_target()?,
                args: self.read_vec_reg()?,
            }),
            34 => Ok(Instruction::ICall {
                dst: self.read_option_reg()?,
                fnptr: self.read_u32()?,
                args: self.read_vec_reg()?,
            }),
            35 => Ok(Instruction::VCall {
                dst: self.read_option_reg()?,
                obj: self.read_u32()?,
                method: MethodId(self.read_u32()?),
                method_type_args: self.read_vec_reg()?,
                args: self.read_vec_reg()?,
            }),
            36 => {
                let n = self.read_len()?;
                let mut clauses = Vec::with_capacity(n);
                for _ in 0..n {
                    clauses.push(self.read_handler_clause()?);
                }
                Ok(Instruction::PushHandler { clauses })
            }
            37 => Ok(Instruction::PopHandler),
            38 => Ok(Instruction::Perform {
                dst: self.read_option_reg()?,
                effect: self.read_effect_spec()?,
                args: self.read_vec_reg()?,
            }),
            39 => Ok(Instruction::Resume {
                dst: self.read_option_reg()?,
                k: self.read_u32()?,
                value: self.read_u32()?,
            }),
            45 => Ok(Instruction::ResumeTail {
                k: self.read_u32()?,
                value: self.read_u32()?,
            }),
            40 => Ok(Instruction::Jump {
                target_pc: self.read_u32()?,
            }),
            41 => Ok(Instruction::JumpIf {
                cond: self.read_u32()?,
                then_pc: self.read_u32()?,
                else_pc: self.read_u32()?,
            }),
            42 => {
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
            43 => Ok(Instruction::Return {
                value: self.read_u32()?,
            }),
            47 => Ok(Instruction::ReturnMulti {
                values: self.read_vec_reg()?,
            }),
            44 => Ok(Instruction::Trap {
                message: self.read_string()?,
            }),
            other => Err(self.err(format!("invalid Instruction opcode {other}"))),
        }
    }
}
