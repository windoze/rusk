use core::marker::PhantomData;
use rusk_bytecode::{AbiType, TypeId};
use rusk_gc::GcRef;

use crate::vm::ContinuationHandle;

/// A runtime value at the VM/host ABI boundary.
///
/// These values are intentionally restricted to ABI-safe primitives plus opaque continuation
/// handles. Conversions between [`AbiValue`] and the VM-internal `Value` happen at host-call
/// boundaries and when producing a final program result.
#[derive(Clone, Debug, PartialEq)]
pub enum AbiValue {
    /// The unit value `()`.
    Unit,
    /// A boolean.
    Bool(bool),
    /// A signed 64-bit integer.
    Int(i64),
    /// A 64-bit floating point number.
    Float(f64),
    /// An unsigned 8-bit integer.
    Byte(u8),
    /// A Unicode scalar value.
    Char(char),
    /// A UTF-8 string.
    String(String),
    /// An arbitrary byte buffer.
    Bytes(Vec<u8>),
    /// An opaque continuation handle pinned in the VM.
    ///
    /// The handle is only meaningful within the VM instance it came from.
    Continuation(ContinuationHandle),
    /// A reference to a VM array object.
    Array(AbiArrayRef),
    /// A reference to a VM tuple object.
    Tuple(AbiTupleRef),
    /// A reference to a VM struct object.
    Struct(AbiStructRef),
    /// A reference to a VM enum object.
    Enum(AbiEnumRef),
}

/// An opaque, VM-validated reference to an array object, carrying an ABI element type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbiArrayRef {
    handle: GcRef,
    readonly: bool,
    elem_ty: Box<AbiType>,
}

impl AbiArrayRef {
    pub(crate) fn new(handle: GcRef, readonly: bool, elem_ty: AbiType) -> Self {
        Self {
            handle,
            readonly,
            elem_ty: Box::new(elem_ty),
        }
    }

    pub(crate) fn handle(&self) -> GcRef {
        self.handle
    }

    pub(crate) fn readonly(&self) -> bool {
        self.readonly
    }

    pub fn elem_ty(&self) -> &AbiType {
        &self.elem_ty
    }
}

/// An opaque, VM-validated reference to a tuple object, carrying ABI element types.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbiTupleRef {
    handle: GcRef,
    readonly: bool,
    item_tys: Vec<AbiType>,
}

impl AbiTupleRef {
    pub(crate) fn new(handle: GcRef, readonly: bool, item_tys: Vec<AbiType>) -> Self {
        Self {
            handle,
            readonly,
            item_tys,
        }
    }

    pub(crate) fn handle(&self) -> GcRef {
        self.handle
    }

    pub(crate) fn readonly(&self) -> bool {
        self.readonly
    }

    pub fn item_tys(&self) -> &[AbiType] {
        &self.item_tys
    }
}

/// An opaque, VM-validated reference to a struct object.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbiStructRef {
    handle: GcRef,
    readonly: bool,
    type_id: TypeId,
}

impl AbiStructRef {
    pub(crate) fn new(handle: GcRef, readonly: bool, type_id: TypeId) -> Self {
        Self {
            handle,
            readonly,
            type_id,
        }
    }

    pub(crate) fn handle(&self) -> GcRef {
        self.handle
    }

    pub(crate) fn readonly(&self) -> bool {
        self.readonly
    }

    pub fn type_id(&self) -> TypeId {
        self.type_id
    }
}

/// An opaque, VM-validated reference to an enum object.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbiEnumRef {
    handle: GcRef,
    readonly: bool,
    type_id: TypeId,
}

impl AbiEnumRef {
    pub(crate) fn new(handle: GcRef, readonly: bool, type_id: TypeId) -> Self {
        Self {
            handle,
            readonly,
            type_id,
        }
    }

    pub(crate) fn handle(&self) -> GcRef {
        self.handle
    }

    pub(crate) fn readonly(&self) -> bool {
        self.readonly
    }

    pub fn type_id(&self) -> TypeId {
        self.type_id
    }
}

impl AbiValue {
    /// Returns the corresponding ABI type tag for this value.
    pub fn ty(&self) -> AbiType {
        match self {
            Self::Unit => AbiType::Unit,
            Self::Bool(_) => AbiType::Bool,
            Self::Int(_) => AbiType::Int,
            Self::Float(_) => AbiType::Float,
            Self::Byte(_) => AbiType::Byte,
            Self::Char(_) => AbiType::Char,
            Self::String(_) => AbiType::String,
            Self::Bytes(_) => AbiType::Bytes,
            Self::Continuation(_) => AbiType::Continuation,
            Self::Array(r) => AbiType::Array(Box::new(r.elem_ty.as_ref().clone())),
            Self::Tuple(r) => AbiType::Tuple(r.item_tys.clone()),
            Self::Struct(r) => AbiType::Struct(r.type_id),
            Self::Enum(r) => AbiType::Enum(r.type_id),
        }
    }

    /// Decodes this ABI value into a Rust type.
    ///
    /// This is intended for ergonomic host import and external effect handling code.
    pub fn decode<T: AbiDecode>(&self) -> Result<T, AbiDecodeError> {
        T::decode(self)
    }
}

impl From<()> for AbiValue {
    fn from((): ()) -> Self {
        Self::Unit
    }
}

impl From<bool> for AbiValue {
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}

impl From<i64> for AbiValue {
    fn from(v: i64) -> Self {
        Self::Int(v)
    }
}

impl From<f64> for AbiValue {
    fn from(v: f64) -> Self {
        Self::Float(v)
    }
}

impl From<u8> for AbiValue {
    fn from(v: u8) -> Self {
        Self::Byte(v)
    }
}

impl From<char> for AbiValue {
    fn from(v: char) -> Self {
        Self::Char(v)
    }
}

impl From<String> for AbiValue {
    fn from(v: String) -> Self {
        Self::String(v)
    }
}

impl From<&str> for AbiValue {
    fn from(v: &str) -> Self {
        Self::String(v.to_string())
    }
}

impl From<Vec<u8>> for AbiValue {
    fn from(v: Vec<u8>) -> Self {
        Self::Bytes(v)
    }
}

impl From<&[u8]> for AbiValue {
    fn from(v: &[u8]) -> Self {
        Self::Bytes(v.to_vec())
    }
}

impl From<ContinuationHandle> for AbiValue {
    fn from(v: ContinuationHandle) -> Self {
        Self::Continuation(v)
    }
}

/// A typed delimited continuation value at the ABI boundary.
///
/// At runtime, this is represented as an opaque [`ContinuationHandle`], but the type parameters
/// preserve the `cont(P) -> R` shape in host APIs.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cont<P, R> {
    handle: ContinuationHandle,
    _marker: PhantomData<fn(P) -> R>,
}

impl<P, R> Cont<P, R> {
    pub fn new(handle: ContinuationHandle) -> Self {
        Self {
            handle,
            _marker: PhantomData,
        }
    }

    pub fn handle(&self) -> &ContinuationHandle {
        &self.handle
    }

    pub fn into_handle(self) -> ContinuationHandle {
        self.handle
    }
}

impl<P, R> From<ContinuationHandle> for Cont<P, R> {
    fn from(handle: ContinuationHandle) -> Self {
        Self::new(handle)
    }
}

impl<P, R> From<Cont<P, R>> for AbiValue {
    fn from(v: Cont<P, R>) -> Self {
        Self::Continuation(v.handle)
    }
}

/// ABI decoding error produced by typed host adapters.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AbiDecodeError {
    /// Argument list length did not match what the typed signature expects.
    ArityMismatch { expected: usize, got: usize },
    /// A value did not match the expected ABI type.
    TypeMismatch {
        /// Argument index when decoding an argument list.
        ///
        /// `None` means the mismatch occurred when decoding a single value.
        index: Option<usize>,
        expected: AbiType,
        got: AbiType,
    },
}

impl AbiDecodeError {
    fn value_mismatch(expected: AbiType, got: AbiType) -> Self {
        Self::TypeMismatch {
            index: None,
            expected,
            got,
        }
    }

    fn arg_mismatch(index: usize, expected: AbiType, got: AbiType) -> Self {
        Self::TypeMismatch {
            index: Some(index),
            expected,
            got,
        }
    }
}

impl core::fmt::Display for AbiDecodeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            AbiDecodeError::ArityMismatch { expected, got } => {
                write!(f, "abi arity mismatch: expected {expected}, got {got}")
            }
            AbiDecodeError::TypeMismatch {
                index: Some(index),
                expected,
                got,
            } => write!(
                f,
                "abi arg {index} type mismatch: expected {expected:?}, got {got:?}"
            ),
            AbiDecodeError::TypeMismatch {
                index: None,
                expected,
                got,
            } => write!(
                f,
                "abi value type mismatch: expected {expected:?}, got {got:?}"
            ),
        }
    }
}

impl std::error::Error for AbiDecodeError {}

/// Rust types that correspond to a single ABI value.
pub trait AbiTypeOf {
    fn abi_type() -> AbiType;
}

impl AbiTypeOf for () {
    fn abi_type() -> AbiType {
        AbiType::Unit
    }
}

impl AbiTypeOf for bool {
    fn abi_type() -> AbiType {
        AbiType::Bool
    }
}

impl AbiTypeOf for i64 {
    fn abi_type() -> AbiType {
        AbiType::Int
    }
}

impl AbiTypeOf for f64 {
    fn abi_type() -> AbiType {
        AbiType::Float
    }
}

impl AbiTypeOf for u8 {
    fn abi_type() -> AbiType {
        AbiType::Byte
    }
}

impl AbiTypeOf for char {
    fn abi_type() -> AbiType {
        AbiType::Char
    }
}

impl AbiTypeOf for String {
    fn abi_type() -> AbiType {
        AbiType::String
    }
}

impl AbiTypeOf for Vec<u8> {
    fn abi_type() -> AbiType {
        AbiType::Bytes
    }
}

impl AbiTypeOf for ContinuationHandle {
    fn abi_type() -> AbiType {
        AbiType::Continuation
    }
}

impl<P, R> AbiTypeOf for Cont<P, R> {
    fn abi_type() -> AbiType {
        AbiType::Continuation
    }
}

/// Decodes a single ABI value into a Rust type.
pub trait AbiDecode: AbiTypeOf + Sized {
    fn decode(value: &AbiValue) -> Result<Self, AbiDecodeError>;
}

impl AbiDecode for () {
    fn decode(value: &AbiValue) -> Result<Self, AbiDecodeError> {
        match value {
            AbiValue::Unit => Ok(()),
            other => Err(AbiDecodeError::value_mismatch(Self::abi_type(), other.ty())),
        }
    }
}

impl AbiDecode for bool {
    fn decode(value: &AbiValue) -> Result<Self, AbiDecodeError> {
        match value {
            AbiValue::Bool(v) => Ok(*v),
            other => Err(AbiDecodeError::value_mismatch(Self::abi_type(), other.ty())),
        }
    }
}

impl AbiDecode for i64 {
    fn decode(value: &AbiValue) -> Result<Self, AbiDecodeError> {
        match value {
            AbiValue::Int(v) => Ok(*v),
            other => Err(AbiDecodeError::value_mismatch(Self::abi_type(), other.ty())),
        }
    }
}

impl AbiDecode for f64 {
    fn decode(value: &AbiValue) -> Result<Self, AbiDecodeError> {
        match value {
            AbiValue::Float(v) => Ok(*v),
            other => Err(AbiDecodeError::value_mismatch(Self::abi_type(), other.ty())),
        }
    }
}

impl AbiDecode for u8 {
    fn decode(value: &AbiValue) -> Result<Self, AbiDecodeError> {
        match value {
            AbiValue::Byte(v) => Ok(*v),
            other => Err(AbiDecodeError::value_mismatch(Self::abi_type(), other.ty())),
        }
    }
}

impl AbiDecode for char {
    fn decode(value: &AbiValue) -> Result<Self, AbiDecodeError> {
        match value {
            AbiValue::Char(v) => Ok(*v),
            other => Err(AbiDecodeError::value_mismatch(Self::abi_type(), other.ty())),
        }
    }
}

impl AbiDecode for String {
    fn decode(value: &AbiValue) -> Result<Self, AbiDecodeError> {
        match value {
            AbiValue::String(v) => Ok(v.clone()),
            other => Err(AbiDecodeError::value_mismatch(Self::abi_type(), other.ty())),
        }
    }
}

impl AbiDecode for Vec<u8> {
    fn decode(value: &AbiValue) -> Result<Self, AbiDecodeError> {
        match value {
            AbiValue::Bytes(v) => Ok(v.clone()),
            other => Err(AbiDecodeError::value_mismatch(Self::abi_type(), other.ty())),
        }
    }
}

impl AbiDecode for ContinuationHandle {
    fn decode(value: &AbiValue) -> Result<Self, AbiDecodeError> {
        match value {
            AbiValue::Continuation(v) => Ok(v.clone()),
            other => Err(AbiDecodeError::value_mismatch(Self::abi_type(), other.ty())),
        }
    }
}

impl<P, R> AbiDecode for Cont<P, R> {
    fn decode(value: &AbiValue) -> Result<Self, AbiDecodeError> {
        let handle = ContinuationHandle::decode(value)?;
        Ok(Self::new(handle))
    }
}

/// Rust types that correspond to an ABI argument list.
///
/// This is implemented for tuples up to a fixed arity.
pub trait AbiArgs<'a>: AbiParamTypes + Sized {
    fn decode(args: &'a [AbiValue]) -> Result<Self, AbiDecodeError>;
}

/// Rust types that correspond to an ABI parameter list (types only).
pub trait AbiParamTypes {
    fn abi_param_types() -> Vec<AbiType>;
}

impl AbiParamTypes for () {
    fn abi_param_types() -> Vec<AbiType> {
        Vec::new()
    }
}

impl<'a> AbiArgs<'a> for () {
    fn decode(args: &'a [AbiValue]) -> Result<Self, AbiDecodeError> {
        if !args.is_empty() {
            return Err(AbiDecodeError::ArityMismatch {
                expected: 0,
                got: args.len(),
            });
        }
        Ok(())
    }
}

macro_rules! impl_abi_args_tuple {
    ($len:expr, $( ($t:ident, $v:ident) : $idx:tt ),+ $(,)?) => {
        impl<$( $t ),+> AbiParamTypes for ( $( $t, )+ )
        where
            $( $t: AbiTypeOf ),+
        {
            fn abi_param_types() -> Vec<AbiType> {
                vec![ $( $t::abi_type(), )+ ]
            }
        }

        impl<'a, $( $t ),+> AbiArgs<'a> for ( $( $t, )+ )
        where
            $( $t: AbiDecode ),+
        {
            fn decode(args: &'a [AbiValue]) -> Result<Self, AbiDecodeError> {
                if args.len() != $len {
                    return Err(AbiDecodeError::ArityMismatch { expected: $len, got: args.len() });
                }
                $(
                    let $v = match &args[$idx] {
                        v if v.ty() == <$t as AbiTypeOf>::abi_type() => <$t as AbiDecode>::decode(v)
                            .map_err(|e| match e {
                                AbiDecodeError::ArityMismatch { .. } => e,
                                AbiDecodeError::TypeMismatch { expected, got, .. } => AbiDecodeError::arg_mismatch($idx, expected, got),
                            })?,
                        v => return Err(AbiDecodeError::arg_mismatch($idx, <$t as AbiTypeOf>::abi_type(), v.ty())),
                    };
                )+
                Ok(( $( $v, )+ ))
            }
        }
    };
}

impl_abi_args_tuple!(1, (T0, v0):0);
impl_abi_args_tuple!(2, (T0, v0):0, (T1, v1):1);
impl_abi_args_tuple!(3, (T0, v0):0, (T1, v1):1, (T2, v2):2);
impl_abi_args_tuple!(4, (T0, v0):0, (T1, v1):1, (T2, v2):2, (T3, v3):3);
impl_abi_args_tuple!(5, (T0, v0):0, (T1, v1):1, (T2, v2):2, (T3, v3):3, (T4, v4):4);
impl_abi_args_tuple!(6, (T0, v0):0, (T1, v1):1, (T2, v2):2, (T3, v3):3, (T4, v4):4, (T5, v5):5);
impl_abi_args_tuple!(7, (T0, v0):0, (T1, v1):1, (T2, v2):2, (T3, v3):3, (T4, v4):4, (T5, v5):5, (T6, v6):6);
impl_abi_args_tuple!(8, (T0, v0):0, (T1, v1):1, (T2, v2):2, (T3, v3):3, (T4, v4):4, (T5, v5):5, (T6, v6):6, (T7, v7):7);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn abi_value_from_primitives() {
        assert_eq!(AbiValue::from(()), AbiValue::Unit);
        assert_eq!(AbiValue::from(true), AbiValue::Bool(true));
        assert_eq!(AbiValue::from(123_i64), AbiValue::Int(123));
        assert_eq!(AbiValue::from(1.25_f64), AbiValue::Float(1.25));
        assert_eq!(AbiValue::from("hi"), AbiValue::String("hi".to_string()));
        assert_eq!(
            AbiValue::from(b"ab".as_slice()),
            AbiValue::Bytes(vec![b'a', b'b'])
        );
    }

    #[test]
    fn abi_value_decode_primitives() {
        assert_eq!(AbiValue::Unit.decode::<()>().unwrap(), ());
        assert!(!AbiValue::Bool(false).decode::<bool>().unwrap());
        assert_eq!(AbiValue::Int(7).decode::<i64>().unwrap(), 7);
        assert_eq!(AbiValue::Float(3.0).decode::<f64>().unwrap(), 3.0);
        assert_eq!(
            AbiValue::String("x".to_string())
                .decode::<String>()
                .unwrap(),
            "x".to_string()
        );
        assert_eq!(
            AbiValue::Bytes(vec![1, 2]).decode::<Vec<u8>>().unwrap(),
            vec![1, 2]
        );
    }

    #[test]
    fn abi_value_decode_type_mismatch() {
        let err = AbiValue::Bool(true).decode::<i64>().unwrap_err();
        assert_eq!(
            err,
            AbiDecodeError::TypeMismatch {
                index: None,
                expected: AbiType::Int,
                got: AbiType::Bool
            }
        );
    }

    #[test]
    fn abi_args_decode_tuples() {
        let args = [AbiValue::Int(1), AbiValue::Bool(true)];
        let got = <(i64, bool) as AbiArgs<'_>>::decode(&args).unwrap();
        assert_eq!(got, (1, true));
    }

    #[test]
    fn abi_args_decode_arity_mismatch() {
        let args = [AbiValue::Int(1)];
        let err = <(i64, bool) as AbiArgs<'_>>::decode(&args).unwrap_err();
        assert_eq!(
            err,
            AbiDecodeError::ArityMismatch {
                expected: 2,
                got: 1
            }
        );
    }

    #[test]
    fn abi_args_decode_arg_type_mismatch() {
        let args = [AbiValue::Bool(true), AbiValue::Bool(false)];
        let err = <(i64, bool) as AbiArgs<'_>>::decode(&args).unwrap_err();
        assert_eq!(
            err,
            AbiDecodeError::TypeMismatch {
                index: Some(0),
                expected: AbiType::Int,
                got: AbiType::Bool
            }
        );
    }

    #[test]
    fn abi_cont_roundtrip() {
        let handle = ContinuationHandle {
            index: 123,
            generation: 456,
        };

        let v: AbiValue = Cont::<i64, i64>::new(handle.clone()).into();
        assert_eq!(v.ty(), AbiType::Continuation);

        let got = v.decode::<Cont<i64, i64>>().unwrap();
        assert_eq!(got.handle(), &handle);
    }
}
