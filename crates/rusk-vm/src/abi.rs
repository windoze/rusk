use rusk_bytecode::AbiType;

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
    /// A UTF-8 string.
    String(String),
    /// An arbitrary byte buffer.
    Bytes(Vec<u8>),
    /// An opaque continuation handle pinned in the VM.
    ///
    /// The handle is only meaningful within the VM instance it came from.
    Continuation(ContinuationHandle),
}

impl AbiValue {
    /// Returns the corresponding ABI type tag for this value.
    pub fn ty(&self) -> AbiType {
        match self {
            Self::Unit => AbiType::Unit,
            Self::Bool(_) => AbiType::Bool,
            Self::Int(_) => AbiType::Int,
            Self::Float(_) => AbiType::Float,
            Self::String(_) => AbiType::String,
            Self::Bytes(_) => AbiType::Bytes,
            Self::Continuation(_) => AbiType::Continuation,
        }
    }
}
