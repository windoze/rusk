use crate::abi::AbiValue;
use crate::error::HostError;

/// A host function callable by the VM.
///
/// The VM invokes host functions through `HostImportId` entries present in the loaded
/// `ExecutableModule`. Implementations are expected to validate argument invariants and return an
/// [`AbiValue`] (or an [`HostError`]) according to the declared ABI signature.
pub trait HostFn: 'static {
    /// Calls the host function with ABI values provided by the VM.
    fn call(&mut self, args: &[AbiValue]) -> Result<AbiValue, HostError>;
}

impl<F> HostFn for F
where
    F: for<'a> FnMut(&'a [AbiValue]) -> Result<AbiValue, HostError> + 'static,
{
    fn call(&mut self, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        self(args)
    }
}
