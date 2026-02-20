/// Public errors returned by the VM API.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VmError {
    /// The VM is in an invalid state for the requested operation.
    InvalidState { message: String },
    /// A continuation handle was invalid or did not match the currently suspended VM state.
    InvalidContinuation { message: String },
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VmError::InvalidState { message } => write!(f, "invalid vm state: {message}"),
            VmError::InvalidContinuation { message } => {
                write!(f, "invalid continuation: {message}")
            }
        }
    }
}

impl std::error::Error for VmError {}

/// An error raised by a host function invoked by the VM.
#[derive(Debug)]
pub struct HostError {
    /// A human-readable error message.
    pub message: String,
}

impl std::fmt::Display for HostError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "host error: {}", self.message)
    }
}

impl std::error::Error for HostError {}
