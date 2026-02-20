#![forbid(unsafe_code)]

extern crate alloc;

mod abi;
mod error;
mod host;
mod metrics;
mod vm;

pub use abi::AbiValue;
pub use error::{HostError, VmError};
pub use host::HostFn;
pub use metrics::VmMetrics;
pub use vm::{
    ContinuationHandle, StepResult, TypeRepId, Vm, vm_drop_continuation, vm_resume, vm_step,
};
