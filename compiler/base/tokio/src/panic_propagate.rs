//! Defines the extension trait for propagating panics and cancellations.

use tokio::task::JoinError;

/// The error type returned by the `panic_propagate` method.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("task was cancelled")]
pub struct Cancelled;

/// Defines the extension trait for propagating panics thrown from asynchronous
/// task.
pub trait PanicPropagate {
    /// The `Ok` type of the result.
    type Result;

    /// Propagates the panic thrown from the asynchronous task or return the
    /// result.
    fn panic_propagate(self) -> Result<Self::Result, Cancelled>;
}

impl<T> PanicPropagate for Result<T, JoinError> {
    type Result = T;

    fn panic_propagate(self) -> Result<Self::Result, Cancelled> {
        match self {
            Ok(value) => Ok(value),
            Err(err) => {
                if err.is_panic() {
                    std::panic::resume_unwind(err.into_panic())
                } else if err.is_cancelled() {
                    Err(Cancelled)
                } else {
                    panic!("unexpected error {err:?}")
                }
            }
        }
    }
}
