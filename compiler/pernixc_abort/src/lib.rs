//! Contains the definition of [`Abort`].

/// The error is caused by the user input that makes it impossible to continue
/// a particular process.
///
/// The diagnostic should've been reported to the user to inform them about the
/// error.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    thiserror::Error,
)]
#[error(
    "encountered an error from the user input that makes it impossible to \
     continue the process"
)]
pub struct Abort;

impl From<Abort> for std::fmt::Error {
    fn from(Abort: Abort) -> Self { Self }
}
