//! This crate contains the information about the target of the compilation.

use serde::{Deserialize, Serialize};

/// Represents an identifier for a target.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum TargetID {
    /// Representing a target that is being compiled at the moment.
    Local,

    /// Representing a `core` target.
    Core,

    /// Represents an externally defined targets that are being consumed by the
    /// current [`Self::Local`].
    Extern(u64),
}
