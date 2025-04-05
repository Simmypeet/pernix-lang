//! This crate contains the information about the target of the compilation.

use derive_more::{Deref, DerefMut};
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
    Deref,
    DerefMut,
)]
pub struct TargetID(pub u64);

impl TargetID {
    /// The core target.
    pub const CORE: Self = Self(0);
}
