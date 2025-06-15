//! This crate contains the information about the target of the compilation.

use derive_new::new;
use pernixc_serialize::{Deserialize, Serialize};

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
    Default,
    Serialize,
    Deserialize,
)]
pub enum TargetID {
    /// Representing a target that is being compiled at the moment.
    #[default]
    Local,

    /// Representing a `core` target.
    Core,

    /// Represents an externally defined targets that are being consumed by the
    /// current [`Self::Local`].
    Extern(u64),
}

impl TargetID {
    /// Creates a new [`Global`] identifier from the given [`TargetID`] and the
    /// given local identifier.
    #[must_use]
    pub const fn make_global<ID>(self, id: ID) -> Global<ID> {
        Global { id, target_id: self }
    }
}

/// A struct used for identifying an entity across different targets.
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
    Serialize,
    Deserialize,
    new,
)]
pub struct Global<ID> {
    /// The identifier to the target that the entity is defined in.
    pub target_id: TargetID,

    /// The identifier to the local entity defined within the target.
    pub id: ID,
}
