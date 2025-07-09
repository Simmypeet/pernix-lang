//! Contains the definitions related to the symbols.
use std::hash::Hash;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

/// Represents a unique identifier for the symbols in the compilation target.
/// This ID is only unique within the context of a single target. If wants to
/// use identifier across multiple targets, it should be combined with the
/// [`Global`]
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
    StableHash,
)]
pub struct ID(pub u64);

impl ID {
    /// The constant symbol ID that is fixed to zero for every target. It
    /// represents the root module of the target.
    pub const ROOT_MODULE: Self = Self(0);
}
