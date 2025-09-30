//! Defines the [`Capture`], representing captured IR for closures, effect
//! handlers, do blocks, etc.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::IR;

/// Specifies what [`Capture`] is being used for.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub enum Kind {
    /// Captured as a do block.
    DoBlock,

    /// Captured as an effect handler.
    EffectHandler,
}

/// Represents a captured IR for closures, effect handlers, do blocks, etc.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct Capture {
    /// The IR of the inner procedure.
    pub ir: IR,

    /// The kind of capture.
    pub kind: Kind,
}
