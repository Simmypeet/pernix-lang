//! Defines the [`Closure`], representing captured IR for closures, effect
//! handlers, do blocks, etc.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::IR;

/// Specifies what [`Closure`] is being used for.
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

/// Represents a captured IR for closures; can be used for annonymous
/// functions, effect handlers, do blocks, etc.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct Closure {
    /// The IR of the inner procedure.
    pub ir: IR,

    /// The kind of capture.
    pub kind: Kind,
}
