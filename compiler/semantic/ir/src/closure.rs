//! Defines the [`Closure`], representing captured IR for closures, effect
//! handlers, do blocks, etc.

use pernixc_arena::Arena;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{address::Address, IR};

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

    /// The list of all captured memories (variables) from the parent IR.
    pub captures: Arena<Capture>,
}

/// Specifies how a memory is captured from the parent IR.
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
pub enum CaptureMode {
    /// Moves the captured memory address into the closure.
    ByValue,

    /// Borrows the captured memory address into the closure.
    ByReference,
}

/// Represents a captured memory from the parent IR.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Capture {
    /// The captured memory address from the parent IR.
    pub parent_memory: Address,

    /// Determines how the memory is captured into the closure object.
    pub capture_mode: CaptureMode,
}
