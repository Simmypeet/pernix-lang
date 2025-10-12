//! Defines the [`Closure`], representing captured IR for closures, effect
//! handlers, do blocks, etc.

use pernixc_arena::Arena;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::r#type::Type;

use crate::address::Address;

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

/// Represents capturing structure used for implementing closures, do blocks,
/// and effect handlers.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct Captures {
    /// All the captures used in the closure.
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
    pub parent_captured_address: Address,

    /// The type of the captured memory address.
    pub address_type: Type,

    /// Determines how the memory is captured into the closure object.
    pub capture_mode: CaptureMode,

    /// The span of the captured memory address.
    pub span: Option<RelativeSpan>,
}
