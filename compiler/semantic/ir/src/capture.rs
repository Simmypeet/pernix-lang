//! Defines the [`Captures`] representing the capturing structure used for
//! implementing closures, do blocks, and effect handlers.

use derive_more::Index;
use pernixc_arena::Arena;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::{
    lifetime::Lifetime,
    r#type::{Qualifier, Type},
};

use crate::address::Address;

pub mod builder;
pub mod pruning;

/// Represents capturing structure used for implementing closures, do blocks,
/// and effect handlers.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    StableHash,
    Serialize,
    Deserialize,
    Default,
    Index,
)]
pub struct Captures {
    /// All the captures used in the closure.
    #[index]
    captures: Arena<Capture>,
}

impl Captures {
    /// Returns an iterator over all capture IDs in the capturing structure.
    #[must_use]
    pub fn ids(
        &self,
    ) -> impl ExactSizeIterator<Item = pernixc_arena::ID<Capture>> + '_ {
        self.captures.ids()
    }
}

/// Specifies how a memory is captured from the parent IR.
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
pub enum CaptureMode {
    /// Moves the captured memory address into the closure.
    ByValue,

    /// Borrows the captured memory address into the closure.
    ByReference(ReferenceCaptureMode),
}

/// Represents a variant of [`CaptureMode::ByReference`]
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
pub struct ReferenceCaptureMode {
    /// The lifetime of the captured memory address.
    pub lifetime: Lifetime,

    /// The reference qualifier of the captured memory address.
    pub qualifier: Qualifier,
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

    /// The drop of the capture memory in (0 being the first to drop).
    pub drop_order: usize,
}
