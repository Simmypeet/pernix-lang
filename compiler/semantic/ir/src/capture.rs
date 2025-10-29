//! Defines the [`Closure`], representing captured IR for closures, effect
//! handlers, do blocks, etc.

use derive_more::{Index, IndexMut};
use pernixc_arena::Arena;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::{
    lifetime::Lifetime,
    r#type::{Qualifier, Type},
};

use crate::address::Address;

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
    IndexMut,
)]
pub struct Captures {
    /// All the captures used in the closure.
    #[index]
    #[index_mut]
    captures: Arena<Capture>,
}

impl Captures {
    /// Inserts a new capture into the capturing structure.
    pub fn insert(&mut self, capture: Capture) -> pernixc_arena::ID<Capture> {
        self.captures.insert(capture)
    }

    /// Removes a capture from the capturing structure.
    pub fn remove(
        &mut self,
        capture_id: pernixc_arena::ID<Capture>,
    ) -> Option<Capture> {
        self.captures.remove(capture_id)
    }

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
}
