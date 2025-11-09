//! Contains the definition of [`ClosureParameters`].

use derive_more::Index;
use pernixc_arena::{OrderedArena, ID};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::r#type::Type;

/// Represents a parameter taken by a closure.
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
pub struct ClosureParameter {
    /// The type of the parameter.
    pub r#type: Type,

    /// The span of the parameter.
    pub span: Option<RelativeSpan>,
}

/// Represents a collection of parameters taken by a closure.
#[derive(
    Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize, Index,
)]
pub struct ClosureParameters(OrderedArena<ClosureParameter>);

impl ClosureParameters {
    /// Returns an iterator over the IDs of the parameters.
    #[must_use]
    pub fn ids(
        &self,
    ) -> impl ExactSizeIterator<Item = ID<ClosureParameter>> + '_ {
        self.0.ids()
    }
}
