//! Contains the definition of [`WhereClause`] component.

use pernixc_lexical::tree::RelativeSpan;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

/// Represents a predicate introduced by either a where clause or implication.
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
pub struct Predicate {
    /// The predicate itself.
    pub predicate: crate::predicate::Predicate,

    /// The span where the predicate was declared.
    pub span: Option<RelativeSpan>,
}

/// Represents the where clause of a particular symbol, defined via the
/// `where: ...` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Default,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct WhereClause {
    /// The list of predicates declared in the where clause.
    pub predicates: Vec<Predicate>,
}
