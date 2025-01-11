use pernixc_base::source_file::Span;
use serde::{Deserialize, Serialize};

use crate::type_system::{model::Default, predicate};

/// Represents a predicate introduced by either a where clause or implication.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Predicate {
    /// The predicate itself.
    pub predicate: predicate::Predicate<Default>,

    /// The span where the predicate was declared.
    #[serde(skip)]
    pub span: Option<Span>,
}

/// A **presistent-derived** component representing the where clause declared in
/// the symbol e.g. `where trait Fizz[..], ..`.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct WhereClause {
    /// The list of predicates declared in the where clause.
    pub predicates: Vec<Predicate>,
}
