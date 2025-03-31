//! Contains the definition of [`WhereClause`] component.

use pernixc_semantic::component::Derived;
use pernixc_source_file::Span;
use serde::{Deserialize, Serialize};

use crate::{predicate, Default};

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
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct WhereClause {
    /// The list of predicates declared in the where clause.
    pub predicates: Vec<Predicate>,
}

impl Derived for WhereClause {
    fn component_name() -> &'static str { "where clause" }
}
