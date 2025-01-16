//! Contains the definition of [`WhereClause`] component.

use pernixc_arena::{Arena, ID};
use pernixc_source_file::Span;
use pernixc_table::{component::Derived, MemberID};
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

/// Represents a forall lifetime declared with `for['a]` syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct ForallLifetime {
    /// The name given to the forall lifetime.
    pub name: String,

    /// The span where the forall lifetime was declared.
    #[serde(skip)]
    pub span: Option<Span>,
}

/// A **presistent-derived** component representing the where clause declared in
/// the symbol e.g. `where trait Fizz[..], ..`.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct WhereClause {
    /// The list of predicates declared in the where clause.
    pub predicates: Vec<Predicate>,

    /// The list of all the forall lifetimes declared in the where clause.
    pub forall_lifetimes: Arena<ForallLifetime>,
}

impl Derived for WhereClause {
    fn component_name() -> &'static str { "wherer clause" }
}

/// Unique ID for the forall lifetime.
pub type ForallLifetimeID = MemberID<ID<ForallLifetime>>;
