//! Contains the definition of [`WhereClause`] component.

use std::sync::Arc;

use pernixc_lexical::tree::RelativeSpan;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;
use pernixc_target::Global;

/// Represents a predicate introduced by either a where clause or implication.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Identifiable,
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
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Default,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(Arc<[Predicate]>)]
#[extend(method(get_where_clause))]
pub struct Key(pub Global<pernixc_symbol::ID>);
