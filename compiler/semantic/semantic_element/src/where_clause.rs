//! Contains the definition of [`WhereClause`] component.

use pernixc_lexical::tree::RelativeSpan;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, storage::intern::Interned,
};

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
    Encode,
    Decode,
)]
pub struct Predicate {
    /// The predicate itself.
    pub predicate: pernixc_term::predicate::Predicate,

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
    Encode,
    Decode,
    Query,
)]
#[value(Interned<[Predicate]>)]
#[extend(name = get_where_clause, by_val)]
pub struct Key {
    /// The global ID of the symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}
