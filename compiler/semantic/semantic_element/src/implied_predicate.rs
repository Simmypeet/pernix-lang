//! Contains the query definition for retrieving all the "implied predicate"s
//! that appear on the function signature.

use pernixc_hash::HashSet;
use pernixc_target::Global;
use pernixc_term::{
    lifetime::Lifetime,
    predicate::{Outlives, Predicate},
    r#type::Type,
};
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, storage::intern::Interned,
};

/// The enumeration of all predicates that can be implied by the compiler.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Identifiable,
)]
#[allow(missing_docs)]
pub enum ImpliedPredicate {
    LifetimeOutlives(Outlives<Lifetime>),
    TypeOutlives(Outlives<Type>),
}

impl From<ImpliedPredicate> for Predicate {
    fn from(predicate: ImpliedPredicate) -> Self {
        match predicate {
            ImpliedPredicate::LifetimeOutlives(outlives) => {
                Self::LifetimeOutlives(outlives)
            }
            ImpliedPredicate::TypeOutlives(outlives) => {
                Self::TypeOutlives(outlives)
            }
        }
    }
}

/// A query for retrieving all the "implied predicate"s appear on the function
/// signature.
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
    Encode,
    Decode,
    Query,
)]
#[value(Interned<HashSet<ImpliedPredicate>>)]
#[extend(name = get_implied_predicates, by_val)]
pub struct Key {
    /// The global ID of the function symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}
