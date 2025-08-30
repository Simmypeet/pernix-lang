//! Contains the query definition for retrieving all the "implied predicate"s
//! that appear on the function signature.

use std::sync::Arc;

use pernixc_hash::HashSet;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::{
    lifetime::Lifetime,
    predicate::{Outlives, Predicate},
    r#type::Type,
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
    Serialize,
    Deserialize,
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
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(Arc<HashSet<ImpliedPredicate>>)]
#[extend(method(get_implied_predicates))]
pub struct Key(pub Global<pernixc_symbol::ID>);
