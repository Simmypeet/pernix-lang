//! Contains the definition of the [`ImpliedPredicates`] component.

use pernixc_hash::HashSet;
use pernixc_query::Value;
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

/// A **presistent-derived** component representing the predicates that have
/// been inferred by signature/definition of the symbol.
///
/// This component can only be found in the function symbol.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    StableHash,
    Serialize,
    Deserialize,
    Value,
)]
#[id(Global<pernixc_symbol::ID>)]
#[extend(method(get_implied_predicates))]
pub struct ImpliedPredicates {
    /// The predicates that have been inferred by the compiler.
    pub implied_predicates: HashSet<ImpliedPredicate>,
}
