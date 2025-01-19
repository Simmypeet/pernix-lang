//! Contains the definition of the [`ImpliedPredicates`] component.

use std::collections::HashSet;

use pernixc_table::component::Derived;
use pernixc_term::{
    lifetime::Lifetime,
    predicate::{Outlives, Predicate},
    r#type::Type,
    Default,
};
use serde::{Deserialize, Serialize};

/// The enumeration of all predicates that can be implied by the compiler.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[allow(missing_docs)]
pub enum ImpliedPredicate {
    LifetimeOutlives(Outlives<Lifetime<Default>>),
    TypeOutlives(Outlives<Type<Default>>),
}

impl From<ImpliedPredicate> for Predicate<Default> {
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
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct ImpliedPredicates {
    /// The predicates that have been inferred by the compiler.
    pub implied_predicates: HashSet<ImpliedPredicate>,
}

impl Derived for ImpliedPredicates {
    fn component_name() -> &'static str { "implied predicates" }
}
