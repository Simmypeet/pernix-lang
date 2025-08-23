use std::fmt::Write;

use derive_new::new;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::name::get_qualified_name;
use pernixc_target::Global;

use super::contains_error;
use crate::{
    generic_arguments::GenericArguments, instantiation::Instantiation,
    lifetime::Lifetime,
};

/// Represents a predicate stating that there exists an implementation for the
/// given trait and generic arguments
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
    new,
)]
pub struct Positive {
    /// The trait to be implemented.
    pub trait_id: Global<pernixc_symbol::ID>,

    /// Whether the implementation is const.
    pub is_const: bool,

    /// The generic arguments supplied to the trait.
    pub generic_arguments: GenericArguments,
}

impl Positive {
    /// Checks if the trait contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.generic_arguments.lifetimes.iter().any(Lifetime::is_error)
            || self.generic_arguments.types.iter().any(contains_error)
            || self.generic_arguments.constants.iter().any(contains_error)
    }

    /// Applies an instantiation to the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        self.generic_arguments.instantiate(instantiation);
    }
}

impl crate::display::Display for Positive {
    async fn fmt(
        &self,
        engine: &pernixc_query::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_>,
    ) -> std::fmt::Result {
        let qualified_name = engine.get_qualified_name(self.trait_id).await;
        write!(formatter, "marker {qualified_name}")?;
        self.generic_arguments.fmt(engine, formatter).await
    }
}

/// Represents a predicate stating that there exists no implementation for the
/// given trait and generic arguments
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
    new,
)]
pub struct Negative {
    /// The trait in question.
    pub trait_id: Global<pernixc_symbol::ID>,

    /// The generic arguments supplied to the trait.
    pub generic_arguments: GenericArguments,
}

impl Negative {
    /// Checks if the trait contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.generic_arguments.lifetimes.iter().any(Lifetime::is_error)
            || self.generic_arguments.types.iter().any(contains_error)
            || self.generic_arguments.constants.iter().any(contains_error)
    }

    /// Applies an instantiation to the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        self.generic_arguments.instantiate(instantiation);
    }
}

impl crate::display::Display for Negative {
    async fn fmt(
        &self,
        engine: &pernixc_query::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_>,
    ) -> std::fmt::Result {
        let qualified_name = engine.get_qualified_name(self.trait_id).await;
        write!(formatter, "marker !{qualified_name}")?;
        self.generic_arguments.fmt(engine, formatter).await
    }
}
