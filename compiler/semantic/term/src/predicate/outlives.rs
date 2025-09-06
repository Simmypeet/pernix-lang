use std::fmt::Write;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use super::contains_error;
use crate::{
    instantiation::{self, Instantiation},
    lifetime::Lifetime,
    visitor,
};

/// A predicate that a term outlives a lifetime.
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
    derive_new::new,
)]
pub struct Outlives<T> {
    /// The term that must outlive the bound.
    pub operand: T,

    /// The lifetime that the term must outlive.
    pub bound: Lifetime,
}

impl<T> Outlives<T> {
    /// Checks if the term contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool
    where
        T: visitor::Element,
    {
        contains_error(&self.operand)
    }

    /// Applies a instantiation to the [`Outlives::operand`] and
    /// [`Outlives::bound`].
    pub fn instantiate(&mut self, instantiation: &Instantiation)
    where
        T: instantiation::Element + visitor::Element + Clone,
    {
        instantiation.instantiate(&mut self.bound);
        instantiation.instantiate(&mut self.operand);
    }
}

impl<T: crate::display::Display> crate::display::Display for Outlives<T> {
    async fn fmt(
        &self,
        engine: &pernixc_query::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        self.operand.fmt(engine, formatter).await?;
        write!(formatter, ": ")?;
        self.bound.fmt(engine, formatter).await
    }
}
