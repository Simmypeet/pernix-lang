use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use super::contains_error;
use crate::{
    instantiation::{self, Instantiation},
    visitor,
};

/// The predicate meaning that the term is a tuple and is unpackable.
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
pub struct Tuple<T>(pub T);

impl<T> Tuple<T> {
    /// Checks if the term contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool
    where
        T: visitor::Element,
    {
        contains_error(&self.0)
    }

    /// Applies a instantiation to the [`Tuple`] term.
    pub fn instantiate(&mut self, instantiation: &Instantiation)
    where
        T: instantiation::Element + visitor::Element + Clone,
    {
        instantiation.instantiate(&mut self.0);
    }
}
