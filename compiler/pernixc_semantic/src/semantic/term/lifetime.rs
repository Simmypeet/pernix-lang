//! Contains the definition of [`Lifetime`].

use std::sync::atomic::{AtomicUsize, Ordering};

use super::{Never, Substructural, Term};
use crate::symbol::LifetimeParameterID;

/// Represents a for-all quantified lifetime, denoted by `for<'a>` syntax, used in higher-ranked
/// trait bounds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Forall(pub(crate) usize);

impl Forall {
    #[allow(missing_docs)]
    pub fn generate() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        Self(COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

/// Represents a lifetime inference variable in hindley-milner type inference.
pub type Inference = Never; /* will be changed */

/// Represents a local lifetime variable.
pub type Local = Never; /* will be changed */

/// Represents a lifetiem annotation term.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Lifetime {
    Static,
    Parameter(LifetimeParameterID),
}

impl Term for Lifetime {
    fn substructural_match(&self, _: &Self) -> Option<Substructural> { None }

    fn get_substructural(substructural: &Substructural) -> &Vec<(Self, Self)> {
        &substructural.lifetimes
    }

    fn get_substructural_mut(substructural: &mut Substructural) -> &mut Vec<(Self, Self)> {
        &mut substructural.lifetimes
    }
}

#[cfg(test)]
mod tests;
