//! Contains the definition of [`Lifetime`].

use std::{
    collections::{HashMap, HashSet},
    sync::atomic::{AtomicUsize, Ordering},
};

use enum_as_inner::EnumAsInner;

use super::{Never, Substructural, Term};
use crate::{
    semantic::{predicate::Satisfiability, unification::Unification},
    symbol::LifetimeParameterID,
};

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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Inference(Never); /* will be changed */

/// Represents a local lifetime variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local(Never); /* will be changed */

/// Represents a lifetiem annotation term.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From,
)]
#[allow(missing_docs)]
pub enum Lifetime {
    Static,
    Parameter(LifetimeParameterID),
    Inference(Inference),
    Local(Local),
}

impl Term for Lifetime {
    fn substructural_match(&self, _: &Self) -> Option<Substructural> { None }

    fn definite_satisfiability(&self) -> Satisfiability { Satisfiability::Satisfied }

    fn get_substructural(substructural: &Substructural) -> &Vec<(Self, Self)> {
        &substructural.lifetimes
    }

    fn get_substructural_mut(substructural: &mut Substructural) -> &mut Vec<(Self, Self)> {
        &mut substructural.lifetimes
    }

    fn get_unification(unification: &Unification) -> &HashMap<Self, HashSet<Self>> {
        &unification.lifetimes
    }

    fn get_unification_mut(unification: &mut Unification) -> &mut HashMap<Self, HashSet<Self>> {
        &mut unification.lifetimes
    }
}

#[cfg(test)]
mod tests;
