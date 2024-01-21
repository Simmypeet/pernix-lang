//! Contains the definition of [`Lifetime`].

use std::{
    collections::{HashMap, HashSet},
    sync::atomic::{AtomicUsize, Ordering},
};

use enum_as_inner::EnumAsInner;

use super::{constant::Constant, r#type::Type, Never, Substructural, Term};
use crate::{
    semantic::{
        predicate::{NonEquality, Outlives, Satisfiability},
        unification::Unification,
        Premise,
    },
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
    // lifetime doesn't have any sub-term
    type SubConstantLocation = Never;
    type SubLifetimeLocation = Never;
    type SubTypeLocation = Never;

    fn get_sub_type(&self, location: Self::SubTypeLocation) -> Option<&Type> { match location {} }

    fn get_sub_type_mut(&mut self, location: Self::SubTypeLocation) -> Option<&mut Type> {
        match location {}
    }

    fn get_sub_lifetime(&self, location: Self::SubLifetimeLocation) -> Option<&Lifetime> {
        match location {}
    }

    fn get_sub_lifetime_mut(
        &mut self,
        location: Self::SubLifetimeLocation,
    ) -> Option<&mut Lifetime> {
        match location {}
    }

    fn get_sub_constant(&self, location: Self::SubConstantLocation) -> Option<&Constant> {
        match location {}
    }

    fn get_sub_constant_mut(
        &mut self,
        location: Self::SubConstantLocation,
    ) -> Option<&mut Constant> {
        match location {}
    }

    fn substructural_match(&self, _: &Self) -> Option<Substructural> { None }

    fn is_tuple(&self) -> bool { false }

    fn outlives_predicates<'a>(premise: &'a Premise) -> impl Iterator<Item = &'a Outlives<Self>>
    where
        Self: 'a,
    {
        premise
            .non_equalitiy_predicates
            .iter()
            .filter_map(NonEquality::as_lifetime_outlives)
    }

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
