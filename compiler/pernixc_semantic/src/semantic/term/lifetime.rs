//! Contains the definition of [`Lifetime`].

use std::{
    collections::{HashMap, HashSet},
    sync::atomic::{AtomicUsize, Ordering},
};

use enum_as_inner::EnumAsInner;

use super::{
    constant::Constant, r#type::Type, AssignSubTermError, GenericArguments,
    GetVarianceError, Match, Never, Substructural, Term,
};
use crate::{
    arena::{Arena, ID},
    semantic::{
        predicate::{NonEquality, Outlives, Satisfiability},
        unification::Unification,
        Premise,
    },
    symbol::{
        GenericParameters, LifetimeParameter, LifetimeParameterID, Variance,
    },
    table::{State, Table},
};

/// Represents a for-all quantified lifetime, denoted by `for<'a>` syntax, used
/// in higher-ranked trait bounds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Forall(pub(crate) usize);

impl Forall {
    #[allow(missing_docs)]
    pub fn generate() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        Self(COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

/// Represents a lifetime inference variable in Hindley Milner type inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Inference(pub Never); /* will be changed */

/// Represents a local lifetime variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local(pub Never); /* will be changed */

/// Represents a lifetime annotation term.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Lifetime {
    Static,
    Parameter(LifetimeParameterID),
    Inference(Inference),
    Local(Local),
    Forall(Forall),
}

impl Term for Lifetime {
    type SubTypeLocation = Never;
    type SubLifetimeLocation = Never;
    type SubConstantLocation = Never;
    type ThisSubTermLocation = Never;
    type GenericParameter = LifetimeParameter;

    fn substructural_match(
        &self,
        _: &Self,
    ) -> Option<
        Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    > {
        None
    }

    fn assign_sub_lifetime(
        &mut self,
        location: Self::SubLifetimeLocation,
        _: Lifetime,
    ) -> Result<(), AssignSubTermError> {
        match location {}
    }

    fn assign_sub_type(
        &mut self,
        location: Self::SubTypeLocation,
        _: Type,
    ) -> Result<(), AssignSubTermError> {
        match location {}
    }

    fn assign_sub_constant(
        &mut self,
        location: Self::SubConstantLocation,
        _: Constant,
    ) -> Result<(), AssignSubTermError> {
        match location {}
    }

    fn get_sub_lifetime_variance(
        &self,
        location: Self::SubLifetimeLocation,
        _: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError> {
        match location {}
    }

    fn get_sub_type_variance(
        &self,
        location: Self::SubTypeLocation,
        _: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError> {
        match location {}
    }

    fn get_sub_constant_variance(
        &self,
        location: Self::SubConstantLocation,
        _: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError> {
        match location {}
    }

    fn is_tuple(&self) -> bool { false }

    fn outlives_predicates<'a>(
        premise: &'a Premise,
    ) -> impl Iterator<Item = &'a Outlives<Self>>
    where
        Self: 'a,
    {
        premise
            .non_equality_predicates
            .iter()
            .filter_map(NonEquality::as_lifetime_outlives)
    }

    fn definite_satisfiability(&self) -> Satisfiability {
        Satisfiability::Satisfied
    }

    fn get_substructural(
        substructural: &Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Match<Self, Self::ThisSubTermLocation>> {
        &substructural.lifetimes
    }

    fn get_substructural_mut(
        substructural: &mut Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Match<Self, Self::ThisSubTermLocation>> {
        &mut substructural.lifetimes
    }

    fn get_generic_parameters(
        parameters: &GenericParameters,
    ) -> &Arena<Self::GenericParameter> {
        &parameters.lifetimes
    }

    fn get_generic_parameter_order(
        parameters: &GenericParameters,
    ) -> &[ID<Self::GenericParameter>] {
        &parameters.lifetime_order
    }

    fn get_unification(
        unification: &Unification,
    ) -> &HashMap<Self, HashSet<Self>> {
        &unification.lifetimes
    }

    fn get_unification_mut(
        unification: &mut Unification,
    ) -> &mut HashMap<Self, HashSet<Self>> {
        &mut unification.lifetimes
    }

    fn get_generic_arguments(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.lifetimes
    }

    fn get_generic_arguments_mut(
        generic_arguments: &mut GenericArguments,
    ) -> &mut Vec<Self> {
        &mut generic_arguments.lifetimes
    }
}

#[cfg(test)]
mod tests;
