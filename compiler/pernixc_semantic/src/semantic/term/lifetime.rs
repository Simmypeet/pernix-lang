//! Contains the definition of [`Lifetime`].

use std::{
    collections::{HashMap, HashSet},
    sync::atomic::{AtomicUsize, Ordering},
};

use enum_as_inner::EnumAsInner;

use super::{
    constant::Constant, r#type::Type, AssignSubTermError, GenericArguments,
    GetVarianceError, MemberSymbol, Never, Term, Tuple,
};
use crate::{
    arena::{Arena, ID},
    semantic::{
        instantiation::Instantiation,
        mapping::Mapping,
        matching::{self, Match, Matching},
        predicate::{NonEquality, Outlives, Satisfiability},
        subterm::Location,
        unification::{Substructural, Unification},
        Premise,
    },
    symbol::{
        GenericID, GenericParameters, LifetimeParameter, LifetimeParameterID,
        MemberID, Variance,
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

impl Location<Lifetime, Lifetime> for Never {
    fn assign_sub_term(
        self,
        _: &mut Lifetime,
        _: Lifetime,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Lifetime) -> Option<Lifetime> { match self {} }

    fn get_sub_variance(
        self,
        _: &Lifetime,
        _: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError> {
        match self {}
    }
}

impl Location<Lifetime, Type> for Never {
    fn assign_sub_term(
        self,
        _: &mut Lifetime,
        _: Type,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Lifetime) -> Option<Type> { match self {} }

    fn get_sub_variance(
        self,
        _: &Lifetime,
        _: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError> {
        match self {}
    }
}

impl Location<Lifetime, Constant> for Never {
    fn assign_sub_term(
        self,
        _: &mut Lifetime,
        _: Constant,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Lifetime) -> Option<Constant> { match self {} }

    fn get_sub_variance(
        self,
        _: &Lifetime,
        _: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError> {
        match self {}
    }
}

impl Match for Lifetime {
    type SubTypeLocation = Never;
    type SubLifetimeLocation = Never;
    type SubConstantLocation = Never;
    type ThisSubTermLocation = Self::SubLifetimeLocation;

    fn substructural_match(
        &self,
        _: &Self,
    ) -> Option<
        matching::Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    > {
        None
    }

    fn get_substructural(
        substructural: &matching::Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &substructural.lifetimes
    }

    fn get_substructural_mut(
        substructural: &mut matching::Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &mut substructural.lifetimes
    }
}

impl Term for Lifetime {
    type GenericParameter = LifetimeParameter;
    type TraitMember = Never;

    fn as_generic_parameter(
        &self,
    ) -> Option<&MemberID<ID<Self::GenericParameter>, GenericID>> {
        self.as_parameter()
    }

    fn as_generic_parameter_mut(
        &mut self,
    ) -> Option<&mut MemberID<ID<Self::GenericParameter>, GenericID>> {
        self.as_parameter_mut()
    }

    fn into_generic_parameter(
        self,
    ) -> Result<MemberID<ID<Self::GenericParameter>, GenericID>, Self> {
        self.into_parameter()
    }

    fn as_trait_member(&self) -> Option<&MemberSymbol<ID<Self::TraitMember>>> {
        None
    }

    fn as_trait_member_mut(
        &mut self,
    ) -> Option<&mut MemberSymbol<ID<Self::TraitMember>>> {
        None
    }

    fn into_trait_member(
        self,
    ) -> Result<MemberSymbol<ID<Self::TraitMember>>, Self> {
        Err(self)
    }

    fn as_tuple(&self) -> Option<&Tuple<Self>> { None }

    fn as_tuple_mut(&mut self) -> Option<&mut Tuple<Self>> { None }

    fn into_tuple(self) -> Result<Tuple<Self>, Self> { Err(self) }

    fn get_adt_fields(&self, _: &Table<impl State>) -> Option<Vec<Self>> {
        None
    }

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

    fn constant_type_predicates<'a>(
        _: &'a Premise,
    ) -> impl Iterator<Item = &'a Self>
    where
        Self: 'a,
    {
        std::iter::empty()
    }

    fn definite_satisfiability(&self) -> Satisfiability {
        Satisfiability::Satisfied
    }

    fn constant_type_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Static => Satisfiability::Satisfied,

            Self::Parameter(_)
            | Self::Inference(_)
            | Self::Local(_)
            | Self::Forall(_) => Satisfiability::Unsatisfied,
        }
    }

    fn get_instantiation(
        instantiation: &Instantiation,
    ) -> &HashMap<MemberID<ID<Self::GenericParameter>, GenericID>, Self> {
        &instantiation.lifetimes
    }

    fn get_instantiation_mut(
        instantiation: &mut Instantiation,
    ) -> &mut HashMap<MemberID<ID<Self::GenericParameter>, GenericID>, Self>
    {
        &mut instantiation.lifetimes
    }

    fn get_substructural_unification<'a, T: Term>(
        substructural: &'a Substructural<T>,
    ) -> impl Iterator<Item = &'a Unification<Self>>
    where
        Self: 'a,
    {
        substructural.lifetimes.values()
    }

    fn get_mapping(mapping: &Mapping) -> &HashMap<Self, HashSet<Self>> {
        &mapping.lifetimes
    }

    fn get_mapping_mut(
        mapping: &mut Mapping,
    ) -> &mut HashMap<Self, HashSet<Self>> {
        &mut mapping.lifetimes
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
