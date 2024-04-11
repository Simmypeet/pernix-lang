//! Contains the definition of [`Lifetime`].

use core::fmt;
use std::{
    collections::{HashMap, HashSet},
    sync::atomic::{AtomicUsize, Ordering},
};

use enum_as_inner::EnumAsInner;

use super::{
    constant::Constant, r#type::Type, AssignSubTermError, GenericArguments,
    Never, Term, Tuple,
};
use crate::{
    arena::{Key, ID},
    semantic::{
        equality,
        instantiation::Instantiation,
        mapping::Mapping,
        matching::{self, Match, Matching},
        predicate::{self, Outlives, Predicate, Satisfiability},
        session::{ExceedLimitError, Limit, Session},
        sub_term::{Location, SubTerm},
        unification::{Substructural, Unification},
        Environment,
    },
    symbol::{GenericID, LifetimeParameter, LifetimeParameterID, MemberID},
    table::{self, State, Table},
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
}

impl SubTerm for Lifetime {
    type SubTypeLocation = Never;
    type SubConstantLocation = Never;
    type SubLifetimeLocation = Never;
    type ThisSubTermLocation = Never;
}

impl Match for Lifetime {
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

impl From<Never> for Lifetime {
    fn from(value: Never) -> Self { match value {} }
}

impl Term for Lifetime {
    type GenericParameter = LifetimeParameter;
    type TraitMember = Never;

    fn normalize(
        &self,
        _: &Environment<impl State>,
        _: &mut Limit<impl Session<Self> + Session<Type> + Session<Constant>>,
    ) -> Result<Option<Self>, ExceedLimitError> {
        Ok(None)
    }

    fn outlives_satisfiability(
        &self,
        lifetime: &Lifetime,
        environment: &Environment<impl State>,
        limit: &mut Limit<
            impl Session<Self> + Session<Type> + Session<Constant>,
        >,
    ) -> Result<Satisfiability, ExceedLimitError> {
        if self.is_static() {
            Ok(Satisfiability::Satisfied)
        } else {
            // reflexivity
            if equality::equals(self, lifetime, environment, limit)? {
                Ok(Satisfiability::Satisfied)
            } else {
                Ok(Satisfiability::Unsatisfied)
            }
        }
    }

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

    fn as_trait_member(&self) -> Option<&Never> { None }

    fn as_trait_member_mut(&mut self) -> Option<&mut Never> { None }

    fn into_trait_member(self) -> Result<Never, Self> { Err(self) }

    fn as_tuple(&self) -> Option<&Tuple<Self>> { None }

    fn as_tuple_mut(&mut self) -> Option<&mut Tuple<Self>> { None }

    fn into_tuple(self) -> Result<Tuple<Self>, Self> { Err(self) }

    fn get_adt_fields(&self, _: &Table<impl State>) -> Option<Vec<Self>> {
        None
    }

    fn as_outlive_predicate(predicate: &Predicate) -> Option<&Outlives<Self>> {
        predicate.as_lifetime_outlives()
    }

    fn as_outlive_predicate_mut(
        predicate: &mut Predicate,
    ) -> Option<&mut Outlives<Self>> {
        predicate.as_lifetime_outlives_mut()
    }

    fn into_outlive_predicate(
        predicate: Predicate,
    ) -> Result<Outlives<Self>, Predicate> {
        predicate.into_lifetime_outlives()
    }

    fn as_constant_type_predicate(_: &Predicate) -> Option<&Self> { None }

    fn as_constant_type_predicate_mut(_: &mut Predicate) -> Option<&mut Self> {
        None
    }

    fn into_constant_type_predicate(
        predicate: Predicate,
    ) -> Result<Self, Predicate> {
        Err(predicate)
    }

    fn as_tuple_predicate(_: &Predicate) -> Option<&predicate::Tuple<Self>> {
        None
    }

    fn as_tuple_predicate_mut(
        _: &mut Predicate,
    ) -> Option<&mut predicate::Tuple<Self>> {
        None
    }

    fn into_tuple_predicate(
        predicate: Predicate,
    ) -> Result<predicate::Tuple<Self>, Predicate> {
        Err(predicate)
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
    ) -> &HashMap<Self, Self> {
        &instantiation.lifetimes
    }

    fn get_instantiation_mut(
        instantiation: &mut Instantiation,
    ) -> &mut HashMap<Self, Self> {
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

    fn get_generic_arguments(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.lifetimes
    }

    fn get_generic_arguments_mut(
        generic_arguments: &mut GenericArguments,
    ) -> &mut Vec<Self> {
        &mut generic_arguments.lifetimes
    }
}

impl<T: State> table::Display<T> for Lifetime {
    fn fmt(
        &self,
        table: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Static => write!(f, "'static"),
            Self::Parameter(parameter) => {
                match &table
                    .get_generic(parameter.parent)
                    .ok_or(fmt::Error)?
                    .generic_declaration()
                    .parameters
                    .lifetimes()
                    .get(parameter.id)
                    .ok_or(fmt::Error)?
                    .name
                {
                    Some(name) => write!(f, "'{name}"),
                    None => write!(f, "'{}", parameter.id.into_index()),
                }
            }
            Self::Forall(_) | Self::Inference(_) | Self::Local(_) => {
                write!(f, "'?")
            }
        }
    }
}

#[cfg(test)]
mod tests;
