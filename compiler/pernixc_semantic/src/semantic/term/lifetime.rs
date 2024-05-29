//! Contains the definition of [`Lifetime`].

use core::fmt;
use std::{
    collections::{HashMap, HashSet},
    sync::atomic::{AtomicUsize, Ordering},
};

use enum_as_inner::EnumAsInner;

use super::{
    constant::Constant, r#type::Type, AssignSubTermError, GenericArguments,
    ModelOf, Never, Term, Tuple,
};
use crate::{
    arena::{Key, ID},
    ir::control_flow_graph::Scope,
    semantic::{
        equality,
        instantiation::Instantiation,
        mapping::Mapping,
        matching::{self, Match, Matching},
        model::{Default, Model},
        normalizer::Normalizer,
        predicate::{self, Outlives, Predicate, Satisfiability},
        session::{ExceedLimitError, Limit, Session},
        sub_term::{Location, SubTerm},
        unification::{Substructural, Unification},
        Environment,
    },
    symbol::{
        table::{self, State, Table},
        GenericID, LifetimeParameter, LifetimeParameterID, MemberID,
    },
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
pub struct Local(pub ID<Scope>); /* will be changed */

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
pub enum Lifetime<M: Model> {
    Static,
    #[from]
    Parameter(LifetimeParameterID),
    Inference(M::LifetimeInference),
    #[from]
    Forall(Forall),
}

impl<M: Model> ModelOf for Lifetime<M> {
    type Model = M;
}

impl<M: Model> Location<Lifetime<M>, Lifetime<M>> for Never {
    fn assign_sub_term(
        self,
        _: &mut Lifetime<M>,
        _: Lifetime<M>,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Lifetime<M>) -> Option<Lifetime<M>> {
        match self {}
    }

    fn get_sub_term_ref(self, _: &Lifetime<M>) -> Option<&Lifetime<M>> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Lifetime<M>) -> Option<&mut Lifetime<M>> {
        match self {}
    }
}

impl<M: Model> Location<Lifetime<M>, Type<M>> for Never {
    fn assign_sub_term(
        self,
        _: &mut Lifetime<M>,
        _: Type<M>,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Lifetime<M>) -> Option<Type<M>> { match self {} }

    fn get_sub_term_ref(self, _: &Lifetime<M>) -> Option<&Type<M>> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Lifetime<M>) -> Option<&mut Type<M>> {
        match self {}
    }
}

impl<M: Model> Location<Lifetime<M>, Constant<M>> for Never {
    fn assign_sub_term(
        self,
        _: &mut Lifetime<M>,
        _: Constant<M>,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Lifetime<M>) -> Option<Constant<M>> {
        match self {}
    }

    fn get_sub_term_ref(self, _: &Lifetime<M>) -> Option<&Constant<M>> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Lifetime<M>) -> Option<&mut Constant<M>> {
        match self {}
    }
}

impl<M: Model> SubTerm for Lifetime<M> {
    type SubTypeLocation = Never;
    type SubConstantLocation = Never;
    type SubLifetimeLocation = Never;
    type ThisSubTermLocation = Never;
}

impl<M: Model> Match for Lifetime<M> {
    fn substructural_match(
        &self,
        _: &Self,
    ) -> Option<
        matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    > {
        None
    }

    fn get_substructural(
        substructural: &matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &substructural.lifetimes
    }

    fn get_substructural_mut(
        substructural: &mut matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &mut substructural.lifetimes
    }
}

impl<M: Model> From<Never> for Lifetime<M> {
    fn from(value: Never) -> Self { match value {} }
}

impl<M: Model> Term for Lifetime<M>
where
    Self: ModelOf<Model = M>,
{
    type GenericParameter = LifetimeParameter;
    type TraitMember = Never;
    type InferenceVariable = M::LifetimeInference;
    type Rebind<Ms: Model> = Lifetime<Ms>;

    fn normalize(
        &self,
        environment: &Environment<M, impl State, impl Normalizer<M>>,
        limit: &mut Limit<
            impl Session<Self> + Session<Type<M>> + Session<Constant<M>>,
        >,
    ) -> Result<Option<Self>, ExceedLimitError> {
        if let Lifetime::Inference(inference) = self {
            Normalizer::normalize_lifetime(inference, environment, limit)
        } else {
            Ok(None)
        }
    }

    fn outlives_satisfiability(
        &self,
        lifetime: &Lifetime<M>,
        environment: &Environment<M, impl State, impl Normalizer<M>>,
        limit: &mut Limit<
            impl Session<Self> + Session<Type<M>> + Session<Constant<M>>,
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

    fn as_inference(&self) -> Option<&Self::InferenceVariable> {
        match self {
            Self::Inference(inference) => Some(inference),
            _ => None,
        }
    }

    fn as_inference_mut(&mut self) -> Option<&mut Self::InferenceVariable> {
        match self {
            Self::Inference(inference) => Some(inference),
            _ => None,
        }
    }

    fn into_inference(self) -> Result<Self::InferenceVariable, Self> {
        match self {
            Self::Inference(inference) => Ok(inference),
            _ => Err(self),
        }
    }

    fn get_adt_fields(&self, _: &Table<impl State>) -> Option<Vec<Self>> {
        None
    }

    fn as_outlive_predicate(
        predicate: &Predicate<M>,
    ) -> Option<&Outlives<Self>> {
        predicate.as_lifetime_outlives()
    }

    fn as_outlive_predicate_mut(
        predicate: &mut Predicate<M>,
    ) -> Option<&mut Outlives<Self>> {
        predicate.as_lifetime_outlives_mut()
    }

    fn into_outlive_predicate(
        predicate: Predicate<M>,
    ) -> Result<Outlives<Self>, Predicate<M>> {
        predicate.into_lifetime_outlives()
    }

    fn as_constant_type_predicate(_: &Predicate<M>) -> Option<&Self> { None }

    fn as_constant_type_predicate_mut(
        _: &mut Predicate<M>,
    ) -> Option<&mut Self> {
        None
    }

    fn into_constant_type_predicate(
        predicate: Predicate<M>,
    ) -> Result<Self, Predicate<M>> {
        Err(predicate)
    }

    fn as_trait_member_equality_predicate(
        _: &Predicate<M>,
    ) -> Option<&predicate::Equality<Never, Self>> {
        None
    }

    fn as_trait_member_equality_predicate_mut(
        _: &mut Predicate<M>,
    ) -> Option<&mut predicate::Equality<Never, Self>> {
        None
    }

    fn into_trait_member_equality_predicate(
        predicate: Predicate<M>,
    ) -> Result<predicate::Equality<Never, Self>, Predicate<M>> {
        Err(predicate)
    }

    fn as_tuple_predicate(_: &Predicate<M>) -> Option<&predicate::Tuple<Self>> {
        None
    }

    fn as_tuple_predicate_mut(
        _: &mut Predicate<M>,
    ) -> Option<&mut predicate::Tuple<Self>> {
        None
    }

    fn into_tuple_predicate(
        predicate: Predicate<M>,
    ) -> Result<predicate::Tuple<Self>, Predicate<M>> {
        Err(predicate)
    }

    fn definite_satisfiability(&self) -> Satisfiability {
        Satisfiability::Satisfied
    }

    fn constant_type_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Static => Satisfiability::Satisfied,

            Self::Parameter(_) | Self::Inference(_) | Self::Forall(_) => {
                Satisfiability::Unsatisfied
            }
        }
    }

    fn get_instantiation(
        instantiation: &Instantiation<M>,
    ) -> &HashMap<Self, Self> {
        &instantiation.lifetimes
    }

    fn get_instantiation_mut(
        instantiation: &mut Instantiation<M>,
    ) -> &mut HashMap<Self, Self> {
        &mut instantiation.lifetimes
    }

    fn get_substructural_unification<'a, T: Term>(
        substructural: &'a Substructural<T>,
    ) -> impl Iterator<Item = &'a Unification<Lifetime<T::Model>>>
    where
        Self: 'a,
    {
        substructural.lifetimes.values()
    }

    fn get_mapping(mapping: &Mapping<M>) -> &HashMap<Self, HashSet<Self>> {
        &mapping.lifetimes
    }

    fn get_mapping_mut(
        mapping: &mut Mapping<M>,
    ) -> &mut HashMap<Self, HashSet<Self>> {
        &mut mapping.lifetimes
    }

    fn get_generic_arguments(
        generic_arguments: &GenericArguments<M>,
    ) -> &[Self] {
        &generic_arguments.lifetimes
    }

    fn get_generic_arguments_mut(
        generic_arguments: &mut GenericArguments<M>,
    ) -> &mut Vec<Self> {
        &mut generic_arguments.lifetimes
    }

    fn from_default_model(term: Lifetime<Default>) -> Self {
        M::from_default_lifetime(term)
    }
}

impl<T: State, M: Model> table::Display<T> for Lifetime<M> {
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
            Self::Forall(_) | Self::Inference(_) => {
                write!(f, "'?")
            }
        }
    }
}

#[cfg(test)]
mod tests;
