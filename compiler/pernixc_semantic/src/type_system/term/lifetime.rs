//! Contains the definition of [`Lifetime`].

use core::fmt;
use std::{
    collections::{BTreeMap, BTreeSet},
    sync::atomic::{AtomicUsize, Ordering},
};

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::source_file::Span;

use super::{
    constant::Constant, r#type::Type, AssignSubTermError, Error,
    GenericArguments, Kind, KindMut, ModelOf, Never, Term, Tuple,
};
use crate::{
    arena::{Key, ID},
    symbol::{
        table::{self, DisplayObject, State, Table},
        GenericID, LifetimeParameter, LifetimeParameterID, MemberID,
    },
    type_system::{
        self,
        equality::Equality,
        instantiation::Instantiation,
        mapping::Mapping,
        matching::{self, Match, Matching},
        model::{Default, Model},
        normalizer::Normalizer,
        observer::Observer,
        predicate::{self, Outlives, Predicate, Satisfiability},
        query::Context,
        sub_term::{Location, SubTerm, TermLocation},
        unification::{Substructural, Unifier},
        Environment, Output,
    },
};

/// Represents a for-all quantified lifetime, denoted by `for['a]` syntax, used
/// in higher-ranked predicates.
#[derive(Debug, Clone, Getters)]
pub struct Forall {
    /// The unique ID of the forall lifetime.
    id: usize,

    /// The span where the forall lifetime was declared.
    ///
    /// This field doesn't influence the equality, ordering, and hashing od the
    /// this struct.
    pub span: Option<Span>,
}

impl Forall {
    /// Generates a new higher-ranked lifetime.
    #[allow(missing_docs)]
    pub fn generate(span: Option<Span>) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        let id = COUNTER.fetch_add(1, Ordering::SeqCst);

        Self { id, span }
    }
}

impl PartialEq for Forall {
    fn eq(&self, other: &Self) -> bool { self.id == other.id }
}

impl Eq for Forall {}

impl PartialOrd for Forall {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for Forall {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.id.cmp(&other.id) }
}

impl std::hash::Hash for Forall {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.id.hash(state) }
}

/// Represents a lifetime annotation term.
#[derive(
    Debug,
    Clone,
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
    #[from]
    Error(Error),
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

impl From<Never> for TermLocation {
    fn from(value: Never) -> Self { match value {} }
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

    fn from_other_model<U: Model>(term: Self::Rebind<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        match term {
            Lifetime::Error(Error) => Self::Error(Error),
            Lifetime::Static => Self::Static,
            Lifetime::Parameter(parameter) => Self::Parameter(parameter),
            Lifetime::Inference(inference) => {
                Self::Inference(M::LifetimeInference::from(inference))
            }
            Lifetime::Forall(forall) => Self::Forall(forall),
        }
    }

    fn try_from_other_model<U: Model, E>(
        term: Self::Rebind<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        match term {
            Lifetime::Error(Error) => Ok(Self::Error(Error)),
            Lifetime::Static => Ok(Self::Static),
            Lifetime::Parameter(parameter) => Ok(Self::Parameter(parameter)),
            Lifetime::Inference(inference) => {
                Ok(Self::Inference(M::LifetimeInference::try_from(inference)?))
            }
            Lifetime::Forall(forall) => Ok(Self::Forall(forall)),
        }
    }

    fn normalize<S: State>(
        &self,
        _: &Environment<M, S, impl Normalizer<M, S>, impl Observer<M, S>>,
        _: &mut Context<M>,
    ) -> Result<Output<Self, M>, type_system::OverflowError> {
        Ok(None)
    }

    fn as_kind(&self) -> Kind<M> { Kind::Lifetime(self) }

    fn as_kind_mut(&mut self) -> KindMut<M> { KindMut::Lifetime(self) }

    fn outlives_satisfiability(&self, other: &Lifetime<M>) -> Satisfiability {
        if self == other {
            return Satisfiability::Satisfied;
        }

        if self.is_static() {
            Satisfiability::Satisfied
        } else {
            Satisfiability::Unsatisfied
        }
    }

    fn from_inference(inference: Self::InferenceVariable) -> Self {
        Self::Inference(inference)
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

    fn as_trait_member_equality_predicate(
        _: &Predicate<M>,
    ) -> Option<&Equality<Never, Self>> {
        None
    }

    fn as_trait_member_equality_predicate_mut(
        _: &mut Predicate<M>,
    ) -> Option<&mut Equality<Never, Self>> {
        None
    }

    fn into_trait_member_equality_predicate(
        predicate: Predicate<M>,
    ) -> Result<Equality<Never, Self>, Predicate<M>> {
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

    fn get_instantiation(
        instantiation: &Instantiation<M>,
    ) -> &BTreeMap<Self, Self> {
        &instantiation.lifetimes
    }

    fn get_instantiation_mut(
        instantiation: &mut Instantiation<M>,
    ) -> &mut BTreeMap<Self, Self> {
        &mut instantiation.lifetimes
    }

    fn get_substructural_unifier<'a, T: Term>(
        substructural: &'a Substructural<T>,
    ) -> impl Iterator<Item = &'a Unifier<Lifetime<T::Model>>>
    where
        Self: 'a,
    {
        substructural.lifetimes.values()
    }

    fn get_mapping(mapping: &Mapping<M>) -> &BTreeMap<Self, BTreeSet<Self>> {
        &mapping.lifetimes
    }

    fn get_mapping_mut(
        mapping: &mut Mapping<M>,
    ) -> &mut BTreeMap<Self, BTreeSet<Self>> {
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

impl<T: State, M: Model> table::Display<T> for Lifetime<M>
where
    M::LifetimeInference: table::Display<T>,
{
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
            Self::Inference(inference) => {
                write!(f, "'{}", DisplayObject { display: inference, table })
            }
            Self::Forall(forall_lifetime) => match &forall_lifetime.span {
                Some(span) => write!(f, "'∀{}", span.str()),
                None => write!(f, "'∀?"),
            },
            Self::Error(_) => write!(f, "'{{error}}"),
        }
    }
}

#[cfg(test)]
mod tests;
