//! Contains the definition of [`Term`]

use std::{cmp::Eq, fmt::Debug, hash::Hash};

use pernixc_arena::ID;
use pernixc_table::MemberID;
use pernixc_term::{
    constant::Constant,
    generic_parameter::{
        ConstantParameter, GenericParameter, LifetimeParameter, TypeParameter,
    },
    instantiation,
    lifetime::Lifetime,
    matching,
    predicate::{Compatible, Predicate},
    r#type::{TraitMember, Type},
    sub_term, visitor, Model, ModelOf, Never,
};

use crate::{
    environment::Environment, equivalences, mapping, normalizer::Normalizer,
    unification, AbruptError, Satisfiability, Succeeded,
};

/// A trait implemented by all three fundamental terms of the language:
/// [`Lifetime`], [`Type`], and [`Constant`].
///
/// This trait provides a common interface for all terms to be used in the
/// type system. Since most of the queries and operations in the type system are
/// generic over the kind of term.
pub trait Term:
    Debug
    + Eq
    + Hash
    + Sized
    + Clone
    + Ord
    + ModelOf
    + visitor::Element
    + matching::Match
    + sub_term::SubTerm
    + unification::Element
    + equivalences::Equivalence
    + mapping::Element
    + instantiation::Element
    + From<MemberID<ID<Self::GenericParameter>>>
    + From<pernixc_term::Error>
    + From<Self::TraitMember>
    + Send
    + Sync
    + 'static
{
    /// The type of generic parameters of this term kind.
    type GenericParameter: GenericParameter + 'static;

    /// The type of trait member symbol that stores this term kind.
    type TraitMember: Debug + Eq + Hash + Sized + Clone + Ord + 'static;

    /// The inference variable type of this term kind.
    type InferenceVariable: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync;

    /// An algorithm that normalizes the term.
    ///
    /// Normalization converts the term into a canonical form. For example,
    /// a type alias is expanded into its definition.
    ///
    /// ```pnx
    /// public type A = int32
    /// ```
    ///
    /// The type `A` is normalized into `int32`.
    #[doc(hidden)]
    fn normalize(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> Result<Option<Succeeded<Self, Self::Model>>, AbruptError>;

    #[doc(hidden)]
    fn as_trait_member_compatible_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&Compatible<Self::TraitMember, Self>>;

    #[doc(hidden)]
    fn as_trait_member_compatible_predicate_mut(
        predicate: &mut Predicate<Self::Model>,
    ) -> Option<&mut Compatible<Self::TraitMember, Self>>;

    #[doc(hidden)]
    fn into_trait_member_copmatible_predicate(
        predicate: Predicate<Self::Model>,
    ) -> Result<Compatible<Self::TraitMember, Self>, Predicate<Self::Model>>;

    #[doc(hidden)]
    fn as_trait_member(&self) -> Option<&Self::TraitMember>;

    #[doc(hidden)]
    fn definite_satisfiability(&self) -> Satisfiability;
}

impl<M: Model> Term for Lifetime<M> {
    type GenericParameter = LifetimeParameter;
    type TraitMember = Never;
    type InferenceVariable = M::LifetimeInference;

    fn normalize(
        &self,
        _: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> Result<Option<Succeeded<Self, Self::Model>>, AbruptError> {
        Ok(None)
    }

    fn as_trait_member_compatible_predicate(
        _: &Predicate<Self::Model>,
    ) -> Option<&Compatible<Self::TraitMember, Self>> {
        None
    }

    fn as_trait_member_compatible_predicate_mut(
        _: &mut Predicate<Self::Model>,
    ) -> Option<&mut Compatible<Self::TraitMember, Self>> {
        None
    }

    fn into_trait_member_copmatible_predicate(
        predicate: Predicate<Self::Model>,
    ) -> Result<Compatible<Self::TraitMember, Self>, Predicate<Self::Model>>
    {
        Err(predicate)
    }

    fn as_trait_member(&self) -> Option<&Self::TraitMember> { None }

    fn definite_satisfiability(&self) -> Satisfiability {
        Satisfiability::Satisfied
    }
}

impl<M: Model> Term for Type<M> {
    type GenericParameter = TypeParameter;
    type TraitMember = TraitMember<M>;
    type InferenceVariable = M::TypeInference;

    fn normalize(
        &self,
        _: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> Result<Option<Succeeded<Self, Self::Model>>, AbruptError> {
        Ok(None)
    }

    fn as_trait_member_compatible_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&Compatible<TraitMember<M>, Self>> {
        if let Predicate::TraitTypeCompatible(compatible) = predicate {
            Some(compatible)
        } else {
            None
        }
    }

    fn as_trait_member_compatible_predicate_mut(
        predicate: &mut Predicate<Self::Model>,
    ) -> Option<&mut Compatible<TraitMember<M>, Self>> {
        if let Predicate::TraitTypeCompatible(compatible) = predicate {
            Some(compatible)
        } else {
            None
        }
    }

    fn into_trait_member_copmatible_predicate(
        predicate: Predicate<Self::Model>,
    ) -> Result<Compatible<TraitMember<M>, Self>, Predicate<Self::Model>> {
        match predicate {
            Predicate::TraitTypeCompatible(compatible) => Ok(compatible),
            predicate => Err(predicate),
        }
    }

    fn as_trait_member(&self) -> Option<&TraitMember<M>> {
        if let Self::TraitMember(trait_type) = self {
            Some(trait_type)
        } else {
            None
        }
    }

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Error(_) | Self::Parameter(_) | Self::Inference(_) => {
                Satisfiability::Unsatisfied
            }

            Self::Primitive(_) => Satisfiability::Satisfied,

            Self::MemberSymbol(_)
            | Self::Pointer(_)
            | Self::Symbol(_)
            | Self::Reference(_)
            | Self::Array(_)
            | Self::Phantom(_)
            | Self::TraitMember(_)
            | Self::Tuple(_) => Satisfiability::Congruent,
        }
    }
}

impl<M: Model> Term for Constant<M> {
    type GenericParameter = ConstantParameter;
    type TraitMember = Never;
    type InferenceVariable = M::ConstantInference;

    fn normalize(
        &self,
        _: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> Result<Option<Succeeded<Self, Self::Model>>, AbruptError> {
        Ok(None)
    }

    fn as_trait_member_compatible_predicate(
        _: &Predicate<Self::Model>,
    ) -> Option<&Compatible<Self::TraitMember, Self>> {
        None
    }

    fn as_trait_member_compatible_predicate_mut(
        _: &mut Predicate<Self::Model>,
    ) -> Option<&mut Compatible<Self::TraitMember, Self>> {
        None
    }

    fn into_trait_member_copmatible_predicate(
        predicate: Predicate<Self::Model>,
    ) -> Result<Compatible<Self::TraitMember, Self>, Predicate<Self::Model>>
    {
        Err(predicate)
    }

    fn as_trait_member(&self) -> Option<&Self::TraitMember> { None }

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Error(_) | Self::Parameter(_) | Self::Inference(_) => {
                Satisfiability::Unsatisfied
            }

            Self::Phantom | Self::Primitive(_) => Satisfiability::Satisfied,

            Self::Struct(_)
            | Self::Enum(_)
            | Self::Array(_)
            | Self::Tuple(_) => Satisfiability::Congruent,
        }
    }
}
