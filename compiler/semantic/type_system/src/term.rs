//! Contains the definition of [`Term`]

use std::{fmt::Debug, hash::Hash};

use pernixc_term::{
    constant::Constant,
    lifetime::Lifetime,
    predicate::{Compatible, Predicate},
    query::generic_parameters::GenericParameter,
    r#type::Type,
    Never,
};

use crate::{
    environment::Environment, normalizer::Normalizer, Error, Succeeded,
};

/// A trait implemented by all three fundamental terms of the language:
/// [`Lifetime`], [`Type`], and [`Constant`].
///
/// This trait provides a common interface for all terms to be used in the
/// type system. Since most of the queries and operations in the type system are
/// generic over the kind of term.
pub trait Term:
    pernixc_term::matching::Match + Clone + Ord + Hash + Send + Sync + 'static
{
    /// The type of generic parameters of this term kind.
    type GenericParameter: GenericParameter + 'static;

    /// The type of trait member symbol that stores this term kind.
    type TraitMember: Into<Self>
        + Debug
        + Eq
        + Hash
        + Sized
        + Clone
        + Ord
        + 'static;

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
        environment: &Environment<impl Normalizer>,
    ) -> impl std::future::Future<Output = Result<Option<Succeeded<Self>>, Error>>;

    #[doc(hidden)]
    fn as_trait_member(&self) -> Option<&Self::TraitMember>;

    #[doc(hidden)]
    fn as_trait_member_compatible_predicate(
        predicate: &Predicate,
    ) -> Option<&Compatible<Self::TraitMember, Self>>;

    #[doc(hidden)]
    fn as_trait_member_compatible_predicate_mut(
        predicate: &mut Predicate,
    ) -> Option<&mut Compatible<Self::TraitMember, Self>>;
}

impl Term for Lifetime {
    type GenericParameter =
        pernixc_term::query::generic_parameters::LifetimeParameter;

    type TraitMember = Never;

    async fn normalize(
        &self,
        _: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Self>>, Error> {
        Ok(None)
    }

    fn as_trait_member(&self) -> Option<&Self::TraitMember> { None }

    fn as_trait_member_compatible_predicate(
        _: &Predicate,
    ) -> Option<&Compatible<Self::TraitMember, Self>> {
        None
    }

    fn as_trait_member_compatible_predicate_mut(
        _: &mut Predicate,
    ) -> Option<&mut Compatible<Self::TraitMember, Self>> {
        None
    }
}

impl Term for Type {
    type GenericParameter =
        pernixc_term::query::generic_parameters::TypeParameter;

    type TraitMember = pernixc_term::generic_arguments::TraitMember;

    async fn normalize(
        &self,
        _: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Self>>, Error> {
        Ok(None)
    }

    fn as_trait_member(&self) -> Option<&<Self as Term>::TraitMember> {
        match self {
            Self::TraitMember(trait_member) => Some(trait_member),
            _ => None,
        }
    }

    fn as_trait_member_compatible_predicate(
        predicate: &Predicate,
    ) -> Option<&Compatible<<Self as Term>::TraitMember, Self>> {
        match predicate {
            Predicate::TraitTypeCompatible(compatible) => Some(compatible),
            _ => None,
        }
    }

    fn as_trait_member_compatible_predicate_mut(
        predicate: &mut Predicate,
    ) -> Option<&mut Compatible<<Self as Term>::TraitMember, Self>> {
        match predicate {
            Predicate::TraitTypeCompatible(compatible) => Some(compatible),
            _ => None,
        }
    }
}

impl Term for Constant {
    type GenericParameter =
        pernixc_term::query::generic_parameters::ConstantParameter;

    type TraitMember = Never;

    async fn normalize(
        &self,
        _: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Self>>, Error> {
        Ok(None)
    }

    fn as_trait_member(&self) -> Option<&Self::TraitMember> { None }

    fn as_trait_member_compatible_predicate(
        _: &Predicate,
    ) -> Option<&Compatible<Self::TraitMember, Self>> {
        None
    }

    fn as_trait_member_compatible_predicate_mut(
        _: &mut Predicate,
    ) -> Option<&mut Compatible<Self::TraitMember, Self>> {
        None
    }
}
