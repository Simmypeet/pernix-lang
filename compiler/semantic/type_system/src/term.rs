//! Contains the definition of [`Term`]

use std::{fmt::Debug, hash::Hash};

use pernixc_term::{
    constant::Constant,
    generic_parameters::GenericParameter,
    lifetime::Lifetime,
    predicate::{Compatible, Outlives, Predicate},
    r#type::Type,
    tuple::{Element, Tuple},
    Never,
};

use crate::{
    environment::Environment, normalizer::Normalizer, Error, Satisfiability,
    Succeeded,
};

/// A trait implemented by all three fundamental terms of the language:
/// [`Lifetime`], [`Type`], and [`Constant`].
///
/// This trait provides a common interface for all terms to be used in the
/// type system. Since most of the queries and operations in the type system are
/// generic over the kind of term.
pub trait Term:
    pernixc_term::visitor::Element
    + pernixc_term::matching::Match
    + crate::equivalence::Impl
    + crate::unification::Element
    + crate::subtype::Impl
    + crate::mapping::Element
    + Debug
    + Clone
    + Ord
    + Hash
    + Send
    + Sync
    + 'static
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

    #[doc(hidden)]
    fn definite_satisfiability(&self) -> Satisfiability;

    #[doc(hidden)]
    fn outlives_satisfiability(&self, lifetime: &Lifetime) -> Satisfiability;

    #[doc(hidden)]
    fn as_outlives_predicate(predicate: &Predicate) -> Option<&Outlives<Self>>;
}

impl Term for Lifetime {
    type GenericParameter = pernixc_term::generic_parameters::LifetimeParameter;

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

    fn definite_satisfiability(&self) -> Satisfiability {
        Satisfiability::Satisfied
    }

    fn outlives_satisfiability(&self, other: &Self) -> Satisfiability {
        if self == other {
            return Satisfiability::Satisfied;
        }

        if self.is_static() {
            Satisfiability::Satisfied
        } else {
            Satisfiability::Unsatisfied
        }
    }

    fn as_outlives_predicate(predicate: &Predicate) -> Option<&Outlives<Self>> {
        predicate.as_lifetime_outlives()
    }
}

impl Term for Type {
    type GenericParameter = pernixc_term::generic_parameters::TypeParameter;

    type TraitMember = pernixc_term::generic_arguments::TraitMember;

    async fn normalize(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Self>>, Error> {
        let normalized = match self {
            // TODO: transform the trait-member into trait-implementation-type
            // equivalent
            /*
            Self::TraitMember(trait_member) => {
                normalize_trait_member(trait_member, environment)?
            }
            */
            // unpack the tuple
            Self::Tuple(tuple) => unpack_tuple(tuple),

            _ => None,
        };

        if let Some(mut normalized) = normalized {
            if let Some(x) =
                Normalizer::normalize_type(&normalized.result, environment)
                    .await?
            {
                normalized.result = x.result;
                normalized.constraints.extend(x.constraints);
            }

            Ok(Some(normalized))
        } else {
            Normalizer::normalize_type(self, environment)
                .await?
                .map_or_else(|| Ok(None), |x| Ok(Some(x)))
        }
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

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Error(_) | Self::Parameter(_) | Self::Inference(_) => {
                Satisfiability::Unsatisfied
            }

            Self::Primitive(_) => Satisfiability::Satisfied,

            Self::MemberSymbol(_)
            | Self::FunctionSignature(_)
            | Self::Pointer(_)
            | Self::Symbol(_)
            | Self::Reference(_)
            | Self::Array(_)
            | Self::Phantom(_)
            | Self::TraitMember(_)
            | Self::Tuple(_) => Satisfiability::Congruent,
        }
    }

    fn outlives_satisfiability(&self, _: &Lifetime) -> Satisfiability {
        match self {
            Self::Primitive(_) => Satisfiability::Satisfied,

            Self::Error(_) | Self::Inference(_) | Self::Parameter(_) => {
                Satisfiability::Unsatisfied
            }

            Self::MemberSymbol(_)
            | Self::FunctionSignature(_)
            | Self::Symbol(_)
            | Self::Pointer(_)
            | Self::Reference(_)
            | Self::Array(_)
            | Self::Tuple(_)
            | Self::TraitMember(_)
            | Self::Phantom(_) => Satisfiability::Congruent,
        }
    }

    fn as_outlives_predicate(predicate: &Predicate) -> Option<&Outlives<Self>> {
        predicate.as_type_outlives()
    }
}

impl Term for Constant {
    type GenericParameter = pernixc_term::generic_parameters::ConstantParameter;

    type TraitMember = Never;

    async fn normalize(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Self>>, Error> {
        let normalized = match self {
            // unpack the tuple
            Self::Tuple(tuple) => unpack_tuple(tuple),

            _ => None,
        };

        if let Some(mut normalized) = normalized {
            if let Some(x) =
                Normalizer::normalize_constant(&normalized.result, environment)
                    .await?
            {
                normalized.result = x.result;
                normalized.constraints.extend(x.constraints);
            }

            Ok(Some(normalized))
        } else {
            Normalizer::normalize_constant(self, environment)
                .await?
                .map_or_else(|| Ok(None), |x| Ok(Some(x)))
        }
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

    fn outlives_satisfiability(&self, _: &Lifetime) -> Satisfiability {
        // constants value do not have lifetimes
        Satisfiability::Satisfied
    }

    fn as_outlives_predicate(_: &Predicate) -> Option<&Outlives<Self>> { None }
}

fn unpack_tuple<T: Term + From<Tuple<T>> + TryInto<Tuple<T>, Error = T>>(
    tuple: &Tuple<T>,
) -> Option<Succeeded<T>> {
    let contain_upacked = tuple.elements.iter().any(|x| x.is_unpacked);

    if !contain_upacked {
        return None;
    }

    if tuple.elements.len() == 1 {
        return Some(Succeeded::new(tuple.elements[0].term.clone()));
    }

    let mut result = Vec::new();

    for element in tuple.elements.iter().cloned() {
        if element.is_unpacked {
            match element.term.try_into() {
                Ok(inner) => {
                    result.extend(inner.elements);
                }
                Err(term) => {
                    result.push(Element { term, is_unpacked: true });
                }
            }
        } else {
            result.push(element);
        }
    }

    Some(Succeeded::new(Tuple { elements: result }.into()))
}
