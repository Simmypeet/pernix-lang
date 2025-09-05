//! Contains the definition of [`Term`]

use std::{fmt::Debug, hash::Hash};

use pernixc_semantic_element::type_alias::get_type_alias;
use pernixc_symbol::{
    kind::{get_kind, Kind},
    member::get_members,
    name::get_name,
    parent::get_parent,
};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_arguments::TraitMember,
    generic_parameters::{get_generic_parameters, GenericParameter},
    inference,
    lifetime::Lifetime,
    predicate::{Compatible, Outlives, Predicate},
    r#type::Type,
    tuple::{Element, Tuple},
    Never,
};

use crate::{
    environment::Environment, normalizer::Normalizer, resolution, Error,
    Satisfiability, Succeeded,
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
    + pernixc_term::instantiation::Element
    + crate::equivalence::Impl
    + crate::unification::Element
    + crate::subtype::Impl
    + crate::mapping::Element
    + crate::predicate::outlives::Impl
    + Debug
    + Clone
    + Ord
    + Hash
    + Send
    + Sync
    + From<inference::Variable<Self>>
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
        + Send
        + Sync
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
    ) -> impl std::future::Future<Output = Result<Option<Succeeded<Self>>, Error>>
           + Send;

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
            Self::TraitMember(trait_member) => {
                normalize_trait_member(trait_member, environment).await?;
                None
            }

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

async fn normalize_trait_member(
    trait_member: &TraitMember,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<Succeeded<Type>>, Error> {
    let trait_id = environment
        .tracked_engine()
        .get_parent(trait_member.id)
        .await
        .expect("should have a trait parent");

    // resolve the trait implementation
    let Some(resolution) = environment
        .query(&resolution::Resolve::new(
            Global::new(trait_member.id.target_id, trait_id),
            trait_member.parent_generic_arguments.clone(),
        ))
        .await?
    else {
        return Ok(None);
    };

    let trait_member_name =
        environment.tracked_engine().get_name(trait_member.id).await;

    // not a trait implementation
    if environment.tracked_engine().get_kind(resolution.result.id).await
        != Kind::PositiveImplementation
    {
        return Ok(None);
    }

    let Some(implementation_member_id) = environment
        .tracked_engine()
        .get_members(resolution.result.id)
        .await
        .member_ids_by_name
        .get(&trait_member_name)
        .copied()
        .map(|x| Global::new(resolution.result.id.target_id, x))
    else {
        return Ok(None);
    };

    // check if is the type
    if environment.tracked_engine().get_kind(implementation_member_id).await
        != Kind::ImplementationType
    {
        return Ok(None);
    }

    let mut final_instantiation = resolution.result.instantiation.clone();

    // should have no collision and no mismatched generic arguments
    // count
    {
        let generic_parameter = environment
            .tracked_engine()
            .get_generic_parameters(implementation_member_id)
            .await?;

        final_instantiation
            .append_from_generic_arguments(
                trait_member.member_generic_arguments.clone(),
                implementation_member_id,
                &generic_parameter,
            )
            .unwrap();
    }

    let mut new_term = (*environment
        .tracked_engine()
        .get_type_alias(implementation_member_id)
        .await?)
        .clone();

    final_instantiation.instantiate(&mut new_term);

    Ok(Some(Succeeded::with_constraints(
        new_term,
        resolution.constraints.clone(),
    )))
}
