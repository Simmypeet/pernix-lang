//! Contains the definition of [`Term`]

use std::{fmt::Debug, hash::Hash};

use pernixc_semantic_element::{
    instance_associated_value::get_instance_associated_value,
    type_alias::get_type_alias,
};
use pernixc_symbol::{
    ID,
    instance_associated::{AssociatedKind, get_instance_associated_equivalent},
};
use pernixc_target::Global;
use pernixc_term::{
    Never,
    constant::Constant,
    generic_parameters::{
        GenericParameter, InstanceParameter, get_generic_parameters,
    },
    inference,
    instance::{Instance, InstanceAssociated},
    instantiation::{Instantiation, get_instantiation},
    lifetime::Lifetime,
    predicate::{Compatible, Outlives, Predicate},
    r#type::Type,
};

use crate::{
    OverflowError, Satisfiability, Succeeded, environment::Environment,
    normalizer::Normalizer,
};

/// A trait implemented by all three fundamental terms of the language:
/// [`Lifetime`], [`Type`], and [`Constant`].
///
/// This trait provides a common interface for all terms to be used in the
/// type system. Since most of the queries and operations in the type system are
/// generic over the kind of term.
#[allow(private_bounds)]
pub trait Term:
    pernixc_term::visitor::Element
    + pernixc_term::matching::Match
    + pernixc_term::instantiation::Element
    + pernixc_term::generic_arguments::Element
    + pernixc_term::error::MakeError
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

    /// The type of instance associated with this term kind.
    type InstanceAssociated: Into<Self>
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
    ) -> impl std::future::Future<
        Output = Result<Option<Succeeded<Self>>, OverflowError>,
    > + Send;

    #[doc(hidden)]
    fn as_instance_associated(&self) -> Option<&Self::InstanceAssociated>;

    #[doc(hidden)]
    fn as_instance_associated_equality_predicate(
        predicate: &Predicate,
    ) -> Option<&Compatible<Self::InstanceAssociated, Self>>;

    #[doc(hidden)]
    fn as_instance_associated_equality_predicate_mut(
        predicate: &mut Predicate,
    ) -> Option<&mut Compatible<Self::InstanceAssociated, Self>>;

    #[doc(hidden)]
    fn definite_satisfiability(&self) -> Satisfiability;

    #[doc(hidden)]
    fn outlives_satisfiability(&self, lifetime: &Lifetime) -> Satisfiability;

    #[doc(hidden)]
    fn as_outlives_predicate(predicate: &Predicate) -> Option<&Outlives<Self>>;
}

impl Term for Lifetime {
    type GenericParameter = pernixc_term::generic_parameters::LifetimeParameter;

    type InstanceAssociated = Never;

    async fn normalize(
        &self,
        _: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Self>>, OverflowError> {
        Ok(None)
    }

    fn as_instance_associated(&self) -> Option<&Self::InstanceAssociated> {
        None
    }

    fn as_instance_associated_equality_predicate(
        _: &Predicate,
    ) -> Option<&Compatible<Self::InstanceAssociated, Self>> {
        None
    }

    fn as_instance_associated_equality_predicate_mut(
        _: &mut Predicate,
    ) -> Option<&mut Compatible<Self::InstanceAssociated, Self>> {
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

    type InstanceAssociated = InstanceAssociated;

    async fn normalize(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Self>>, OverflowError> {
        let normalized = match self {
            Self::InstanceAssociated(trait_member) => {
                normalize_instance_associated_type(trait_member, environment)
                    .await
            }

            // unpack the tuple
            Self::Tuple(tuple) => tuple.unpack_one_level(),

            _ => None,
        };

        if let Some(normalized) = normalized {
            Normalizer::normalize_type(&normalized, environment)
                .await?
                .map_or_else(
                    || Ok(Some(Succeeded::new(normalized))),
                    |x| Ok(Some(x)),
                )
        } else {
            Normalizer::normalize_type(self, environment)
                .await?
                .map_or_else(|| Ok(None), |x| Ok(Some(x)))
        }
    }

    fn as_instance_associated(
        &self,
    ) -> Option<&<Self as Term>::InstanceAssociated> {
        match self {
            Self::InstanceAssociated(trait_member) => Some(trait_member),
            _ => None,
        }
    }

    fn as_instance_associated_equality_predicate(
        predicate: &Predicate,
    ) -> Option<&Compatible<<Self as Term>::InstanceAssociated, Self>> {
        match predicate {
            Predicate::InstanceAssociatedTypeEquality(compatible) => {
                Some(compatible)
            }
            _ => None,
        }
    }

    fn as_instance_associated_equality_predicate_mut(
        predicate: &mut Predicate,
    ) -> Option<&mut Compatible<<Self as Term>::InstanceAssociated, Self>> {
        match predicate {
            Predicate::InstanceAssociatedTypeEquality(compatible) => {
                Some(compatible)
            }
            _ => None,
        }
    }

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Error(_) | Self::Parameter(_) | Self::Inference(_) => {
                Satisfiability::Unsatisfied
            }

            Self::Primitive(_) => Satisfiability::Satisfied,

            Self::AssociatedSymbol(_)
            | Self::FunctionSignature(_)
            | Self::Pointer(_)
            | Self::Symbol(_)
            | Self::Reference(_)
            | Self::Array(_)
            | Self::Phantom(_)
            | Self::InstanceAssociated(_)
            | Self::Tuple(_) => Satisfiability::Congruent,
        }
    }

    fn outlives_satisfiability(&self, _: &Lifetime) -> Satisfiability {
        match self {
            Self::Primitive(_) => Satisfiability::Satisfied,

            Self::Error(_) | Self::Inference(_) | Self::Parameter(_) => {
                Satisfiability::Unsatisfied
            }

            Self::AssociatedSymbol(_)
            | Self::FunctionSignature(_)
            | Self::Symbol(_)
            | Self::Pointer(_)
            | Self::Reference(_)
            | Self::Array(_)
            | Self::Tuple(_)
            | Self::InstanceAssociated(_)
            | Self::Phantom(_) => Satisfiability::Congruent,
        }
    }

    fn as_outlives_predicate(predicate: &Predicate) -> Option<&Outlives<Self>> {
        predicate.as_type_outlives()
    }
}

impl Term for Constant {
    type GenericParameter = pernixc_term::generic_parameters::ConstantParameter;

    type InstanceAssociated = Never;

    async fn normalize(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Self>>, OverflowError> {
        let normalized = match self {
            // unpack the tuple
            Self::Tuple(tuple) => tuple.unpack_one_level().map(Succeeded::new),

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

    fn as_instance_associated(&self) -> Option<&Self::InstanceAssociated> {
        None
    }

    fn as_instance_associated_equality_predicate(
        _: &Predicate,
    ) -> Option<&Compatible<Self::InstanceAssociated, Self>> {
        None
    }

    fn as_instance_associated_equality_predicate_mut(
        _: &mut Predicate,
    ) -> Option<&mut Compatible<Self::InstanceAssociated, Self>> {
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

async fn normalize_instance_associated_type(
    instance_associated: &InstanceAssociated,
    environment: &Environment<'_, impl Normalizer>,
) -> Option<Type> {
    if let Some(normalized) = normalize_instance_associated_type_inner(
        instance_associated,
        environment,
    )
    .await
    {
        return Some(normalized);
    }

    // normalize the inner instance
    let inner_instance_associated_instance =
        instance_associated.instance_as_associated_instance()?;

    let normalized_instance_associated_instance =
        normalize_instance_associated_instance(
            inner_instance_associated_instance,
            environment,
        )
        .await?;

    Some(Type::InstanceAssociated(InstanceAssociated::new(
        Box::new(normalized_instance_associated_instance),
        instance_associated.trait_associated_symbol_id(),
        instance_associated.associated_instance_generic_arguments().clone(),
    )))
}

async fn normalize_instance_associated_type_inner(
    instance_associated: &InstanceAssociated,
    environment: &Environment<'_, impl Normalizer>,
) -> Option<Type> {
    let (instantiation, equiv_instance_associated) =
        try_normalize_instance_associated(
            instance_associated,
            environment,
            AssociatedKind::Type,
        )
        .await?;

    let mut instance_associated_type = (*environment
        .tracked_engine()
        .get_type_alias(equiv_instance_associated)
        .await)
        .clone();

    instantiation.instantiate(&mut instance_associated_type);

    Some(instance_associated_type)
}

impl Term for Instance {
    type GenericParameter = InstanceParameter;

    type InstanceAssociated = InstanceAssociated;

    async fn normalize(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<Succeeded<Self>>, OverflowError> {
        let normalized = match self {
            Self::InstanceAssociated(trait_member) => {
                normalize_instance_associated_instance(
                    trait_member,
                    environment,
                )
                .await
            }

            _ => None,
        };

        if let Some(normalized) = normalized {
            Normalizer::normalize_instance(&normalized, environment)
                .await?
                .map_or_else(
                    || Ok(Some(Succeeded::new(normalized))),
                    |x| Ok(Some(x)),
                )
        } else {
            Normalizer::normalize_instance(self, environment)
                .await?
                .map_or_else(|| Ok(None), |x| Ok(Some(x)))
        }
    }

    fn as_instance_associated(
        &self,
    ) -> Option<&<Self as Term>::InstanceAssociated> {
        match self {
            Self::InstanceAssociated(instance_associated) => {
                Some(instance_associated)
            }
            _ => None,
        }
    }

    fn as_instance_associated_equality_predicate(
        _predicate: &Predicate,
    ) -> Option<&Compatible<<Self as Term>::InstanceAssociated, Self>> {
        None
    }

    fn as_instance_associated_equality_predicate_mut(
        _predicate: &mut Predicate,
    ) -> Option<&mut Compatible<<Self as Term>::InstanceAssociated, Self>> {
        None
    }

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::AnonymousTrait(_)
            | Self::Error(_)
            | Self::Inference(_)
            | Self::Parameter(_) => Satisfiability::Unsatisfied,

            Self::Symbol(_) | Self::InstanceAssociated(_) => {
                Satisfiability::Congruent
            }
        }
    }

    fn outlives_satisfiability(&self, _: &Lifetime) -> Satisfiability {
        // NOTE: we never test for outlives satisfiability of instances, so we
        // can just return satisfied here.
        Satisfiability::Satisfied
    }

    fn as_outlives_predicate(_: &Predicate) -> Option<&Outlives<Self>> { None }
}

async fn try_normalize_instance_associated(
    instance_associated: &InstanceAssociated,
    environment: &Environment<'_, impl Normalizer>,
    associated_kind: AssociatedKind,
) -> Option<(Instantiation, Global<ID>)> {
    // the instance itself must be resolved to symbol in order to normalize
    // further.
    let instance_symbol = instance_associated.instance_as_symbol()?;

    let equiv_instance_associated = environment
        .tracked_engine()
        .get_instance_associated_equivalent(
            instance_symbol.id(),
            instance_associated.trait_associated_symbol_id(),
            associated_kind,
        )
        .await?;

    // create instantiation from the "instance symbol"
    let mut instantiation = environment
        .tracked_engine()
        .get_instantiation(
            instance_symbol.id(),
            instance_symbol.generic_arguments().clone(),
        )
        .await
        .unwrap();

    // append instantiation from the "instance associated instance symbol"
    {
        let instance_associated_instance_generic_parameters = environment
            .tracked_engine()
            .get_generic_parameters(equiv_instance_associated)
            .await;

        instantiation
            .append_from_generic_arguments(
                instance_associated
                    .associated_instance_generic_arguments()
                    .clone(),
                equiv_instance_associated,
                &instance_associated_instance_generic_parameters,
            )
            .unwrap();
    }

    Some((instantiation, equiv_instance_associated))
}

async fn normalize_instance_associated_instance(
    instance_associated: &InstanceAssociated,
    environment: &Environment<'_, impl Normalizer>,
) -> Option<Instance> {
    let (instantiation, equiv_instance_associated) =
        try_normalize_instance_associated(
            instance_associated,
            environment,
            AssociatedKind::Instance,
        )
        .await?;

    let instance_associated_instance = environment
        .tracked_engine()
        .get_instance_associated_value(equiv_instance_associated)
        .await;

    let mut new_instance_value = (*instance_associated_instance).clone();
    instantiation.instantiate(&mut new_instance_value);

    Some(new_instance_value)
}
