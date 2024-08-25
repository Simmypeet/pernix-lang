use core::fmt;
use std::collections::BTreeSet;

use enum_as_inner::EnumAsInner;
use thiserror::Error;

use super::contains_error;
use crate::{
    arena::ID,
    symbol::{
        self,
        table::{self, representation::Index, DisplayObject, State, Table},
        Generic, TraitImplementationID,
    },
    type_system::{
        compatible::Compatible,
        deduction::{self, Deduction},
        instantiation::Instantiation,
        model::{Default, Model},
        normalizer::Normalizer,
        observer::Observer,
        order,
        predicate::Predicate,
        query::Context,
        term::{lifetime::Lifetime, r#type::Type, GenericArguments},
        variance::Variance,
        Compute, Environment, LifetimeConstraint, Output, OverflowError,
        Premise, Succeeded, TraitContext,
    },
};

/// An enumeration of ways a trait predicate can be satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Satisfied<M: Model> {
    /// The trait predicate was proven to be satisfied by
    /// [`TraitContext::InTrait`] flag.
    ByTraitContext,

    /// The trait predicate was proven to be satisfied by searching for the
    /// matching trait implementation.
    ByImplementation(Implementation<M>),

    /// The trait predicate was proven to be satisfied by the premise.
    ByPremise,
}

/// Represents a predicate stating that there exists an implementation for the
/// given trait and generic arguments
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait<M: Model> {
    /// The trait to be implemented.
    pub id: ID<symbol::Trait>,

    /// Whether the implementation is const.
    pub is_const: bool,

    /// The generic arguments supplied to the trait.
    pub generic_arguments: GenericArguments<M>,
}

impl<M: Model> Trait<M> {
    /// Converts the [`Trait`] predicate from the [`Default`] model to the model
    /// `M`.
    pub fn from_default_model(predicate: Trait<Default>) -> Self {
        Self {
            id: predicate.id,
            is_const: predicate.is_const,
            generic_arguments: GenericArguments::from_default_model(
                predicate.generic_arguments,
            ),
        }
    }
}

impl<T: State, M: Model> table::Display<T> for Trait<M>
where
    GenericArguments<M>: table::Display<T>,
{
    fn fmt(
        &self,
        table: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "trait ")?;

        if self.is_const {
            write!(f, "const ")?;
        }

        write!(
            f,
            "{}{}",
            table.get_qualified_name(self.id.into()).ok_or(fmt::Error)?,
            DisplayObject { display: &self.generic_arguments, table }
        )
    }
}

impl<M: Model> Trait<M> {
    /// Checks if the trait contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.generic_arguments.lifetimes.iter().any(Lifetime::is_error)
            || self.generic_arguments.types.iter().any(contains_error)
            || self.generic_arguments.constants.iter().any(contains_error)
    }
}

impl<M: Model> Compute for Trait<M> {
    type Error = OverflowError;
    type Parameter = ();

    #[allow(private_bounds, private_interfaces)]
    fn implementation<S: State>(
        &self,
        environment: &Environment<
            Self::Model,
            S,
            impl Normalizer<Self::Model, S>,
            impl Observer<Self::Model, S>,
        >,
        context: &mut Context<Self::Model>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error> {
        // if this query was made in some trait implementation or trait, then
        // check if the trait implementation is the same as the query one.
        if let Some(result) = is_in_active_trait_implementation(
            self.id,
            false,
            &self.generic_arguments,
            environment,
            context,
        )? {
            return Ok(Some(result));
        }

        // manually search for the trait implementation
        match resolve_implementation_with_context(
            self.id,
            &self.generic_arguments,
            environment,
            context,
        ) {
            Ok(implementation) => {
                return Ok(Some(Succeeded::with_constraints(
                    Satisfied::ByImplementation(implementation.result),
                    implementation.constraints,
                )));
            }

            Err(ResolveError::Overflow(exceed_limit_error)) => {
                return Err(exceed_limit_error);
            }

            Err(_) => {}
        }

        // look for the premise that matches
        for trait_premise in environment
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_trait)
        {
            // skip if the trait id is different
            if trait_premise.id != self.id {
                continue;
            }

            let Some(compatiblity) =
                self.generic_arguments.compatible_with_context(
                    &trait_premise.generic_arguments,
                    Variance::Invariant,
                    environment,
                    context,
                )?
            else {
                continue;
            };

            if !compatiblity.result.forall_lifetime_errors.is_empty() {
                continue;
            }

            return Ok(Some(Succeeded::with_constraints(
                Satisfied::ByPremise,
                compatiblity.constraints,
            )));
        }

        Ok(None)
    }

    fn on_cyclic(
        &self,
        _: Self::Parameter,
        _: Self::InProgress,
        _: Self::InProgress,
        _: &[crate::type_system::query::Record<Self::Model>],
    ) -> Result<Option<Self::Result>, Self::Error> {
        Ok(Some(Succeeded::new(
            Satisfied::ByTraitContext, /* doesn't matter */
        )))
    }
}

impl<M: Model> Trait<M> {
    /// Applies an instantiation to the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation<M>) {
        self.generic_arguments.instantiate(instantiation);
    }
}

/// A result of a trait implementation resolution query.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Implementation<M: Model> {
    /// The deduced substitution for the generic arguments of the trait
    /// implementation.
    pub instantiation: Instantiation<M>,

    /// The ID of the resolved trait implementation.
    pub id: ID<symbol::PositiveTraitImplementation>,

    /// If `true`, the implementation is not general enough to accomodate the
    /// forall lifetime requirements.
    pub is_not_general_enough: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum ResolveError {
    #[error("the trait id was invalid")]
    InvalidID,
    #[error("the trait was ambiguous")]
    AmbiguousTrait,
    #[error(transparent)]
    Overflow(#[from] OverflowError),
    #[error("the trait implementation was not found")]
    NotFound,
    #[error(
        "the generic arguments contained a term that can be rewritten in \
         multiple ways and caused an ambiguity in trait resolution"
    )]
    AmbiguousTerm,
}

/// Resolves for the trait implementation for the given trait and generic
/// arguments.
///
/// # Errors
///
/// - [`ResolveError::InvalidID`]: If the `trait_id` is invalid.
/// - [`ResolveError::NonDefiniteGenericArguments`]: If the `generic_arguments`
///   are not definite.
/// - [`ResolveError::AmbiguousTrait`]: If the trait defined in the table is
///   ambiguous (multiple trait implementation matches).
/// - [`ResolveError::NotFound`]: If the trait implementation was not found.
/// - [`ResolveError::Overflow`]: If the session limit was exceeded; see
///   [`OverflowError`] for more information.
#[allow(clippy::too_many_lines)]
pub fn resolve_implementation<M: Model, S: State>(
    trait_id: ID<symbol::Trait>,
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<M, S, impl Normalizer<M, S>, impl Observer<M, S>>,
) -> Result<Succeeded<Implementation<M>, M>, ResolveError> {
    resolve_implementation_with_context(
        trait_id,
        generic_arguments,
        environment,
        &mut Context::new(),
    )
}

/// Resolves for the trait implementation for the given trait and generic
/// arguments.
///
/// # Errors
///
/// - [`ResolveError::InvalidID`]: If the `trait_id` is invalid.
/// - [`ResolveError::NonDefiniteGenericArguments`]: If the `generic_arguments`
///   are not definite.
/// - [`ResolveError::AmbiguousTrait`]: If the trait defined in the table is
///   ambiguous (multiple trait implementation matches).
/// - [`ResolveError::NotFound`]: If the trait implementation was not found.
/// - [`ResolveError::Overflow`]: If the session limit was exceeded; see
///   [`OverflowError`] for more information.
pub fn resolve_implementation_with_context<M: Model, S: State>(
    trait_id: ID<symbol::Trait>,
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<M, S, impl Normalizer<M, S>, impl Observer<M, S>>,
    context: &mut Context<M>,
) -> Result<Succeeded<Implementation<M>, M>, ResolveError> {
    let trait_symbol =
        environment.table.get(trait_id).ok_or(ResolveError::InvalidID)?;

    if let Some(result) = is_in_active_trait_implementation(
        trait_id,
        true,
        generic_arguments,
        environment,
        context,
    )? {
        let Satisfied::ByImplementation(implementation) = result.result else {
            unreachable!()
        };

        return Ok(Succeeded::with_constraints(
            implementation,
            result.constraints,
        ));
    }

    let definite =
        generic_arguments.definite_with_context(environment, context)?;

    let (default_environment, _) =
        Environment::<M, _, _, _>::new(Premise::default(), environment.table());

    let mut candidate: Option<(
        TraitImplementationID,
        Deduction<M>,
        BTreeSet<LifetimeConstraint<M>>,
    )> = None;

    for (key, arguments, is_final) in
        trait_symbol.implementations().iter().copied().map(|k| {
            (
                k,
                GenericArguments::from_default_model(
                    environment
                        .table
                        .get_implementation(k.into())
                        .unwrap()
                        .arguments()
                        .clone(),
                ),
                match k {
                    TraitImplementationID::Positive(id) => {
                        environment.table().get(id).unwrap().is_final
                    }
                    TraitImplementationID::Negative(id) => {
                        environment.table().get(id).unwrap().is_final
                    }
                },
            )
        })
    {
        // builds the unification
        let Succeeded { result: deduction, constraints: lifetime_constraints } =
            match arguments.deduce_with_context(
                generic_arguments,
                environment,
                context,
            ) {
                Ok(unification) => unification,

                Err(deduction::Error::TypeSystem(overflow)) => {
                    return Err(overflow.into())
                }

                Err(
                    deduction::Error::MismatchedGenericArgumentCount(_)
                    | deduction::Error::UnificationFailure(_),
                ) => continue,
            };

        // the trait implementation is not final and requires the term to be
        // definite to check.
        if !is_final && definite.is_none() {
            continue;
        }

        // check if satisfies all the predicate
        if !is_final {
            let generic_symbol =
                environment.table.get_generic(key.into()).unwrap();

            if !predicate_satisfies(
                &*generic_symbol,
                &deduction.instantiation,
                environment,
                context,
            )? {
                continue;
            }
        }

        // assign the candidate
        match &mut candidate {
            Some((
                candidate_id,
                candidate_instantiation,
                candidate_lifetime_constraints,
            )) => {
                // check which one is more specific
                match GenericArguments::from_default_model(
                    environment
                        .table
                        .get_implementation(key.into())
                        .unwrap()
                        .arguments()
                        .clone(),
                )
                .order(
                    &GenericArguments::from_default_model(
                        environment
                            .table
                            .get_implementation((*candidate_id).into())
                            .unwrap()
                            .arguments()
                            .clone(),
                    ),
                    &default_environment,
                )? {
                    order::Order::Incompatible => {
                        return Err(ResolveError::AmbiguousTerm)
                    }
                    order::Order::MoreGeneral => {}
                    order::Order::MoreSpecific => {
                        *candidate_id = key;
                        *candidate_instantiation = deduction;
                        *candidate_lifetime_constraints = lifetime_constraints;
                    }
                    order::Order::Ambiguous => {
                        return Err(ResolveError::AmbiguousTrait)
                    }
                }
            }

            candidate @ None => {
                *candidate = Some((key, deduction, lifetime_constraints));
            }
        }
    }

    match candidate {
        Some((
            TraitImplementationID::Positive(id),
            deduction,
            mut lifetime_constraints,
        )) => Ok(Succeeded {
            result: Implementation {
                instantiation: deduction.instantiation,
                is_not_general_enough: deduction.is_not_general_enough,
                id,
            },
            constraints: {
                lifetime_constraints
                    .extend(definite.into_iter().flat_map(|x| x.constraints));
                lifetime_constraints
            },
        }),

        None | Some((TraitImplementationID::Negative(_), _, _)) => {
            Err(ResolveError::NotFound)
        }
    }
}

fn predicate_satisfies<M: Model, S: State>(
    generic_symbol: &dyn Generic,
    substitution: &Instantiation<M>,
    environment: &Environment<M, S, impl Normalizer<M, S>, impl Observer<M, S>>,
    context: &mut Context<M>,
) -> Result<bool, OverflowError> {
    // check if satisfies all the predicate
    for mut predicate in generic_symbol
        .generic_declaration()
        .predicates
        .iter()
        .map(|x| Predicate::from_default_model(x.predicate.clone()))
    {
        predicate.instantiate(substitution);

        if !match predicate {
            Predicate::TraitTypeEquality(equality) => {
                Type::TraitMember(equality.lhs)
                    .compatible_with_context(
                        &equality.rhs,
                        Variance::Covariant,
                        environment,
                        context,
                    )?
                    .is_some()
            }
            Predicate::ConstantType(constant_type) => constant_type
                .query_with_context(environment, context)?
                .is_some(),

            Predicate::TupleType(tuple_type) => {
                tuple_type.query_with_context(environment, context)?.is_some()
            }

            Predicate::TupleConstant(tuple_constant) => tuple_constant
                .query_with_context(environment, context)?
                .is_some(),

            Predicate::Trait(tr) => {
                tr.query_with_context(environment, context)?.is_some()
            }

            Predicate::TypeOutlives(_) | Predicate::LifetimeOutlives(_) => true,
        } {
            return Ok(false);
        }
    }

    Ok(true)
}

fn is_in_active_trait_implementation<M: Model, S: State>(
    trait_id: ID<symbol::Trait>,
    need_implementation: bool,
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<M, S, impl Normalizer<M, S>, impl Observer<M, S>>,
    context: &mut Context<M>,
) -> Result<Output<Satisfied<M>, M>, OverflowError> {
    match environment.premise.trait_context {
        TraitContext::InTraitImplementation(trait_implementation) => {
            let Some(implementation) =
                environment.table.get(trait_implementation)
            else {
                return Ok(None);
            };

            // check if the trait id is the same
            if implementation.implemented_id() != trait_id {
                return Ok(None);
            }

            let Some(_) = generic_arguments.compatible_with_context(
                &GenericArguments::from_default_model(
                    implementation.arguments.clone(),
                ),
                Variance::Invariant,
                environment,
                context,
            )?
            else {
                return Ok(None);
            };

            match generic_arguments.deduce_with_context(
                &GenericArguments::from_default_model(
                    implementation.arguments.clone(),
                ),
                environment,
                context,
            ) {
                Ok(result) => Ok(Some(Succeeded {
                    result: Satisfied::ByImplementation(Implementation {
                        instantiation: result.result.instantiation,
                        id: trait_implementation,
                        is_not_general_enough: result
                            .result
                            .is_not_general_enough,
                    }),
                    constraints: result.constraints,
                })),
                Err(deduction::Error::TypeSystem(err)) => return Err(err),
                _ => return Ok(None),
            }
        }

        TraitContext::InTrait(env_trait_id) => {
            if need_implementation || env_trait_id != trait_id {
                return Ok(None);
            }

            let trait_symbol = environment.table.get(trait_id).unwrap();

            let trait_generic_arguments = trait_symbol
                .generic_declaration()
                .parameters
                .create_identity_generic_arguments(env_trait_id.into());

            let Some(compatiblity) = generic_arguments
                .compatible_with_context(
                    &trait_generic_arguments,
                    Variance::Invariant,
                    environment,
                    context,
                )?
            else {
                return Ok(None);
            };

            if !compatiblity.result.forall_lifetime_errors.is_empty() {
                Ok(None)
            } else {
                Ok(Some(Succeeded::with_constraints(
                    Satisfied::ByTraitContext,
                    compatiblity.constraints,
                )))
            }
        }

        TraitContext::Normal => Ok(None),
    }
}

#[cfg(test)]
mod tests;
