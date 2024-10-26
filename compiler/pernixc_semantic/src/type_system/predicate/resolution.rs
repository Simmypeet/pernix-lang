use std::{collections::BTreeSet, ops::Deref};

use thiserror::Error;

use super::Predicate;
use crate::{
    arena::ID,
    symbol::{
        table::{
            representation::{Element, Index, Representation},
            State,
        },
        Generic, GlobalID, Implementation as _, Implemented,
        ResolvableImplementation, ResolvableImplementedID,
    },
    type_system::{
        compatible::Compatible,
        deduction::{self, Deduction},
        environment::Environment,
        instantiation::Instantiation,
        model::{self, Model},
        normalizer::Normalizer,
        observer::Observer,
        order,
        query::Context,
        term::{r#type::Type, GenericArguments},
        variance::Variance,
        Compute, LifetimeConstraint, OverflowError, Premise, Succeeded,
    },
};

/// A result of a implementation resolution query.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Implementation<ID, M: Model> {
    /// The deduced substitution for the generic arguments of the trait
    /// implementation.
    pub instantiation: Instantiation<M>,

    /// The ID of the resolved trait implementation.
    pub id: ID,

    /// If `true`, the implementation is not general enough to accomodate the
    /// forall lifetime requirements.
    pub is_not_general_enough: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("the `implemented_id` is invalid to the table")]
    InvalidID,
    #[error(
        "the generic arguments contained a term that can be rewritten in \
         multiple ways and caused an ambiguity in resolution"
    )]
    Ambiguous,
    #[error("no matching implementation was found")]
    NotFound,
    #[error(transparent)]
    Overflow(#[from] OverflowError),
}

/// Resolves the implementation for the given `implemented_id` and
/// `generic_arguments` with explicitly specified `context`.
///
/// # Errors
///
/// See [`Error`] for more information.
#[allow(clippy::too_many_lines)]
pub fn resolve_implementation_with_context<
    'a,
    M: Model,
    S: State,
    ImplementationID: TryFrom<GlobalID> + Copy,
    ImplementedSymbol: Implemented<ImplementationID> + Element,
>(
    implemented_id: ID<ImplementedSymbol>,
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<
        'a,
        M,
        S,
        impl Normalizer<M, S>,
        impl Observer<M, S>,
    >,
    context: &mut Context<M>,
) -> Result<Succeeded<Implementation<ImplementationID, M>, M>, Error>
where
    Representation<S::Container>: Index<ImplementationID>,

    <Representation<S::Container> as Index<ImplementationID>>::Output<'a>:
        Deref,

    <<Representation<S::Container> as Index<ImplementationID>>::Output<'a>
        as Deref>::Target: ResolvableImplementation<ID<ImplementedSymbol>>,

    ID<ImplementedSymbol>: Into<ResolvableImplementedID>,
{
    Observer::on_resolving_implementation(
        implemented_id.into(),
        generic_arguments,
        environment,
    )?;

    // we might be in the implementation site already
    if let Some(result) = is_in_active_implementation(
        implemented_id,
        generic_arguments,
        environment,
        context,
    )? {
        return Ok(result);
    }

    let definite =
        generic_arguments.definite_with_context(environment, context)?;

    let (default_environment, _) =
        Environment::<M, _, _, _>::new(Premise::default(), environment.table());

    let implemented_symbol = Index::<ID<ImplementedSymbol>>::get(
        &**environment.table(),
        implemented_id,
    )
    .ok_or(Error::InvalidID)?;

    // the current candidate
    #[allow(clippy::type_complexity)]
    let mut candidate: Option<(
        ImplementationID,
        Deduction<M>,
        BTreeSet<LifetimeConstraint<M>>,
        GenericArguments<M>,
    )> = None;

    for implementation_id in
        implemented_symbol.implementations().iter().copied()
    {
        // build the unification
        let implementation_symbol = Index::<ImplementationID>::get(
            &**environment.table(),
            implementation_id,
        )
        .unwrap();

        let implementation_generic_arguments =
            GenericArguments::from_default_model(
                implementation_symbol.arguments().clone(),
            );

        // build the unification
        let Succeeded { result: deduction, constraints: lifetime_constraints } =
            match implementation_generic_arguments.deduce_with_context(
                generic_arguments,
                environment,
                context,
            ) {
                Ok(unification) => unification,

                Err(deduction::Error::Overflow(overflow)) => {
                    return Err(overflow.into())
                }

                Err(
                    deduction::Error::MismatchedGenericArgumentCount(_)
                    | deduction::Error::UnificationFailure(_),
                ) => continue,
            };

        if !implementation_symbol.is_final() {
            // the implementation is not final, therefore, it requires generic
            // arguments to be definite
            if definite.is_none() {
                continue;
            }

            // all predicates must satisfy to continue
            if !predicate_satisfies(
                implementation_symbol
                    .generic_declaration()
                    .predicates
                    .iter()
                    .map(|x| &x.predicate),
                &deduction.instantiation,
                environment,
                context,
            )? {
                continue;
            }
        }

        drop(implementation_symbol);

        // compare with the current candidate
        match &mut candidate {
            Some((
                candidate_id,
                candidate_instantiation,
                candidate_lifetime_constraints,
                candidate_generic_arguments,
            )) => {
                // check which one is more specific
                match implementation_generic_arguments
                    .order(candidate_generic_arguments, &default_environment)?
                {
                    order::Order::Ambiguous | order::Order::Incompatible => {
                        return Err(Error::Ambiguous)
                    }
                    order::Order::MoreGeneral => {}
                    order::Order::MoreSpecific => {
                        *candidate_id = implementation_id;
                        *candidate_instantiation = deduction;
                        *candidate_lifetime_constraints = lifetime_constraints;
                        *candidate_generic_arguments =
                            implementation_generic_arguments;
                    }
                }
            }

            candidate @ None => {
                *candidate = Some((
                    implementation_id,
                    deduction,
                    lifetime_constraints,
                    implementation_generic_arguments,
                ));
            }
        }
    }

    match candidate {
        Some((implementation_id, deduction, mut lifetime_constraints, _)) => {
            Ok(Succeeded {
                result: Implementation {
                    instantiation: deduction.instantiation,
                    id: implementation_id,
                    is_not_general_enough: deduction.is_not_general_enough,
                },
                constraints: {
                    lifetime_constraints.extend(
                        definite.into_iter().flat_map(|x| x.constraints),
                    );
                    lifetime_constraints
                },
            })
        }
        None => Err(Error::NotFound),
    }
}

/// Resolves the implementation for the given `implemented_id` and
/// `generic_arguments`.
///
/// # Errors
///
/// See [`Error`] for more information.
pub fn resolve_implementation<
    'a,
    M: Model,
    S: State,
    ImplementationID: TryFrom<GlobalID> + Copy,
    ImplementedSymbol: Implemented<ImplementationID> + Element,
>(
    implemented_id: ID<ImplementedSymbol>,
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<
        'a,
        M,
        S,
        impl Normalizer<M, S>,
        impl Observer<M, S>,
    >,
) -> Result<Succeeded<Implementation<ImplementationID, M>, M>, Error>
where
    Representation<S::Container>: Index<ImplementationID>,

    <Representation<S::Container> as Index<ImplementationID>>::Output<'a>:
        Deref,

    <<Representation<S::Container> as Index<ImplementationID>>::Output<'a>
        as Deref>::Target: ResolvableImplementation<ID<ImplementedSymbol>>,

    ID<ImplementedSymbol>: Into<ResolvableImplementedID>,
{
    resolve_implementation_with_context(
        implemented_id,
        generic_arguments,
        environment,
        &mut Context::default(),
    )
}

#[allow(clippy::type_complexity)]
fn is_in_active_implementation<
    'a,
    M: Model,
    S: State,
    ImplementationID: TryFrom<GlobalID> + Copy,
    ImplementedSymbol: Implemented<ImplementationID> + Element,
>(
    implemented_id: ID<ImplementedSymbol>,
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<'a,M, S, impl Normalizer<M, S>, impl Observer<M, S>>,
    context: &mut Context<M>,
)  -> Result<Option<Succeeded<Implementation<ImplementationID, M>, M>>, Error>
where
    Representation<S::Container>: Index<ImplementationID>,

    <Representation<S::Container> as Index<ImplementationID>>::Output<'a>:
        Deref,

    <<Representation<S::Container> as Index<ImplementationID>>::Output<'a>
        as Deref>::Target: ResolvableImplementation<ID<ImplementedSymbol>>,

    ID<ImplementedSymbol>: Into<ResolvableImplementedID>,

{
    let Some(query_site) = environment.premise.query_site else {
        return Ok(None);
    };

    for global_id in
        if let Some(iter) = environment.table.scope_walker(query_site) {
            iter
        } else {
            return Ok(None);
        }
    {
        // must be an implementation
        let Ok(implementation_id) = ImplementationID::try_from(global_id)
        else {
            continue;
        };

        let implementation = Index::<ImplementationID>::get(
            &**environment.table,
            implementation_id,
        )
        .unwrap();

        // the implemented_id must be the same
        if implementation.implemented_id() != implemented_id {
            continue;
        }

        let implementation_arguments = GenericArguments::from_default_model(
            implementation.arguments().clone(),
        );
        let Some(_) = generic_arguments.compatible_with_context(
            &implementation_arguments,
            Variance::Invariant,
            environment,
            context,
        )?
        else {
            continue;
        };

        match generic_arguments.deduce_with_context(
            &implementation_arguments,
            environment,
            context,
        ) {
            Ok(result) => {
                return Ok(Some(Succeeded {
                    result: Implementation {
                        instantiation: result.result.instantiation,
                        id: implementation_id,
                        is_not_general_enough: result
                            .result
                            .is_not_general_enough,
                    },
                    constraints: result.constraints,
                }))
            }

            Err(deduction::Error::Overflow(err)) => {
                return Err(Error::Overflow(err))
            }

            _ => continue,
        }
    }

    Ok(None)
}

fn predicate_satisfies<'a, M: Model, S: State>(
    predicates: impl Iterator<Item = &'a Predicate<model::Default>>,
    substitution: &Instantiation<M>,
    environment: &Environment<M, S, impl Normalizer<M, S>, impl Observer<M, S>>,
    context: &mut Context<M>,
) -> Result<bool, OverflowError> {
    // check if satisfies all the predicate
    for mut predicate in
        predicates.map(|x| Predicate::from_default_model(x.clone()))
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

            Predicate::PositiveTrait(tr) => {
                tr.query_with_context(environment, context)?.is_some()
            }

            Predicate::NegativeTrait(tr) => {
                tr.query_with_context(environment, context)?.is_some()
            }

            Predicate::PositiveMarker(tr) => {
                tr.query_with_context(environment, context)?.is_some()
            }

            Predicate::NegativeMarker(tr) => {
                tr.query_with_context(environment, context)?.is_some()
            }

            Predicate::TypeOutlives(_) | Predicate::LifetimeOutlives(_) => true,
        } {
            return Ok(false);
        }
    }

    Ok(true)
}
