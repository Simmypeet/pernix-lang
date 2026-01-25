//! Contains the logic for resolving implementation for traits and markers.

use std::{collections::BTreeSet, ops::Deref, sync::Arc};

use pernixc_semantic_element::{
    implemented::get_implemented,
    implements::get_implements,
    implements_arguments::get_implements_argument,
    variance::Variance,
    where_clause::{self, get_where_clause},
};
use pernixc_symbol::{
    final_implements::get_is_implements_final,
    kind::{Kind, get_kind},
    parent::scope_walker,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments, instantiation::Instantiation,
    predicate::Predicate, r#type::Type,
};
use qbice::storage::intern::Interned;

use crate::{
    Error, Succeeded,
    deduction::{self, Deduction},
    environment::{BoxedFuture, Environment, Query},
    lifetime_constraint::LifetimeConstraint,
    normalizer::Normalizer,
    order::{self, Order},
};

/// A result of a implementation resolution query.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Implementation {
    /// The deduced substitution for the generic arguments of the trait
    /// implementation.
    pub instantiation: Instantiation,

    /// The ID of the resolved implementation.
    pub id: Global<pernixc_symbol::ID>,

    /// If `true`, the implementation is not general enough to accomodate the
    /// forall lifetime requirements.
    pub is_not_general_enough: bool,
}

/// A query for resolving a matching `implements`.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_new::new,
)]
pub struct Resolve {
    /// The `trait` or `marker` to resolve the `implements` for.
    pub implemented_id: Global<pernixc_symbol::ID>,

    /// The generic arguments supplied to the [`Self::implemented_id`]
    pub generic_arguments: GenericArguments,
}

impl Query for Resolve {
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<Implementation>;
    type Error = Error;

    #[allow(clippy::too_many_lines)]
    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> BoxedFuture<'x, Self::Result, Self::Error> {
        Box::pin(async move {
            let symbol_kind = environment
                .tracked_engine()
                .get_kind(self.implemented_id)
                .await;

            // check that it must be trait or marker
            assert!(matches!(symbol_kind, Kind::Trait | Kind::Marker));

            // we might be in the implementation site already
            if let Some(result) = is_in_active_implementation(
                self.implemented_id,
                &self.generic_arguments,
                environment,
            )
            .await?
            {
                return Ok(Some(Arc::new(result)));
            }

            let definite = environment
                .generic_arguments_definite(&self.generic_arguments)
                .await?;

            // the current candidate
            #[allow(clippy::type_complexity)]
            let mut candidate: Option<(
                Global<pernixc_symbol::ID>,
                Deduction,
                BTreeSet<LifetimeConstraint>,
                GenericArguments,
            )> = None;

            let implementations = environment
                .tracked_engine()
                .get_implemented(self.implemented_id)
                .await
                .iter()
                .copied()
                .collect::<Vec<_>>();

            for current_impl_id in implementations {
                let Some(implementation_generic_arguments) = environment
                    .tracked_engine()
                    .get_implements_argument(current_impl_id)
                    .await
                else {
                    continue;
                };

                // build the unification
                let Succeeded {
                    result: deduction,
                    constraints: lifetime_constraints,
                } = match environment
                    .deduce(
                        &implementation_generic_arguments,
                        &self.generic_arguments,
                    )
                    .await
                {
                    Ok(unification) => unification,

                    Err(deduction::Error::Overflow(error)) => {
                        return Err(error.into());
                    }

                    Err(
                        deduction::Error::MismatchedGenericArgumentCount(_)
                        | deduction::Error::UnificationFailure(_),
                    ) => continue,
                };

                let is_final = match symbol_kind {
                    Kind::Trait => {
                        environment
                            .tracked_engine()
                            .get_is_implements_final(current_impl_id)
                            .await
                    }

                    // every marker's implementaions are final
                    Kind::Marker => true,

                    _ => unreachable!(),
                };

                if !is_final {
                    // the implementation is not final, therefore, it requires
                    // generic arguments to be definite
                    if definite.is_none() {
                        continue;
                    }

                    // all predicates must satisfy to continue
                    let where_clause = environment
                        .tracked_engine()
                        .get_where_clause(current_impl_id)
                        .await;

                    if !predicate_satisfies(
                        where_clause,
                        &deduction.instantiation,
                        environment,
                    )
                    .await?
                    {
                        continue;
                    }
                }

                // compare with the current candidate
                match &mut candidate {
                    Some((
                        candidate_id,
                        candidate_instantiation,
                        candidate_lifetime_constraints,
                        candidate_generic_arguments,
                    )) => {
                        // check which one is more specific
                        match environment
                            .tracked_engine()
                            .query(&order::Key::new(
                                current_impl_id,
                                *candidate_id,
                            ))
                            .await
                        {
                            Ok(Some(
                                Order::Ambiguous | Order::Incompatible,
                            )) => {
                                return Ok(None);
                            }

                            Ok(Some(Order::MoreGeneral) | None) => {}

                            Ok(Some(Order::MoreSpecific)) => {
                                *candidate_id = current_impl_id;
                                *candidate_instantiation = deduction;
                                *candidate_lifetime_constraints =
                                    lifetime_constraints;
                                *candidate_generic_arguments =
                                    implementation_generic_arguments
                                        .deref()
                                        .clone();
                            }

                            Err(error) => {
                                return Err(Error::Overflow(error));
                            }
                        }
                    }

                    candidate @ None => {
                        *candidate = Some((
                            current_impl_id,
                            deduction,
                            lifetime_constraints,
                            implementation_generic_arguments.deref().clone(),
                        ));
                    }
                }
            }

            println!(
                "Done resolving implementation for {:?}",
                self.implemented_id
            );
            match candidate {
                Some((
                    implementation_id,
                    deduction,
                    mut lifetime_constraints,
                    _,
                )) => Ok(Some(Arc::new(Succeeded {
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
                }))),
                None => Ok(None),
            }
        })
    }
}

#[allow(clippy::type_complexity)]
async fn is_in_active_implementation(
    implemented_id: Global<pernixc_symbol::ID>,
    generic_arguments: &GenericArguments,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<Succeeded<Implementation>>, Error> {
    let Some(query_site) = environment.premise().query_site else {
        return Ok(None);
    };

    let mut scope_walker =
        environment.tracked_engine().scope_walker(query_site);

    while let Some(current_id) = scope_walker.next().await {
        let current_id = Global::new(query_site.target_id, current_id);
        let current_kind =
            environment.tracked_engine().get_kind(current_id).await;

        // must be the implementation kind
        if !matches!(
            current_kind,
            Kind::PositiveImplementation | Kind::NegativeImplementation
        ) {
            continue;
        }

        // must be an implementation
        if environment.tracked_engine().get_implements(current_id).await
            != Some(implemented_id)
        {
            continue;
        }

        let Some(implementation_arguments) = environment
            .tracked_engine()
            .get_implements_argument(current_id)
            .await
        else {
            continue;
        };

        let Some(_) = environment
            .subtypes_generic_arguments(
                generic_arguments,
                &implementation_arguments,
            )
            .await?
        else {
            continue;
        };

        match environment
            .deduce(generic_arguments, &implementation_arguments)
            .await
        {
            Ok(result) => {
                return Ok(Some(Succeeded {
                    result: Implementation {
                        instantiation: result.result.instantiation,
                        id: current_id,
                        is_not_general_enough: result
                            .result
                            .is_not_general_enough,
                    },
                    constraints: result.constraints,
                }));
            }

            Err(deduction::Error::Overflow(err)) => {
                return Err(Error::Overflow(err));
            }

            _ => {}
        }
    }

    Ok(None)
}

async fn predicate_satisfies(
    predicates: Interned<[where_clause::Predicate]>,
    substitution: &Instantiation,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<bool, Error> {
    // check if satisfies all the predicate
    for mut predicate in predicates.iter().map(|x| x.predicate.clone()) {
        predicate.instantiate(substitution);

        if !match predicate {
            Predicate::TraitTypeCompatible(equality) => environment
                .subtypes(
                    Type::TraitMember(equality.lhs),
                    equality.rhs.clone(),
                    Variance::Covariant,
                )
                .await?
                .is_some(),

            Predicate::ConstantType(constant_type) => {
                environment.query(&constant_type).await?.is_some()
            }

            Predicate::TupleType(tuple_type) => {
                environment.query(&tuple_type).await?.is_some()
            }

            Predicate::PositiveMarker(tr) => {
                environment.query(&tr).await?.is_some()
            }

            Predicate::NegativeMarker(tr) => {
                environment.query(&tr).await?.is_none()
            }

            Predicate::PositiveTrait(tr) => {
                environment.query(&tr).await?.is_some()
            }

            Predicate::NegativeTrait(tr) => {
                environment.query(&tr).await?.is_some()
            }

            Predicate::TypeOutlives(_) | Predicate::LifetimeOutlives(_) => true,
        } {
            return Ok(false);
        }
    }

    Ok(true)
}
