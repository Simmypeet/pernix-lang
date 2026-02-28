//! Contains the well-formedness check implementation.

use std::collections::BTreeSet;

use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    implements_arguments::get_implements_argument,
    implied_predicate::get_implied_predicates, variance::Variance,
    where_clause::get_where_clause,
};
use pernixc_symbol::kind::get_kind;
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    instantiation::{self, Instantiation},
    predicate::{Outlives, Predicate},
    r#type::Type,
    visitor::RecursiveIterator,
};

use crate::{
    Succeeded, UnrecoverableError,
    deduction::Deduction,
    diagnostic::{
        AdtImplementationIsNotGeneralEnough, Diagnostic,
        MismatchedImplementationArguments, PredicateSatisfiabilityOverflow,
        UnsatisfiedPredicate,
    },
    environment::Environment,
    lifetime_constraint::LifetimeConstraint,
    normalizer::Normalizer,
};

/// The result of [`Environment::wf_check_implementation`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplementsCheckResult {
    deduction: Deduction,
    constraints: BTreeSet<LifetimeConstraint>,
}

impl ImplementsCheckResult {
    /// Retrieves the instantiation retrieved from the deduction.
    #[must_use]
    pub fn into_instantiation(self) -> instantiation::Instantiation {
        self.deduction.instantiation
    }
}

impl<N: Normalizer> Environment<'_, N> {
    /// Checks if the given `predicate` is satisfied in the given `environment`.
    ///
    /// If `do_outlives_check` is true, then the outlives constraints/predicates
    /// will be checked using symbolic evaluation. Otherwise, the outlives
    /// constraints/predicates will be assumed to be satisfied and returned as
    /// lifetime constraints in the Ok result.
    #[allow(clippy::too_many_lines)]
    pub async fn predicate_satisfied(
        &self,
        predicate: Predicate,
        instantiation_span: &RelativeSpan,
        predicate_declaration_span: Option<&RelativeSpan>,
        do_outlives_check: bool,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<BTreeSet<LifetimeConstraint>, UnrecoverableError> {
        let mut diagnostics = Vec::new();

        let result = self
            .predicate_satisfied_internal(
                predicate,
                instantiation_span,
                predicate_declaration_span,
                do_outlives_check,
                &mut diagnostics,
                handler,
            )
            .await?;

        handler.receieve_many(diagnostics);

        Ok(result)
    }

    /// Checks if the given `predicate` is satisfied in the given `environment`.
    ///
    /// If `do_outlives_check` is true, then the outlives constraints/predicates
    /// will be checked using symbolic evaluation. Otherwise, the outlives
    /// constraints/predicates will be assumed to be satisfied and returned as
    /// lifetime constraints in the Ok result.
    pub async fn predicate_satisfied_as_diagnostics(
        &self,
        predicate: Predicate,
        instantiation_span: &RelativeSpan,
        predicate_declaration_span: Option<&RelativeSpan>,
        do_outlives_check: bool,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<
        (Vec<Diagnostic>, BTreeSet<LifetimeConstraint>),
        UnrecoverableError,
    > {
        let mut diagnostics = Vec::new();

        let constraints = self
            .predicate_satisfied_internal(
                predicate,
                instantiation_span,
                predicate_declaration_span,
                do_outlives_check,
                &mut diagnostics,
                handler,
            )
            .await?;

        Ok((diagnostics, constraints))
    }

    #[allow(clippy::too_many_lines)]
    async fn predicate_satisfied_internal(
        &self,
        predicate: Predicate,
        instantiation_span: &RelativeSpan,
        predicate_declaration_span: Option<&RelativeSpan>,
        do_outlives_check: bool,
        diagnostics: &mut Vec<Diagnostic>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<BTreeSet<LifetimeConstraint>, UnrecoverableError> {
        let result = match &predicate {
            Predicate::InstanceAssociatedTypeEquality(equality) => {
                let result = self
                    .subtypes(
                        Type::InstanceAssociated(equality.lhs.clone()),
                        equality.rhs.clone(),
                        Variance::Covariant,
                    )
                    .await;

                match result {
                    Ok(Some(result)) => {
                        if result.result.forall_lifetime_errors.is_empty() {
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            )))
                        } else {
                            Ok(None)
                        }
                    }

                    Ok(None) => Ok(None),

                    Err(error) => Err(error),
                }
            }

            Predicate::ConstantType(constant_type) => {
                self.query(constant_type).await.map(|x| x.as_deref().cloned())
            }

            Predicate::LifetimeOutlives(outlives) => {
                if do_outlives_check {
                    match self.query(outlives).await {
                        Ok(true) => return Ok(BTreeSet::new()),

                        Ok(false) => {
                            diagnostics.push(Diagnostic::UnsatisfiedPredicate(
                                UnsatisfiedPredicate {
                                    predicate,
                                    instantiation_span: *instantiation_span,
                                    predicate_declaration_span:
                                        predicate_declaration_span.copied(),
                                },
                            ));

                            return Ok(BTreeSet::new());
                        }
                        Err(overflow_error) => {
                            handler.receive(
                                Diagnostic::PredicateSatisfiabilityOverflow(
                                    PredicateSatisfiabilityOverflow {
                                        predicate,
                                        instantiation_span: *instantiation_span,
                                        predicate_declaration_span:
                                            predicate_declaration_span.copied(),
                                        overflow_error,
                                    },
                                ),
                            );

                            return Err(UnrecoverableError::Reported);
                        }
                    }
                }

                Ok(Some(Succeeded::satisfied_with(
                    std::iter::once(LifetimeConstraint::LifetimeOutlives(
                        outlives.clone(),
                    ))
                    .collect(),
                )))
            }

            Predicate::TypeOutlives(outlives) => {
                if do_outlives_check {
                    match self.query(outlives).await {
                        Ok(true) => return Ok(BTreeSet::new()),

                        Ok(false) => {
                            diagnostics.push(Diagnostic::UnsatisfiedPredicate(
                                UnsatisfiedPredicate {
                                    predicate,
                                    instantiation_span: *instantiation_span,
                                    predicate_declaration_span:
                                        predicate_declaration_span.copied(),
                                },
                            ));

                            return Ok(BTreeSet::new());
                        }

                        Err(overflow_error) => {
                            handler.receive(
                                Diagnostic::PredicateSatisfiabilityOverflow(
                                    PredicateSatisfiabilityOverflow {
                                        predicate,
                                        predicate_declaration_span:
                                            predicate_declaration_span.copied(),
                                        instantiation_span: *instantiation_span,
                                        overflow_error,
                                    },
                                ),
                            );

                            return Err(UnrecoverableError::Reported);
                        }
                    }
                }

                Ok(Some(Succeeded::satisfied_with(
                    RecursiveIterator::new(&outlives.bound)
                        .filter_map(|x| x.0.into_lifetime().ok())
                        .map(|x| {
                            LifetimeConstraint::LifetimeOutlives(Outlives::new(
                                x.clone(),
                                outlives.bound.clone(),
                            ))
                        })
                        .collect(),
                )))
            }

            Predicate::TupleType(tuple) => {
                self.query(tuple).await.map(|x| x.as_deref().cloned())
            }

            Predicate::PositiveMarker(_) => {
                todo!()
            }

            Predicate::NegativeMarker(_) => {
                todo!()
            }
        };

        match result {
            Ok(Some(Succeeded { constraints, .. })) => {
                self.handle_satisfy(
                    constraints,
                    instantiation_span,
                    predicate_declaration_span,
                    do_outlives_check,
                    diagnostics,
                    handler,
                )
                .await
            }

            Ok(None) => {
                diagnostics.push(Diagnostic::UnsatisfiedPredicate(
                    UnsatisfiedPredicate {
                        predicate,
                        instantiation_span: *instantiation_span,
                        predicate_declaration_span: predicate_declaration_span
                            .copied(),
                    },
                ));

                Ok(BTreeSet::new())
            }

            Err(overflow_error) => {
                handler.receive(Diagnostic::PredicateSatisfiabilityOverflow(
                    PredicateSatisfiabilityOverflow {
                        predicate,
                        predicate_declaration_span: predicate_declaration_span
                            .copied(),
                        instantiation_span: *instantiation_span,
                        overflow_error,
                    },
                ));

                Ok(BTreeSet::new())
            }
        }
    }

    async fn handle_satisfy(
        &self,
        constraints: BTreeSet<LifetimeConstraint>,
        instantiation_span: &RelativeSpan,
        predicate_declaration_span: Option<&RelativeSpan>,
        do_outlives_check: bool,
        diagnostics: &mut Vec<Diagnostic>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<BTreeSet<LifetimeConstraint>, UnrecoverableError> {
        // if do_outlives_check is false, then we don't need to check
        if !do_outlives_check {
            return Ok(constraints);
        }

        for constraint in constraints {
            match constraint.satisfies(self).await {
                Ok(true) => {}

                Ok(false) => {
                    diagnostics.push(Diagnostic::UnsatisfiedPredicate(
                        UnsatisfiedPredicate {
                            predicate: constraint.into_predicate(),
                            instantiation_span: *instantiation_span,
                            predicate_declaration_span:
                                predicate_declaration_span.copied(),
                        },
                    ));
                }

                Err(e) => {
                    e.report_as_undecidable_predicate(
                        constraint.into_predicate(),
                        None,
                        *instantiation_span,
                        handler,
                    );

                    return Err(UnrecoverableError::Reported);
                }
            }
        }

        Ok(BTreeSet::new())
    }

    #[allow(clippy::type_complexity)]
    async fn get_all_predicates(
        table: &TrackedEngine,
        global_id: Global<pernixc_symbol::ID>,
        instantiation: Option<&Instantiation>,
    ) -> Vec<(Predicate, Option<RelativeSpan>)> {
        let symbol_kind = table.get_kind(global_id).await;
        let mut predicates = Vec::new();

        if symbol_kind.has_where_clause() {
            let where_cluase = table.get_where_clause(global_id).await;

            for predicate in where_cluase.iter() {
                predicates.push((predicate.predicate.clone(), predicate.span));
            }
        }

        if symbol_kind.has_implied_predicates() {
            let implied_predicates =
                table.get_implied_predicates(global_id).await;

            for predicate in implied_predicates.iter() {
                predicates.push((predicate.clone().into(), None));
            }
        }

        // instantiate the predicates
        if let Some(instantiation) = instantiation {
            for predicate in predicates.iter_mut().map(|x| &mut x.0) {
                predicate.instantiate(instantiation);
            }
        }

        predicates
    }

    /// Checks the well-formedness of the implementation with the given
    /// `impl_id` and returns the lifetime constraints that need to be
    /// satisfied for the implementation to be well-formed.
    ///
    /// If `do_outlives_check` is true, then the outlives constraints/predicates
    /// will be checked using symbolic evaluation. Otherwise, the outlives
    /// constraints/predicates will be assumed to be satisfied and returned as
    /// lifetime constraints in the Ok result.
    pub async fn wf_check_implementation(
        &self,
        impl_id: Global<pernixc_symbol::ID>,
        instantiation_span: &RelativeSpan,
        generic_arguments: &GenericArguments,
        do_outlives_check: bool,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Option<ImplementsCheckResult>, UnrecoverableError> {
        // deduce the generic arguments
        let Some(impl_arguments) =
            self.tracked_engine().get_implements_argument(impl_id).await
        else {
            return Ok(None); // can't continue
        };

        let result = match self.deduce(&impl_arguments, generic_arguments).await
        {
            Ok(Some(deduced)) => deduced,

            Ok(None) => {
                handler.receive(Diagnostic::MismatchedImplementationArguments(
                    MismatchedImplementationArguments {
                        adt_implementation_id: impl_id,
                        found_generic_arguments: generic_arguments.clone(),
                        instantiation_span: *instantiation_span,
                    },
                ));

                return Ok(None); // can't continue
            }

            Err(error) => {
                return Err(error.report_as_type_calculating_overflow(
                    *instantiation_span,
                    handler,
                ));
            }
        };

        // check if the deduced generic arguments are correct
        let mut wf_lifetime_constraints = self
            .wf_check_instantiation(
                impl_id,
                instantiation_span,
                &result.result.instantiation,
                do_outlives_check,
                handler,
            )
            .await?;

        // the implementation is not general enough
        if result.result.is_not_general_enough {
            handler.receive(Diagnostic::AdtImplementationIsNotGeneralEnough(
                AdtImplementationIsNotGeneralEnough {
                    adt_implementation_id: impl_id,
                    generic_arguments: generic_arguments.clone(),
                    instantiation_span: *instantiation_span,
                },
            ));
        }

        if do_outlives_check {
            for constraint in result.constraints {
                match constraint.satisfies(self).await {
                    Ok(true) => {}

                    Ok(false) => {
                        handler.receive(Diagnostic::UnsatisfiedPredicate(
                            UnsatisfiedPredicate {
                                predicate: constraint.into_predicate(),
                                instantiation_span: *instantiation_span,
                                predicate_declaration_span: None,
                            },
                        ));
                    }

                    Err(e) => {
                        e.report_as_undecidable_predicate(
                            constraint.into_predicate(),
                            None,
                            *instantiation_span,
                            handler,
                        );

                        return Err(UnrecoverableError::Reported);
                    }
                }
            }
        } else {
            wf_lifetime_constraints.extend(result.constraints);
        }

        Ok(Some(ImplementsCheckResult {
            deduction: result.result,
            constraints: wf_lifetime_constraints,
        }))
    }

    /// Checks the where clause predicate requirements declared in the given
    /// `generic_id`
    ///
    /// This doesn't include the additional requirements such as checking trait
    /// predicate satisfiabiltiy if the `generic_id` is trait.
    ///
    /// The `do_outlives_check` flag indicates whether outlives predicates
    /// should be checked using symbolic evaluation or just assumed to be
    /// satisfied. If false, then the outlives predicates will be returned
    /// as lifetime constraints in the Ok result.
    pub async fn wf_check_instantiation(
        &self,
        generic_id: Global<pernixc_symbol::ID>,
        instantiation_span: &RelativeSpan,
        instantiation: &instantiation::Instantiation,
        do_outlives_check: bool,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<BTreeSet<LifetimeConstraint>, UnrecoverableError> {
        let predicates = Self::get_all_predicates(
            self.tracked_engine(),
            generic_id,
            Some(instantiation),
        )
        .await;

        let mut lifetime_constraints = BTreeSet::new();
        let mut diagnostics = Vec::new();

        for (predicate, span) in predicates {
            let new_lifetime_constraints = self
                .predicate_satisfied_internal(
                    predicate,
                    instantiation_span,
                    span.as_ref(),
                    do_outlives_check,
                    &mut diagnostics,
                    handler,
                )
                .await?;

            lifetime_constraints.extend(new_lifetime_constraints);
        }

        handler.receieve_many(diagnostics);

        Ok(lifetime_constraints)
    }
}
