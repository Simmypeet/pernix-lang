//! Contains the well-formedness check implementation.

use std::collections::BTreeSet;

use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_semantic_element::{
    implied_predicate::get_implied_predicates, variance::Variance,
    where_clause::get_where_clause,
};
use pernixc_symbol::kind::get_kind;
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    instantiation::{self, Instantiation},
    lifetime::Lifetime,
    predicate::{Outlives, Predicate},
    r#type::Type,
    visitor::RecursiveIterator,
};

use crate::{
    diagnostic::{
        Diagnostic, ImplementationIsNotGeneralEnough,
        PredicateSatisfiabilityOverflow, UnsatisfiedPredicate,
    },
    environment::Environment,
    lifetime_constraint::LifetimeConstraint,
    normalizer::Normalizer,
    predicate::{marker, r#trait},
    Succeeded,
};

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
        instantiation_span: RelativeSpan,
        predicate_declaration_span: Option<RelativeSpan>,
        do_outlives_check: bool,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<BTreeSet<LifetimeConstraint>, CyclicError> {
        let result = match &predicate {
            Predicate::TraitTypeCompatible(equality) => {
                let result = self
                    .subtypes(
                        Type::TraitMember(equality.lhs.clone()),
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
                        Ok(Some(_)) => return Ok(BTreeSet::new()),

                        Ok(None) => {
                            handler.receive(Diagnostic::UnsatisfiedPredicate(
                                UnsatisfiedPredicate {
                                    predicate,
                                    instantiation_span,
                                    predicate_declaration_span,
                                },
                            ));

                            return Ok(BTreeSet::new());
                        }
                        Err(crate::Error::Overflow(overflow_error)) => {
                            handler.receive(
                                Diagnostic::PredicateSatisfiabilityOverflow(
                                    PredicateSatisfiabilityOverflow {
                                        predicate,
                                        predicate_declaration_span,
                                        instantiation_span,
                                        overflow_error,
                                    },
                                ),
                            );

                            return Ok(BTreeSet::new());
                        }

                        Err(crate::Error::CyclicDependency(error)) => {
                            return Err(error)
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
                        Ok(Some(_)) => return Ok(BTreeSet::new()),

                        Ok(None) => {
                            handler.receive(Diagnostic::UnsatisfiedPredicate(
                                UnsatisfiedPredicate {
                                    predicate,
                                    instantiation_span,
                                    predicate_declaration_span,
                                },
                            ));

                            return Ok(BTreeSet::new());
                        }
                        Err(crate::Error::Overflow(overflow_error)) => {
                            handler.receive(
                                Diagnostic::PredicateSatisfiabilityOverflow(
                                    PredicateSatisfiabilityOverflow {
                                        predicate,
                                        predicate_declaration_span,
                                        instantiation_span,
                                        overflow_error,
                                    },
                                ),
                            );

                            return Ok(BTreeSet::new());
                        }

                        Err(crate::Error::CyclicDependency(error)) => {
                            return Err(error)
                        }
                    }
                }

                Ok(Some(Succeeded::satisfied_with(
                    RecursiveIterator::new(&outlives.bound)
                        .filter_map(|x| x.0.into_lifetime().ok())
                        .map(|x| {
                            LifetimeConstraint::LifetimeOutlives(Outlives::new(
                                *x,
                                outlives.bound,
                            ))
                        })
                        .collect(),
                )))
            }

            Predicate::TupleType(tuple) => {
                self.query(tuple).await.map(|x| x.as_deref().cloned())
            }

            Predicate::PositiveTrait(positive) => {
                match self.query(positive).await {
                    Ok(None) => Ok(None),
                    Ok(Some(result)) => match &result.result {
                        r#trait::PositiveSatisfied::Premise
                        | r#trait::PositiveSatisfied::Environment
                        | r#trait::PositiveSatisfied::Cyclic => {
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            )))
                        }

                        r#trait::PositiveSatisfied::Implementation(
                            implementation,
                        ) => {
                            let mut lt_constraints =
                                Box::pin(self.check_implementation_satisfied(
                                    implementation.id,
                                    &implementation.instantiation,
                                    &positive.generic_arguments,
                                    instantiation_span,
                                    predicate_declaration_span,
                                    do_outlives_check,
                                    implementation.is_not_general_enough,
                                    handler,
                                ))
                                .await?;

                            lt_constraints
                                .extend(result.constraints.iter().cloned());

                            Ok(Some(Succeeded::satisfied_with(lt_constraints)))
                        }
                    },
                    Err(error) => Err(error),
                }
            }

            Predicate::NegativeTrait(negative) => match self
                .query(negative)
                .await
            {
                Ok(None) => Ok(None),

                Ok(Some(result)) => match &result.result {
                    r#trait::NegativeSatisfied::UnsatisfiedPositive
                    | r#trait::NegativeSatisfied::Premise => Ok(Some(
                        Succeeded::satisfied_with(result.constraints.clone()),
                    )),
                    r#trait::NegativeSatisfied::Implementation(
                        implementation,
                    ) => {
                        let mut lt_constraints =
                            Box::pin(self.check_implementation_satisfied(
                                implementation.id,
                                &implementation.instantiation,
                                &negative.generic_arguments,
                                instantiation_span,
                                predicate_declaration_span,
                                do_outlives_check,
                                implementation.is_not_general_enough,
                                handler,
                            ))
                            .await?;
                        lt_constraints
                            .extend(result.constraints.iter().cloned());

                        Ok(Some(Succeeded::satisfied_with(lt_constraints)))
                    }
                },

                Err(overflow) => Err(overflow),
            },

            Predicate::PositiveMarker(positive) => {
                match self.query(positive).await {
                    Ok(None) => Ok(None),

                    Ok(Some(result)) => {
                        let mut new_constraints =
                            Box::pin(self.handle_positive_marker_satisfied(
                                &result.result,
                                &positive.generic_arguments,
                                instantiation_span,
                                predicate_declaration_span,
                                do_outlives_check,
                                handler,
                            ))
                            .await?;

                        new_constraints
                            .extend(result.constraints.iter().cloned());

                        Ok(Some(Succeeded::satisfied_with(new_constraints)))
                    }

                    Err(error) => Err(error),
                }
            }

            Predicate::NegativeMarker(negative) => match self
                .query(negative)
                .await
            {
                Ok(Some(result)) => match &result.result {
                    marker::NegativeSatisfied::UnsatisfiedPositive
                    | marker::NegativeSatisfied::Premise => Ok(Some(
                        Succeeded::satisfied_with(result.constraints.clone()),
                    )),

                    marker::NegativeSatisfied::Implementation(
                        implementation,
                    ) => {
                        let mut lt_constraints =
                            Box::pin(self.check_implementation_satisfied(
                                implementation.id,
                                &implementation.instantiation,
                                &negative.generic_arguments,
                                instantiation_span,
                                predicate_declaration_span,
                                do_outlives_check,
                                implementation.is_not_general_enough,
                                handler,
                            ))
                            .await?;

                        lt_constraints
                            .extend(result.constraints.iter().cloned());

                        Ok(Some(Succeeded::satisfied_with(lt_constraints)))
                    }
                },
                Ok(None) => Ok(None),
                Err(error) => Err(error),
            },
        };

        match result {
            Ok(Some(Succeeded { constraints, .. })) => {
                self.handle_satisfy(
                    constraints,
                    instantiation_span,
                    predicate_declaration_span,
                    do_outlives_check,
                    handler,
                )
                .await
            }

            Ok(None) => {
                handler.receive(Diagnostic::UnsatisfiedPredicate(
                    UnsatisfiedPredicate {
                        predicate,
                        instantiation_span,
                        predicate_declaration_span,
                    },
                ));

                Ok(BTreeSet::new())
            }

            Err(crate::Error::Overflow(overflow_error)) => {
                handler.receive(Diagnostic::PredicateSatisfiabilityOverflow(
                    PredicateSatisfiabilityOverflow {
                        predicate,
                        predicate_declaration_span,
                        instantiation_span,
                        overflow_error,
                    },
                ));

                Ok(BTreeSet::new())
            }

            Err(crate::Error::CyclicDependency(error)) => Err(error),
        }
    }

    async fn handle_satisfy(
        &self,
        constraints: BTreeSet<LifetimeConstraint>,
        instantiation_span: RelativeSpan,
        predicate_declaration_span: Option<RelativeSpan>,
        do_outlives_check: bool,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<BTreeSet<LifetimeConstraint>, CyclicError> {
        // if do_outlives_check is false, then we don't need to check
        if !do_outlives_check {
            return Ok(constraints);
        }

        for constraint in constraints {
            match constraint {
                LifetimeConstraint::LifetimeOutlives(pred) => {
                    match self.query(&pred).await {
                        Ok(None) => {
                            handler.receive(Diagnostic::UnsatisfiedPredicate(
                                UnsatisfiedPredicate {
                                    predicate: Predicate::LifetimeOutlives(
                                        pred,
                                    ),
                                    instantiation_span,
                                    predicate_declaration_span,
                                },
                            ));
                        }

                        Err(crate::Error::CyclicDependency(error)) => {
                            return Err(error);
                        }

                        Err(crate::Error::Overflow(overflow_error)) => {
                            handler.receive(
                                Diagnostic::PredicateSatisfiabilityOverflow(
                                    PredicateSatisfiabilityOverflow {
                                        predicate: Predicate::LifetimeOutlives(
                                            pred,
                                        ),
                                        instantiation_span,
                                        predicate_declaration_span,
                                        overflow_error,
                                    },
                                ),
                            );
                        }

                        Ok(Some(_)) => {}
                    }
                }
            }
        }

        Ok(BTreeSet::new())
    }

    #[allow(clippy::type_complexity)]
    async fn handle_positive_marker_satisfied(
        &self,
        result: &marker::PositiveSatisfied,
        pred_generic_arguments: &GenericArguments,
        instantiation_span: RelativeSpan,
        predicate_declaration_span: Option<RelativeSpan>,
        do_outlives_check: bool,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<BTreeSet<LifetimeConstraint>, CyclicError> {
        match result {
            marker::PositiveSatisfied::Premise
            | marker::PositiveSatisfied::Environment
            | marker::PositiveSatisfied::Cyclic => Ok(BTreeSet::new()),

            marker::PositiveSatisfied::Implementation(implementation) => {
                self.check_implementation_satisfied(
                    implementation.id,
                    &implementation.instantiation,
                    pred_generic_arguments,
                    instantiation_span,
                    predicate_declaration_span,
                    do_outlives_check,
                    implementation.is_not_general_enough,
                    handler,
                )
                .await
            }

            marker::PositiveSatisfied::Congruence(btree_map) => {
                let mut constraints = BTreeSet::new();

                for result in btree_map.values() {
                    constraints.extend(result.constraints.iter().cloned());

                    let new_constraints =
                        Box::pin(self.handle_positive_marker_satisfied(
                            &result.result,
                            pred_generic_arguments,
                            instantiation_span,
                            predicate_declaration_span,
                            do_outlives_check,
                            handler,
                        ))
                        .await?;

                    constraints.extend(new_constraints);
                }

                Ok(constraints)
            }
        }
    }

    #[allow(clippy::type_complexity)]
    async fn get_all_predicates(
        table: &TrackedEngine,
        global_id: Global<pernixc_symbol::ID>,
        instantiation: Option<&Instantiation>,
    ) -> Result<Vec<(Predicate, Option<RelativeSpan>)>, CyclicError> {
        let symbol_kind = table.get_kind(global_id).await;
        let mut predicates = Vec::new();

        if symbol_kind.has_where_clause() {
            let where_cluase = table.get_where_clause(global_id).await?;

            for predicate in where_cluase.iter() {
                predicates.push((predicate.predicate.clone(), predicate.span));
            }
        }

        if symbol_kind.has_implied_predicates() {
            let implied_predicates =
                table.get_implied_predicates(global_id).await?;

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

        Ok(predicates)
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
    pub async fn wf_check(
        &self,
        generic_id: Global<pernixc_symbol::ID>,
        instantiation_span: RelativeSpan,
        instantiation: &instantiation::Instantiation,
        do_outlives_check: bool,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<BTreeSet<LifetimeConstraint>, CyclicError> {
        let predicates = Self::get_all_predicates(
            self.tracked_engine(),
            generic_id,
            Some(instantiation),
        )
        .await?;

        let mut lifetime_constraints = BTreeSet::new();

        for (predicate, span) in predicates {
            let new_lifetime_constraints = self
                .predicate_satisfied(
                    predicate,
                    instantiation_span,
                    span,
                    do_outlives_check,
                    handler,
                )
                .await?;

            lifetime_constraints.extend(new_lifetime_constraints);
        }

        Ok(lifetime_constraints)
    }

    #[allow(clippy::too_many_arguments, clippy::type_complexity)]
    async fn check_implementation_satisfied(
        &self,
        id: Global<pernixc_symbol::ID>,
        instantiation: &Instantiation,
        generic_arguments: &GenericArguments,
        instantiation_span: RelativeSpan,
        predicate_declaration_span: Option<RelativeSpan>,
        do_outlives_check: bool,
        mut is_not_general_enough: bool,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<BTreeSet<LifetimeConstraint>, CyclicError> {
        let mut lifetime_constraints = BTreeSet::new();

        // check for each predicate in the implementation
        for (mut predicate, span) in Self::get_all_predicates(
            self.tracked_engine(),
            id,
            Some(instantiation),
        )
        .await?
        {
            match &predicate {
                Predicate::LifetimeOutlives(outlives) => {
                    if outlives.operand.is_forall() {
                        is_not_general_enough = true;
                        continue;
                    }
                }
                Predicate::TypeOutlives(outlives) => {
                    if RecursiveIterator::new(&outlives.operand).any(|x| {
                        x.0.as_lifetime().is_some_and(|x| x.is_forall())
                    }) {
                        is_not_general_enough = true;
                        continue;
                    }
                }
                _ => {}
            }

            match &mut predicate {
                Predicate::LifetimeOutlives(outlives)
                    if outlives.bound.is_forall() =>
                {
                    outlives.bound = Lifetime::Static;
                }

                Predicate::TypeOutlives(outlives)
                    if outlives.bound.is_forall() =>
                {
                    outlives.bound = Lifetime::Static;
                }

                _ => {}
            }

            let new_lifetime_constraints = self
                .predicate_satisfied(
                    predicate,
                    instantiation_span,
                    span,
                    do_outlives_check,
                    handler,
                )
                .await?;

            lifetime_constraints.extend(new_lifetime_constraints);
        }

        if is_not_general_enough {
            handler.receive(Diagnostic::ImplementationIsNotGeneralEnough(
                ImplementationIsNotGeneralEnough {
                    resolvable_implementation_id: id,
                    generic_arguments: generic_arguments.clone(),
                    predicate_declaration_span,
                    instantiation_span,
                },
            ));
        }

        Ok(lifetime_constraints)
    }
}
