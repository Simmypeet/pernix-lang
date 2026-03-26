//! Contains the well-formedness check implementation.

use std::ops::Deref;

use pernixc_extend::extend;
use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    implements_arguments::get_implements_argument,
    implied_predicate::get_implied_predicates, trait_ref::get_trait_ref,
    variance::Variance, where_clause::get_where_clause,
};
use pernixc_symbol::kind::get_kind;
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::get_generic_parameters,
    instance::{Instance, TraitRef},
    instantiation::{self, Instantiation},
    predicate::{Outlives, Predicate},
    r#type::Type,
    visitor::RecursiveIterator,
};

use crate::{
    UnrecoverableError,
    constraints::Constraints,
    deduction::Deduction,
    diagnostic::{
        AdtImplementationIsNotGeneralEnough, Diagnostic,
        FoundNegativeImplementation, ImplementationIsNotGeneralEnough,
        MismatchedImplementationArguments, MismatchedTraitRef,
        PredicateSatisfiabilityOverflow, RequiredBy, RequiredByImplements,
        UnsatisfiedPredicate,
    },
    environment::Environment,
    lifetime_constraint::LifetimeConstraint,
    normalizer::Normalizer,
    predicate::marker::PositiveError,
    resolution::{UnsatisfiedCause, UnsatisfiedPredicates},
};

/// The result of [`Environment::wf_check_implementation`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplementsCheckResult {
    deduction: Deduction,
    constraints: Constraints,
}

impl ImplementsCheckResult {
    /// Retrieves the instantiation retrieved from the deduction.
    #[must_use]
    pub fn into_instantiation(self) -> instantiation::Instantiation {
        self.deduction.instantiation
    }
}

/// An obligation for well-formedness checking.
///
/// Contains the symbol ID to check and the instances that need to be checked
/// via `check_instantiated_instance_arguments`.
#[derive(Debug, Clone)]
pub struct WfCheckObligation<'a> {
    /// The symbol ID to check.
    symbol_id: Global<pernixc_symbol::ID>,
    /// The instances to check via `check_instantiated_instance_arguments`.
    instances: &'a [Instance],
}

impl<'a> WfCheckObligation<'a> {
    /// Creates a new `WfCheckObligation`.
    #[must_use]
    pub const fn new(
        symbol_id: Global<pernixc_symbol::ID>,
        instances: &'a [Instance],
    ) -> Self {
        Self { symbol_id, instances }
    }

    /// Returns the symbol ID to check.
    #[must_use]
    pub const fn symbol_id(&self) -> Global<pernixc_symbol::ID> {
        self.symbol_id
    }

    /// Returns the instances to check.
    #[must_use]
    pub const fn instances(&self) -> &'a [Instance] { self.instances }
}

/// A trait for types that can create well-formedness check obligations.
pub trait CreateWfCheckObligation {
    /// Creates a well-formedness check obligation for this type.
    fn create_wf_check_obligation(&self) -> WfCheckObligation<'_>;
}

impl CreateWfCheckObligation for pernixc_term::generic_arguments::Symbol {
    fn create_wf_check_obligation(&self) -> WfCheckObligation<'_> {
        WfCheckObligation::new(self.id(), self.generic_arguments().instances())
    }
}

impl CreateWfCheckObligation for pernixc_term::instance::InstanceAssociated {
    fn create_wf_check_obligation(&self) -> WfCheckObligation<'_> {
        WfCheckObligation::new(
            self.trait_associated_symbol_id(),
            self.associated_instance_generic_arguments().instances(),
        )
    }
}

/// Extension method for creating a well-formedness check obligation for the
/// member of an [`AssociatedSymbol`].
#[extend]
pub fn create_wf_check_obligation_for_member(
    self: &pernixc_term::generic_arguments::AssociatedSymbol,
) -> WfCheckObligation<'_> {
    WfCheckObligation::new(
        self.id(),
        self.member_generic_arguments().instances(),
    )
}

/// Extension method for creating a well-formedness check obligation for the
/// parent of an [`AssociatedSymbol`].
#[extend]
pub async fn create_wf_check_obligation_for_parent<'a>(
    self: &'a pernixc_term::generic_arguments::AssociatedSymbol,
    engine: &TrackedEngine,
) -> WfCheckObligation<'a> {
    use pernixc_symbol::parent::get_parent_global;
    let parent_id = engine.get_parent_global(self.id()).await.unwrap();
    WfCheckObligation::new(
        parent_id,
        self.parent_generic_arguments().instances(),
    )
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
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Constraints, UnrecoverableError> {
        let mut diagnostics = Vec::new();

        let result = self
            .predicate_satisfied_internal(
                predicate,
                instantiation_span,
                predicate_declaration_span,
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
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<
        (Vec<Diagnostic>, Constraints),
        UnrecoverableError,
    > {
        let mut diagnostics = Vec::new();

        let constraints = self
            .predicate_satisfied_internal(
                predicate,
                instantiation_span,
                predicate_declaration_span,
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
        diagnostics: &mut Vec<Diagnostic>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Constraints, UnrecoverableError> {
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
                            Ok(Some(result.constraints.clone()))
                        } else {
                            Ok(None)
                        }
                    }

                    Ok(None) => Ok(None),

                    Err(error) => Err(error),
                }
            }

            Predicate::ConstantType(constant_type) => self
                .query(constant_type)
                .await
                .map(|x| x.map(|x| x.constraints.clone())),

            Predicate::LifetimeOutlives(outlives) => {
                if self.do_outlives_check() {
                    match self.query(outlives).await {
                        Ok(true) => return Ok(Constraints::new()),

                        Ok(false) => {
                            diagnostics.push(Diagnostic::UnsatisfiedPredicate(
                                UnsatisfiedPredicate::builder()
                                    .predicate(predicate)
                                    .instantiation_span(*instantiation_span)
                                    .maybe_predicate_declaration_span(
                                        predicate_declaration_span.copied(),
                                    )
                                    .build(),
                            ));

                            return Ok(Constraints::new());
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

                Ok(Some(
                    std::iter::once(LifetimeConstraint::LifetimeOutlives(
                        outlives.clone(),
                    ))
                    .collect(),
                ))
            }

            Predicate::TypeOutlives(outlives) => {
                if self.do_outlives_check() {
                    match self.query(outlives).await {
                        Ok(true) => return Ok(Constraints::new()),

                        Ok(false) => {
                            diagnostics.push(Diagnostic::UnsatisfiedPredicate(
                                UnsatisfiedPredicate::builder()
                                    .predicate(predicate)
                                    .instantiation_span(*instantiation_span)
                                    .maybe_predicate_declaration_span(
                                        predicate_declaration_span.copied(),
                                    )
                                    .build(),
                            ));

                            return Ok(Constraints::new());
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

                Ok(Some(
                    RecursiveIterator::new(&outlives.bound)
                        .filter_map(|x| x.0.into_lifetime().ok())
                        .map(|x| {
                            LifetimeConstraint::LifetimeOutlives(Outlives::new(
                                x.clone(),
                                outlives.bound.clone(),
                            ))
                        })
                        .collect(),
                ))
            }

            Predicate::TupleType(tuple) => self
                .query(tuple)
                .await
                .map(|x| x.map(|x| x.constraints.clone())),

            Predicate::PositiveMarker(marker) => {
                match self.query(marker).await {
                    Ok(Ok(succeeded)) => {
                        Ok(Some(succeeded.constraints.clone()))
                    }

                    Ok(Err(error)) => {
                        self.generate_marker_error(
                            marker,
                            instantiation_span,
                            &error,
                            diagnostics,
                            vec![
                                RequiredBy::builder()
                                    .maybe_predicate_declaration_span(
                                        predicate_declaration_span.copied(),
                                    )
                                    .build(),
                            ],
                        );

                        return Ok(Constraints::new());
                    }

                    Err(overflow_error) => Err(overflow_error),
                }
            }

            Predicate::NegativeMarker(marker) => {
                match self.query(marker).await {
                    Ok(test) => Ok(test.map(|x| x.constraints.clone())),

                    Err(overflow_error) => Err(overflow_error),
                }
            }
        };

        match result {
            Ok(Some(constraints)) => {
                self.handle_satisfy(
                    constraints,
                    instantiation_span,
                    predicate_declaration_span,
                    diagnostics,
                    handler,
                )
                .await
            }

            Ok(None) => {
                diagnostics.push(Diagnostic::UnsatisfiedPredicate(
                    UnsatisfiedPredicate::builder()
                        .predicate(predicate)
                        .instantiation_span(*instantiation_span)
                        .maybe_predicate_declaration_span(
                            predicate_declaration_span.copied(),
                        )
                        .build(),
                ));

                Ok(Constraints::new())
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

                Ok(Constraints::new())
            }
        }
    }

    async fn handle_satisfy(
        &self,
        constraints: Constraints,
        instantiation_span: &RelativeSpan,
        predicate_declaration_span: Option<&RelativeSpan>,
        diagnostics: &mut Vec<Diagnostic>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Constraints, UnrecoverableError> {
        // if do_outlives_check is false, then we don't need to check
        if !self.do_outlives_check() {
            return Ok(constraints);
        }

        for constraint in constraints {
            match constraint.satisfies(self).await {
                Ok(true) => {}

                Ok(false) => {
                    diagnostics.push(Diagnostic::UnsatisfiedPredicate(
                        UnsatisfiedPredicate::builder()
                            .predicate(constraint.into_predicate())
                            .instantiation_span(*instantiation_span)
                            .maybe_predicate_declaration_span(
                                predicate_declaration_span.copied(),
                            )
                            .build(),
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

        Ok(Constraints::new())
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

        if self.do_outlives_check() {
            for constraint in result.constraints {
                match constraint.satisfies(self).await {
                    Ok(true) => {}

                    Ok(false) => {
                        handler.receive(Diagnostic::UnsatisfiedPredicate(
                            UnsatisfiedPredicate::builder()
                                .predicate(constraint.into_predicate())
                                .instantiation_span(*instantiation_span)
                                .build(),
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

    async fn check_instance_trait_ref_internal(
        &self,
        instance: &Instance,
        instance_trait_ref: &TraitRef,
        expected_trait_ref: &TraitRef,
        instantiation_span: &RelativeSpan,
        lifetime_constraints: &mut Constraints,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        if instance_trait_ref.trait_id() != expected_trait_ref.trait_id() {
            handler.receive(Diagnostic::MismatchedTraitRef(
                MismatchedTraitRef::builder()
                    .expected_trait_ref(expected_trait_ref.clone())
                    .found_trait_ref(instance_trait_ref.clone())
                    .instance(instance.clone())
                    .span(*instantiation_span)
                    .build(),
            ));

            return Ok(());
        }

        let ok = match self
            .subtypes_generic_arguments(
                expected_trait_ref.generic_arguments(),
                instance_trait_ref.generic_arguments(),
            )
            .await
        {
            Ok(Some(res)) => {
                if res.result.forall_lifetime_errors.is_empty() {
                    if self.do_outlives_check() {
                        self.check_lifetime_constraints(
                            res.constraints.iter(),
                            instantiation_span,
                            handler,
                        )
                        .await;
                    } else {
                        lifetime_constraints
                            .extend(res.constraints.iter().cloned());
                    }

                    true
                } else {
                    false
                }
            }

            Ok(None) => false,

            Err(overflow_error) => {
                return Err(overflow_error.report_as_type_check_overflow(
                    *instantiation_span,
                    handler,
                ));
            }
        };

        if !ok {
            handler.receive(Diagnostic::MismatchedTraitRef(
                MismatchedTraitRef::builder()
                    .expected_trait_ref(expected_trait_ref.clone())
                    .found_trait_ref(instance_trait_ref.clone())
                    .span(*instantiation_span)
                    .instance(instance.clone())
                    .build(),
            ));
        }

        Ok(())
    }

    /// Checks if the given `instance`'s trait-ref matches the
    /// `expected_trait_ref` and returns the lifetime constraints that need to
    /// be satisfied for the instance to be well-formed.
    pub async fn check_instance_trait_ref(
        &self,
        instance: &Instance,
        expected_trait_ref: &TraitRef,
        instantiation_span: &RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Constraints, UnrecoverableError> {
        let Some(instance_trait_ref) =
            instance.get_trait_ref(self.tracked_engine()).await
        else {
            return Ok(Constraints::new()); // can't continue
        };

        let mut lifetime_constraints = Constraints::new();
        self.check_instance_trait_ref_internal(
            instance,
            &instance_trait_ref,
            expected_trait_ref,
            instantiation_span,
            &mut lifetime_constraints,
            handler,
        )
        .await?;

        Ok(lifetime_constraints)
    }

    /// Type-checks the trait-ref of the instance arguments supplied to the
    /// generic with the given `generic_id`.
    pub async fn check_instantiated_instance_arguments(
        &self,
        generic_id: Global<pernixc_symbol::ID>,
        instance_arguments: &[Instance],
        instantiation_span: &RelativeSpan,
        instantiation: &Instantiation,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Constraints, UnrecoverableError> {
        let generic_parameters =
            self.tracked_engine().get_generic_parameters(generic_id).await;

        let mut lifetime_constraints = Constraints::new();

        for ((_, instance_parameter), instance_arg) in generic_parameters
            .instance_parameters_as_order()
            .zip(instance_arguments)
        {
            let Some(instance_trait_ref) =
                instance_arg.get_trait_ref(self.tracked_engine()).await
            else {
                continue;
            };

            let Some(mut expected_trait_ref) =
                instance_parameter.trait_ref().map(|x| x.deref().clone())
            else {
                continue;
            };

            expected_trait_ref.instantiate(instantiation);

            self.check_instance_trait_ref_internal(
                instance_arg,
                &instance_trait_ref,
                &expected_trait_ref,
                instantiation_span,
                &mut lifetime_constraints,
                handler,
            )
            .await?;
        }

        Ok(lifetime_constraints)
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
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Constraints, UnrecoverableError> {
        let predicates = Self::get_all_predicates(
            self.tracked_engine(),
            generic_id,
            Some(instantiation),
        )
        .await;

        let mut lifetime_constraints = Constraints::new();
        let mut diagnostics = Vec::new();

        for (predicate, span) in predicates {
            let new_lifetime_constraints = self
                .predicate_satisfied_internal(
                    predicate,
                    instantiation_span,
                    span.as_ref(),
                    &mut diagnostics,
                    handler,
                )
                .await?;

            lifetime_constraints.extend(new_lifetime_constraints);
        }

        handler.receieve_many(diagnostics);

        Ok(lifetime_constraints)
    }

    /// Checks both the well-formedness of an instantiation and the instance
    /// arguments.
    ///
    /// This is a convenience method that combines
    /// [`wf_check_instantiation`](Self::wf_check_instantiation) and
    /// [`check_instantiated_instance_arguments`](Self::check_instantiated_instance_arguments).
    pub async fn wf_check_obligation(
        &self,
        obligation: &WfCheckObligation<'_>,
        instantiation_span: &RelativeSpan,
        instantiation: &instantiation::Instantiation,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Constraints, UnrecoverableError> {
        let mut lifetime_constraints = self
            .wf_check_instantiation(
                obligation.symbol_id,
                instantiation_span,
                instantiation,
                handler,
            )
            .await?;

        let instance_constraints = self
            .check_instantiated_instance_arguments(
                obligation.symbol_id,
                obligation.instances,
                instantiation_span,
                instantiation,
                handler,
            )
            .await?;

        lifetime_constraints.extend(instance_constraints);

        Ok(lifetime_constraints)
    }

    fn handle_unsatisfied_predicate_in_marker(
        &self,
        predicate: &pernixc_term::predicate::PositiveMarker,
        instantiation_span: &RelativeSpan,
        unsatisfied_predicates: &UnsatisfiedPredicates,
        diagnostics: &mut Vec<Diagnostic>,
        requirement_stack: &[RequiredBy],
    ) {
        for unsatisfied_predicate in
            unsatisfied_predicates.unsatisfied_predicates()
        {
            match unsatisfied_predicate.cause() {
                UnsatisfiedCause::NoInformation => {
                    let mut requirement_stack = requirement_stack.to_vec();

                    requirement_stack.push(
                        RequiredBy::builder()
                            .by_implements(
                                RequiredByImplements::builder()
                                    .resolved_implements_id(
                                        unsatisfied_predicates
                                            .implementation()
                                            .id,
                                    )
                                    .predicate(predicate.clone())
                                    .build(),
                            )
                            .maybe_predicate_declaration_span(
                                unsatisfied_predicate
                                    .predicate_declaration_span()
                                    .copied(),
                            )
                            .build(),
                    );

                    diagnostics.push(
                        UnsatisfiedPredicate::builder_with_required_by_stack()
                            .predicate(
                                unsatisfied_predicate.predicate().clone(),
                            )
                            .instantiation_span(*instantiation_span)
                            .requirement_stack(requirement_stack)
                            .build()
                            .into(),
                    );
                }
                UnsatisfiedCause::PositiveMarker(positive_error) => {
                    let mut requirement_stack = requirement_stack.to_vec();

                    requirement_stack.push(
                        RequiredBy::builder()
                            .by_implements(
                                RequiredByImplements::builder()
                                    .resolved_implements_id(
                                        unsatisfied_predicates
                                            .implementation()
                                            .id,
                                    )
                                    .predicate(predicate.clone())
                                    .build(),
                            )
                            .maybe_predicate_declaration_span(
                                unsatisfied_predicate
                                    .predicate_declaration_span()
                                    .copied(),
                            )
                            .build(),
                    );

                    self.generate_marker_error(
                        unsatisfied_predicate
                            .predicate()
                            .as_positive_marker()
                            .unwrap(),
                        instantiation_span,
                        positive_error,
                        diagnostics,
                        requirement_stack,
                    );
                }
            }
        }
    }

    fn handle_resolution_error_in_marker(
        &self,
        predicate: &pernixc_term::predicate::PositiveMarker,
        instantiation_span: &RelativeSpan,
        error: &crate::resolution::Error,
        diagnostics: &mut Vec<Diagnostic>,
        requirement_stack: Vec<RequiredBy>,
    ) {
        match error {
            crate::resolution::Error::IsNotGeneralEnough(implementation) => {
                diagnostics.push(Diagnostic::ImplementationIsNotGeneralEnough(
                    ImplementationIsNotGeneralEnough::builder()
                        .resolvable_implementation_id(implementation.id)
                        .generic_arguments(
                            predicate.generic_arguments().clone(),
                        )
                        .instantiation_span(*instantiation_span)
                        .required_by_stack(requirement_stack)
                        .build(),
                ));
            }

            crate::resolution::Error::UnsatisfiedPredicates(
                unsatisfied_predicates,
            ) => self.handle_unsatisfied_predicate_in_marker(
                predicate,
                instantiation_span,
                unsatisfied_predicates,
                diagnostics,
                &requirement_stack,
            ),

            crate::resolution::Error::Ambiguous => {
                // NOTE: wf_check isn't responsible for reporting ambiguous
                // marker implements. It should've been reported by the
                // `implements` overlapping check.
            }

            crate::resolution::Error::NotFound
            | crate::resolution::Error::Cyclic => {
                diagnostics.push(Diagnostic::UnsatisfiedPredicate(
                    UnsatisfiedPredicate::builder_with_required_by_stack()
                        .predicate(predicate.clone().into())
                        .instantiation_span(*instantiation_span)
                        .requirement_stack(requirement_stack)
                        .build(),
                ));
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn generate_marker_error(
        &self,
        predicate: &pernixc_term::predicate::PositiveMarker,
        instantiation_span: &RelativeSpan,
        error: &PositiveError,
        diagnostics: &mut Vec<Diagnostic>,
        requirement_stack: Vec<RequiredBy>,
    ) {
        match error {
            PositiveError::ImplementationResolution(error) => {
                self.handle_resolution_error_in_marker(
                    predicate,
                    instantiation_span,
                    error,
                    diagnostics,
                    requirement_stack,
                );
            }

            PositiveError::Structural(structural_errors) => {
                for a in structural_errors.iter() {
                    self.generate_marker_error(
                        a.sub_predicate(),
                        instantiation_span,
                        a.error(),
                        diagnostics,
                        requirement_stack.clone(),
                    );
                }
            }

            PositiveError::NegativeMarkerImplementation(succeeded) => {
                diagnostics.push(Diagnostic::FoundNegativeImplementation(
                    FoundNegativeImplementation::builder()
                        .predicate(predicate.clone())
                        .instantiation_span(*instantiation_span)
                        .requirement_stack(requirement_stack)
                        .negative_implementation_id(succeeded.result.id)
                        .build(),
                ));
            }

            PositiveError::Cyclic => {
                diagnostics.push(Diagnostic::UnsatisfiedPredicate(
                    UnsatisfiedPredicate::builder_with_required_by_stack()
                        .predicate(predicate.clone().into())
                        .instantiation_span(*instantiation_span)
                        .requirement_stack(requirement_stack)
                        .build(),
                ));
            }
        }
    }
}
