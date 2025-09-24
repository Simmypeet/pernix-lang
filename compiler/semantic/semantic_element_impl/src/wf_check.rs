//! Performs the well-formedness checking for every symbol
//! resolutions/occurrences in a symbol.

use std::{borrow::Cow, collections::BTreeSet, sync::Arc};

use diagnostic::Diagnostic;
use enum_as_inner::EnumAsInner;
use pernixc_handler::{Handler, Storage};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_resolution::qualified_identifier::Resolution;
use pernixc_semantic_element::{
    implements_arguments::get_implements_argument, variance::Variance,
    where_clause::get_where_clause,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{get_kind, Kind},
    parent::get_parent,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::get_generic_parameters,
    instantiation::Instantiation,
    lifetime::Lifetime,
    predicate::{self, Outlives, PositiveTrait, Predicate},
    r#type::Type,
    visitor::RecursiveIterator,
};
use pernixc_type_system::{
    deduction,
    diagnostic::{ImplementationIsNotGeneralEnough, UnsatisfiedPredicate},
    environment::{get_active_premise, Environment},
    lifetime_constraint::LifetimeConstraint,
    normalizer,
    predicate::{marker, r#trait},
    resolution, OverflowError, Succeeded,
};

use crate::{
    build, function_signature, implements_qualified_identifier,
    occurrences::Occurrences,
    wf_check::diagnostic::{
        AdtImplementationIsNotGeneralEnough, MismatchedImplementationArguments,
    },
};

pub mod diagnostic;

/// An enumeration of ways the predicate can be erroneous.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub(super) enum PredicateError {
    /// The predicate isn't satisfied.
    Unsatisfied {
        predicate: Predicate,
        predicate_declaration_span: Option<RelativeSpan>,
    },

    /// The type system can't determine if the predicate is satisfiable or not.
    Undecidable {
        predicate: Predicate,
        predicate_declaration_span: Option<RelativeSpan>,
        overflow_error: OverflowError,
    },

    /// The solved trait/marker implementation is not general enough for the
    /// forall lifetime requirements.
    ImplementationIsNotGeneralEnough {
        resolved_implementation: resolution::Implementation,
        generic_arguments: GenericArguments,
        predicate_declaration_span: Option<RelativeSpan>,
    },
}

impl PredicateError {
    pub(super) fn report(
        self,
        instantiation_span: RelativeSpan,
        handler: &Storage<Diagnostic>,
    ) {
        match self {
            Self::Unsatisfied { predicate, predicate_declaration_span } => {
                handler.receive(UnsatisfiedPredicate {
                    predicate,
                    instantiation_span,
                    predicate_declaration_span,
                });
            }

            Self::Undecidable {
                predicate,
                predicate_declaration_span,
                overflow_error,
            } => {
                if predicate.contains_error() {
                    return;
                }

                overflow_error.report_as_undecidable_predicate(
                    predicate,
                    predicate_declaration_span,
                    instantiation_span,
                    handler,
                );
            }

            Self::ImplementationIsNotGeneralEnough {
                resolved_implementation,
                predicate_declaration_span,
                generic_arguments,
            } => {
                if generic_arguments.contains_error() {
                    return;
                }

                handler.receive(ImplementationIsNotGeneralEnough {
                    resolvable_implementation_id: resolved_implementation.id,
                    instantiation_span,
                    predicate_declaration_span,
                    generic_arguments,
                });
            }
        }
    }
}

/// Checker for the well-formedness of the instantiations.
pub(super) struct Checker<'a> {
    environment: &'a Environment<'a, normalizer::NoOp>,
    handler: &'a Storage<Diagnostic>,
}

impl Checker<'_> {
    async fn create_instantiation_for_adt(
        &self,
        adt_implementation_id: Global<pernixc_symbol::ID>,
        parent_generic_arguments: &GenericArguments,
        resolution_span: &RelativeSpan,
    ) -> Result<Option<Instantiation>, executor::CyclicError> {
        // deduce the generic arguments
        let Some(arguments) = self
            .environment
            .tracked_engine()
            .get_implements_argument(adt_implementation_id)
            .await?
        else {
            return Ok(None);
        };

        let result = match self
            .environment
            .deduce(&arguments, parent_generic_arguments)
            .await
        {
            Ok(deduced) => deduced,

            Err(deduction::Error::MismatchedGenericArgumentCount(_)) => {
                unreachable!()
            }

            Err(deduction::Error::UnificationFailure(_)) => {
                self.handler.receive(MismatchedImplementationArguments {
                    adt_implementation_id,
                    found_generic_arguments: parent_generic_arguments.clone(),
                    instantiation_span: *resolution_span,
                });

                return Ok(None); // can't continue
            }

            Err(deduction::Error::CyclicDependency(error)) => {
                return Err(error);
            }

            Err(deduction::Error::Overflow(error)) => {
                error.report_as_type_calculating_overflow(
                    *resolution_span,
                    self.handler,
                );

                return Ok(None);
            }
        };

        self.environment
            .check_lifetime_constraints(
                result.constraints.iter(),
                resolution_span,
                self.handler,
            )
            .await?;

        // check if the deduced generic arguments are correct
        self.check_instantiation_predicates(
            adt_implementation_id,
            &result.result.instantiation,
            resolution_span,
        )
        .await?;

        // the implementation is not general enough
        if result.result.is_not_general_enough {
            self.handler.receive(AdtImplementationIsNotGeneralEnough {
                adt_implementation_id,
                generic_arguments: parent_generic_arguments.clone(),
                instantiation_span: *resolution_span,
            });
        }

        Ok(Some(result.result.instantiation))
    }

    async fn check_trait_instantiation(
        &self,
        trait_id: Global<pernixc_symbol::ID>,
        generic_arguments: GenericArguments,
        instantiation_span: &RelativeSpan,
    ) -> Result<(), executor::CyclicError> {
        for error in self
            .predicate_satisfied(
                Predicate::PositiveTrait(PositiveTrait {
                    trait_id,
                    is_const: false,
                    generic_arguments,
                }),
                None,
            )
            .await?
        {
            error.report(*instantiation_span, self.handler);
        }

        Ok(())
    }

    /// Checks if the given `resolution` is well-formed. The errors are reported
    /// to the `handler`.
    #[allow(clippy::too_many_lines, unused)]
    pub(super) async fn check_resolution_occurrence(
        &self,
        resolution: &Resolution,
        resolution_span: &RelativeSpan,
    ) -> Result<(), executor::CyclicError> {
        match resolution {
            Resolution::EffectOperation(_)
            | Resolution::Module(_)
            | Resolution::Variant(_) => Ok(()),

            Resolution::Generic(generic) => {
                self.check_instantiation_predicates_by_generic_arguments(
                    generic.id,
                    generic.generic_arguments.clone(),
                    resolution_span,
                )
                .await
            }

            Resolution::MemberGeneric(member_generic) => {
                // additional adt implementation check

                // the trait implementation doesn't need to be checked here
                // because it can never be referred directly in the source code
                let symbol_kind = self
                    .environment
                    .tracked_engine()
                    .get_kind(member_generic.id)
                    .await;

                let adt_implementation_check = match symbol_kind {
                    Kind::ImplementationConstant
                    | Kind::ImplementationFunction
                    | Kind::ImplementationType => {
                        let parent = member_generic.id.target_id.make_global(
                            self.environment
                                .tracked_engine()
                                .get_parent(member_generic.id)
                                .await
                                .unwrap(),
                        );

                        let parent_kind = self
                            .environment
                            .tracked_engine()
                            .get_kind(parent)
                            .await;

                        if parent_kind.is_adt() {
                            Some(parent)
                        } else {
                            None
                        }
                    }

                    Kind::TraitConstant
                    | Kind::TraitFunction
                    | Kind::TraitType => {
                        let trait_id = Global::new(
                            member_generic.id.target_id,
                            self.environment
                                .tracked_engine()
                                .get_parent(member_generic.id)
                                .await
                                .unwrap(),
                        );

                        self.check_trait_instantiation(
                            trait_id,
                            member_generic.parent_generic_arguments.clone(),
                            resolution_span,
                        )
                        .await;

                        None
                    }

                    _ => None,
                };

                // extract the instantiation for the parent
                let mut parent_instantiation =
                    if let Some(adt_implementation_id) =
                        adt_implementation_check
                    {
                        let Some(instantiation) = self
                            .create_instantiation_for_adt(
                                adt_implementation_id,
                                &member_generic.parent_generic_arguments,
                                resolution_span,
                            )
                            .await?
                        else {
                            return Ok(());
                        };

                        instantiation
                    } else {
                        let parent_id = Global::new(
                            member_generic.id.target_id,
                            self.environment
                                .tracked_engine()
                                .get_parent(member_generic.id)
                                .await
                                .unwrap(),
                        );

                        let parent_generic_params = self
                            .environment
                            .tracked_engine()
                            .get_generic_parameters(parent_id)
                            .await?;

                        Instantiation::from_generic_arguments(
                            member_generic.parent_generic_arguments.clone(),
                            parent_id,
                            &parent_generic_params,
                        )
                        .expect("should have no mismatched")
                    };

                let generic_parameters = self
                    .environment
                    .tracked_engine()
                    .get_generic_parameters(member_generic.id)
                    .await?;

                parent_instantiation
                    .append_from_generic_arguments(
                        member_generic.member_generic_arguments.clone(),
                        member_generic.id,
                        &generic_parameters,
                    )
                    .unwrap();

                self.check_instantiation_predicates(
                    member_generic.id,
                    &parent_instantiation,
                    resolution_span,
                )
                .await
            }
        }
    }

    /// Do where clause predicates check for the given instantiation. The errors
    /// are reported to the `handler`.
    pub(super) async fn check_instantiation_predicates_by_generic_arguments(
        &self,
        instantiated: Global<pernixc_symbol::ID>,
        generic_arguments: GenericArguments,
        instantiation_span: &RelativeSpan,
    ) -> Result<(), executor::CyclicError> {
        // convert the generic arguments to an instantiation and delegate the
        // check to the `check_instantiation_predicates` method
        let generic_parameters = self
            .environment
            .tracked_engine()
            .get_generic_parameters(instantiated)
            .await?;

        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments,
            instantiated,
            &generic_parameters,
        )
        .unwrap();

        self.check_instantiation_predicates(
            instantiated,
            &instantiation,
            instantiation_span,
        )
        .await
    }

    /// Do where clause predicates check for the given instantiation. The errors
    /// are reported to the `handler`.
    pub(super) async fn check_instantiation_predicates(
        &self,
        instantiated: Global<pernixc_symbol::ID>,
        instantiation: &Instantiation,
        instantiation_span: &RelativeSpan,
    ) -> Result<(), executor::CyclicError> {
        let where_clause = self
            .environment
            .tracked_engine()
            .get_where_clause(instantiated)
            .await?;

        #[allow(clippy::significant_drop_in_scrutinee)]
        for predicate_info in where_clause.iter() {
            let mut predicate = predicate_info.predicate.clone();

            predicate.instantiate(instantiation);

            let errors = self
                .predicate_satisfied(predicate, predicate_info.span)
                .await?;

            for error in errors {
                error.report(*instantiation_span, self.handler);
            }
        }

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    async fn check_implementation_satisfied(
        &self,
        implementation_id: Global<pernixc_symbol::ID>,
        instantiation: &Instantiation,
        generic_arguments: &GenericArguments,
        predicate_declaration_span: Option<RelativeSpan>,
        mut is_not_general_enough: bool,
    ) -> Result<Vec<PredicateError>, executor::CyclicError> {
        let mut errors = Vec::new();
        let where_clause = self
            .environment
            .tracked_engine()
            .get_where_clause(implementation_id)
            .await?;

        // check for each predicate in the implementation
        for predicate in where_clause.iter() {
            let mut predicate_instantiated = predicate.predicate.clone();

            predicate_instantiated.instantiate(instantiation);

            // if found an outlives predicate where the bound (rhs) is a forall
            // lifetime, then replace it with the static lifetime; since static
            // lifetime outlives all lifetimes. However, if found on the operand
            // (lhs), then report it as `non-general-enough`.

            // check for all lifetime on the operand
            match &predicate_instantiated {
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

            // check for all lifetime on the bound and replace it with static
            match &mut predicate_instantiated {
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

            errors.extend(
                self.predicate_satisfied(
                    predicate_instantiated,
                    predicate.span,
                )
                .await?,
            );
        }

        if is_not_general_enough {
            errors.push(PredicateError::ImplementationIsNotGeneralEnough {
                resolved_implementation: resolution::Implementation {
                    instantiation: instantiation.clone(),
                    id: implementation_id,
                    is_not_general_enough,
                },
                generic_arguments: generic_arguments.clone(),
                predicate_declaration_span,
            });
        }

        Ok(errors)
    }

    async fn handle_positive_marker_satisfied(
        &self,
        result: marker::PositiveSatisfied,
        pred_generic_arguments: &GenericArguments,
        predicate_declaration_span: Option<RelativeSpan>,
    ) -> Result<
        (BTreeSet<LifetimeConstraint>, Vec<PredicateError>),
        executor::CyclicError,
    > {
        match result {
            marker::PositiveSatisfied::Premise
            | marker::PositiveSatisfied::Environment
            | marker::PositiveSatisfied::Cyclic => {
                Ok((BTreeSet::new(), Vec::new()))
            }

            marker::PositiveSatisfied::Implementation(implementation) => Ok((
                BTreeSet::new(),
                self.check_implementation_satisfied(
                    implementation.id,
                    &implementation.instantiation,
                    pred_generic_arguments,
                    predicate_declaration_span,
                    implementation.is_not_general_enough,
                )
                .await?,
            )),

            marker::PositiveSatisfied::Congruence(btree_map) => {
                let mut constraints = BTreeSet::new();
                let mut pred_errors = Vec::new();

                for (_, result) in btree_map {
                    constraints.extend(result.constraints.iter().cloned());

                    let (new_constraints, new_pred_errors) =
                        Box::pin(self.handle_positive_marker_satisfied(
                            result.result.clone(),
                            pred_generic_arguments,
                            predicate_declaration_span,
                        ))
                        .await?;

                    constraints.extend(new_constraints);
                    pred_errors.extend(new_pred_errors);
                }

                Ok((constraints, pred_errors))
            }
        }
    }

    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    pub(super) async fn predicate_satisfied(
        &self,
        predicate: Predicate,
        predicate_declaration_span: Option<RelativeSpan>,
    ) -> Result<Vec<PredicateError>, executor::CyclicError> {
        let (result, mut extra_predicate_error) = match &predicate {
            Predicate::TraitTypeCompatible(eq) => {
                let result = self
                    .environment
                    .subtypes(
                        Type::TraitMember(eq.lhs.clone()),
                        eq.rhs.clone(),
                        Variance::Covariant,
                    )
                    .await;

                (
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
                    },
                    Vec::new(),
                )
            }

            Predicate::ConstantType(pred) => (
                self.environment
                    .query(pred)
                    .await
                    .map(|x| x.as_deref().cloned()),
                Vec::new(),
            ),

            Predicate::LifetimeOutlives(pred) => {
                return match self.environment.query(pred).await {
                    Ok(Some(_)) => Ok(vec![]),

                    Ok(None) => Ok(vec![PredicateError::Unsatisfied {
                        predicate,
                        predicate_declaration_span,
                    }]),

                    Err(pernixc_type_system::Error::CyclicDependency(err)) => {
                        Err(err)
                    }

                    Err(pernixc_type_system::Error::Overflow(
                        overflow_error,
                    )) => Ok(vec![PredicateError::Undecidable {
                        predicate,
                        predicate_declaration_span,
                        overflow_error,
                    }]),
                };
            }

            Predicate::TypeOutlives(pred) => {
                return match self.environment.query(pred).await {
                    Ok(Some(_)) => Ok(vec![]),

                    Ok(None) => Ok(vec![PredicateError::Unsatisfied {
                        predicate,
                        predicate_declaration_span,
                    }]),

                    Err(pernixc_type_system::Error::CyclicDependency(err)) => {
                        Err(err)
                    }

                    Err(pernixc_type_system::Error::Overflow(
                        overflow_error,
                    )) => Ok(vec![PredicateError::Undecidable {
                        predicate,
                        predicate_declaration_span,
                        overflow_error,
                    }]),
                };
            }

            Predicate::TupleType(pred) => (
                self.environment
                    .query(pred)
                    .await
                    .map(|x| x.as_deref().cloned()),
                Vec::new(),
            ),

            Predicate::PositiveMarker(pred) => {
                match self.environment.query(pred).await {
                    Ok(None) => (Ok(None), Vec::new()),

                    Ok(Some(result)) => {
                        let (new_constraints, pred_errors) =
                            Box::pin(self.handle_positive_marker_satisfied(
                                result.result.clone(),
                                &pred.generic_arguments,
                                predicate_declaration_span,
                            ))
                            .await?;

                        let mut constraints = result.constraints.clone();
                        constraints.extend(new_constraints);

                        (
                            Ok(Some(Succeeded::satisfied_with(constraints))),
                            pred_errors,
                        )
                    }

                    Err(error) => (Err(error), Vec::new()),
                }
            }

            Predicate::PositiveTrait(pred) => {
                match self.environment.query(pred).await {
                    Ok(None) => (Ok(None), Vec::new()),
                    Ok(Some(result)) => match result.result.clone() {
                        r#trait::PositiveSatisfied::Cyclic
                        | r#trait::PositiveSatisfied::Premise
                        | r#trait::PositiveSatisfied::Environment => (
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            ))),
                            Vec::new(),
                        ),

                        r#trait::PositiveSatisfied::Implementation(
                            implementation,
                        ) => (
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            ))),
                            Box::pin(self.check_implementation_satisfied(
                                implementation.id,
                                &implementation.instantiation,
                                &pred.generic_arguments,
                                predicate_declaration_span,
                                implementation.is_not_general_enough,
                            ))
                            .await?,
                        ),
                    },
                    Err(error) => (Err(error), Vec::new()),
                }
            }

            Predicate::NegativeTrait(pred) => {
                match self.environment.query(pred).await {
                    Ok(None) => (Ok(None), Vec::new()),

                    Ok(Some(result)) => match result.result.clone() {
                        r#trait::NegativeSatisfied::Premise
                        | r#trait::NegativeSatisfied::UnsatisfiedPositive => (
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            ))),
                            Vec::new(),
                        ),

                        r#trait::NegativeSatisfied::Implementation(
                            implementation,
                        ) => (
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            ))),
                            Box::pin(self.check_implementation_satisfied(
                                implementation.id,
                                &implementation.instantiation,
                                &pred.generic_arguments,
                                predicate_declaration_span,
                                implementation.is_not_general_enough,
                            ))
                            .await?,
                        ),
                    },

                    Err(error) => (Err(error), Vec::new()),
                }
            }
            Predicate::NegativeMarker(negative) => {
                match self.environment.query(negative).await {
                    Ok(Some(result)) => match result.result.clone() {
                        marker::NegativeSatisfied::UnsatisfiedPositive
                        | marker::NegativeSatisfied::Premise => (
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            ))),
                            Vec::new(),
                        ),

                        marker::NegativeSatisfied::Implementation(
                            implementation,
                        ) => (
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            ))),
                            Box::pin(self.check_implementation_satisfied(
                                implementation.id,
                                &implementation.instantiation,
                                &negative.generic_arguments,
                                predicate_declaration_span,
                                implementation.is_not_general_enough,
                            ))
                            .await?,
                        ),
                    },
                    Ok(None) => (Ok(None), Vec::new()),

                    Err(error) => (Err(error), Vec::new()),
                }
            }
        };

        match result {
            Ok(Some(Succeeded { constraints, .. })) => {
                // if do_outlives_check is false, then we don't need to check

                for constraint in constraints {
                    match constraint {
                        LifetimeConstraint::LifetimeOutlives(pred) => match self
                            .environment
                            .query(&pred)
                            .await
                        {
                            Ok(None) => {
                                extra_predicate_error.push(
                                    PredicateError::Unsatisfied {
                                        predicate: Predicate::LifetimeOutlives(
                                            pred,
                                        ),

                                        predicate_declaration_span: None,
                                    },
                                );
                            }
                            Err(pernixc_type_system::Error::Overflow(
                                overflow_error,
                            )) => {
                                extra_predicate_error.push(
                                    PredicateError::Undecidable {
                                        predicate: Predicate::LifetimeOutlives(
                                            pred,
                                        ),

                                        predicate_declaration_span: None,
                                        overflow_error,
                                    },
                                );
                            }

                            Err(
                                pernixc_type_system::Error::CyclicDependency(
                                    err,
                                ),
                            ) => {
                                return Err(err);
                            }

                            Ok(Some(_)) => {}
                        },
                    }
                }

                Ok(extra_predicate_error)
            }

            Ok(None) => {
                extra_predicate_error.push(PredicateError::Unsatisfied {
                    predicate,
                    predicate_declaration_span,
                });

                Ok(extra_predicate_error)
            }

            Err(pernixc_type_system::Error::CyclicDependency(err)) => Err(err),

            Err(pernixc_type_system::Error::Overflow(overflow_error)) => {
                extra_predicate_error.push(PredicateError::Undecidable {
                    predicate,
                    predicate_declaration_span,
                    overflow_error,
                });

                Ok(extra_predicate_error)
            }
        }
    }

    /// Do predicates check for the given type occurrences.
    #[allow(clippy::too_many_lines)]
    pub(super) async fn check_type_ocurrence(
        &self,
        ty: &Type,
        instantiation_span: &RelativeSpan,
    ) -> Result<(), executor::CyclicError> {
        match ty {
            Type::Error(_)
            | Type::Tuple(_)
            | Type::Phantom(_)
            | Type::Pointer(_)
            | Type::Primitive(_)
            | Type::Parameter(_)
            | Type::Symbol(_)
            | Type::TraitMember(_)
            | Type::MemberSymbol(_)
            | Type::FunctionSignature(_)
            | Type::Inference(_) => {
                // no additional check
                Ok(())
            }

            Type::Reference(reference) => {
                let outlives = Outlives::new(
                    (*reference.pointee).clone(),
                    reference.lifetime.clone(),
                );

                match self.environment.query(&outlives).await {
                    Err(pernixc_type_system::Error::CyclicDependency(err)) => {
                        Err(err)
                    }

                    Ok(Some(_)) => Ok(()),

                    Ok(None) => {
                        self.handler.receive(UnsatisfiedPredicate {
                            predicate: Predicate::TypeOutlives(outlives),
                            instantiation_span: *instantiation_span,
                            predicate_declaration_span: None,
                        });

                        Ok(())
                    }

                    Err(pernixc_type_system::Error::Overflow(
                        overflow_error,
                    )) => {
                        overflow_error.report_as_undecidable_predicate(
                            Predicate::TypeOutlives(outlives),
                            None,
                            *instantiation_span,
                            self.handler,
                        );

                        Ok(())
                    }
                }
            }

            Type::Array(_) => {
                todo!("implements type check of the array length with usize")
                /*
                let expected_type = Type::Primitive(Primitive::Usize);
                let type_check =
                    TypeCheck::new(array.length.clone(), expected_type.clone());

                match self.environment.query(&type_check) {
                    Ok(Some(result)) => {
                        for LifetimeConstraint::LifetimeOutlives(outlives) in
                            &result.constraints
                        {
                            match self.environment.query(outlives) {
                                Ok(None) => {
                                    self.handler.receive(Box::new(
                                        UnsatisfiedPredicate {
                                            predicate:
                                                Predicate::LifetimeOutlives(
                                                    outlives.clone(),
                                                ),
                                            predicate_declaration_span: None,
                                            instantiation_span:
                                                instantiation_span.clone(),
                                        },
                                    ));
                                }

                                Err(Error::Abort(Abort)) | Ok(Some(_)) => {}

                                Err(Error::Overflow(overflow_error)) => {
                                    overflow_error
                                        .report_as_undecidable_predicate(
                                            Predicate::LifetimeOutlives(
                                                outlives.clone(),
                                            ),
                                            None,
                                            instantiation_span.clone(),
                                            self.handler,
                                        );
                                }
                            }
                        }
                    }

                    Ok(None) => {
                        self.handler.receive(Box::new(
                            ConstantArgumentTypeMismatched {
                                span: instantiation_span.clone(),
                                expected_type,
                                constant_argument: array.length.clone(),
                            },
                        ));
                    }

                    Err(Error::Abort(Abort)) => {}

                    Err(Error::Overflow(overflow_error)) => {
                        overflow_error.report_as_type_check_overflow(
                            instantiation_span.clone(),
                            self.handler,
                        );
                    }
                }
                */
            }
        }
    }

    #[allow(unused)]
    async fn check_unpacked_ocurrences(
        &self,
        unpacked_term: Type,
        instantiation_span: &RelativeSpan,
    ) -> Result<(), executor::CyclicError> {
        let tuple_predicate = predicate::Tuple(unpacked_term);

        let errors =
            self.predicate_satisfied(tuple_predicate.into(), None).await?;

        for error in errors {
            error.report(*instantiation_span, self.handler);
        }

        Ok(())
    }
}

pub(super) async fn check_occurrences(
    occurrences: &Occurrences,
    environment: &Environment<'_, normalizer::NoOp>,
    storage: &Storage<Diagnostic>,
) -> Result<(), executor::CyclicError> {
    let checker = Checker { environment, handler: storage };

    // check resolution occurrences
    for (resolution, span) in &occurrences.resolutions {
        checker.check_resolution_occurrence(resolution, span).await?;
    }

    // check type occurrences
    for (ty, syn) in &occurrences.types {
        checker.check_type_ocurrence(ty, &syn.span()).await?;
    }

    // check unpacked type occurrences
    for (unpacked, syn) in &occurrences.unpacked_types {
        checker
            .check_unpacked_ocurrences(unpacked.clone(), &syn.span())
            .await?;
    }

    // check unpacked constant occurrences
    for _ in &occurrences.unpacked_constants {
        // TODO: check that the type of constant must be a tuple
    }

    Ok(())
}

pernixc_register::register!(Key, Executor);

#[pernixc_query::query(
    key(Key),
    value(Arc<[Diagnostic]>),
    id(Global<pernixc_symbol::ID>),
    executor(Executor)
)]
pub async fn wf_check_executors(
    global_id: Global<pernixc_symbol::ID>,
    tracked_engine: &TrackedEngine,
) -> Result<Arc<[Diagnostic]>, executor::CyclicError> {
    let mut occurrences = Vec::new();
    let kind = tracked_engine.get_kind(global_id).await;

    if kind.has_generic_parameters() {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(
                    pernixc_term::generic_parameters::Key(global_id),
                ))
                .await?,
        );
    }

    if kind.has_where_clause() {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(
                    pernixc_semantic_element::where_clause::Key(global_id),
                ))
                .await?,
        );
    }

    if kind.has_type_alias() {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(
                    pernixc_semantic_element::type_alias::Key(global_id),
                ))
                .await?,
        );
    }

    if kind == Kind::Variant {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(
                    pernixc_semantic_element::variant::Key(global_id),
                ))
                .await?,
        );
    }

    if kind == Kind::Struct {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(
                    pernixc_semantic_element::fields::Key(global_id),
                ))
                .await?,
        );
    }

    if kind.is_implementation() {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(
                    implements_qualified_identifier::Key(global_id),
                ))
                .await?,
        );
    }

    if kind.has_function_signature() {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(function_signature::Key(
                    global_id,
                )))
                .await?,
        );
    }

    let active_premise = tracked_engine.get_active_premise(global_id).await?;
    let environment = Environment::new(
        Cow::Borrowed(&active_premise),
        Cow::Borrowed(tracked_engine),
        normalizer::NO_OP,
    );

    let storage = Storage::<Diagnostic>::default();
    for occurrence in occurrences {
        check_occurrences(&occurrence, &environment, &storage).await?;
    }

    Ok(storage.into_vec().into())
}
