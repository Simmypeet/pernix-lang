//! Contains code related to well-formedness checking of each instantiation of
//! symbols and types.

use std::{
    collections::{BTreeSet, HashSet},
    ops::Deref,
};

use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};

use super::{
    builder::{self, TypeSystem},
    occurrences::Occurrences,
};
use crate::{
    arena::ID,
    error::{
        self, AdtImplementationIsNotGeneralEnough, AmbiguousPredicates,
        ConstantArgumentTypeMismatched, DefinitePremisePredicate, Error,
        ImplementationIsNotGeneralEnough,
        MismatchedGenericParameterCountInImplementation,
        MismatchedImplementationArguments,
        MismatchedImplementationConstantTypeParameter,
        OverflowCalculatingRequirementForInstantiation, OverflowOperation,
        RecursiveTraitTypeEquality, TypeSystemOverflow, UnsatisfiedPredicate,
        UnusedGenericParameterInImplementation,
    },
    symbol::{
        self,
        table::{
            self,
            representation::{
                building::finalizing::{
                    symbol::{
                        negative_trait_implementation,
                        positive_trait_implementation,
                    },
                    Finalizer,
                },
                Element, Index, RwLockContainer,
            },
            resolution, Building, Table,
        },
        ConstantParameter, ConstantParameterID, GenericID, GenericParameter,
        GenericParameters, GenericTemplate, GlobalID, ImplementationTemplate,
        LifetimeParameter, LifetimeParameterID, ResolvableImplementationID,
        TraitImplementationMemberID, TraitMemberID, TypeParameter,
        TypeParameterID,
    },
    type_system::{
        compatible::{Compatibility, Compatible},
        deduction,
        environment::{self, Environment},
        instantiation::{self, Instantiation},
        model::{Default, Model},
        normalizer::{self, Normalizer},
        observer::{self, Observer},
        predicate::{
            self, NegativeMarkerSatisfied, NegativeTraitSatisfied, Outlives,
            PositiveMarkerSatisfied, PositiveTraitSatisfied, Predicate, Tuple,
        },
        simplify,
        term::{
            self, constant,
            lifetime::{self, Lifetime},
            r#type, GenericArguments, Term,
        },
        type_check::TypeCheck,
        variance::Variance,
        Compute, LifetimeConstraint, OverflowError, Premise, Satisfied,
        Succeeded,
    },
};

/// An enumeration of ways the predicate can be erroneous.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum PredicateError<M: Model> {
    /// The predicate isn't satisfied.
    Unsatisfied {
        predicate: Predicate<M>,
        predicate_declaration_span: Option<Span>,
    },

    /// The type system can't determine if the predicate is satisfiable or not.
    Undecidable {
        predicate: Predicate<M>,
        predicate_declaration_span: Option<Span>,
    },

    /// The solved trait/marker implementation is not general enough for the
    /// forall lifetime requirements.
    ImplementationIsNotGeneralEnough {
        resolved_implementation:
            predicate::Implementation<ResolvableImplementationID, M>,
        generic_arguments: GenericArguments<M>,
        predicate_declaration_span: Option<Span>,
    },
}

impl<M: Model> PredicateError<M>
where
    predicate::Predicate<M>: table::Display<table::Suboptimal>,

    M::LifetimeInference: table::Display<table::Suboptimal>,
    M::TypeInference: table::Display<table::Suboptimal>,
    M::ConstantInference: table::Display<table::Suboptimal>,
{
    fn report(
        self,
        instantiation_span: Span,
        handler: &dyn Handler<Box<dyn Error>>,
    ) {
        match self {
            Self::Unsatisfied { predicate, predicate_declaration_span } => {
                handler.receive(Box::new(UnsatisfiedPredicate {
                    predicate,
                    instantiation_span,
                    predicate_declaration_span,
                }));
            }

            Self::Undecidable { predicate, .. } => {
                if predicate.contains_error() {
                    return;
                }

                handler.receive(Box::new(TypeSystemOverflow {
                    operation: OverflowOperation::Predicate(predicate),
                    overflow_span: instantiation_span,
                    overflow_error: OverflowError,
                }));
            }

            Self::ImplementationIsNotGeneralEnough {
                resolved_implementation,
                predicate_declaration_span,
                generic_arguments,
            } => {
                if generic_arguments.contains_error() {
                    return;
                }

                handler.receive(Box::new(ImplementationIsNotGeneralEnough {
                    resolvable_implementation_id: resolved_implementation.id,
                    instantiation_span,
                    predicate_declaration_span,
                    generic_arguments,
                }));
            }
        }
    }
}

impl<
        'a,
        M: Model,
        N: Normalizer<M, Building<RwLockContainer, Finalizer>>,
        O: Observer<M, Building<RwLockContainer, Finalizer>>,
    > Environment<'a, M, Building<RwLockContainer, Finalizer>, N, O>
where
    predicate::Predicate<M>: table::Display<table::Suboptimal>,

    M::LifetimeInference: table::Display<table::Suboptimal>,
    M::TypeInference: table::Display<table::Suboptimal>,
    M::ConstantInference: table::Display<table::Suboptimal>,
{
    fn create_instantiation_for_adt(
        &self,
        adt_implementation_id: ID<symbol::AdtImplementation>,
        parent_generic_arguments: &GenericArguments<M>,
        resolution_span: &Span,
        do_outlives_check: bool,
        checking_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Option<Instantiation<M>> {
        // deduce the generic arguments
        let adt_implementation =
            self.table().get(adt_implementation_id).unwrap();

        let arguments = adt_implementation.arguments.clone();
        drop(adt_implementation);

        let result = match GenericArguments::from_default_model(arguments)
            .deduce(parent_generic_arguments, self)
        {
            Ok(deduced) => deduced,

            Err(deduction::Error::MismatchedGenericArgumentCount(_)) => {
                unreachable!()
            }

            Err(deduction::Error::UnificationFailure(_)) => {
                handler.receive(Box::new(MismatchedImplementationArguments {
                    adt_implementation_id,
                    found_generic_arguments: parent_generic_arguments.clone(),
                    instantiation_span: resolution_span.clone(),
                }));

                return None; // can't continue
            }

            Err(deduction::Error::Overflow(_)) => {
                handler.receive(Box::new(
                    OverflowCalculatingRequirementForInstantiation {
                        instantiation_span: resolution_span.clone(),
                    },
                ));

                return None;
            }
        };

        self.check_lifetime_constraints(
            result.constraints,
            resolution_span,
            handler,
        );

        // check if the deduced generic arguments are correct
        self.check_instantiation_predicates(
            adt_implementation_id.into(),
            &result.result.instantiation,
            resolution_span,
            do_outlives_check,
            checking_site,
            handler,
        );

        // the implementation is not general enough
        if result.result.is_not_general_enough {
            handler.receive(Box::new(AdtImplementationIsNotGeneralEnough {
                adt_implementation_id,
                generic_arguments: parent_generic_arguments.clone(),
                instantiation_span: resolution_span.clone(),
            }));
        }

        Some(result.result.instantiation)
    }

    fn check_trait_instantiation(
        &self,
        trait_id: ID<symbol::Trait>,
        generic_arguments: GenericArguments<M>,
        instantiation_span: &Span,
        do_outlives_check: bool,
        checking_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        for error in self.predicate_satisfied(
            Predicate::PositiveTrait(predicate::PositiveTrait {
                id: trait_id,
                is_const: false,
                generic_arguments,
            }),
            None,
            do_outlives_check,
            checking_site,
            handler,
        ) {
            error.report(instantiation_span.clone(), handler);
        }
    }

    /// Checks if the given `resolution` is well-formed. The errors are reported
    /// to the `handler`.
    ///
    /// # Parameters
    ///
    /// - `resolution`: The resolution to check.
    /// - `resolution_span`: The span location of the `resolution`.
    /// - `do_outlives_check`: Determines if the outlives predicates should be
    ///   checked.
    /// - `session`: The session to use for caching and limiting the
    ///   computation.
    /// - `handler`: The handler to report the errors.
    #[allow(clippy::too_many_lines, unused)]
    pub(super) fn check_resolution_occurrence(
        &self,
        resolution: &resolution::Resolution<M>,
        resolution_span: &Span,
        do_outlives_check: bool,
        checking_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match resolution {
            resolution::Resolution::Module(_)
            | resolution::Resolution::PositiveTraitImplementation(_)
            | resolution::Resolution::Variant(_) => {}

            resolution::Resolution::Generic(generic) => {
                self.check_instantiation_predicates_by_generic_arguments(
                    generic.id.into(),
                    generic.generic_arguments.clone(),
                    resolution_span,
                    do_outlives_check,
                    checking_site,
                    handler,
                );
            }

            resolution::Resolution::MemberGeneric(member_generic) => {
                // additional adt implementation check

                // the trait implementation doesn't need to be checked here
                // because it can never be referred directly in the source code
                let adt_implementation_check = match member_generic.id {
                    resolution::MemberGenericID::AdtImplementationFunction(
                        id,
                    ) => Some(self.table().get(id).unwrap().parent_id),

                    id @ (resolution::MemberGenericID::TraitConstant(_)
                    | resolution::MemberGenericID::TraitFunction(_)
                    | resolution::MemberGenericID::TraitType(_)) => {
                        let trait_id = self
                            .table()
                            .get_global(id.into())
                            .unwrap()
                            .parent_global_id()
                            .expect("should have a parent")
                            .into_trait()
                            .expect("should've been a trait");

                        self.check_trait_instantiation(
                            trait_id,
                            member_generic.parent_generic_arguments.clone(),
                            resolution_span,
                            do_outlives_check,
                            checking_site,
                            handler,
                        );

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
                                do_outlives_check,
                                checking_site,
                                handler,
                            )
                        else {
                            return;
                        };

                        instantiation
                    } else {
                        let parent_id = GenericID::try_from(
                            self.table()
                                .get_global(member_generic.id.into())
                                .unwrap()
                                .parent_global_id()
                                .expect("should have a parent"),
                        )
                        .expect("parent should be a generic");

                        Instantiation::from_generic_arguments(
                            member_generic.parent_generic_arguments.clone(),
                            parent_id,
                            &self
                                .table()
                                .get_generic(parent_id)
                                .unwrap()
                                .generic_declaration()
                                .parameters,
                        )
                        .expect("should have no mismatched")
                    };

                parent_instantiation
                    .append_from_generic_arguments(
                        member_generic.generic_arguments.clone(),
                        member_generic.id.into(),
                        &self
                            .table()
                            .get_generic(member_generic.id.into())
                            .unwrap()
                            .generic_declaration()
                            .parameters,
                    )
                    .unwrap();

                self.check_instantiation_predicates(
                    member_generic.id.into(),
                    &parent_instantiation,
                    resolution_span,
                    do_outlives_check,
                    checking_site,
                    handler,
                );
            }
        }
    }

    /// Do where clause predicates check for the given instantiation. The errors
    /// are reported to the `handler`.
    ///
    /// # Parameters
    ///
    /// - `instantiated`: The [`GenericID`] of the instantiated symbol.
    /// - `generic_arguments`: The generic arguments supplied to the
    ///   `instantiated`.
    /// - `instantiation_span`: The span location of the instantiation, used for
    ///   error reporting
    /// - `do_outlives_check`: Determines if the outlives predicates should be
    ///   checked.
    /// - `session`: The session to use for caching and limiting the
    ///   computation.
    /// - `handler`: The handler to report the errors.
    pub(super) fn check_instantiation_predicates_by_generic_arguments(
        &self,
        instantiated: GenericID,
        generic_arguments: GenericArguments<M>,
        instantiation_span: &Span,
        do_outlives_check: bool,
        checking_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        // convert the generic arguments to an instantiation and delegate the
        // check to the `check_instantiation_predicates` method
        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments,
            instantiated,
            &self
                .table()
                .get_generic(instantiated)
                .unwrap()
                .generic_declaration()
                .parameters,
        )
        .unwrap();

        self.check_instantiation_predicates(
            instantiated,
            &instantiation,
            instantiation_span,
            do_outlives_check,
            checking_site,
            handler,
        );
    }

    /// Do where clause predicates check for the given instantiation. The errors
    /// are reported to the `handler`.
    ///
    /// # Parameters
    ///
    /// - `instantiated`: The [`GenericID`] of the instantiated symbol.
    /// - `instantiation`: The instantiation of the `instantiated`.
    /// - `instantiation_span`: The span location of the instantiation, used for
    ///   error reporting
    /// - `do_outlives_check`: Determines if the outlives predicates should be
    ///   checked.
    /// - `session`: The session to use for caching and limiting the
    ///   computation.
    /// - `handler`: The handler to report the errors.
    pub(super) fn check_instantiation_predicates(
        &self,
        instantiated: GenericID,
        instantiation: &Instantiation<M>,
        instantiation_span: &Span,
        do_outlives_check: bool,
        checking_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        predicate::Predicate<M>: table::Display<table::Suboptimal>,
    {
        // get all the predicates and instantiate them with the given generic
        // arguments
        let _ = builder::build_for_where_clause(
            self.table(),
            instantiated.into(),
            Some(checking_site),
            handler,
        );

        let instantiated_sym = self.table().get_generic(instantiated).unwrap();

        #[allow(clippy::significant_drop_in_scrutinee)]
        for predicate_info in &instantiated_sym.generic_declaration().predicates
        {
            let mut predicate =
                Predicate::from_default_model(predicate_info.predicate.clone());

            predicate.instantiate(instantiation);

            let errors = self.predicate_satisfied(
                predicate,
                predicate_info.span.clone(),
                do_outlives_check,
                checking_site,
                handler,
            );

            for error in errors {
                error.report(instantiation_span.clone(), handler);
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn check_implementation_satisfied(
        &self,
        id: ResolvableImplementationID,
        instantiation: &Instantiation<M>,
        generic_arguments: &GenericArguments<M>,
        predicate_declaration_span: Option<Span>,
        checking_site: GlobalID,
        do_outlives_check: bool,
        is_not_general_enough: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Vec<PredicateError<M>> {
        let mut errors = Vec::new();

        if is_not_general_enough {
            errors.push(PredicateError::ImplementationIsNotGeneralEnough {
                resolved_implementation: predicate::Implementation {
                    instantiation: instantiation.clone(),
                    id,
                    is_not_general_enough,
                },
                generic_arguments: generic_arguments.clone(),
                predicate_declaration_span,
            });
        }

        let _ = match id {
            ResolvableImplementationID::PositiveTrait(id) => {
                self.table().build_to(
                    id,
                    Some(checking_site),
                    positive_trait_implementation::WHERE_CLAUSE_STATE,
                    handler,
                )
            }
            ResolvableImplementationID::NegativeTrait(id) => {
                self.table().build_to(
                    id,
                    Some(checking_site),
                    negative_trait_implementation::WHERE_CLAUSE_STATE,
                    handler,
                )
            }
            ResolvableImplementationID::PositiveMarker(id) => {
                self.table().build_to(
                    id,
                    Some(checking_site),
                    positive_trait_implementation::WHERE_CLAUSE_STATE,
                    handler,
                )
            }
            ResolvableImplementationID::NegativeMarker(id) => {
                self.table().build_to(
                    id,
                    Some(checking_site),
                    negative_trait_implementation::WHERE_CLAUSE_STATE,
                    handler,
                )
            }
        };

        let predicates = self
            .table()
            .get_generic(id.into())
            .unwrap()
            .generic_declaration()
            .predicates
            .clone();

        // check for each predicate in the implementation
        for predicate in predicates {
            let mut predicate_instantiated =
                Predicate::from_default_model(predicate.predicate);

            predicate_instantiated.instantiate(instantiation);

            errors.extend(self.predicate_satisfied(
                predicate_instantiated,
                predicate.span,
                do_outlives_check,
                checking_site,
                handler,
            ));
        }

        errors
    }

    fn handle_positive_marker_satisfied(
        &self,
        result: PositiveMarkerSatisfied<M>,
        pred_generic_arguments: &GenericArguments<M>,
        predicate_declaration_span: Option<Span>,
        do_outlives_check: bool,
        checking_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> (BTreeSet<LifetimeConstraint<M>>, Vec<PredicateError<M>>) {
        match result {
            PositiveMarkerSatisfied::ByPremise
            | PositiveMarkerSatisfied::ByEnvironment
            | PositiveMarkerSatisfied::ByCyclic => {
                (BTreeSet::new(), Vec::new())
            }

            PositiveMarkerSatisfied::ByImplementation(implementation) => (
                BTreeSet::new(),
                self.check_implementation_satisfied(
                    implementation.id.into(),
                    &implementation.instantiation,
                    pred_generic_arguments,
                    predicate_declaration_span,
                    checking_site,
                    do_outlives_check,
                    implementation.is_not_general_enough,
                    handler,
                ),
            ),

            PositiveMarkerSatisfied::ByCongruence(btree_map) => {
                let mut constraints = BTreeSet::new();
                let mut pred_errors = Vec::new();

                for (_, result) in btree_map {
                    constraints.extend(result.constraints);

                    let (new_constraints, new_pred_errors) = self
                        .handle_positive_marker_satisfied(
                            result.result,
                            pred_generic_arguments,
                            predicate_declaration_span.clone(),
                            do_outlives_check,
                            checking_site,
                            handler,
                        );

                    constraints.extend(new_constraints);
                    pred_errors.extend(new_pred_errors);
                }

                (constraints, pred_errors)
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn predicate_satisfied(
        &self,
        predicate: Predicate<M>,
        predicate_declaration_span: Option<Span>,
        do_outlives_check: bool,
        checking_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Vec<PredicateError<M>> {
        let (result, mut extra_predicate_error) = match &predicate {
            Predicate::TraitTypeEquality(eq) => {
                let result = r#type::Type::TraitMember(eq.lhs.clone())
                    .compatible(&eq.rhs, Variance::Covariant, self);

                (
                    match result {
                        Ok(Some(Succeeded {
                            result: Compatibility { forall_lifetime_errors, .. },
                            constraints,
                        })) => {
                            if forall_lifetime_errors.is_empty() {
                                Ok(Some(Succeeded::satisfied_with(constraints)))
                            } else {
                                Ok(None)
                            }
                        }

                        Ok(None) => Ok(None),

                        Err(OverflowError) => Err(OverflowError),
                    },
                    Vec::new(),
                )
            }
            Predicate::ConstantType(pred) => (pred.query(self), Vec::new()),
            Predicate::LifetimeOutlives(pred) => {
                if !do_outlives_check {
                    return Vec::new();
                }

                return match pred.query(self) {
                    Ok(Some(Satisfied)) => vec![],

                    Ok(None) => {
                        vec![PredicateError::Unsatisfied {
                            predicate,
                            predicate_declaration_span,
                        }]
                    }

                    Err(OverflowError) => {
                        vec![PredicateError::Undecidable {
                            predicate,
                            predicate_declaration_span,
                        }]
                    }
                };
            }
            Predicate::TypeOutlives(pred) => {
                if !do_outlives_check {
                    return Vec::new();
                }

                return match pred.query(self) {
                    Ok(Some(Satisfied)) => vec![],

                    Ok(None) => {
                        vec![PredicateError::Unsatisfied {
                            predicate,
                            predicate_declaration_span,
                        }]
                    }

                    Err(OverflowError) => {
                        vec![PredicateError::Undecidable {
                            predicate,
                            predicate_declaration_span,
                        }]
                    }
                };
            }
            Predicate::TupleType(pred) => (pred.query(self), Vec::new()),
            Predicate::PositiveMarker(pred) => match pred.query(self) {
                Ok(None) => (Ok(None), Vec::new()),

                Ok(Some(Succeeded { result, mut constraints })) => {
                    let (new_constraints, pred_errors) = self
                        .handle_positive_marker_satisfied(
                            result,
                            &pred.generic_arguments,
                            predicate_declaration_span.clone(),
                            do_outlives_check,
                            checking_site,
                            handler,
                        );

                    constraints.extend(new_constraints);

                    (
                        Ok(Some(Succeeded::satisfied_with(constraints))),
                        pred_errors,
                    )
                }

                Err(OverflowError) => (Err(OverflowError), Vec::new()),
            },
            Predicate::PositiveTrait(pred) => match pred.query(self) {
                Ok(None) => (Ok(None), Vec::new()),
                Ok(Some(Succeeded { result, constraints })) => match result {
                    PositiveTraitSatisfied::ByCyclic
                    | PositiveTraitSatisfied::ByPremise
                    | PositiveTraitSatisfied::ByEnvironment => (
                        Ok(Some(Succeeded::satisfied_with(constraints))),
                        Vec::new(),
                    ),

                    PositiveTraitSatisfied::ByImplementation(
                        implementation,
                    ) => (
                        Ok(Some(Succeeded::satisfied_with(constraints))),
                        self.check_implementation_satisfied(
                            implementation.id.into(),
                            &implementation.instantiation,
                            &pred.generic_arguments,
                            predicate_declaration_span.clone(),
                            checking_site,
                            do_outlives_check,
                            implementation.is_not_general_enough,
                            handler,
                        ),
                    ),
                },
                Err(OverflowError) => (Err(OverflowError), Vec::new()),
            },

            Predicate::NegativeTrait(pred) => match pred.query(self) {
                Ok(None) => (Ok(None), Vec::new()),
                Ok(Some(Succeeded { result, constraints })) => match result {
                    NegativeTraitSatisfied::ByPremise
                    | NegativeTraitSatisfied::ByUnsatisfiedPositive => (
                        Ok(Some(Succeeded::satisfied_with(constraints))),
                        Vec::new(),
                    ),

                    NegativeTraitSatisfied::ByImplementation(
                        implementation,
                    ) => (
                        Ok(Some(Succeeded::satisfied_with(constraints))),
                        self.check_implementation_satisfied(
                            implementation.id.into(),
                            &implementation.instantiation,
                            &pred.generic_arguments,
                            predicate_declaration_span.clone(),
                            checking_site,
                            do_outlives_check,
                            implementation.is_not_general_enough,
                            handler,
                        ),
                    ),
                },
                Err(OverflowError) => (Err(OverflowError), Vec::new()),
            },
            Predicate::NegativeMarker(negative) => match negative.query(self) {
                Ok(Some(Succeeded { result, constraints })) => match result {
                    NegativeMarkerSatisfied::ByUnsatisfiedPositive
                    | NegativeMarkerSatisfied::ByPremise => (
                        Ok(Some(Succeeded::satisfied_with(constraints))),
                        Vec::new(),
                    ),

                    NegativeMarkerSatisfied::ByImplementation(
                        implementation,
                    ) => (
                        Ok(Some(Succeeded::satisfied_with(constraints))),
                        self.check_implementation_satisfied(
                            implementation.id.into(),
                            &implementation.instantiation,
                            &negative.generic_arguments,
                            predicate_declaration_span.clone(),
                            checking_site,
                            do_outlives_check,
                            implementation.is_not_general_enough,
                            handler,
                        ),
                    ),
                },
                Ok(None) => (Ok(None), Vec::new()),
                Err(OverflowError) => (Err(OverflowError), Vec::new()),
            },
        };

        match result {
            Ok(Some(Succeeded { result: Satisfied, constraints })) => {
                // if do_outlives_check is false, then we don't need to check
                if !do_outlives_check {
                    return Vec::new();
                }

                for constraint in constraints {
                    match constraint {
                        LifetimeConstraint::LifetimeOutlives(pred) => {
                            match pred.query(self) {
                                Ok(None) => {
                                    extra_predicate_error.push(
                                        PredicateError::Unsatisfied {
                                            predicate:
                                                Predicate::LifetimeOutlives(
                                                    pred,
                                                ),

                                            predicate_declaration_span: None,
                                        },
                                    );
                                }
                                Err(_) => {
                                    extra_predicate_error.push(
                                        PredicateError::Undecidable {
                                            predicate:
                                                Predicate::LifetimeOutlives(
                                                    pred,
                                                ),

                                            predicate_declaration_span: None,
                                        },
                                    );
                                }

                                Ok(Some(_)) => {}
                            }
                        }
                    }
                }

                extra_predicate_error
            }

            Ok(None) => {
                extra_predicate_error.push(PredicateError::Unsatisfied {
                    predicate,
                    predicate_declaration_span,
                });

                extra_predicate_error
            }

            Err(OverflowError) => {
                extra_predicate_error.push(PredicateError::Undecidable {
                    predicate,
                    predicate_declaration_span,
                });

                extra_predicate_error
            }
        }
    }

    /// Do predicates check for the given type occurrences.
    #[allow(unused)]
    pub(super) fn check_type_ocurrence(
        &self,
        ty: &r#type::Type<M>,
        instantiation_span: &Span,
        do_outlives_check: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match ty {
            r#type::Type::Error(_)
            | term::r#type::Type::Tuple(_)
            | term::r#type::Type::Phantom(_)
            | term::r#type::Type::Pointer(_)
            | term::r#type::Type::Primitive(_)
            | term::r#type::Type::Parameter(_)
            | term::r#type::Type::Symbol(_)
            | term::r#type::Type::TraitMember(_)
            | term::r#type::Type::MemberSymbol(_)
            | term::r#type::Type::Inference(_) => { /* no additional check */ }

            term::r#type::Type::Reference(reference) => {
                if do_outlives_check {
                    let outlives = Outlives {
                        operand: (*reference.pointee).clone(),
                        bound: reference.lifetime.clone(),
                    };

                    match outlives.query(self) {
                        Ok(Some(Satisfied)) => {}

                        Ok(None) => {
                            handler.receive(Box::new(UnsatisfiedPredicate {
                                predicate: predicate::Predicate::TypeOutlives(
                                    Outlives {
                                        operand: reference
                                            .pointee
                                            .deref()
                                            .clone(),
                                        bound: reference.lifetime.clone(),
                                    },
                                ),
                                instantiation_span: instantiation_span.clone(),
                                predicate_declaration_span: None,
                            }));
                        }

                        Err(overflow_error) => {
                            handler.receive(Box::new(TypeSystemOverflow {
                                operation: OverflowOperation::Predicate(
                                    predicate::Predicate::TypeOutlives(
                                        outlives,
                                    ),
                                ),
                                overflow_span: instantiation_span.clone(),
                                overflow_error,
                            }));
                        }
                    }
                }
            }

            term::r#type::Type::Array(array) => {
                let expected_type =
                    r#type::Type::Primitive(r#type::Primitive::Usize);
                let type_check =
                    TypeCheck::new(array.length.clone(), expected_type.clone());

                match type_check.query(self) {
                    Ok(Some(Succeeded { result: Satisfied, .. })) => {}

                    Ok(None) => {
                        handler.receive(Box::new(
                            ConstantArgumentTypeMismatched {
                                span: instantiation_span.clone(),
                                expected_type,
                                constant_argument: array.length.clone(),
                            },
                        ));
                    }

                    Err(_) => {
                        handler.receive(Box::new(
                            OverflowCalculatingRequirementForInstantiation {
                                instantiation_span: instantiation_span.clone(),
                            },
                        ));
                    }
                }
            }
        }
    }

    #[allow(unused)]
    fn check_unpacked_ocurrences<U: Term<Model = M> + 'a>(
        &self,
        unpacked_term: U,
        instantiation_span: &Span,
        checking_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        predicate::Predicate<M>:
            From<Tuple<U>> + table::Display<table::Suboptimal>,
    {
        let tuple_predicate = Tuple(unpacked_term);

        let errors = self.predicate_satisfied(
            tuple_predicate.into(),
            None,
            true,
            checking_site,
            handler,
        );

        for error in errors {
            error.report(instantiation_span.clone(), handler);
        }
    }

    /// Checks if the gien list of lifetime constraints are satisfiable or not.
    ///
    /// If the lifetime constraints are not satisfiable, then the errors are
    /// reported to the `handler`.
    pub(super) fn check_lifetime_constraints(
        &self,
        lifetime_constraints: impl IntoIterator<Item = LifetimeConstraint<M>>,
        instantiation_span: &Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        for lifetime_constraint in lifetime_constraints {
            match lifetime_constraint {
                LifetimeConstraint::LifetimeOutlives(outlives) => {
                    match outlives.query(self) {
                        Ok(Some(Satisfied)) => {}

                        Ok(None) => {
                            handler.receive(Box::new(UnsatisfiedPredicate {
                                predicate: Predicate::LifetimeOutlives(
                                    outlives,
                                ),
                                instantiation_span: instantiation_span.clone(),
                                predicate_declaration_span: None,
                            }));
                        }

                        Err(overflow_error) => {
                            handler.receive(Box::new(TypeSystemOverflow {
                                operation: OverflowOperation::Predicate(
                                    predicate::Predicate::LifetimeOutlives(
                                        outlives,
                                    ),
                                ),
                                overflow_span: instantiation_span.clone(),
                                overflow_error,
                            }));
                        }
                    }
                }
            }
        }
    }

    /// Simplifies the given [`r#type::Type`] term and immediately checks for
    /// the lifetime constraints coming with simplification.
    ///
    /// The errors (if any) are reported to the `handler`.
    pub fn simplify_and_check_lifetime_constraints(
        &self,
        ty: &r#type::Type<M>,
        instantiation_span: &Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> r#type::Type<M> {
        let Succeeded { result: simplified, constraints } =
            match simplify::simplify(ty, self) {
                Ok(a) => a,
                Err(overflow_error) => {
                    handler.receive(Box::new(TypeSystemOverflow {
                        operation: OverflowOperation::TypeOf,
                        overflow_span: instantiation_span.clone(),
                        overflow_error,
                    }));

                    return r#type::Type::Error(term::Error);
                }
            };

        self.check_lifetime_constraints(
            constraints,
            instantiation_span,
            handler,
        );

        simplified
    }
}

impl Table<Building<RwLockContainer, Finalizer>> {
    /// Checks the well-formedness of the where clause predicates
    #[allow(unused)]
    pub fn check_where_clause(
        &self,
        id: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        if GenericID::try_from(id).is_err() {
            return;
        };

        let observer = TypeSystem::new(id, handler);
        let (_, environment_errors) = Environment::new_with(
            self.get_active_premise::<Default>(id).unwrap(),
            self,
            normalizer::NO_OP,
            &observer,
        );
        let active_premise_with_span = self
            .get_active_premise_predicates_with_span::<Default>(id)
            .unwrap();

        for error in environment_errors {
            match error {
                environment::Error::AmbiguousPredicates(predicates) => {
                    let spans = predicates
                        .iter()
                        .flat_map(|x| {
                            active_premise_with_span.get(x).unwrap().iter()
                        })
                        .cloned()
                        .collect::<Vec<_>>();

                    handler.receive(Box::new(AmbiguousPredicates {
                        predicates,
                        predicate_declaration_spans: spans,
                    }));
                }

                environment::Error::DefinintePremise(definite) => {
                    let span = active_premise_with_span.get(&definite).unwrap();

                    for span in span {
                        handler.receive(Box::new(DefinitePremisePredicate {
                            predicate: definite.clone(),
                            span: span.clone(),
                        }));
                    }
                }

                environment::Error::RecursiveTraitTypeEqualityPredicate(
                    pred,
                ) => {
                    let pred = Predicate::TraitTypeEquality(pred);
                    let spans =
                        active_premise_with_span.get(&pred).unwrap().clone();

                    handler.receive(Box::new(RecursiveTraitTypeEquality {
                        trait_type_equality: pred
                            .into_trait_type_equality()
                            .unwrap(),
                        predicate_declaration_spans: spans,
                    }));
                }

                environment::Error::Overflow(pred, _) => {
                    let spans = active_premise_with_span.get(&pred).unwrap();

                    for span in spans {
                        handler.receive(Box::new(
                            OverflowCalculatingRequirementForInstantiation {
                                instantiation_span: span.clone(),
                            },
                        ));
                    }
                }
            }
        }
    }

    /// Checks if the occurrences of symbols are valid (i.e. they satisfy the
    /// where clause predicates).
    #[allow(unused)]
    pub fn check_occurrences(
        &self,
        id: GlobalID,
        occurrences: &Occurrences,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let active_premise = self.get_active_premise(id).unwrap();
        let observer = TypeSystem::new(id, handler);
        let (environment, _) = Environment::new_with(
            active_premise,
            self,
            normalizer::NO_OP,
            &observer,
        );

        // check resolution occurrences
        for (resolution, span) in occurrences.resolutions() {
            environment.check_resolution_occurrence(
                resolution, span, true, id, handler,
            );
        }

        // check type occurrences
        for (ty, syn) in occurrences.types() {
            environment.check_type_ocurrence(ty, &syn.span(), true, handler);
        }

        // check unpacked type occurrences
        for (unpacked, syn) in occurrences.unpacked_types() {
            environment.check_unpacked_ocurrences(
                unpacked.clone(),
                &syn.span(),
                id,
                handler,
            );
        }

        // check unpacked constant occurrences
        for _ in occurrences.unpacked_constants() {
            // TODO: check that the type of constant must be a tuple
        }
    }
}

/// Contains all the unused generic parameters in a generic arguments.
struct UnusedGenericParameters {
    lifetimes: HashSet<LifetimeParameterID>,
    types: HashSet<TypeParameterID>,
    constants: HashSet<ConstantParameterID>,
}

impl UnusedGenericParameters {
    /// Gets the list of unused generic parameters.
    ///
    /// # Parameters
    ///
    /// - `generic_id`: The [`GenericID`] where the `generic_parameters` are
    ///   from.
    /// - `generic_parameters`: The [`GenericParameters`] to compare the
    /// - `generic_arguments`: The generic arguments to search for unused
    ///   parameters.
    pub(super) fn get_unused_generic_parameters(
        generic_id: GenericID,
        generic_parameters: &GenericParameters,
        generic_arguments: &GenericArguments<Default>,
    ) -> Self {
        let mut unused_generic_arguments = Self {
            lifetimes: generic_parameters
                .lifetime_order()
                .iter()
                .map(|x| LifetimeParameterID { parent: generic_id, id: *x })
                .collect(),
            types: generic_parameters
                .type_order()
                .iter()
                .map(|x| TypeParameterID { parent: generic_id, id: *x })
                .collect(),
            constants: generic_parameters
                .constant_order()
                .iter()
                .map(|x| ConstantParameterID { parent: generic_id, id: *x })
                .collect(),
        };

        unused_generic_arguments.check_in_generic_arguments(generic_arguments);

        unused_generic_arguments
    }

    /// Reports the unused generic parameters in an implementation.
    pub(super) fn report_as_unused_generic_parameters_in_implementation<
        ID: Into<GenericID> + Copy + std::fmt::Debug + Send + Sync + 'static,
    >(
        &self,
        implementation_kind_id: ID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        for unused_lt in &self.lifetimes {
            handler.receive(Box::new(UnusedGenericParameterInImplementation {
                generic_parameter_id: unused_lt.id.into(),
                implementation_id: implementation_kind_id.into(),
            }));
        }

        for unused_ty in &self.types {
            handler.receive(Box::new(UnusedGenericParameterInImplementation {
                generic_parameter_id: unused_ty.id.into(),
                implementation_id: implementation_kind_id.into(),
            }));
        }

        for unused_val in &self.constants {
            handler.receive(Box::new(UnusedGenericParameterInImplementation {
                generic_parameter_id: unused_val.id.into(),
                implementation_id: implementation_kind_id.into(),
            }));
        }
    }

    fn check_in_generic_arguments(&mut self, args: &GenericArguments<Default>) {
        for lt in &args.lifetimes {
            self.check_in_lifetime(lt);
        }

        for ty in &args.types {
            self.check_in_type(ty);
        }

        for val in &args.constants {
            self.check_in_constant(val);
        }
    }

    fn check_in_type(&mut self, ty: &r#type::Type<Default>) {
        match ty {
            r#type::Type::Parameter(parameter) => {
                self.types.remove(parameter);
            }
            r#type::Type::MemberSymbol(symbol) => {
                self.check_in_generic_arguments(
                    &symbol.parent_generic_arguments,
                );
                self.check_in_generic_arguments(
                    &symbol.member_generic_arguments,
                );
            }
            r#type::Type::Symbol(symbol) => {
                self.check_in_generic_arguments(&symbol.generic_arguments);
            }
            r#type::Type::Pointer(pointer) => {
                self.check_in_type(&pointer.pointee);
            }
            r#type::Type::Reference(reference) => {
                self.check_in_lifetime(&reference.lifetime);
                self.check_in_type(&reference.pointee);
            }
            r#type::Type::Array(array) => {
                self.check_in_type(&array.r#type);
                self.check_in_constant(&array.length);
            }
            r#type::Type::Tuple(tuple) => {
                for ty in &tuple.elements {
                    self.check_in_type(&ty.term);
                }
            }
            r#type::Type::Phantom(phantom) => {
                self.check_in_type(&phantom.0);
            }

            r#type::Type::TraitMember(_)
            | r#type::Type::Inference(_)
            | r#type::Type::Error(_)
            | r#type::Type::Primitive(_) => {}
        }
    }

    fn check_in_lifetime(&mut self, lt: &lifetime::Lifetime<Default>) {
        match lt {
            lifetime::Lifetime::Parameter(parameter) => {
                self.lifetimes.remove(parameter);
            }

            lifetime::Lifetime::Inference(_)
            | lifetime::Lifetime::Static
            | lifetime::Lifetime::Error(_)
            | lifetime::Lifetime::Forall(_) => {}
        }
    }

    fn check_in_constant(&mut self, val: &constant::Constant<Default>) {
        match val {
            constant::Constant::Parameter(parameter) => {
                self.constants.remove(parameter);
            }

            constant::Constant::Phantom
            | constant::Constant::Error(_)
            | constant::Constant::Primitive(_)
            | constant::Constant::Inference(_) => {}

            constant::Constant::Struct(val) => {
                for field in &val.fields {
                    self.check_in_constant(field);
                }
            }

            constant::Constant::Enum(val) => {
                if let Some(value) = &val.associated_value {
                    self.check_in_constant(value);
                }
            }

            constant::Constant::Array(array) => {
                for element in &array.elements {
                    self.check_in_constant(element);
                }
            }

            constant::Constant::Tuple(tuple) => {
                for element in &tuple.elements {
                    self.check_in_constant(&element.term);
                }
            }
        }
    }
}

impl Table<Building<RwLockContainer, Finalizer>> {
    /// Checks if the given `implementation_member_id` has a matching generic
    /// declaration with the `trait_member_id`.
    ///
    /// Assumes that `implementation_member_id` is an implementation member of
    /// the trait member `trait_member_id`, `trait_instantiation` is the
    /// instantiation of the implemented trait.
    ///
    /// The errors (if any) are reported to the handler.
    ///
    /// # Checks
    ///
    /// - Checks if the generic parameters count match.
    /// - Checks if the constant parameters' types match.
    /// - Checks if the predicates are satisfied and exactly the same.
    #[allow(
        clippy::too_many_lines,
        clippy::significant_drop_tightening,
        clippy::significant_drop_in_scrutinee
    )]
    pub fn implementation_member_check(
        &self,
        implementation_member_id: TraitImplementationMemberID,
        trait_member_id: TraitMemberID,
        mut trait_instantiation: Instantiation<Default>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let implementation_member_sym =
            self.get_generic(implementation_member_id.into()).unwrap();
        let trait_member_sym =
            self.get_generic(trait_member_id.into()).unwrap();

        // check generic parameter count check
        Self::check_implementation_member_generic_parameter_count::<
            LifetimeParameter,
        >(
            implementation_member_id,
            trait_member_id,
            &implementation_member_sym.generic_declaration().parameters,
            &trait_member_sym.generic_declaration().parameters,
            handler,
        );

        Self::check_implementation_member_generic_parameter_count::<
            TypeParameter,
        >(
            implementation_member_id,
            trait_member_id,
            &implementation_member_sym.generic_declaration().parameters,
            &trait_member_sym.generic_declaration().parameters,
            handler,
        );

        Self::check_implementation_member_generic_parameter_count::<
            ConstantParameter,
        >(
            implementation_member_id,
            trait_member_id,
            &implementation_member_sym.generic_declaration().parameters,
            &trait_member_sym.generic_declaration().parameters,
            handler,
        );

        trait_instantiation.lifetimes.extend(
            implementation_member_sym
                .generic_declaration()
                .parameters
                .lifetime_order()
                .iter()
                .copied()
                .zip(
                    trait_member_sym
                        .generic_declaration()
                        .parameters
                        .lifetime_order()
                        .iter()
                        .copied(),
                )
                .map(|(im, tr)| {
                    (
                        Lifetime::Parameter(LifetimeParameterID {
                            parent: trait_member_id.into(),
                            id: tr,
                        }),
                        Lifetime::Parameter(LifetimeParameterID {
                            parent: implementation_member_id.into(),
                            id: im,
                        }),
                    )
                }),
        );
        trait_instantiation.types.extend(
            implementation_member_sym
                .generic_declaration()
                .parameters
                .type_order()
                .iter()
                .copied()
                .zip(
                    trait_member_sym
                        .generic_declaration()
                        .parameters
                        .type_order()
                        .iter()
                        .copied(),
                )
                .map(|(im, tr)| {
                    (
                        r#type::Type::Parameter(TypeParameterID {
                            parent: trait_member_id.into(),
                            id: tr,
                        }),
                        r#type::Type::Parameter(TypeParameterID {
                            parent: implementation_member_id.into(),
                            id: im,
                        }),
                    )
                }),
        );
        trait_instantiation.constants.extend(
            implementation_member_sym
                .generic_declaration()
                .parameters
                .constant_order()
                .iter()
                .copied()
                .zip(
                    trait_member_sym
                        .generic_declaration()
                        .parameters
                        .constant_order()
                        .iter()
                        .copied(),
                )
                .map(|(im, tr)| {
                    (
                        constant::Constant::Parameter(ConstantParameterID {
                            parent: trait_member_id.into(),
                            id: tr,
                        }),
                        constant::Constant::Parameter(ConstantParameterID {
                            parent: implementation_member_id.into(),
                            id: im,
                        }),
                    )
                }),
        );

        // check if the constant type matches
        let implementation_member_active_premise =
            self.get_active_premise(implementation_member_id.into()).unwrap();

        let (environment, _) = Environment::new_with(
            implementation_member_active_premise.clone(),
            self,
            normalizer::NO_OP,
            observer::NO_OP,
        );

        for ((tr_const_id, tr_const_param), (im_const_id, im_const_param)) in
            trait_member_sym
                .generic_declaration()
                .parameters
                .constant_parameters_as_order()
                .zip(
                    implementation_member_sym
                        .generic_declaration()
                        .parameters
                        .constant_parameters_as_order(),
                )
        {
            let mut tr_const_ty = tr_const_param.r#type.clone();
            instantiation::instantiate(&mut tr_const_ty, &trait_instantiation);

            match im_const_param.r#type.compatible(
                &tr_const_ty,
                Variance::Covariant,
                &environment,
            ) {
                Ok(Some(_)) => {}

                Err(_) | Ok(None) => handler.receive(Box::new(
                    MismatchedImplementationConstantTypeParameter {
                        implementation_member_constant_parameter_id:
                            ConstantParameterID {
                                parent: implementation_member_id.into(),
                                id: im_const_id,
                            },
                        trait_member_constant_parameter_id: {
                            ConstantParameterID {
                                parent: trait_member_id.into(),
                                id: tr_const_id,
                            }
                        },
                    },
                )),
            }
        }

        // check if the predicates match
        for predicate in &trait_member_sym.generic_declaration().predicates {
            let mut predicate_instantiated = predicate.predicate.clone();
            predicate_instantiated.instantiate(&trait_instantiation);

            for error in environment.predicate_satisfied(
                predicate_instantiated,
                predicate.span.clone(),
                true,
                implementation_member_id.into(),
                handler,
            ) {
                error.report(
                    implementation_member_sym.span().cloned().unwrap(),
                    handler,
                );
            }
        }

        let trait_member_active_premise = {
            let stub = self.get_active_premise(trait_member_id.into()).unwrap();

            Premise {
                query_site: implementation_member_active_premise.query_site,
                predicates: stub
                    .predicates
                    .into_iter()
                    .map(|mut x| {
                        x.instantiate(&trait_instantiation);
                        x
                    })
                    .collect(),
            }
        };

        let (environment, _) = Environment::new_with(
            trait_member_active_premise,
            self,
            normalizer::NO_OP,
            observer::NO_OP,
        );

        // check for any extraneous predicates defined in the implementation
        // member that are not in the trait member
        for predicate_decl in
            &implementation_member_sym.generic_declaration().predicates
        {
            for error in environment.predicate_satisfied(
                predicate_decl.predicate.clone(),
                predicate_decl.span.clone(),
                true,
                trait_member_id.into(),
                handler,
            ) {
                error.report(
                    implementation_member_sym.span().cloned().unwrap(),
                    handler,
                );
            }
        }
    }

    fn check_implementation_member_generic_parameter_count<
        T: GenericParameter,
    >(
        implementation_member_id: TraitImplementationMemberID,
        trait_member_id: TraitMemberID,
        implementation_member_generic_parameters: &GenericParameters,
        trait_member_generic_parameters: &GenericParameters,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        if T::get_generic_parameters_arena(
            implementation_member_generic_parameters,
        )
        .len()
            != T::get_generic_parameters_arena(trait_member_generic_parameters)
                .len()
        {
            handler.receive(Box::new(
                MismatchedGenericParameterCountInImplementation {
                    implementation_member_id,
                    trait_member_id,
                    expected_count: T::get_generic_parameters_arena(
                        trait_member_generic_parameters,
                    )
                    .len(),
                    declared_count: T::get_generic_parameters_arena(
                        implementation_member_generic_parameters,
                    )
                    .len(),
                    generic_kind: T::kind(),
                },
            ));
        }
    }

    /// Checks if the given implementation has a matching signature with its
    /// implemented symbol.
    ///
    /// The errors (if any) are reported to the handler.
    ///
    /// # Checks
    ///
    /// - Checks if all the generic parameters are used in the implementation
    ///   arguments.
    /// - Checks if the implementation's generic arguments satisfy the
    ///   implemented symbol's predicates.
    pub fn implementation_signature_check<
        ParentID: Copy + Into<GlobalID>,
        ImplementedID: Copy + Into<GenericID>,
        Definition: 'static,
    >(
        &self,
        implementation_id: ID<
            GenericTemplate<
                ParentID,
                ImplementationTemplate<ImplementedID, Definition>,
            >,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        GenericTemplate<
            ParentID,
            ImplementationTemplate<ImplementedID, Definition>,
        >: Element,
        ID<
            GenericTemplate<
                ParentID,
                ImplementationTemplate<ImplementedID, Definition>,
            >,
        >: Into<GlobalID> + Into<GenericID>,
    {
        let implementation_sym = self.get(implementation_id).unwrap();

        // check if all the generic parameters are used in the
        // implementation arguments
        let unused_generic_parameters =
            UnusedGenericParameters::get_unused_generic_parameters(
                implementation_id.into(),
                &implementation_sym.generic_declaration.parameters,
                &implementation_sym.arguments,
            );
        unused_generic_parameters
            .report_as_unused_generic_parameters_in_implementation(
                implementation_id,
                handler,
            );

        let implemented_sym =
            self.get_generic(implementation_sym.implemented_id.into()).unwrap();

        // early return if the generic parameters count does not match
        if implementation_sym.arguments.lifetimes.len()
            != implemented_sym
                .generic_declaration()
                .parameters
                .lifetimes()
                .len()
            || implementation_sym.arguments.types.len()
                != implemented_sym
                    .generic_declaration()
                    .parameters
                    .types()
                    .len()
            || implementation_sym.arguments.constants.len()
                != implemented_sym
                    .generic_declaration()
                    .parameters
                    .constants()
                    .len()
        {
            return;
        }

        drop(implemented_sym);

        // check if the signature matches the trait definition
        let premise =
            self.get_active_premise(implementation_id.into()).unwrap();

        let (environment, _) = Environment::new_with(
            premise,
            self,
            normalizer::NO_OP,
            observer::NO_OP,
        );

        environment.check_instantiation_predicates_by_generic_arguments(
            implementation_sym.implemented_id.into(),
            implementation_sym.arguments.clone(),
            implementation_sym.span().as_ref().unwrap(),
            true,
            implementation_id.into(),
            handler,
        );
    }
}

#[cfg(test)]
mod test;
