//! Contains the code related to checking the well-formedness of instantiation.

use std::collections::BTreeSet;

use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;

use super::{
    compatible::{Compatibility, Compatible},
    environment::Environment,
    instantiation::{self, Instantiation},
    model::Model,
    normalizer::Normalizer,
    observer::Observer,
    predicate::{
        self, NegativeMarkerSatisfied, NegativeTraitSatisfied, Outlives,
        PositiveMarkerSatisfied, PositiveTraitSatisfied, Predicate,
    },
    term::{r#type::Type, GenericArguments},
    variance::Variance,
    visitor::RecursiveIterator,
    Compute, LifetimeConstraint, OverflowError, Satisfied, Succeeded,
};
use crate::symbol::{table, GenericID, ResolvableImplementationID};

/// Representing an unsatisfied in where clause predicate.csjjj
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unsatisfied<M: Model> {
    /// The predicate that was unsatisfied.
    pub predicate: Predicate<M>,

    /// The span where the where clause predicate was declared.
    pub predicate_declaration_span: Option<Span>,
}

/// The satisfiability of the predicate can't be decided (most likely Overflow
/// error).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Undecidable<M: Model> {
    /// The undecidable predicate.
    pub predicate: Predicate<M>,

    /// The span where the where clause predicate was declared.
    pub predicate_declaration_span: Option<Span>,
}

/// The implementation's lifetime arguments are not general enough to satisfied
/// the predicate having for-all lifetimes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementationIsNotGeneralEnough<M: Model> {
    /// The resolved implementation.
    pub resolved_implementation:
        predicate::Implementation<ResolvableImplementationID, M>,

    /// The generic arguments of the predicate.
    pub generic_arguments: GenericArguments<M>,

    /// The span where the where clause predicate was declared.
    pub predicate_declaration_span: Option<Span>,
}

/// Represents an error found while checking for the well-formedness.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Error<M: Model> {
    Unsatisfied(Unsatisfied<M>),
    Undecidable(Undecidable<M>),
    ImplementationIsNotGeneralEnough(ImplementationIsNotGeneralEnough<M>),
}

/// Checks the where clause predicate requirements declared in the given
/// `generic_id`
///
/// This doesn't include the additional requirements such as checking trait
/// predicate satisfiabiltiy if the `generic_id` is [`GenericID::Trait`].
pub fn check<M: Model, T: table::State>(
    generic_id: GenericID,
    instantiation: &instantiation::Instantiation<M>,
    do_outlives_check: bool,
    environment: &Environment<M, T, impl Normalizer<M, T>, impl Observer<M, T>>,
) -> (BTreeSet<LifetimeConstraint<M>>, Vec<Error<M>>) {
    let predicates = environment
        .table()
        .get_generic(generic_id)
        .unwrap()
        .generic_declaration()
        .predicates
        .iter()
        .map(|x| {
            let mut predicate =
                Predicate::from_default_model(x.predicate.clone());

            predicate.instantiate(instantiation);

            (predicate, x.span.clone())
        })
        .collect::<Vec<_>>();

    let mut lifetime_constraints = BTreeSet::new();
    let mut errors = Vec::new();

    for (predicate, span) in predicates {
        let (new_lifetime_constraints, new_erros) = predicate_satisfied(
            predicate,
            span,
            do_outlives_check,
            environment,
        );

        lifetime_constraints.extend(new_lifetime_constraints);
        errors.extend(new_erros);
    }

    (lifetime_constraints, errors)
}

#[allow(clippy::too_many_arguments)]
fn check_implementation_satisfied<M: Model, T: table::State>(
    id: ResolvableImplementationID,
    instantiation: &Instantiation<M>,
    generic_arguments: &GenericArguments<M>,
    predicate_declaration_span: Option<Span>,
    do_outlives_check: bool,
    is_not_general_enough: bool,
    environment: &Environment<M, T, impl Normalizer<M, T>, impl Observer<M, T>>,
) -> (BTreeSet<LifetimeConstraint<M>>, Vec<Error<M>>) {
    let mut lifetime_constraints = BTreeSet::new();
    let mut errors = Vec::new();

    if is_not_general_enough {
        errors.push(Error::ImplementationIsNotGeneralEnough(
            ImplementationIsNotGeneralEnough {
                resolved_implementation: predicate::Implementation {
                    instantiation: instantiation.clone(),
                    id,
                    is_not_general_enough,
                },
                generic_arguments: generic_arguments.clone(),
                predicate_declaration_span,
            },
        ));
    }

    let predicates = environment
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

        let (new_lifetime_constraints, new_erros) = predicate_satisfied(
            predicate_instantiated,
            predicate.span,
            do_outlives_check,
            environment,
        );

        lifetime_constraints.extend(new_lifetime_constraints);
        errors.extend(new_erros);
    }

    (lifetime_constraints, errors)
}

fn handle_positive_marker_satisfied<M: Model, T: table::State>(
    result: PositiveMarkerSatisfied<M>,
    pred_generic_arguments: &GenericArguments<M>,
    predicate_declaration_span: Option<Span>,
    do_outlives_check: bool,
    environment: &Environment<M, T, impl Normalizer<M, T>, impl Observer<M, T>>,
) -> (BTreeSet<LifetimeConstraint<M>>, Vec<Error<M>>) {
    match result {
        PositiveMarkerSatisfied::ByPremise
        | PositiveMarkerSatisfied::ByEnvironment
        | PositiveMarkerSatisfied::ByCyclic => (BTreeSet::new(), Vec::new()),

        PositiveMarkerSatisfied::ByImplementation(implementation) => {
            check_implementation_satisfied(
                implementation.id.into(),
                &implementation.instantiation,
                pred_generic_arguments,
                predicate_declaration_span,
                do_outlives_check,
                implementation.is_not_general_enough,
                environment,
            )
        }

        PositiveMarkerSatisfied::ByCongruence(btree_map) => {
            let mut constraints = BTreeSet::new();
            let mut pred_errors = Vec::new();

            for (_, result) in btree_map {
                constraints.extend(result.constraints);

                let (new_constraints, new_pred_errors) =
                    handle_positive_marker_satisfied(
                        result.result,
                        pred_generic_arguments,
                        predicate_declaration_span.clone(),
                        do_outlives_check,
                        environment,
                    );

                constraints.extend(new_constraints);
                pred_errors.extend(new_pred_errors);
            }

            (constraints, pred_errors)
        }
    }
}

/// Checks if the given `predicate` is satisfied in the given `environment`.
pub fn predicate_satisfied<M: Model, T: table::State>(
    predicate: Predicate<M>,
    predicate_declaration_span: Option<Span>,
    do_outlives_check: bool,
    environment: &Environment<M, T, impl Normalizer<M, T>, impl Observer<M, T>>,
) -> (BTreeSet<LifetimeConstraint<M>>, Vec<Error<M>>) {
    let (result, mut extra_predicate_error) = match &predicate {
        Predicate::TraitTypeEquality(equality) => {
            let result = Type::TraitMember(equality.lhs.clone()).compatible(
                &equality.rhs,
                Variance::Covariant,
                environment,
            );

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
        Predicate::ConstantType(constant_type) => {
            (constant_type.query(environment), Vec::new())
        }
        Predicate::LifetimeOutlives(outlives) => {
            if !do_outlives_check {
                (
                    Ok(Some(Succeeded::satisfied_with(
                        std::iter::once(LifetimeConstraint::LifetimeOutlives(
                            outlives.clone(),
                        ))
                        .collect(),
                    ))),
                    Vec::new(),
                )
            } else {
                match outlives.query(environment) {
                    Ok(Some(Satisfied)) => {
                        return (BTreeSet::new(), Vec::new())
                    }

                    Ok(None) => {
                        return (BTreeSet::new(), vec![Error::Unsatisfied(
                            Unsatisfied {
                                predicate,
                                predicate_declaration_span,
                            },
                        )])
                    }
                    Err(OverflowError) => {
                        return (BTreeSet::new(), vec![Error::Undecidable(
                            Undecidable {
                                predicate,
                                predicate_declaration_span,
                            },
                        )])
                    }
                }
            }
        }
        Predicate::TypeOutlives(outlives) => {
            if !do_outlives_check {
                (
                    Ok(Some(Succeeded::satisfied_with(
                        RecursiveIterator::new(&outlives.bound)
                            .filter_map(|x| x.0.into_lifetime().ok())
                            .map(|x| {
                                LifetimeConstraint::LifetimeOutlives(
                                    Outlives::new(
                                        x.clone(),
                                        outlives.bound.clone(),
                                    ),
                                )
                            })
                            .collect(),
                    ))),
                    Vec::new(),
                )
            } else {
                match outlives.query(environment) {
                    Ok(Some(Satisfied)) => {
                        return (BTreeSet::new(), Vec::new())
                    }

                    Ok(None) => {
                        return (BTreeSet::new(), vec![Error::Unsatisfied(
                            Unsatisfied {
                                predicate,
                                predicate_declaration_span,
                            },
                        )])
                    }
                    Err(OverflowError) => {
                        return (BTreeSet::new(), vec![Error::Undecidable(
                            Undecidable {
                                predicate,
                                predicate_declaration_span,
                            },
                        )])
                    }
                }
            }
        }
        Predicate::TupleType(tuple) => (tuple.query(environment), Vec::new()),
        Predicate::PositiveTrait(positive) => match positive.query(environment)
        {
            Ok(None) => (Ok(None), Vec::new()),
            Ok(Some(Succeeded { result, constraints })) => match result {
                PositiveTraitSatisfied::ByCyclic
                | PositiveTraitSatisfied::ByPremise
                | PositiveTraitSatisfied::ByEnvironment => (
                    Ok(Some(Succeeded::satisfied_with(constraints))),
                    Vec::new(),
                ),

                PositiveTraitSatisfied::ByImplementation(implementation) => {
                    let (mut lt_constraints, errors) =
                        check_implementation_satisfied(
                            implementation.id.into(),
                            &implementation.instantiation,
                            &positive.generic_arguments,
                            predicate_declaration_span.clone(),
                            do_outlives_check,
                            implementation.is_not_general_enough,
                            environment,
                        );
                    lt_constraints.extend(constraints);

                    (
                        Ok(Some(Succeeded::satisfied_with(lt_constraints))),
                        errors,
                    )
                }
            },
            Err(OverflowError) => (Err(OverflowError), Vec::new()),
        },
        Predicate::NegativeTrait(negative) => match negative.query(environment)
        {
            Ok(None) => (Ok(None), Vec::new()),
            Ok(Some(Succeeded { result, constraints })) => match result {
                NegativeTraitSatisfied::ByPremise
                | NegativeTraitSatisfied::ByUnsatisfiedPositive => (
                    Ok(Some(Succeeded::satisfied_with(constraints))),
                    Vec::new(),
                ),

                NegativeTraitSatisfied::ByImplementation(implementation) => {
                    let (mut lt_constraints, errors) =
                        check_implementation_satisfied(
                            implementation.id.into(),
                            &implementation.instantiation,
                            &negative.generic_arguments,
                            predicate_declaration_span.clone(),
                            do_outlives_check,
                            implementation.is_not_general_enough,
                            environment,
                        );
                    lt_constraints.extend(constraints);

                    (
                        Ok(Some(Succeeded::satisfied_with(lt_constraints))),
                        errors,
                    )
                }
            },
            Err(OverflowError) => (Err(OverflowError), Vec::new()),
        },
        Predicate::PositiveMarker(positive) => {
            match positive.query(environment) {
                Ok(None) => (Ok(None), Vec::new()),

                Ok(Some(Succeeded { result, mut constraints })) => {
                    let (new_constraints, pred_errors) =
                        handle_positive_marker_satisfied(
                            result,
                            &positive.generic_arguments,
                            predicate_declaration_span.clone(),
                            do_outlives_check,
                            environment,
                        );

                    constraints.extend(new_constraints);

                    (
                        Ok(Some(Succeeded::satisfied_with(constraints))),
                        pred_errors,
                    )
                }

                Err(OverflowError) => (Err(OverflowError), Vec::new()),
            }
        }
        Predicate::NegativeMarker(negative) => {
            match negative.query(environment) {
                Ok(Some(Succeeded { result, constraints })) => match result {
                    NegativeMarkerSatisfied::ByUnsatisfiedPositive
                    | NegativeMarkerSatisfied::ByPremise => (
                        Ok(Some(Succeeded::satisfied_with(constraints))),
                        Vec::new(),
                    ),

                    NegativeMarkerSatisfied::ByImplementation(
                        implementation,
                    ) => {
                        let (mut lt_constraints, errors) =
                            check_implementation_satisfied(
                                implementation.id.into(),
                                &implementation.instantiation,
                                &negative.generic_arguments,
                                predicate_declaration_span.clone(),
                                do_outlives_check,
                                implementation.is_not_general_enough,
                                environment,
                            );
                        lt_constraints.extend(constraints);

                        (
                            Ok(Some(Succeeded::satisfied_with(lt_constraints))),
                            errors,
                        )
                    }
                },
                Ok(None) => (Ok(None), Vec::new()),
                Err(OverflowError) => (Err(OverflowError), Vec::new()),
            }
        }
    };

    match result {
        Ok(Some(Succeeded { result: Satisfied, constraints })) => {
            // if do_outlives_check is false, then we don't need to check
            if !do_outlives_check {
                return (constraints, extra_predicate_error);
            }

            for constraint in constraints {
                match constraint {
                    LifetimeConstraint::LifetimeOutlives(pred) => {
                        match pred.query(environment) {
                            Ok(None) => {
                                extra_predicate_error.push(Error::Unsatisfied(
                                    Unsatisfied {
                                        predicate: Predicate::LifetimeOutlives(
                                            pred,
                                        ),

                                        predicate_declaration_span: None,
                                    },
                                ));
                            }
                            Err(_) => {
                                extra_predicate_error.push(Error::Undecidable(
                                    Undecidable {
                                        predicate: Predicate::LifetimeOutlives(
                                            pred,
                                        ),

                                        predicate_declaration_span: None,
                                    },
                                ));
                            }

                            Ok(Some(_)) => {}
                        }
                    }
                }
            }

            (BTreeSet::new(), extra_predicate_error)
        }

        Ok(None) => {
            extra_predicate_error.push(Error::Unsatisfied(Unsatisfied {
                predicate,
                predicate_declaration_span,
            }));

            (BTreeSet::new(), extra_predicate_error)
        }

        Err(OverflowError) => {
            extra_predicate_error.push(Error::Undecidable(Undecidable {
                predicate,
                predicate_declaration_span,
            }));

            (BTreeSet::new(), extra_predicate_error)
        }
    }
}
