//! Contains the code related to checking the well-formedness of instantiation.

use std::collections::BTreeSet;

use enum_as_inner::EnumAsInner;
use pernixc_abort::Abort;
use pernixc_semantic::{
    component::{
        derived::{
            implied_predicates::ImpliedPredicates, variances::Variance,
            where_clause::WhereClause,
        },
        input::SymbolKind,
    },
    table::{GlobalID, Table},
    term::{
        generic_arguments::GenericArguments,
        instantiation::{self, Instantiation},
        lifetime::Lifetime,
        predicate::{Outlives, Predicate},
        r#type::Type,
        visitor::RecursiveIterator,
        Model,
    },
};
use pernixc_source_file::Span;

use super::{
    compatible::{Compatibility, Compatible},
    environment::Environment,
    normalizer::Normalizer,
};
use crate::{
    predicate::{
        NegativeMarkerSatisfied, NegativeTraitSatisfied,
        PositiveMarkerSatisfied, PositiveTraitSatisfied,
    },
    resolution::Implementation,
    LifetimeConstraint, OverflowError, Succeeded,
};

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

    /// The overflow error that occurred.
    pub overflow_error: OverflowError,
}

/// The implementation's lifetime arguments are not general enough to satisfied
/// the predicate having for-all lifetimes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementationIsNotGeneralEnough<M: Model> {
    /// The resolved implementation.
    pub resolved_implementation: Implementation<M>,

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

#[allow(clippy::type_complexity)]
fn get_all_predicates<M: Model>(
    table: &Table,
    global_id: GlobalID,
    instantiation: Option<&Instantiation<M>>,
) -> Result<Vec<(Predicate<M>, Option<Span>)>, Abort> {
    let symbol_kind = *table.get::<SymbolKind>(global_id);
    let mut predicates = Vec::new();

    if symbol_kind.has_where_clause() {
        let where_cluase = table.query::<WhereClause>(global_id)?;

        for predicate in &where_cluase.predicates {
            predicates.push((
                Predicate::from_other_model(predicate.predicate.clone()),
                predicate.span.clone(),
            ));
        }
    }

    if symbol_kind.has_implied_predicates() {
        let implied_predicates = table.query::<ImpliedPredicates>(global_id)?;

        for predicate in &implied_predicates.implied_predicates {
            predicates.push((
                Predicate::from_other_model(predicate.clone().into()),
                None,
            ));
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
/// # Errors
///
/// Returns [`Abort`] returned when querying components from the table.
#[allow(clippy::type_complexity)]
pub fn check<M: Model>(
    generic_id: GlobalID,
    instantiation: &instantiation::Instantiation<M>,
    do_outlives_check: bool,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<(BTreeSet<LifetimeConstraint<M>>, Vec<Error<M>>), Abort> {
    let predicates = get_all_predicates(
        environment.table(),
        generic_id,
        Some(instantiation),
    )?;

    let mut lifetime_constraints = BTreeSet::new();
    let mut errors = Vec::new();

    for (predicate, span) in predicates {
        let (new_lifetime_constraints, new_erros) = predicate_satisfied(
            predicate,
            span,
            do_outlives_check,
            environment,
        )?;

        lifetime_constraints.extend(new_lifetime_constraints);
        errors.extend(new_erros);
    }

    Ok((lifetime_constraints, errors))
}

#[allow(clippy::too_many_arguments, clippy::type_complexity)]
fn check_implementation_satisfied<M: Model>(
    id: GlobalID,
    instantiation: &Instantiation<M>,
    generic_arguments: &GenericArguments<M>,
    predicate_declaration_span: Option<Span>,
    do_outlives_check: bool,
    mut is_not_general_enough: bool,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<(BTreeSet<LifetimeConstraint<M>>, Vec<Error<M>>), Abort> {
    let mut lifetime_constraints = BTreeSet::new();
    let mut errors = Vec::new();

    // check for each predicate in the implementation
    for (mut predicate, span) in
        get_all_predicates(environment.table(), id, Some(instantiation))?
    {
        match &predicate {
            Predicate::LifetimeOutlives(outlives) => {
                if outlives.operand.is_forall() {
                    is_not_general_enough = true;
                    continue;
                }
            }
            Predicate::TypeOutlives(outlives) => {
                if RecursiveIterator::new(&outlives.operand)
                    .any(|x| x.0.as_lifetime().is_some_and(|x| x.is_forall()))
                {
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

            Predicate::TypeOutlives(outlives) if outlives.bound.is_forall() => {
                outlives.bound = Lifetime::Static;
            }

            _ => {}
        }

        let (new_lifetime_constraints, new_erros) = predicate_satisfied(
            predicate,
            span,
            do_outlives_check,
            environment,
        )?;

        lifetime_constraints.extend(new_lifetime_constraints);
        errors.extend(new_erros);
    }

    if is_not_general_enough {
        errors.push(Error::ImplementationIsNotGeneralEnough(
            ImplementationIsNotGeneralEnough {
                resolved_implementation: Implementation {
                    instantiation: instantiation.clone(),
                    id,
                    is_not_general_enough,
                },
                generic_arguments: generic_arguments.clone(),
                predicate_declaration_span,
            },
        ));
    }

    Ok((lifetime_constraints, errors))
}

#[allow(clippy::type_complexity)]
fn handle_positive_marker_satisfied<M: Model>(
    result: &PositiveMarkerSatisfied<M>,
    pred_generic_arguments: &GenericArguments<M>,
    predicate_declaration_span: Option<Span>,
    do_outlives_check: bool,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<(BTreeSet<LifetimeConstraint<M>>, Vec<Error<M>>), Abort> {
    match result {
        PositiveMarkerSatisfied::Premise
        | PositiveMarkerSatisfied::Environment
        | PositiveMarkerSatisfied::Cyclic => Ok((BTreeSet::new(), Vec::new())),

        PositiveMarkerSatisfied::Implementation(implementation) => {
            check_implementation_satisfied(
                implementation.id,
                &implementation.instantiation,
                pred_generic_arguments,
                predicate_declaration_span,
                do_outlives_check,
                implementation.is_not_general_enough,
                environment,
            )
        }

        PositiveMarkerSatisfied::Congruence(btree_map) => {
            let mut constraints = BTreeSet::new();
            let mut pred_errors = Vec::new();

            for result in btree_map.values() {
                constraints.extend(result.constraints.iter().cloned());

                let (new_constraints, new_pred_errors) =
                    handle_positive_marker_satisfied(
                        &result.result,
                        pred_generic_arguments,
                        predicate_declaration_span.clone(),
                        do_outlives_check,
                        environment,
                    )?;

                constraints.extend(new_constraints);
                pred_errors.extend(new_pred_errors);
            }

            Ok((constraints, pred_errors))
        }
    }
}

/// Checks if the given `predicate` is satisfied in the given `environment`.
///
/// # Errors
///
/// Returns [`Abort`] returned when querying components from the
/// table.
#[allow(clippy::too_many_lines, clippy::type_complexity)]
pub fn predicate_satisfied<M: Model>(
    predicate: Predicate<M>,
    predicate_declaration_span: Option<Span>,
    do_outlives_check: bool,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<(BTreeSet<LifetimeConstraint<M>>, Vec<Error<M>>), Abort> {
    let (result, mut extra_predicate_error) = match &predicate {
        Predicate::TraitTypeCompatible(equality) => {
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

                    Err(error) => Err(error),
                },
                Vec::new(),
            )
        }
        Predicate::ConstantType(constant_type) => (
            environment.query(constant_type).map(|x| x.as_deref().cloned()),
            Vec::new(),
        ),
        Predicate::LifetimeOutlives(outlives) => {
            if do_outlives_check {
                match environment.query(outlives) {
                    Ok(Some(_)) => return Ok((BTreeSet::new(), Vec::new())),

                    Ok(None) => {
                        return Ok((BTreeSet::new(), vec![Error::Unsatisfied(
                            Unsatisfied {
                                predicate,
                                predicate_declaration_span,
                            },
                        )]))
                    }
                    Err(crate::Error::Overflow(overflow_error)) => {
                        return Ok((BTreeSet::new(), vec![Error::Undecidable(
                            Undecidable {
                                predicate,
                                predicate_declaration_span,
                                overflow_error,
                            },
                        )]))
                    }

                    Err(crate::Error::Abort(error)) => return Err(error),
                }
            }
            (
                Ok(Some(Succeeded::satisfied_with(
                    std::iter::once(LifetimeConstraint::LifetimeOutlives(
                        outlives.clone(),
                    ))
                    .collect(),
                ))),
                Vec::new(),
            )
        }
        Predicate::TypeOutlives(outlives) => {
            if do_outlives_check {
                match environment.query(outlives) {
                    Ok(Some(_)) => return Ok((BTreeSet::new(), Vec::new())),

                    Ok(None) => {
                        return Ok((BTreeSet::new(), vec![Error::Unsatisfied(
                            Unsatisfied {
                                predicate,
                                predicate_declaration_span,
                            },
                        )]))
                    }

                    Err(crate::Error::Overflow(overflow_error)) => {
                        return Ok((BTreeSet::new(), vec![Error::Undecidable(
                            Undecidable {
                                predicate,
                                predicate_declaration_span,
                                overflow_error,
                            },
                        )]))
                    }

                    Err(crate::Error::Abort(error)) => return Err(error),
                }
            }

            (
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
                ))),
                Vec::new(),
            )
        }

        Predicate::TupleType(tuple) => (
            environment.query(tuple).map(|x| x.as_deref().cloned()),
            Vec::new(),
        ),

        Predicate::PositiveTrait(positive) => match environment.query(positive)
        {
            Ok(None) => (Ok(None), Vec::new()),
            Ok(Some(result)) => match &result.result {
                PositiveTraitSatisfied::Premise
                | PositiveTraitSatisfied::Environment
                | PositiveTraitSatisfied::Cyclic => (
                    Ok(Some(Succeeded::satisfied_with(
                        result.constraints.clone(),
                    ))),
                    Vec::new(),
                ),

                PositiveTraitSatisfied::Implementation(implementation) => {
                    let (mut lt_constraints, errors) =
                        check_implementation_satisfied(
                            implementation.id,
                            &implementation.instantiation,
                            &positive.generic_arguments,
                            predicate_declaration_span.clone(),
                            do_outlives_check,
                            implementation.is_not_general_enough,
                            environment,
                        )?;
                    lt_constraints.extend(result.constraints.iter().cloned());

                    (
                        Ok(Some(Succeeded::satisfied_with(lt_constraints))),
                        errors,
                    )
                }
            },
            Err(error) => (Err(error), Vec::new()),
        },

        Predicate::NegativeTrait(negative) => match environment.query(negative)
        {
            Ok(None) => (Ok(None), Vec::new()),
            Ok(Some(result)) => match &result.result {
                NegativeTraitSatisfied::UnsatisfiedPositive
                | NegativeTraitSatisfied::Premise => (
                    Ok(Some(Succeeded::satisfied_with(
                        result.constraints.clone(),
                    ))),
                    Vec::new(),
                ),
                NegativeTraitSatisfied::Implementation(implementation) => {
                    let (mut lt_constraints, errors) =
                        check_implementation_satisfied(
                            implementation.id,
                            &implementation.instantiation,
                            &negative.generic_arguments,
                            predicate_declaration_span.clone(),
                            do_outlives_check,
                            implementation.is_not_general_enough,
                            environment,
                        )?;
                    lt_constraints.extend(result.constraints.iter().cloned());

                    (
                        Ok(Some(Succeeded::satisfied_with(lt_constraints))),
                        errors,
                    )
                }
            },

            Err(overflow) => (Err(overflow), Vec::new()),
        },

        Predicate::PositiveMarker(positive) => {
            match environment.query(positive) {
                Ok(None) => (Ok(None), Vec::new()),

                Ok(Some(result)) => {
                    let (new_constraints, pred_errors) =
                        handle_positive_marker_satisfied(
                            &result.result,
                            &positive.generic_arguments,
                            predicate_declaration_span.clone(),
                            do_outlives_check,
                            environment,
                        )?;

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
        Predicate::NegativeMarker(negative) => {
            match environment.query(negative) {
                Ok(Some(result)) => match &result.result {
                    NegativeMarkerSatisfied::UnsatisfiedPositive
                    | NegativeMarkerSatisfied::Premise => (
                        Ok(Some(Succeeded::satisfied_with(
                            result.constraints.clone(),
                        ))),
                        Vec::new(),
                    ),

                    NegativeMarkerSatisfied::Implementation(implementation) => {
                        let (mut lt_constraints, errors) =
                            check_implementation_satisfied(
                                implementation.id,
                                &implementation.instantiation,
                                &negative.generic_arguments,
                                predicate_declaration_span.clone(),
                                do_outlives_check,
                                implementation.is_not_general_enough,
                                environment,
                            )?;

                        lt_constraints
                            .extend(result.constraints.iter().cloned());

                        (
                            Ok(Some(Succeeded::satisfied_with(lt_constraints))),
                            errors,
                        )
                    }
                },
                Ok(None) => (Ok(None), Vec::new()),
                Err(error) => (Err(error), Vec::new()),
            }
        }
    };

    match result {
        Ok(Some(Succeeded { constraints, .. })) => {
            // if do_outlives_check is false, then we don't need to check
            if !do_outlives_check {
                return Ok((constraints, extra_predicate_error));
            }

            for constraint in constraints {
                match constraint {
                    LifetimeConstraint::LifetimeOutlives(pred) => {
                        match environment.query(&pred) {
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

                            Err(crate::Error::Abort(error)) => {
                                return Err(error);
                            }

                            Err(crate::Error::Overflow(overflow_error)) => {
                                extra_predicate_error.push(Error::Undecidable(
                                    Undecidable {
                                        predicate: Predicate::LifetimeOutlives(
                                            pred,
                                        ),

                                        predicate_declaration_span: None,
                                        overflow_error,
                                    },
                                ));
                            }

                            Ok(Some(_)) => {}
                        }
                    }
                }
            }

            Ok((BTreeSet::new(), extra_predicate_error))
        }

        Ok(None) => {
            extra_predicate_error.push(Error::Unsatisfied(Unsatisfied {
                predicate,
                predicate_declaration_span,
            }));

            Ok((BTreeSet::new(), extra_predicate_error))
        }

        Err(crate::Error::Overflow(overflow_error)) => {
            extra_predicate_error.push(Error::Undecidable(Undecidable {
                predicate,
                predicate_declaration_span,
                overflow_error,
            }));

            Ok((BTreeSet::new(), extra_predicate_error))
        }

        Err(crate::Error::Abort(error)) => Err(error),
    }
}
