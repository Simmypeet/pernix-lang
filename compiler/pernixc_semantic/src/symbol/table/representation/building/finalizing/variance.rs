//! Contains the code related to resolving the correct [`Variance`] for each
//! generic parameters defined in the ADTs.

use pernixc_base::diagnostic::Handler;

use crate::{
    error::Error,
    symbol::{
        table::{State, Table},
        GenericID, GenericParameterVariances, GenericParameters,
        LifetimeParameterID, TypeParameterID, Variance,
    },
    type_system::{
        environment::Environment,
        equality::Equality,
        model::Default,
        normalizer, observer,
        sub_term::TermLocation,
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        variance::GetVarianceError,
        visitor, Compute, Premise,
    },
};

struct TermCollector<'a, Term, T: State> {
    target: &'a Term,
    locations: Vec<Vec<TermLocation>>,
    environment:
        &'a Environment<'a, Default, T, normalizer::NoOp, observer::NoOp>,
}

macro_rules! implements_visitor {
    ($first_term:ty, $second_term:ty) => {
        impl<'a, 'v, T: State> visitor::Recursive<'v, $first_term>
            for TermCollector<'a, $second_term, T>
        {
            fn visit(
                &mut self,
                _: &'v $first_term,
                _: impl Iterator<Item = TermLocation>,
            ) -> bool {
                true
            }
        }
    };
}

implements_visitor!(Lifetime<Default>, Type<Default>);
implements_visitor!(Lifetime<Default>, Constant<Default>);
implements_visitor!(Type<Default>, Lifetime<Default>);
implements_visitor!(Type<Default>, Constant<Default>);
implements_visitor!(Constant<Default>, Lifetime<Default>);
implements_visitor!(Constant<Default>, Type<Default>);

impl<'a, 'v, U: Term<Model = Default>, T: State> visitor::Recursive<'v, U>
    for TermCollector<'a, U, T>
{
    fn visit(
        &mut self,
        term: &U,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if let Ok(Some(_)) = Equality::new(term.clone(), self.target.clone())
            .query(self.environment)
        {
            self.locations.push(locations.collect());
        }

        true
    }
}

fn get_variance_for<U: Term, T: State>(
    term: &U,
    respect_to_type: &Type<Default>,
    environment: &Environment<Default, T, normalizer::NoOp, observer::NoOp>,
) -> Option<Variance>
where
    for<'a, 'v> TermCollector<'a, U, T>: visitor::Recursive<'v, Lifetime<Default>>
        + visitor::Recursive<'v, Type<Default>>
        + visitor::Recursive<'v, Constant<Default>>,
{
    let locations = get_all_term_locations(term, respect_to_type, environment);

    let mut current_variance: Option<Variance> = None;

    for locations in locations {
        match environment.get_variance_of(
            respect_to_type,
            Variance::Covariant,
            locations.into_iter(),
        ) {
            // successfully get the variance
            Ok(variance) => match current_variance {
                Some(current_variance_unwrap) => {
                    current_variance =
                        Some(current_variance_unwrap.combine(variance));
                }
                None => current_variance = Some(variance),
            },

            Err(error) => {
                // the only possible error should be `NoVarianceInfo`
                assert!(matches!(error, GetVarianceError::NoVarianceInfo(_)));
            }
        }
    }

    current_variance
}

impl<T: State> Table<T> {
    #[allow(clippy::needless_pass_by_value, clippy::too_many_arguments)]
    pub(super) fn build_variance<'a>(
        &self,
        generic_parameters: &GenericParameters,
        generic_parameter_variances: &mut GenericParameterVariances,
        active_premise: Premise<Default>,
        generic_id: GenericID,
        type_usages: impl Iterator<Item = &'a Type<Default>> + Clone,
        partial_variance: bool,
        _: &dyn Handler<Box<dyn Error>>,
    ) {
        let (environment, _) = Environment::new_with(
            active_premise,
            self,
            &normalizer::NO_OP,
            &observer::NO_OP,
        );

        for (id, _) in generic_parameters.lifetime_parameters_as_order() {
            let lifetime_term = Lifetime::Parameter(LifetimeParameterID {
                parent: generic_id,
                id,
            });

            for ty in type_usages.clone() {
                let variance =
                    get_variance_for(&lifetime_term, ty, &environment);
                {
                    match (
                        generic_parameter_variances
                            .variances_by_lifetime_ids
                            .get_mut(&id),
                        variance,
                    ) {
                        (None, None) => {
                            if !partial_variance {
                                assert!(generic_parameter_variances
                                    .variances_by_lifetime_ids
                                    .insert(id, Variance::Covariant)
                                    .is_none());
                            }
                        }

                        (None, Some(variance)) => {
                            assert!(generic_parameter_variances
                                .variances_by_lifetime_ids
                                .insert(id, variance)
                                .is_none());
                        }

                        (Some(current), Some(variance)) => {
                            *current = current.combine(variance);
                        }

                        (Some(_), None) => {}
                    }
                }
            }
        }

        for (id, _) in generic_parameters.type_parameters_as_order() {
            let type_term =
                Type::Parameter(TypeParameterID { parent: generic_id, id });

            for ty in type_usages.clone() {
                let variance = get_variance_for(&type_term, ty, &environment);

                match (
                    generic_parameter_variances
                        .variances_by_type_ids
                        .get_mut(&id),
                    variance,
                ) {
                    (None, None) => {
                        if !partial_variance {
                            assert!(generic_parameter_variances
                                .variances_by_type_ids
                                .insert(id, Variance::Covariant)
                                .is_none());
                        }
                    }

                    (None, Some(variance)) => {
                        assert!(generic_parameter_variances
                            .variances_by_type_ids
                            .insert(id, variance)
                            .is_none());
                    }

                    (Some(current), Some(variance)) => {
                        *current = current.combine(variance);
                    }

                    (Some(_), None) => {}
                }
            }
        }
    }
}

fn get_all_term_locations<Term: visitor::Element, T: State>(
    target_term: &Term,
    respect_to_type: &Type<Default>,
    environment: &Environment<Default, T, normalizer::NoOp, observer::NoOp>,
) -> Vec<Vec<TermLocation>>
where
    for<'a, 'v> TermCollector<'a, Term, T>: visitor::Recursive<'v, Lifetime<Default>>
        + visitor::Recursive<'v, Type<Default>>
        + visitor::Recursive<'v, Constant<Default>>,
{
    let mut collector = TermCollector {
        target: target_term,
        locations: Vec::new(),

        environment,
    };

    visitor::accept_recursive(respect_to_type, &mut collector);

    collector.locations
}

// #[cfg(test)]
// mod tests;
