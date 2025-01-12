//! Contains the logic for simplifying the type term.

use std::{collections::BTreeSet, sync::Arc};

use super::{
    model::Model,
    normalizer::Normalizer,
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, ModelOf, Term,
    },
    visitor::Mutable,
    AbruptError, Environment, LifetimeConstraint, Succeeded,
};
use crate::type_system::{
    mapping::Mapping, predicate::Outlives, unification::Unification,
    LifetimeUnifyingPredicate,
};

pub(super) trait Simplify: ModelOf + Sized {
    #[allow(private_interfaces)]
    fn simplified_mut(
        simplified: &mut Simplified<Self::Model>,
    ) -> &mut BTreeSet<Self>;
}

impl<M: Model> Simplify for Lifetime<M> {
    #[allow(private_interfaces)]
    fn simplified_mut(simplified: &mut Simplified<M>) -> &mut BTreeSet<Self> {
        &mut simplified.lifetimes
    }
}

impl<M: Model> Simplify for Type<M> {
    #[allow(private_interfaces)]
    fn simplified_mut(simplified: &mut Simplified<M>) -> &mut BTreeSet<Self> {
        &mut simplified.types
    }
}

impl<M: Model> Simplify for Constant<M> {
    #[allow(private_interfaces)]
    fn simplified_mut(simplified: &mut Simplified<M>) -> &mut BTreeSet<Self> {
        &mut simplified.constants
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct Simplified<M: Model> {
    lifetimes: BTreeSet<Lifetime<M>>,
    types: BTreeSet<Type<M>>,
    constants: BTreeSet<Constant<M>>,
}

struct Visitor<'e, 't, 's, N: Normalizer<M>, M: Model> {
    environment: &'e Environment<'t, M, N>,
    simplified: &'s mut Simplified<M>,
    lifetime_constraints: BTreeSet<LifetimeConstraint<M>>,
    abrupt_error: Option<AbruptError>,
}

impl<'e, 't, 's, U: Term, N: Normalizer<U::Model>> Mutable<U>
    for Visitor<'e, 't, 's, N, U::Model>
{
    fn visit(&mut self, term: &mut U, _: U::Location) -> bool {
        if self.abrupt_error.is_some() {
            return false;
        }

        match simplify_internal(term, self.environment, self.simplified) {
            Ok(Some(Succeeded { result, constraints })) => {
                *term = result;
                self.lifetime_constraints.extend(constraints);

                true
            }

            Ok(None) => true,

            Err(err) => {
                self.abrupt_error = Some(err);
                false
            }
        }
    }
}

#[allow(clippy::too_many_lines)]
fn simplify_internal<T: Term>(
    term: &T,
    environment: &Environment<T::Model, impl Normalizer<T::Model>>,
    simplified: &mut Simplified<T::Model>,
) -> Result<Option<Succeeded<T, T::Model>>, AbruptError> {
    if !T::simplified_mut(simplified).insert(term.clone()) {
        return Ok(None);
    }

    // recursively simplify the term
    let mut new_term = term.clone();
    let mut visitor = Visitor {
        environment,
        simplified,
        lifetime_constraints: BTreeSet::new(),
        abrupt_error: None,
    };
    let _ = new_term.accept_one_level_mut(&mut visitor);

    // extract out the lifetime constraints
    let Visitor {
        lifetime_constraints: mut outside_constraints,
        abrupt_error: overflow_error,
        ..
    } = visitor;

    if let Some(err) = overflow_error {
        return Err(err);
    }

    // check for trait type equality
    'out: {
        if new_term.as_trait_member().is_none() {
            break 'out;
        }

        let mut equivalent = None;

        let mut environment_cloned = environment.clone();
        let all_trait_type_equalities = environment_cloned
            .premise
            .predicates
            .iter()
            .filter(|x| T::as_trait_member_equality_predicate(x).is_some())
            .cloned()
            .collect::<Vec<_>>();

        // look for the equivalents
        'choice: for (i, trait_type_equality) in
            all_trait_type_equalities.iter().enumerate()
        {
            let Some(trait_type_equality_unwrapped) =
                T::as_trait_member_equality_predicate(trait_type_equality)
            else {
                unreachable!()
            };

            let lhs_trait_member =
                T::from(trait_type_equality_unwrapped.lhs.clone());

            assert!(environment_cloned
                .premise
                .predicates
                .remove(trait_type_equality));

            if let Some(unifier) = environment.query(&Unification::new(
                new_term.clone(),
                lhs_trait_member,
                Arc::new(LifetimeUnifyingPredicate),
            ))? {
                let mut constraints = unifier.constraints.clone();

                for (j, another_trait_type_equality) in
                    all_trait_type_equalities.iter().enumerate()
                {
                    // skip the current trait type equality
                    if i == j {
                        continue;
                    }

                    assert!(environment_cloned
                        .premise
                        .predicates
                        .remove(another_trait_type_equality));

                    // check if the current rhs is equivalent to the other lhs
                    let Some(another_trait_type_equality_unwrapped) =
                        T::as_trait_member_equality_predicate(
                            another_trait_type_equality,
                        )
                    else {
                        unreachable!()
                    };

                    let unification = environment.query(&Unification::new(
                        trait_type_equality_unwrapped.rhs.clone(),
                        T::from(
                            another_trait_type_equality_unwrapped.lhs.clone(),
                        ),
                        Arc::new(LifetimeUnifyingPredicate),
                    ))?;

                    // add back the other trait type equality
                    environment_cloned
                        .premise
                        .predicates
                        .insert(another_trait_type_equality.clone());

                    // skip if the unification is successful
                    if matches!(unification, Some(_)) {
                        environment_cloned
                            .premise
                            .predicates
                            .insert(trait_type_equality.clone());
                        continue 'choice;
                    }
                }

                // multiple equivalent is not allowed
                if equivalent.is_some() {
                    break 'out;
                }

                let mappings = Mapping::from_unifier(unifier.result.clone());

                assert!(mappings.types.is_empty());
                assert!(mappings.constants.is_empty());

                for (lhs, values) in mappings.lifetimes {
                    for rhs in values {
                        if lhs == rhs {
                            continue;
                        }

                        constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(
                                Outlives::new(lhs.clone(), rhs.clone()),
                            ),
                        );
                        constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(
                                Outlives::new(rhs.clone(), lhs.clone()),
                            ),
                        );
                    }
                }

                equivalent = Some(Succeeded {
                    result: trait_type_equality_unwrapped.rhs.clone(),
                    constraints,
                });
            }

            environment_cloned
                .premise
                .predicates
                .insert(trait_type_equality.clone());
        }

        // there must be exactly only one equivalent to avoid ambiguity
        if let Some(Succeeded { result: equivalent_term, constraints }) =
            equivalent
        {
            let Some(Succeeded {
                result: equivalent,
                constraints: new_constraints,
            }) = simplify_internal(&equivalent_term, environment, simplified)?
            else {
                break 'out;
            };

            outside_constraints.extend(constraints);
            outside_constraints.extend(new_constraints);

            assert!(T::simplified_mut(simplified).remove(term));

            return Ok(Some(Succeeded {
                result: equivalent,
                constraints: outside_constraints,
            }));
        }
    }

    // do normalization
    'out: {
        let Ok(Some(Succeeded { result: normalized, constraints })) =
            new_term.normalize(environment)
        else {
            break 'out;
        };

        let Some(Succeeded {
            result: equivalent,
            constraints: new_constraints,
        }) = simplify_internal(&normalized, environment, simplified)?
        else {
            break 'out;
        };

        outside_constraints.extend(constraints);
        outside_constraints.extend(new_constraints);

        assert!(T::simplified_mut(simplified).remove(term));
        return Ok(Some(Succeeded {
            result: equivalent,
            constraints: outside_constraints,
        }));
    }

    assert!(T::simplified_mut(simplified).remove(term));
    Ok(Some(Succeeded { result: new_term, constraints: outside_constraints }))
}

/// Simplifies a term by recursively applying the normalization and trait member
/// equality.
pub fn simplify<T: Term>(
    term: &T,
    environment: &mut Environment<T::Model, impl Normalizer<T::Model>>,
) -> Result<Succeeded<T, T::Model>, AbruptError> {
    simplify_internal(term, environment, &mut Simplified::default())
        .map(|x| x.unwrap_or_else(|| Succeeded::new(term.clone())))
}

// TODO: bring test back
// #[cfg(test)]
// mod tests;
