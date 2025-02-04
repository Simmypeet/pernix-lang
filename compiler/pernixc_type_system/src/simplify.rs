//! Contains the logic for simplifying the type term.

use std::{collections::BTreeSet, sync::Arc};

use pernixc_term::{predicate::Outlives, visitor::Mutable, Model};

use crate::{
    environment::{Environment, Query},
    mapping::Mapping,
    normalizer::Normalizer,
    term::Term,
    unification::Unification,
    AbruptError, LifetimeConstraint, LifetimeUnifyingPredicate, Succeeded,
};

struct Visitor<'e, N: Normalizer<M>, M: Model> {
    environment: &'e Environment<'e, M, N>,
    lifetime_constraints: BTreeSet<LifetimeConstraint<M>>,
    abrupt_error: Option<AbruptError>,
}

impl<'e, U: Term, N: Normalizer<U::Model>> Mutable<U>
    for Visitor<'e, N, U::Model>
{
    fn visit(&mut self, term: &mut U, _: U::Location) -> bool {
        if self.abrupt_error.is_some() {
            return false;
        }

        match self.environment.query(&Simplify(term.clone())) {
            Ok(Some(succeeded)) => {
                *term = succeeded.result.clone();
                self.lifetime_constraints
                    .extend(succeeded.constraints.iter().cloned());

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

/*


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
*/

/// A query for simplifying a term by recursively applying the normalization and
/// trait member equality.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Simplify<T: Term>(pub T);

impl<T: Term> Query for Simplify<T> {
    type Model = T::Model;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<T, T::Model>;
    type Error = AbruptError;

    #[allow(clippy::too_many_lines)]
    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        // recursively simplify the term
        let mut new_term = self.0.clone();
        let mut visitor = Visitor {
            environment,
            lifetime_constraints: BTreeSet::new(),
            abrupt_error: None,
        };
        let _ = new_term.accept_one_level_mut(&mut visitor);
        if let Some(error) = visitor.abrupt_error {
            return Err(error);
        }

        // simplify by equality
        if self.0.as_trait_member().is_some() {
            let mut equivalent: Option<Succeeded<T, T::Model>> = None;

            for predicate in environment
                .premise()
                .predicates
                .iter()
                .filter_map(T::as_trait_member_compatible_predicate)
            {
                let lhs = T::from(predicate.lhs.clone());

                // check if unifiable
                let Some(unifier) = environment.query(&Unification::new(
                    self.0.clone(),
                    lhs.clone(),
                    LifetimeUnifyingPredicate,
                ))?
                else {
                    continue;
                };

                // recursively simplify the equivalent
                let new_equiv =
                    environment.query(&Self(predicate.rhs.clone()))?;

                let Some(mut new_equiv) = new_equiv.map(|new_equiv| {
                    Succeeded::with_constraints(
                        new_equiv.result.clone(),
                        new_equiv
                            .constraints
                            .iter()
                            .chain(&unifier.constraints)
                            .cloned()
                            .collect(),
                    )
                }) else {
                    continue;
                };

                let mappings = Mapping::from_unifier(unifier.result.clone());

                for (lhs, values) in mappings.lifetimes {
                    for rhs in values {
                        if lhs == rhs {
                            continue;
                        }

                        new_equiv.constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(
                                Outlives::new(lhs.clone(), rhs.clone()),
                            ),
                        );
                        new_equiv.constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(
                                Outlives::new(rhs.clone(), lhs.clone()),
                            ),
                        );
                    }
                }

                if let Some(existing_equiv) = &mut equivalent {
                    if *existing_equiv != new_equiv {
                        equivalent = None;
                        break;
                    }
                } else {
                    equivalent = Some(new_equiv);
                }
            }

            if let Some(mut equivalent) = equivalent {
                equivalent.constraints.extend(visitor.lifetime_constraints);
                return Ok(Some(Arc::new(equivalent)));
            }
        }

        'out: {
            if let Some(normalization) = new_term.normalize(environment)? {
                let new_equiv =
                    environment.query(&Self(normalization.result))?;

                let Some(mut new_equiv) = new_equiv.map(|new_equiv| {
                    Succeeded::with_constraints(
                        new_equiv.result.clone(),
                        new_equiv
                            .constraints
                            .iter()
                            .chain(&normalization.constraints)
                            .cloned()
                            .collect(),
                    )
                }) else {
                    break 'out;
                };

                new_equiv.constraints.extend(visitor.lifetime_constraints);
                new_equiv.constraints.extend(normalization.constraints);

                return Ok(Some(Arc::new(new_equiv)));
            }
        }

        Ok(Some(Arc::new(Succeeded::with_constraints(
            new_term,
            visitor.lifetime_constraints,
        ))))
    }
}

impl<M: Model, N: Normalizer<M>> Environment<'_, M, N> {
    /// Shorthand for querying the [`Simplify`].
    ///
    /// # Errors
    ///
    /// See [`AbruptError`] for more information.
    pub fn simplify<T: Term<Model = M>>(
        &self,
        term: T,
    ) -> Result<Arc<Succeeded<T, M>>, AbruptError> {
        Ok(self.query(&Simplify(term))?.unwrap())
    }
}

#[cfg(test)]
mod test;
