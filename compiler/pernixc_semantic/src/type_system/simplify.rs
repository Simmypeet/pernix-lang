//! Contains the logic for simplifying the type term.

use std::{collections::HashSet, sync::Arc};

use super::{
    model::Model,
    normalizer::Normalizer,
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, ModelOf, Term,
    },
    visitor::Mutable,
    Environment, LifetimeConstraint, Succeeded,
};
use crate::{
    symbol::table::State,
    type_system::{
        mapping::Mapping, query::Context, unification::Unification, Compute,
        LifetimeUnifyingPredicate,
    },
    unordered_pair::UnorderedPair,
};

pub(super) trait Simplify: ModelOf + Sized {
    #[allow(private_interfaces)]
    fn simplified_mut(
        simplified: &mut Simplified<Self::Model>,
    ) -> &mut HashSet<Self>;
}

impl<M: Model> Simplify for Lifetime<M> {
    #[allow(private_interfaces)]
    fn simplified_mut(simplified: &mut Simplified<M>) -> &mut HashSet<Self> {
        &mut simplified.simplified_lifetimes
    }
}

impl<M: Model> Simplify for Type<M> {
    #[allow(private_interfaces)]
    fn simplified_mut(simplified: &mut Simplified<M>) -> &mut HashSet<Self> {
        &mut simplified.simplified_types
    }
}

impl<M: Model> Simplify for Constant<M> {
    #[allow(private_interfaces)]
    fn simplified_mut(simplified: &mut Simplified<M>) -> &mut HashSet<Self> {
        &mut simplified.simplified_constants
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct Simplified<M: Model> {
    simplified_lifetimes: HashSet<Lifetime<M>>,
    simplified_types: HashSet<Type<M>>,
    simplified_constants: HashSet<Constant<M>>,
}

struct Visitor<'e, 's, T: State, N: Normalizer<M>, M: Model> {
    environment: &'e Environment<'e, M, T, N>,
    simplified: &'s mut Simplified<M>,
    lifetime_constraints: HashSet<LifetimeConstraint<M>>,
}

impl<'e, 's, U: Term, T: State, N: Normalizer<U::Model>> Mutable<U>
    for Visitor<'e, 's, T, N, U::Model>
{
    fn visit(&mut self, term: &mut U, _: U::Location) -> bool {
        let Some(Succeeded { result, constraints }) =
            simplify_internal(term, self.environment, &mut self.simplified)
        else {
            return true;
        };

        *term = result;
        self.lifetime_constraints.extend(constraints);

        true
    }
}

fn simplify_internal<T: Term>(
    term: &T,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    simplified: &mut Simplified<T::Model>,
) -> Option<Succeeded<T, T::Model>> {
    if !T::simplified_mut(simplified).insert(term.clone()) {
        return None;
    }

    // recursively simplify the term
    let mut new_term = term.clone();
    let mut visitor = Visitor {
        environment,
        simplified,
        lifetime_constraints: HashSet::new(),
    };
    let _ = new_term.accept_one_level_mut(&mut visitor);

    // extract out the lifetime constraints
    let Visitor { mut lifetime_constraints, .. } = visitor;

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
                .remove(&trait_type_equality));

            if let Ok(Some(Succeeded { result: unifier, mut constraints })) =
                Unification::new(
                    new_term.clone(),
                    lhs_trait_member,
                    Arc::new(LifetimeUnifyingPredicate),
                )
                .query(&environment_cloned)
            {
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

                    let unification = Unification::new(
                        trait_type_equality_unwrapped.rhs.clone(),
                        T::from(
                            another_trait_type_equality_unwrapped.lhs.clone(),
                        ),
                        Arc::new(LifetimeUnifyingPredicate),
                    )
                    .query(&environment_cloned);

                    dbg!(
                        &unification,
                        &environment_cloned.premise,
                        trait_type_equality_unwrapped,
                        another_trait_type_equality
                    );

                    // add back the other trait type equality
                    environment_cloned
                        .premise
                        .predicates
                        .insert(another_trait_type_equality.clone());

                    // skip if the unification is successful
                    if matches!(unification, Ok(Some(_))) {
                        continue 'choice;
                    }
                }

                // multiple equivalent is not allowed
                if equivalent.is_some() {
                    break 'out;
                }

                let mappings = Mapping::from_unifier(unifier);

                assert!(mappings.types.is_empty());
                assert!(mappings.constants.is_empty());

                for (lhs, values) in mappings.lifetimes {
                    for rhs in values {
                        if lhs == rhs {
                            continue;
                        }

                        constraints.insert(
                            LifetimeConstraint::LifetimeMatching(
                                UnorderedPair::new(lhs.clone(), rhs),
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
            }) = simplify_internal(&equivalent_term, environment, simplified)
            else {
                break 'out;
            };

            lifetime_constraints.extend(constraints);
            lifetime_constraints.extend(new_constraints);

            assert!(T::simplified_mut(simplified).remove(term));
            return Some(Succeeded {
                result: equivalent,
                constraints: lifetime_constraints,
            });
        }
    }

    // do normalization
    'out: {
        let Ok(Some(Succeeded { result: normalized, constraints })) =
            new_term.normalize(environment, &mut Context::new())
        else {
            break 'out;
        };

        let Some(Succeeded {
            result: equivalent,
            constraints: new_constraints,
        }) = simplify_internal(&normalized, environment, simplified)
        else {
            break 'out;
        };

        lifetime_constraints.extend(constraints);
        lifetime_constraints.extend(new_constraints);

        assert!(T::simplified_mut(simplified).remove(term));
        return Some(Succeeded {
            result: equivalent,
            constraints: lifetime_constraints,
        });
    }

    assert!(T::simplified_mut(simplified).remove(term));
    return Some(Succeeded {
        result: new_term,
        constraints: lifetime_constraints,
    });
}

/// Simplifies a term by recursively applying the normalization and trait member
/// equality.
pub fn simplify<T: Term>(
    term: &T,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
) -> Succeeded<T, T::Model> {
    simplify_internal(term, environment, &mut Simplified::default())
        .unwrap_or_else(|| Succeeded::new(term.clone()))
}

#[cfg(test)]
mod tests;
