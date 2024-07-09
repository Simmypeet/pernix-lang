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

        let mut equivalents = Vec::new();

        // look for the equivalents
        for trait_type_equality in environment
            .premise
            .predicates
            .iter()
            .filter_map(T::as_trait_member_equality_predicate)
        {
            let lhs_trait_member = T::from(trait_type_equality.lhs.clone());

            if let Ok(Some(Succeeded { result: unifier, mut constraints })) =
                Unification::new(
                    new_term.clone(),
                    lhs_trait_member,
                    Arc::new(LifetimeUnifyingPredicate),
                )
                .query(environment)
            {
                let mappings = Mapping::from_unifier(unifier);

                assert!(mappings.types.is_empty());
                assert!(mappings.constants.is_empty());

                for (lhs, values) in mappings.lifetimes {
                    for rhs in values {
                        constraints.insert(
                            LifetimeConstraint::LifetimeMatching(
                                lhs.clone(),
                                rhs,
                            ),
                        );
                    }
                }

                equivalents.push(Succeeded {
                    result: trait_type_equality.rhs.clone(),
                    constraints,
                })
            }
        }

        // there must be exactly only one equivalent to avoid ambiguity
        if equivalents.len() == 1 {
            let Succeeded { result: equivalent_term, constraints } =
                equivalents.pop().unwrap();

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
