//! Contains logic related to trait resolution.

use lazy_static::lazy_static;
use thiserror::Error;

use super::{
    model::Model,
    predicate::{LifetimeOutlives, Premises, TypeOutlives},
    session::Session,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments},
    unification, Semantic,
};
use crate::{
    arena::ID,
    semantic::{self, session},
    symbol::{self, semantic::Symbolic, Trait},
    table::{State, Success, Table},
};

/// Enumeration containing the predicates related to lifetime constraints.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeConstraint<M: Model> {
    LifetimeOutlives(LifetimeOutlives<M>),
    TypeOutlives(TypeOutlives<M>),
}

/// Represents the implements resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Implementation<M: Model> {
    /// The deduced generic arguments to the implementation.
    pub deduced_generic_arguments: GenericArguments<M>,

    /// The resolved implements id.
    pub implementation_id: ID<symbol::TraitImplementation>,

    /// List of lifetime constraints that are introduced by the implements.
    pub lifetime_constraints: Vec<LifetimeConstraint<M>>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct TypeParameterUnifingConfig;

impl<S: Model> unification::Config<Type<S>> for TypeParameterUnifingConfig {
    fn unifiable(&mut self, lhs: &Type<S>, _: &Type<S>) -> bool { lhs.is_parameter() }
}

impl<S: Model> unification::Config<Constant<S>> for TypeParameterUnifingConfig {
    fn unifiable(&mut self, lhs: &Constant<S>, _: &Constant<S>) -> bool { lhs.is_parameter() }
}

impl<S: Model> unification::Config<Lifetime<S>> for TypeParameterUnifingConfig {
    fn unifiable(&mut self, lhs: &Lifetime<S>, _: &Lifetime<S>) -> bool { lhs.is_parameter() }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct TraitResolvingUnifingConfig {
    symetric: bool,
}

impl<S: Model> unification::Config<Type<S>> for TraitResolvingUnifingConfig {
    fn unifiable(&mut self, lhs: &Type<S>, rhs: &Type<S>) -> bool {
        (lhs.is_parameter() || lhs.is_trait_member())
            || if self.symetric {
                rhs.is_trait_member() || rhs.is_parameter()
            } else {
                false
            }
    }
}

impl<S: Model> unification::Config<Constant<S>> for TraitResolvingUnifingConfig {
    fn unifiable(&mut self, lhs: &Constant<S>, rhs: &Constant<S>) -> bool {
        (lhs.is_parameter() || lhs.is_trait_member())
            || if self.symetric {
                rhs.is_trait_member() || rhs.is_parameter()
            } else {
                false
            }
    }
}

impl<S: Model> unification::Config<Lifetime<S>> for TraitResolvingUnifingConfig {
    fn unifiable(&mut self, lhs: &Lifetime<S>, rhs: &Lifetime<S>) -> bool {
        lhs.is_parameter()
            || if self.symetric {
                rhs.is_parameter()
            } else {
                false
            }
    }
}

/// The error type for the trait resolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("invalid ID was found in the arguments")]
    InvalidID,
    #[error("implementation resolution requires generic arguments to be definite")]
    NonDefiniteGenericArguments,
    #[error("the table is malformed, contains ambiguous implementation")]
    SuboptimalTable,
}

impl<T: State> Table<T> {
    /// Resolves for the implementation of the given trait with the given generic arguments.
    ///
    /// # Errors
    ///
    /// See [`Error`] for more details.
    #[allow(clippy::significant_drop_in_scrutinee, clippy::too_many_lines)]
    pub fn resolve_implementation<
        M: Model,
        S: Semantic<Type<M>> + Semantic<Constant<M>> + Semantic<Lifetime<M>>,
        R: Session<Type<M>> + Session<Constant<M>> + Session<Lifetime<M>>,
    >(
        &self,
        trait_id: ID<Trait>,
        generic_arguments: &GenericArguments<M>,
        premises: &Premises<M>,
        semantic: &S,
        session: &mut R,
    ) -> Result<Vec<Implementation<M>>, Error> {
        todo!()
        /*
            let traits = self.get(trait_id).ok_or(Error::InvalidID)?;

            if generic_arguments.is_definite(premises, self, semantic, session) {
                return Err(Error::NonDefiniteGenericArguments);
            }

            let mut candidates: Vec<(TraitImplementationKindID, Substitution<M>)> = Vec::new();

            'outer: for (key, arguments) in traits
                .implementations
                .iter()
                .map(|k| {
                    (
                        TraitImplementationKindID::Positive(*k),
                        self.get(*k)
                            .unwrap()
                            .signature
                            .arguments
                            .clone()
                            .into_other_model::<M>(),
                    )
                })
                .chain(traits.negative_implementations.iter().map(|k| {
                    (
                        TraitImplementationKindID::Negative(*k),
                        self.get(*k)
                            .unwrap()
                            .signature
                            .arguments
                            .clone()
                            .into_other_model::<M>(),
                    )
                }))
            {
                // builds the unification
                let Some(unification) = build_unification(
                    &arguments,
                    generic_arguments,
                    premises,
                    self,
                    semantic,
                    session,
                ) else {
                    continue;
                };

                if let Some(candidate) = candidates.last() {
                    let contender_signature = self.get_trait_implementation_signature(key).unwrap();
                    let order = self
                        .get_trait_implementation_signature(candidate.0)
                        .unwrap()
                        .arguments
                        .order(&contender_signature.arguments);

                    match order {
                        Order::Incompatible => {
                            return Err(Error::AmbiguousTerm);
                        }

                        // properly sorted insert the new candidate
                        Order::MoreGeneral | Order::MoreSpecific => {
                            let order =
                                candidates.binary_search_by(|existing| {
                                    match contender_signature.arguments.order(
                                        &self
                                            .get_trait_implementation_signature(existing.0)
                                            .unwrap()
                                            .arguments,
                                    ) {
                                        Order::Incompatible | Order::Ambiguous => unreachable!(),
                                        Order::MoreGeneral => Ordering::Greater,
                                        Order::MoreSpecific => Ordering::Less,
                                    }
                                });
                            drop(contender_signature);

                            match order {
                                Ok(_) => unreachable!(),
                                Err(pos) => {
                                    candidates.insert(pos, (key, unification));
                                    continue 'outer;
                                }
                            }
                        }
                        Order::Ambiguous => return Err(Error::SuboptimalTable),
                    }
                } else {
                    candidates.push((key, unification));
                }
            }

            drop(traits);

            'candidate: for candidate in candidates.into_iter().rev() {
                let deduced_substitution = candidate.1;
                let mut lifetime_constraints = Vec::new();
                let mut existing_lifetime_constraints = HashSet::new();

                let parent_trait_id = self
                    .get_trait_implementation_signature(candidate.0)
                    .unwrap()
                    .implemented_id;
                let trait_substitution = Substitution::from_generic_arguments(
                    self.get_trait_implementation_signature(candidate.0)
                        .unwrap()
                        .arguments
                        .clone()
                        .into_other_model(),
                    candidate.0.into(),
                );

                for parent_predicate in &self
                    .get(parent_trait_id)
                    .unwrap()
                    .generic_declaration
                    .predicates
                {
                    match parent_predicate.predicate.clone().into_other_model() {
                        Predicate::LifetimeOutlives(mut x) => {
                            x.apply(&trait_substitution);
                            x.apply(&deduced_substitution);

                            existing_lifetime_constraints
                                .insert(LifetimeConstraint::LifetimeOutlives(x));
                        }
                        Predicate::TypeOutlives(mut x) => {
                            x.apply(&trait_substitution);
                            x.apply(&deduced_substitution);

                            existing_lifetime_constraints.insert(LifetimeConstraint::TypeOutlives(x));
                        }

                        Predicate::ConstantType(_)
                        | Predicate::TypeEquals(_)
                        | Predicate::ConstantEquals(_)
                        | Predicate::Trait(_) => {}
                    }
                }

                for mut predicate in self
                    .get_generic(candidate.0.into())
                    .unwrap()
                    .generic_declaration()
                    .predicates
                    .iter()
                    .map(|x| x.predicate.clone().into_other_model())
                {
                    predicate.apply(&deduced_substitution);

                    if !match predicate {
                        // outlives predicates are not checked, serves as a hint for the borrow
                        // checker
                        Predicate::LifetimeOutlives(x) => {
                            let new_lifetime_constraint =
                                LifetimeConstraint::LifetimeOutlives(x.clone());

                            if !existing_lifetime_constraints.contains(&new_lifetime_constraint) {
                                lifetime_constraints.push(LifetimeConstraint::LifetimeOutlives(x));
                            }

                            true
                        }
                        Predicate::TypeOutlives(x) => {
                            let new_lifetime_constraint = LifetimeConstraint::TypeOutlives(x.clone());

                            if !existing_lifetime_constraints.contains(&new_lifetime_constraint) {
                                lifetime_constraints.push(LifetimeConstraint::TypeOutlives(x));
                            }

                            true
                        }

                        Predicate::TypeEquals(x) => x.satisfies(premises, self, semantic, session),
                        Predicate::ConstantEquals(x) => x.satisfies(premises, self, semantic, session),

                        Predicate::Trait(x) => {
                            if let TraitSatisfiability::Satisfiable(sat) = predicate::Trait::satisfies(
                                x.trait_id,
                                x.const_trait,
                                &x.generic_arguments,
                                premises,
                                self,
                                semantic,
                                session,
                            ) {
                                lifetime_constraints.extend(sat);
                                true
                            } else {
                                false
                            }
                        }
                        Predicate::ConstantType(_) => todo!(),
                    } {
                        continue 'candidate;
                    }
                }

                match candidate.0 {
                    TraitImplementationKindID::Positive(positive) => {
                        return Ok(Implementation {
                            deduced_unification: deduced_substitution,
                            implementation_id: positive,
                            lifetime_constraints,
                        });
                    }
                    TraitImplementationKindID::Negative(_) => {
                        return Err(Error::NegativeImplementation)
                    }
                }
            }

            Err(Error::ImplementationNotFound)
        */
    }
}

/// The order in terms of speciality.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Order {
    Incompatible,
    MoreGeneral,
    MoreSpecific,
    Ambiguous,
}

impl GenericArguments<Symbolic> {
    /// In terms of speciality, compares the given generic arguments with the current one and
    /// returns the order.
    #[must_use]
    pub fn order(&self, other: &Self) -> Order {
        lazy_static! {
            static ref DEFAULT_TABLE: Table<Success> = Table::default();
        }

        let self_to_other = self.unify(
            other,
            &Premises::default(),
            &DEFAULT_TABLE,
            &semantic::Default,
            &mut session::Default::default(),
            &mut TraitResolvingUnifingConfig { symetric: true },
        );
        let other_to_self = self.unify(
            other,
            &Premises::default(),
            &DEFAULT_TABLE,
            &semantic::Default,
            &mut session::Default::default(),
            &mut TraitResolvingUnifingConfig { symetric: true },
        );

        match (self_to_other, other_to_self) {
            (Some(self_to_other), Some(other_to_self)) => {
                let self_to_other_map_count = self_to_other
                    .types
                    .keys()
                    .filter(|x| x.is_parameter() || x.is_trait_member())
                    .count()
                    + self_to_other
                        .constants
                        .keys()
                        .filter(|x| x.is_parameter() || x.is_trait_member())
                        .count();

                let other_to_self_map_count = other_to_self
                    .types
                    .keys()
                    .filter(|x| x.is_parameter() || x.is_trait_member())
                    .count()
                    + other_to_self
                        .constants
                        .keys()
                        .filter(|x| x.is_parameter() || x.is_trait_member())
                        .count();

                match self_to_other_map_count.cmp(&other_to_self_map_count) {
                    std::cmp::Ordering::Less => Order::MoreSpecific,
                    std::cmp::Ordering::Greater => Order::MoreGeneral,
                    std::cmp::Ordering::Equal => Order::Ambiguous,
                }
            }
            (Some(_), None) => Order::MoreGeneral,
            (None, Some(_)) => Order::MoreSpecific,
            (None, None) => Order::Incompatible,
        }
    }
}
