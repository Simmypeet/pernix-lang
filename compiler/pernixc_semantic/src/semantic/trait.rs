//! Contains logic related to trait resolution.

use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap, HashSet},
};

use lazy_static::lazy_static;
use thiserror::Error;

use super::{
    model::Model,
    predicate::{self, LifetimeOutlives, Premises, TraitSatisfiability, TypeOutlives},
    session::Session,
    substitution::Substitution,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments},
    unification, Semantic,
};
use crate::{
    arena::ID,
    semantic::{
        self, model::Entity, predicate::Predicate, session, substitution::Substitute, term::Term,
    },
    symbol::{self, semantic::Symbolic, ImplementationKindID, Trait},
    table::{Index, Table},
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
    /// Deduced trait implements's generic parameters substitution.
    ///
    /// The substitution will contain mapping of the generic parameters of the implementation to
    /// the generic arguments supplied to the trait.
    pub deduced_unification: Substitution<M>,

    /// The resolved implements id.
    pub implementation_id: ID<symbol::Implementation>,

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
    #[error("trait implementation not found")]
    ImplementationNotFound,
    #[error("the term can be rewritten into multiple terms and causes ambiguity")]
    AmbiguousTerm,
    #[error("implementation resolution requires generic arguments to be definite")]
    NonDefiniteGenericArguments,
    #[error("resolved into a negative implementation")]
    NegativeImplementation,
    #[error("the table is malformed, contains ambiguous implementation")]
    SuboptimalTable,
}

fn extract<T: Eq + std::hash::Hash>(
    map: HashMap<T, T>,
    mut predicate: impl FnMut(&T) -> bool,
) -> (HashMap<T, T>, HashMap<T, T>) {
    let mut positive = HashMap::new();
    let mut negative = HashMap::new();

    for (key, value) in map {
        if predicate(&key) {
            positive.insert(key, value);
        } else {
            negative.insert(key, value);
        }
    }

    (positive, negative)
}

fn extract_unification_from_eq_predicate<
    M: Model,
    T: Term<Model = Symbolic> + Entity<Model = Symbolic>,
    S: Semantic<<T as Entity>::Rebind<M>>
        + Semantic<Type<M>>
        + Semantic<Constant<M>>
        + Semantic<Lifetime<M>>,
    R: Session<<T as Entity>::Rebind<M>>
        + Session<Type<M>>
        + Session<Constant<M>>
        + Session<Lifetime<M>>,
>(
    premises: &Premises<M>,
    table: &Table,
    semantic: &mut S,
    session: &mut R,
    base_unification: &Substitution<M>,
    eq: &predicate::Equals<T>,
) -> Option<Substitution<M>>
where
    <T as Entity>::Rebind<M>: Term<Model = M>,
    TypeParameterUnifingConfig: unification::Config<<T as Entity>::Rebind<M>>,
{
    let mut lhs = eq.lhs.clone().into_other_model::<M>();
    let mut rhs = eq.rhs.clone().into_other_model::<M>();

    lhs.apply(base_unification);
    rhs.apply(base_unification);

    if !lhs.definite(premises, table, semantic, session) {
        return None;
    }

    rhs.unify(
        &lhs,
        premises,
        table,
        semantic,
        session,
        &mut TypeParameterUnifingConfig,
    )
}

fn append_new_unification<
    T: Term,
    S: Semantic<T>
        + Semantic<Type<<T as Term>::Model>>
        + Semantic<Constant<<T as Term>::Model>>
        + Semantic<Lifetime<<T as Term>::Model>>,
    R: Session<T>
        + Session<Type<<T as Term>::Model>>
        + Session<Constant<<T as Term>::Model>>
        + Session<Lifetime<<T as Term>::Model>>,
>(
    premises: &Premises<<T as Term>::Model>,
    table: &Table,
    semantic: &mut S,
    session: &mut R,
    base: &mut Substitution<<T as Term>::Model>,
    mapping: HashMap<T, T>,
) -> bool {
    let map = <T as Substitute>::get_mut(base);

    for (key, value) in mapping {
        match map.entry(key) {
            Entry::Occupied(entry) => {
                if !entry
                    .get()
                    .equals(&value, premises, table, semantic, session)
                {
                    return false;
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(value);
            }
        }
    }

    true
}

fn mapping_equals<
    T: Term,
    S: Semantic<T>
        + Semantic<Type<<T as Term>::Model>>
        + Semantic<Constant<<T as Term>::Model>>
        + Semantic<Lifetime<<T as Term>::Model>>,
    R: Session<T>
        + Session<Type<<T as Term>::Model>>
        + Session<Constant<<T as Term>::Model>>
        + Session<Lifetime<<T as Term>::Model>>,
>(
    mappigs: HashMap<T, T>,
    deduced_unification: &Substitution<<T as Term>::Model>,
    premises: &Premises<<T as Term>::Model>,
    table: &Table,
    semantic: &mut S,
    session: &mut R,
) -> bool {
    for (mut key, value) in mappigs {
        key.apply(deduced_unification);

        if !key.equals(&value, premises, table, semantic, session) {
            return false;
        }
    }

    true
}

#[allow(clippy::too_many_lines)]
fn build_unification<
    M: Model,
    S: Semantic<Type<M>> + Semantic<Constant<M>> + Semantic<Lifetime<M>>,
    R: Session<Type<M>> + Session<Constant<M>> + Session<Lifetime<M>>,
>(
    key: ImplementationKindID,
    implements_arguments: &GenericArguments<M>,
    arguments: &GenericArguments<M>,
    premises: &Premises<M>,
    table: &Table,
    semantic: &mut S,
    session: &mut R,
) -> Option<Substitution<M>> {
    // gets the deduced generic arguments
    let Some(base_unification) = implements_arguments.unify(
        arguments,
        premises,
        table,
        semantic,
        session,
        &mut TraitResolvingUnifingConfig { symetric: false },
    ) else {
        return None;
    };

    // exract the trait members
    let (mut base_unification, trait_type_map, trait_constant_map) = {
        let (type_param_map, trait_type_map) =
            extract(base_unification.types, Type::is_trait_member);
        let (constant_param_map, trait_constant_map) =
            extract(base_unification.constants, Constant::is_trait_member);

        (
            Substitution {
                types: type_param_map,
                constants: constant_param_map,
                lifetimes: base_unification.lifetimes,
            },
            trait_type_map,
            trait_constant_map,
        )
    };

    let mut unification_from_predicates = Vec::new();
    for predicate in &table
        .get_implementation_signature(key)
        .unwrap()
        .generic_declaration
        .predicates
    {
        let uni = match &predicate.predicate {
            Predicate::TypeEquals(ty_eq) => extract_unification_from_eq_predicate(
                premises,
                table,
                semantic,
                session,
                &base_unification,
                ty_eq,
            ),
            Predicate::ConstantEquals(const_eq) => extract_unification_from_eq_predicate(
                premises,
                table,
                semantic,
                session,
                &base_unification,
                const_eq,
            ),
            _ => continue,
        };

        if let Some(uni) = uni {
            unification_from_predicates.push(uni);
        } else {
            return None;
        }
    }

    for unification in unification_from_predicates {
        if !append_new_unification(
            premises,
            table,
            semantic,
            session,
            &mut base_unification,
            unification.types,
        ) {
            return None;
        }

        if !append_new_unification(
            premises,
            table,
            semantic,
            session,
            &mut base_unification,
            unification.constants,
        ) {
            return None;
        }

        if !append_new_unification(
            premises,
            table,
            semantic,
            session,
            &mut base_unification,
            unification.lifetimes,
        ) {
            return None;
        }
    }

    if !(mapping_equals(
        trait_type_map,
        &base_unification,
        premises,
        table,
        semantic,
        session,
    ) && mapping_equals(
        trait_constant_map,
        &base_unification,
        premises,
        table,
        semantic,
        session,
    )) {
        return None;
    }

    Some(base_unification)
}

impl Table {
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
        semantic: &mut S,
        session: &mut R,
    ) -> Result<Implementation<M>, Error> {
        let traits = self.get(trait_id).ok_or(Error::InvalidID)?;

        if generic_arguments.is_definite(premises, self, semantic, session) {
            return Err(Error::NonDefiniteGenericArguments);
        }

        let mut candidates: Vec<(ImplementationKindID, Substitution<M>)> = Vec::new();

        'outer: for (key, arguments) in traits
            .implementations
            .iter()
            .map(|k| {
                (
                    ImplementationKindID::Positive(*k),
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
                    ImplementationKindID::Negative(*k),
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
                key,
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
                let contender_signature = self.get_implementation_signature(key).unwrap();
                let order = self
                    .get_implementation_signature(candidate.0)
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
                                        .get_implementation_signature(existing.0)
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
                .get_implementation_signature(candidate.0)
                .unwrap()
                .trait_id;
            let trait_substitution = Substitution::from_generic_arguments(
                self.get_implementation_signature(candidate.0)
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
                    Predicate::TypeEquals(_)
                    | Predicate::ConstantEquals(_)
                    | Predicate::Trait(_)
                    | Predicate::ConstantType(_) => {}
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

                    Predicate::ConstantType(x) => x.satisfies(premises, self, semantic, session),
                } {
                    continue 'candidate;
                }
            }

            match candidate.0 {
                ImplementationKindID::Positive(positive) => {
                    return Ok(Implementation {
                        deduced_unification: deduced_substitution,
                        implementation_id: positive,
                        lifetime_constraints,
                    });
                }
                ImplementationKindID::Negative(_) => return Err(Error::NegativeImplementation),
            }
        }

        Err(Error::ImplementationNotFound)
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
            static ref DEFAULT_TABLE: Table = Table::default();
        }

        let self_to_other = self.unify(
            other,
            &Premises::default(),
            &DEFAULT_TABLE,
            &mut semantic::Default,
            &mut session::Default::default(),
            &mut TraitResolvingUnifingConfig { symetric: true },
        );
        let other_to_self = self.unify(
            other,
            &Premises::default(),
            &DEFAULT_TABLE,
            &mut semantic::Default,
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

// #[cfg(test)]
// mod tests;
