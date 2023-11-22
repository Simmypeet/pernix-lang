//! Contains logic related to trait resolution.

use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
};

use lazy_static::lazy_static;
use thiserror::Error;

use super::{predicate::TraitSatisfiability, unification::Config, QueryRecords};
use crate::{
    arena::ID,
    entity::{
        constant::Constant,
        predicate::{
            ConstantEquals, ConstantType, Predicate, Premises, RegionOutlives, TypeEquals,
            TypeOutlives,
        },
        r#type::Type,
        region::Region,
        Entity, GenericArguments, Model, Substitution,
    },
    symbol::{self, ImplementationKindID, Trait},
    table::{Index, Table},
};

/// Enumeration containing the predicates related to region constraints.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum RegionConstraint<S: Model> {
    RegionOutlives(RegionOutlives<S>),
    TypeOutlives(TypeOutlives<S>),
}

/// Represents the implements resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Implementation<S: Model> {
    /// Deduced trait implements's generic parameters substitution.
    ///
    /// The substitution will contain mapping of the generic parameters of the implementation to
    /// the generic arguments supplied to the trait.
    pub deduced_unification: Substitution<S>,

    /// The resolved implements id.
    pub implementation_id: ID<symbol::Implementation>,

    /// List of region constraints that are introduced by the implements.
    pub region_constraints: Vec<RegionConstraint<S>>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct TraitResolvingUnifingConfig {
    symetric: bool,
}

impl<S: Model> Config<S> for TraitResolvingUnifingConfig {
    fn type_mappable(&self, unifier: &Type<S>, target: &Type<S>) -> bool {
        (unifier.is_parameter() || unifier.is_trait_member())
            || if self.symetric {
                target.is_trait_member() || target.is_parameter()
            } else {
                false
            }
    }

    fn constant_mappable(&self, unifier: &Constant<S>, target: &Constant<S>) -> bool {
        (unifier.is_parameter() || unifier.is_trait_member())
            || if self.symetric {
                target.is_trait_member() || target.is_parameter()
            } else {
                false
            }
    }

    fn region_mappable(&self, unifier: &Region<S>, target: &Region<S>) -> bool {
        unifier.is_named()
            || if self.symetric {
                target.is_named()
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
}

impl Table {
    /// Resolves for the implementation of the given trait with the given generic arguments.
    ///
    /// # Errors
    ///
    /// See [`Error`] for more details.
    pub fn resolve_implementation<S: Model>(
        &self,
        trait_id: ID<Trait>,
        generic_arguments: &GenericArguments<S>,
        premises: &Premises<S>,
    ) -> Result<Implementation<S>, Error> {
        self.resolve_implementation_internal(
            trait_id,
            generic_arguments,
            premises,
            &mut QueryRecords::default(),
        )
    }

    #[allow(clippy::too_many_lines, clippy::significant_drop_in_scrutinee)]
    pub(super) fn resolve_implementation_internal<S: Model>(
        &self,
        trait_id: ID<Trait>,
        generic_arguments: &GenericArguments<S>,
        premises: &Premises<S>,
        records: &mut QueryRecords<S>,
    ) -> Result<Implementation<S>, Error> {
        let traits = self.get(trait_id).ok_or(Error::InvalidID)?;

        if !generic_arguments.is_definite_internal(premises, self, records) {
            return Err(Error::NonDefiniteGenericArguments);
        }

        let mut candidates: Vec<(ImplementationKindID, Substitution<S>)> = Vec::new();

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
                        .into_other_model::<S>(),
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
                        .into_other_model::<S>(),
                )
            }))
        {
            // gets the deduced generic arguments
            let Ok(mut unification) = GenericArguments::unify_internal(
                &arguments,
                generic_arguments,
                premises,
                self,
                records,
                &TraitResolvingUnifingConfig { symetric: false },
                Substitution::default(),
            ) else {
                continue;
            };

            macro_rules! trait_member_extraction {
                ($map_name:ident, $unification:ident, $kind:ident) => {
                    let $map_name = {
                        let mut $map_name = HashMap::new();
                        let mut $kind = HashMap::new();

                        for (key, value) in $unification.$kind.into_iter() {
                            match key.into_trait_member() {
                                Ok(trait_member) => {
                                    $map_name.insert(trait_member, value);
                                }
                                Err(ty) => {
                                    $kind.insert(ty, value);
                                }
                            }
                        }

                        $unification.$kind = $kind;

                        $map_name
                    };
                };
            }

            trait_member_extraction!(trait_type_mapping, unification, types);
            trait_member_extraction!(trait_constant_mapping, unification, constants);

            macro_rules! trait_member_confirmation {
                ($trait_maps:ident, $kind:ident) => {
                    for (mut trait_member, target) in $trait_maps
                        .into_iter()
                        .map(|(trait_member, target)| ($kind::TraitMember(trait_member), target))
                    {
                        trait_member.apply(&unification);
                        if !$kind::equals_internal(&trait_member, &target, premises, self, records)
                        {
                            continue 'outer;
                        }
                    }
                };
            }

            trait_member_confirmation!(trait_type_mapping, Type);
            trait_member_confirmation!(trait_constant_mapping, Constant);

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
                    Order::Ambiguous => unreachable!(),
                }
            } else {
                candidates.push((key, unification));
            }
        }

        drop(traits);

        'candidate: for candidate in candidates.into_iter().rev() {
            let deduced_substitution = candidate.1;
            let mut region_constraints = Vec::new();
            let mut existing_region_constraints = HashSet::new();

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
                    Predicate::RegionOutlives(mut x) => {
                        x.apply(&trait_substitution);
                        x.apply(&deduced_substitution);

                        existing_region_constraints.insert(RegionConstraint::RegionOutlives(x));
                    }
                    Predicate::TypeOutlives(mut x) => {
                        x.apply(&trait_substitution);
                        x.apply(&deduced_substitution);

                        existing_region_constraints.insert(RegionConstraint::TypeOutlives(x));
                    }
                    Predicate::TypeEquals(_)
                    | Predicate::ConstantEquals(_)
                    | Predicate::Trait(_)
                    | Predicate::ConstantType(_) => todo!(),
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
                    Predicate::RegionOutlives(x) => {
                        let new_region_constraint = RegionConstraint::RegionOutlives(x.clone());

                        if !existing_region_constraints.contains(&new_region_constraint) {
                            region_constraints.push(RegionConstraint::RegionOutlives(x));
                        }

                        true
                    }
                    Predicate::TypeOutlives(x) => {
                        let new_region_constraint = RegionConstraint::TypeOutlives(x.clone());

                        if !existing_region_constraints.contains(&new_region_constraint) {
                            region_constraints.push(RegionConstraint::TypeOutlives(x));
                        }

                        true
                    }

                    Predicate::TypeEquals(x) => {
                        TypeEquals::satisfies_internal(&x.lhs, &x.rhs, premises, self, records)
                    }
                    Predicate::ConstantEquals(x) => {
                        ConstantEquals::satisfies_internal(&x.lhs, &x.rhs, premises, self, records)
                    }

                    Predicate::Trait(x) => {
                        if let TraitSatisfiability::Satisfiable(sat) = x.satisfies(premises, self) {
                            region_constraints.extend(sat);
                            true
                        } else {
                            false
                        }
                    }

                    Predicate::ConstantType(x) => {
                        ConstantType::satisfies_internal(&x.r#type, premises, self, records)
                    }
                } {
                    continue 'candidate;
                }
            }

            match candidate.0 {
                ImplementationKindID::Positive(positive) => {
                    return Ok(Implementation {
                        deduced_unification: deduced_substitution,
                        implementation_id: positive,
                        region_constraints,
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

impl<S: Model> GenericArguments<S> {
    /// In terms of speciality, compares the given generic arguments with the current one and
    /// returns the order.
    #[must_use]
    pub fn order(&self, other: &Self) -> Order {
        lazy_static! {
            static ref DEFAULT_TABLE: Table = Table::default();
        }

        let self_to_other = Self::unify(
            self,
            other,
            &Premises::default(),
            &DEFAULT_TABLE,
            &TraitResolvingUnifingConfig { symetric: true },
        );
        let other_to_self = Self::unify(
            other,
            self,
            &Premises::default(),
            &DEFAULT_TABLE,
            &TraitResolvingUnifingConfig { symetric: true },
        );

        match (self_to_other, other_to_self) {
            (Ok(self_to_other), Ok(other_to_self)) => {
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
            (Ok(_), Err(_)) => Order::MoreGeneral,
            (Err(_), Ok(_)) => Order::MoreSpecific,
            (Err(_), Err(_)) => Order::Incompatible,
        }
    }
}

#[cfg(test)]
mod tests;
