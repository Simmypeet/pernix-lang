//! Contains logic related to trait resolution.

use std::{cell::Cell, collections::HashMap};

use super::{unification::Config, Mapping, QueryRecords};
use crate::{
    arena::ID,
    entity::{
        constant::Constant, r#type::Type, region::Region, Entity, GenericArguments, Model,
        Substitution,
    },
    symbol::{self, Trait},
    table::{Index, Table},
};

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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ImplementationKey {
    Positive(ID<symbol::Implementation>),
    Negative(ID<symbol::NegativeImplementation>),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct TraitResolvingUnifingConfig;

impl<S: Model> Config<S> for TraitResolvingUnifingConfig {
    fn type_mappable(&self, unifier: &Type<S>, _: &Type<S>) -> bool {
        unifier.is_parameter() || unifier.is_trait_member()
    }

    fn constant_mappable(&self, unifier: &Constant<S>, _: &Constant<S>) -> bool {
        unifier.is_parameter() || unifier.is_trait_member()
    }

    fn region_mappable(&self, unifier: &Region<S>, _: &Region<S>) -> bool { unifier.is_named() }
}

impl Table {
    /// Resolves for the implements of the given trait.
    ///
    /// # Parameters
    ///
    /// * `trait_id`: The trait to resolve for.
    /// * `generic_arguments`: The generic arguments of the trait.
    /// * `mapping`: The maping premises defined in the current context.
    ///
    /// # Returns
    ///
    /// Returns [`None`] if the input parameters are invalid such as the trait ID is not in the
    /// table, or the generic arguments are not valid for the trait. Otherwise, returns a vector of
    /// [`Implementation`] that are resolved.
    #[must_use]
    pub fn resolve_implementation<S: Model>(
        &self,
        trait_id: ID<Trait>,
        generic_arguments: &GenericArguments<S>,
        mapping: &Mapping<S>,
    ) -> Vec<Implementation<S>> {
        self.resolve_implementation_internal(
            trait_id,
            generic_arguments,
            mapping,
            &Cell::new(QueryRecords::default()),
        )
    }

    #[allow(clippy::too_many_lines, clippy::significant_drop_in_scrutinee)]
    pub(super) fn resolve_implementation_internal<S: Model>(
        &self,
        trait_id: ID<Trait>,
        generic_arguments: &GenericArguments<S>,
        premise_mapping: &Mapping<S>,
        records: &Cell<QueryRecords<S>>,
    ) -> Vec<Implementation<S>> {
        let Some(traits) = self.get(trait_id) else {
            return Vec::new();
        };

        if !generic_arguments.is_definite_internal(premise_mapping, self, records) {
            return Vec::new();
        }

        let mut candidates: Vec<(ImplementationKey, GenericArguments<S>, Substitution<S>)> =
            Vec::new();

        'outer: for (key, arguments) in traits
            .implementations
            .iter()
            .enumerate()
            .map(|(k, v)| {
                (
                    ImplementationKey::Positive(ID::new(k)),
                    self.get(*v)
                        .unwrap()
                        .signature
                        .arguments
                        .clone()
                        .into_other_model::<S>(),
                )
            })
            .chain(
                traits
                    .negative_implementations
                    .iter()
                    .enumerate()
                    .map(|(k, v)| {
                        (
                            ImplementationKey::Negative(ID::new(k)),
                            self.get(*v)
                                .unwrap()
                                .signature
                                .arguments
                                .clone()
                                .into_other_model::<S>(),
                        )
                    }),
            )
        {
            // gets the deduced generic arguments
            let Ok(mut unification) = GenericArguments::unify_internal(
                &arguments,
                generic_arguments,
                premise_mapping,
                self,
                records,
                &TraitResolvingUnifingConfig,
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
                        if !$kind::equals_internal(
                            &trait_member,
                            &target,
                            premise_mapping,
                            self,
                            records,
                        ) {
                            continue 'outer;
                        }
                    }
                };
            }

            trait_member_confirmation!(trait_type_mapping, Type);
            trait_member_confirmation!(trait_constant_mapping, Constant);

            for candidate in &mut candidates {
                let order = candidate.1.order(&arguments);
                match order {
                    Order::Incompatible => continue,
                    Order::MoreGeneral => continue 'outer,
                    Order::MoreSpecific => {
                        *candidate = (candidate.0, arguments, unification);
                        continue 'outer;
                    }
                    Order::Ambiguous => unreachable!("Ambiguous order"),
                }
            }

            candidates.push((key, arguments, unification));
        }

        candidates
            .into_iter()
            .filter_map(|(implementation, _, deduced)| match implementation {
                ImplementationKey::Positive(id) => Some(Implementation {
                    deduced_unification: deduced,
                    implementation_id: id,
                }),
                ImplementationKey::Negative(_) => None,
            })
            .collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Order {
    Incompatible,
    MoreGeneral,
    MoreSpecific,
    Ambiguous,
}

impl<S: Model> GenericArguments<S> {
    fn order(&self, other: &Self) -> Order {
        let self_to_other = Self::unify(
            self,
            other,
            &Mapping::default(),
            &Table::default(),
            &TraitResolvingUnifingConfig,
        );
        let other_to_self = Self::unify(
            other,
            self,
            &Mapping::default(),
            &Table::default(),
            &TraitResolvingUnifingConfig,
        );

        match (self_to_other, other_to_self) {
            (Ok(self_to_other), Ok(other_to_self)) => {
                let self_to_other_map_count = self_to_other.types.len()
                    + self_to_other.constants.len()
                    + self_to_other.regions.len();
                let other_to_self_map_count = other_to_self.types.len()
                    + other_to_self.constants.len()
                    + other_to_self.regions.len();

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
