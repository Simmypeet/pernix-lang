//! Contains logic related to trait resolution.

use std::{cell::Cell, collections::HashMap};

use super::{unification::Config, Mapping, QueryRecords, Substitution};
use crate::{
    arena::ID,
    entity::{constant::Constant, r#type::Type, region::Region, GenericArguments, Model},
    symbol::{self, Symbolic, Trait},
    table::Table,
};

/// Represents the implements resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Implementation {
    /// Deduced trait implements's generic parameters substitution.
    ///
    /// The substitution will contain mapping of the generic parameters of the implementation to
    /// the generic arguments supplied to the trait.
    pub deduced_unification: Substitution<Symbolic>,

    /// The resolved implements id.
    pub implementation_id: ID<symbol::Implementation>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ImplementationKey {
    Positive(ID<symbol::Implementation>),
    Negative(ID<symbol::ImplementationSignature>),
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

    fn region_mappable(&self, _: &Region<S>, _: &Region<S>) -> bool { true }
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
        mapping: &Mapping<S>,
    ) -> Vec<Implementation> {
        todo!()
    }

    pub(super) fn resolve_implementation_internal<S: Model>(
        &self,
        trait_id: ID<Trait>,
        generic_arguments: &GenericArguments<S>,
        premise_mapping: &Mapping<S>,
        records: &Cell<QueryRecords<S>>,
    ) -> Vec<Implementation> {
        let Some(traits) = self.traits().get(trait_id) else {
            return Vec::new();
        };

        if !generic_arguments.is_definite_internal(premise_mapping, self, records) {
            return Vec::new();
        }

        for (key, signature) in traits
            .implementations
            .iter()
            .enumerate()
            .map(|(k, v)| (ImplementationKey::Positive(ID::new(k)), &v.signature))
            .chain(
                traits
                    .negative_implementations
                    .iter()
                    .enumerate()
                    .map(|(k, v)| (ImplementationKey::Negative(ID::new(k)), &v.signature)),
            )
        {
            // gets the deduced generic arguments
            let Ok(mut unification) = GenericArguments::unify_internal(
                &signature.arguments.clone().into_other_model(),
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
                () => {};
            }
        }

        todo!()
    }
}
