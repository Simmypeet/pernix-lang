//! Contains the [`Key`] query definition.

use std::ops::Deref;

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_target::Global;
use pernixc_term::{
    generic_parameters::get_generic_parameters,
    instance::{Instance, TraitRef},
    instantiation::{
        get_instantiation, get_instantiation_for_associated_symbol,
    },
};
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

/// A query for retrieving the [`TraitRef`] of a the `instance` symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Query,
)]
#[value(Option<Interned<TraitRef>>)]
#[extend(name = get_trait_ref, by_val)]
pub struct Key {
    /// The global ID of the `instance` symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}

/// Resolves the trait ref ID of the given instance.
#[extend]
pub async fn get_trait_ref_id_of_instance(
    self: &TrackedEngine,
    instance: &Instance,
) -> Option<Global<pernixc_symbol::ID>> {
    match instance {
        Instance::Symbol(symbol) => {
            self.get_trait_ref(symbol.id()).await.map(|x| x.trait_id())
        }

        Instance::Parameter(member_id) => self
            .get_generic_parameters(member_id.parent_id())
            .await[member_id.id()]
        .trait_ref()
        .map(|x| x.trait_id()),

        Instance::InstanceAssociated(instance_associated) => self
            .get_trait_ref(instance_associated.trait_associated_symbol_id())
            .await
            .map(|x| x.trait_id()),

        Instance::Inference(_) | Instance::Error(_) => None,
    }
}

/// Resolves the trait ref of the given instance.
#[extend]
pub async fn get_trait_ref_of_instance(
    self: &TrackedEngine,
    instance: &Instance,
) -> Option<TraitRef> {
    match instance {
        Instance::Symbol(symbol) => {
            let mut trait_ref = self
                .get_trait_ref(symbol.id())
                .await
                .map(|x| x.deref().clone())?;

            let inst = self
                .get_instantiation(
                    symbol.id(),
                    symbol.generic_arguments().clone(),
                )
                .await
                .unwrap();

            trait_ref.instantiate(&inst);

            Some(trait_ref)
        }

        Instance::Parameter(member_id) => self
            .get_generic_parameters(member_id.parent_id())
            .await[member_id.id()]
        .trait_ref()
        .map(|x| x.deref().clone()),

        Instance::InstanceAssociated(instance_associated) => {
            let parent_trait_ref = Box::pin(
                self.get_trait_ref_of_instance(instance_associated.instance()),
            )
            .await?;

            let mut trait_ref = self
                .get_trait_ref(instance_associated.trait_associated_symbol_id())
                .await?
                .deref()
                .clone();

            let instantiation = self
                .get_instantiation_for_associated_symbol(
                    instance_associated.trait_associated_symbol_id(),
                    parent_trait_ref.generic_arguments().clone(),
                    instance_associated
                        .associated_instance_generic_arguments()
                        .clone(),
                )
                .await
                .unwrap();

            trait_ref.instantiate(&instantiation);

            Some(trait_ref)
        }

        Instance::Inference(_) | Instance::Error(_) => None,
    }
}
