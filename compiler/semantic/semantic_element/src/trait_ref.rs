//! Contains the [`Key`] query definition.

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_target::Global;
use pernixc_term::{
    generic_parameters::get_generic_parameters,
    instance::{Instance, TraitRef},
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
        .map(pernixc_term::instance::TraitRef::trait_id),

        Instance::InstanceAssociated(instance_associated) => self
            .get_trait_ref(instance_associated.trait_associated_symbol_id())
            .await
            .map(|x| x.trait_id()),

        Instance::Inference(_) | Instance::Error(_) => None,
    }
}
