//! Contains the [`Key`] query definition.

use std::ops::Deref;

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_target::Global;
use pernixc_term::{
    self,
    generic_arguments::create_identity_generic_arguments,
    generic_parameters::get_generic_parameters,
    instance::{Instance, InstanceAssociated, TraitRef},
    instantiation::Instantiation,
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
#[extend(name = get_trait_ref_of_instance_symbol, by_val)]
pub struct Key {
    /// The global ID of the `instance` symbol.
    pub symbol_id: Global<pernixc_symbol::SymbolID>,
}

/// Resolves the trait ref ID of the given instance.
#[extend]
pub async fn get_trait_ref_id_of_instance(
    self: &TrackedEngine,
    instance: &Instance,
) -> Option<Global<pernixc_symbol::SymbolID>> {
    match instance {
        Instance::Symbol(symbol) => self
            .get_trait_ref_of_instance_symbol(symbol.id())
            .await
            .map(|x| x.trait_id()),

        Instance::AnonymousTrait(tr) => Some(tr.trait_id()),

        Instance::Parameter(member_id) => self
            .get_generic_parameters(member_id.parent_id())
            .await[member_id.id()]
        .trait_ref()
        .map(|x| x.trait_id()),

        Instance::InstanceAssociated(instance_associated) => self
            .get_trait_ref_of_instance_symbol(
                instance_associated.trait_associated_symbol_id(),
            )
            .await
            .map(|x| x.trait_id()),

        Instance::Inference(_) | Instance::Error(_) => None,
    }
}

/// Resolves the trait ref of the given instance.
#[extend]
pub async fn get_trait_ref(
    self: &Instance,
    engine: &TrackedEngine,
) -> Option<TraitRef> {
    match self {
        Instance::Symbol(symbol) => {
            let mut trait_ref = engine
                .get_trait_ref_of_instance_symbol(symbol.id())
                .await
                .map(|x| x.deref().clone())?;

            let inst = symbol.create_instantiation(engine).await;

            trait_ref.instantiate(&inst);

            Some(trait_ref)
        }

        Instance::AnonymousTrait(tr) => {
            let ident_generic_args =
                tr.trait_id().create_identity_generic_arguments(engine).await;

            Some(TraitRef::new(tr.trait_id(), ident_generic_args))
        }

        Instance::Parameter(member_id) => engine
            .get_generic_parameters(member_id.parent_id())
            .await[member_id.id()]
        .trait_ref()
        .map(|x| x.deref().clone()),

        Instance::InstanceAssociated(instance_associated) => {
            let mut trait_ref = engine
                .get_trait_ref_of_instance_symbol(
                    instance_associated.trait_associated_symbol_id(),
                )
                .await?
                .deref()
                .clone();

            let instantiation =
                Box::pin(instance_associated.create_instantiation(engine))
                    .await?;

            trait_ref.instantiate(&instantiation);

            Some(trait_ref)
        }

        Instance::Inference(_) | Instance::Error(_) => None,
    }
}

/// Creates an [`Instantiation`] based on the [`TraitRef`] of this
/// `InstanceAssociated` and the generic arguments supplied to this
/// `InstanceAssociated`.
#[extend]
pub async fn create_instantiation(
    self: &InstanceAssociated,
    engine: &TrackedEngine,
) -> Option<Instantiation> {
    let parent_trait_ref = self.instance().get_trait_ref(engine).await?;

    let mut instantiation = parent_trait_ref.create_instantiation(engine).await;
    instantiation.append_from_generic_arguments(
        self.associated_instance_generic_arguments(),
        self.trait_associated_symbol_id(),
        &*engine
            .get_generic_parameters(self.trait_associated_symbol_id())
            .await,
    );
    instantiation.insert_instance_mapping(
        Instance::new_anonymous_trait(parent_trait_ref.trait_id()),
        self.instance().clone(),
    );

    Some(instantiation)
}
