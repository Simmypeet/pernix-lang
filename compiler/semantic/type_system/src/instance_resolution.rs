//! Contains the logic for resolving `instance`.
//!
//! The module for resolving an `instance` when the `instance` term is left
//! out for inference.

use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    parent::scope_walker,
};
use pernixc_target::Global;
use pernixc_term::generic_parameters::{
    InstanceParameterID, get_generic_parameters,
};
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, executor,
    program::Registration, storage::intern::Interned,
};

/// An instance available in a certain scope.
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
    Decode,
    Encode,
)]
pub enum AvailableInstance {
    /// The query site is already inside an instance symbol.
    InInstance(Global<pernixc_symbol::ID>),

    /// The instance is available from because the instance parameter can be
    /// seen in the current scope.
    FromInstanceParameter(InstanceParameterID),
}

/// Contains a list of instances available in a certain scope.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Decode,
    Encode,
    Identifiable,
)]
pub struct InherentInstanceScope {
    available_instances: Vec<AvailableInstance>,
}

/// A key for querying the inherent instances available in a certain scope.
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
    Decode,
    Encode,
    Query,
)]
#[value(Interned<InherentInstanceScope>)]
pub struct InherentInstanceScopeKey {
    /// The site where the inherent instances are being queried.
    pub current_site: Global<pernixc_symbol::ID>,
}

#[executor(config = Config)]
async fn inherent_instance_scope_executor(
    key: &InherentInstanceScopeKey,
    engine: &TrackedEngine,
) -> Interned<InherentInstanceScope> {
    let mut scope_walker = engine.scope_walker(key.current_site);
    let mut available_instances = Vec::new();

    while let Some(scope_id) = scope_walker.next().await {
        let scope_id = key.current_site.target_id.make_global(scope_id);

        let kind = engine.get_kind(scope_id).await;

        // if the current scope itself is an insace, add it to the list
        if kind == Kind::Instance {
            available_instances.push(AvailableInstance::InInstance(scope_id));
        }

        // if this kind of symbol can have generic parameters, extract the
        // instance parameters
        if kind.has_generic_parameters() {
            let generic_params = engine.get_generic_parameters(scope_id).await;

            for instance_param in generic_params.instance_parameter_order() {
                available_instances.push(
                    AvailableInstance::FromInstanceParameter(
                        InstanceParameterID::new(scope_id, instance_param),
                    ),
                );
            }
        }
    }

    engine.intern(InherentInstanceScope { available_instances })
}

#[distributed_slice(PERNIX_PROGRAM)]
static INHERENT_INSTANCE_SCOPE_EXECUTOR: Registration<Config> =
    Registration::new::<InherentInstanceScopeKey, InherentInstanceScopeExecutor>(
    );
