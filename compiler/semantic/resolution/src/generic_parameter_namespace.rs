//! Contains the helper query for creating an extra namespace that includes all
//! the generic parameters scope.

use std::{collections::hash_map::Entry, sync::Arc};

use linkme::distributed_slice;
use pernixc_extend::extend;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::{kind::get_kind, parent::scope_walker};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_parameters::{
        ConstantParameterID, LifetimeParameterID, TypeParameterID,
        get_generic_parameters,
    },
    lifetime::Lifetime,
    r#type::Type,
};
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
};

use crate::ExtraNamespace;

/// The key type used to query the generic parameter namespace.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Arc<ExtraNamespace>)]
#[extend(name = get_generic_parameter_namespace, by_val)]
pub struct Key {
    /// The global ID of the symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}

/// Gets the [`ExtraNamespace`] that includes all the generic parameters in the
/// scope.
#[executor(config = Config)]
async fn get_generic_parameter_namespace_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Arc<ExtraNamespace> {
    let mut extra_namespace = ExtraNamespace::default();

    let mut scope_walker = engine.scope_walker(key.symbol_id);
    while let Some(scope) = scope_walker.next().await {
        let scope = Global::new(key.symbol_id.target_id, scope);
        let symbol_kind = engine.get_kind(scope).await;

        if !symbol_kind.has_generic_parameters() {
            continue;
        }

        let generic_parameter = engine.get_generic_parameters(scope).await;

        for (name, lt) in generic_parameter.lifetime_parameter_ids_by_name() {
            if let Entry::Vacant(entry) =
                extra_namespace.lifetimes.entry(name.clone())
            {
                entry.insert(Lifetime::Parameter(LifetimeParameterID {
                    parent_id: scope,
                    id: *lt,
                }));
            }
        }

        for (name, ty) in generic_parameter.type_parameter_ids_by_name() {
            if let Entry::Vacant(entry) =
                extra_namespace.types.entry(name.clone())
            {
                entry.insert(Type::Parameter(TypeParameterID {
                    parent_id: scope,
                    id: *ty,
                }));
            }
        }

        for (name, constant) in
            generic_parameter.constant_parameter_ids_by_name()
        {
            if let Entry::Vacant(entry) =
                extra_namespace.constants.entry(name.clone())
            {
                entry.insert(Constant::Parameter(ConstantParameterID {
                    parent_id: scope,
                    id: *constant,
                }));
            }
        }
    }

    Arc::new(extra_namespace)
}

#[distributed_slice(PERNIX_PROGRAM)]
static GET_GENERIC_PARAMETER_NAMESPACE_EXECUTOR: Registration<Config> =
    Registration::new::<Key, GetGenericParameterNamespaceExecutor>();
