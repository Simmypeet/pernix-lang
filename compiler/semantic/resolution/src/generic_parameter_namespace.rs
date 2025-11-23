//! Contains the helper query for creating an extra namespace that includes all
//! the generic parameters scope.

use std::{collections::hash_map::Entry, sync::Arc};

use pernixc_query::{TrackedEngine, runtime::executor};
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

use crate::ExtraNamespace;

/// Gets the [`ExtraNamespace`] that includes all the generic parameters in the
/// scope.
#[pernixc_query::query(
    key(Key),
    executor(Executor),
    value(Arc<ExtraNamespace>),
    id(Global<pernixc_symbol::ID>),
    extend(method(get_generic_parameter_namespace))
)]
pub async fn get_generic_parameter_namespace(
    global_id: Global<pernixc_symbol::ID>,
    engine: &TrackedEngine,
) -> Result<Arc<ExtraNamespace>, executor::CyclicError> {
    let mut extra_namespace = ExtraNamespace::default();

    let mut scope_walker = engine.scope_walker(global_id);
    while let Some(scope) = scope_walker.next().await {
        let scope = Global::new(global_id.target_id, scope);
        let symbol_kind = engine.get_kind(scope).await;

        if !symbol_kind.has_generic_parameters() {
            continue;
        }

        let generic_parameter = engine.get_generic_parameters(scope).await?;

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

    Ok(Arc::new(extra_namespace))
}

pernixc_register::register!(Key, Executor);
