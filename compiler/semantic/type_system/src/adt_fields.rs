//! Defines the convenient helper query [`get_adt_fields`].

use std::{ops::Deref, sync::Arc};

use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_symbol::{
    kind::{get_kind, Kind},
    member::get_members,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_parameters::get_generic_parameters, instantiation::Instantiation,
    r#type::Type,
};

/// Retrieves all the type terms that appear as field in the given ADT (struct
/// or enum) ID.
#[pernixc_query::query(
    key(Key),
    value(Arc<[Type]>),
    id(Global<pernixc_symbol::ID>),
    executor(Executor),
    extend(method(get_adt_fields))
)]
pub async fn adt_fields(
    adt_id: Global<pernixc_symbol::ID>,
    engine: &TrackedEngine,
) -> Result<Arc<[Type]>, executor::CyclicError> {
    let kind = engine.get_kind(adt_id).await;
    let mut results = Vec::new();

    match kind {
        Kind::Enum => {
            let variants = engine.get_members(adt_id).await;

            for variant_id in variants
                .member_ids_by_name
                .values()
                .copied()
                .chain(variants.unnameds.iter().copied())
            {
                let Some(ty) = engine
                    .query(&pernixc_semantic_element::variant::Key(
                        adt_id.target_id.make_global(variant_id),
                    ))
                    .await?
                else {
                    continue;
                };

                results.push(ty.deref().clone());
            }
        }

        Kind::Struct => {
            results.extend(
                engine
                    .query(&pernixc_semantic_element::fields::Key(adt_id))
                    .await?
                    .fields
                    .iter()
                    .map(|x| x.1.r#type.clone()),
            );
        }

        _ => panic!("should've been either an `enum` or `struct` symbol"),
    }

    Ok(results.into())
}

/// Retrieves all the fields of an ADT (struct or enum) with an instantiation
/// from the given [`generic_arguments`] applied.
#[pernixc_extend::extend]
pub async fn get_instantiated_adt_fields(
    self: &TrackedEngine,
    adt_id: Global<pernixc_symbol::ID>,
    generic_arguments: &pernixc_term::generic_arguments::GenericArguments,
) -> Result<Vec<Type>, executor::CyclicError> {
    let types = self.get_adt_fields(adt_id).await?;
    let generic_parameters = self.get_generic_parameters(adt_id).await?;

    let instantiation = Instantiation::from_generic_arguments(
        generic_arguments.clone(),
        adt_id,
        &generic_parameters,
    )
    .unwrap();

    let mut results = Vec::with_capacity(types.len());

    for mut ty in types.iter().cloned() {
        instantiation.instantiate(&mut ty);
        results.push(ty);
    }

    Ok(results)
}
