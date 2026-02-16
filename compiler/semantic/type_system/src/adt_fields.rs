//! Defines the convenient helper query [`get_adt_fields`].

use std::ops::Deref;

use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_semantic_element::{
    fields::get_fields, variant::get_variant_associated_type,
};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    member::get_members,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_parameters::get_generic_parameters, instantiation::Instantiation,
    r#type::Type,
};
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
    storage::intern::Interned,
};

/// Retrieves all the type terms that appear as field in the given ADT (struct
/// or enum) ID.
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
#[value(Interned<[Type]>)]
#[extend(name = get_adt_fields, by_val)]
pub struct Key {
    /// The global ID of the ADT (struct or enum) symbol.
    pub adt_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config)]
async fn adt_fields_executor(
    &Key { adt_id }: &Key,
    engine: &TrackedEngine,
) -> Interned<[Type]> {
    let kind = engine.get_kind(adt_id).await;
    let mut results: Vec<Type> = Vec::new();

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
                    .get_variant_associated_type(
                        adt_id.target_id.make_global(variant_id),
                    )
                    .await
                else {
                    continue;
                };

                results.push(ty.deref().clone());
            }
        }

        Kind::Struct => {
            results.extend(
                engine
                    .get_fields(adt_id)
                    .await
                    .fields
                    .iter()
                    .map(|x| x.1.r#type.clone()),
            );
        }

        _ => panic!("should've been either an `enum` or `struct` symbol"),
    }

    engine.intern_unsized(results)
}

#[distributed_slice(PERNIX_PROGRAM)]
static ADT_FIELDS_EXECUTOR: Registration<Config> =
    Registration::new::<Key, AdtFieldsExecutor>();

/// Retrieves all the fields of an ADT (struct or enum) with an instantiation
/// from the given [`generic_arguments`] applied.
#[pernixc_extend::extend]
pub async fn get_instantiated_adt_fields(
    self: &TrackedEngine,
    adt_id: Global<pernixc_symbol::ID>,
    generic_arguments: &pernixc_term::generic_arguments::GenericArguments,
) -> Vec<Type> {
    let types = self.get_adt_fields(adt_id).await;
    let generic_parameters = self.get_generic_parameters(adt_id).await;

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

    results
}
