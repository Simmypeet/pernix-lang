//! Defines the convenient helper query [`get_adt_fields`].

use std::ops::Deref;

use derive_new::new;
use linkme::distributed_slice;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_semantic_element::{
    fields::get_fields, variant::get_variant_associated_type,
};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    member::get_members,
    span::get_span,
};
use pernixc_target::Global;
use pernixc_term::{generic_arguments::Symbol, r#type::Type};
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, executor,
    program::Registration, storage::intern::Interned,
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
#[value(Interned<[FieldType]>)]
#[extend(name = get_adt_fields, by_val)]
pub struct Key {
    /// The global ID of the ADT (struct or enum) symbol.
    pub adt_id: Global<pernixc_symbol::ID>,
}

/// The type of each field in the ADT, along with the span of the field if
/// available.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Identifiable,
    new,
)]
pub struct FieldType {
    r#type: Type,
    span: Option<RelativeSpan>,
}

impl FieldType {
    /// Creates a new [`FieldType`] with the given type and span.
    #[must_use]
    pub const fn new_no_span(r#type: Type) -> Self {
        Self { r#type, span: None }
    }

    /// Retrieves the type of the field.
    #[must_use]
    pub const fn r#type(&self) -> &Type { &self.r#type }

    /// Retrieves the span of the field, if available.
    #[must_use]
    pub const fn span(&self) -> Option<&RelativeSpan> { self.span.as_ref() }

    /// Converts the [`FieldType`] into its underlying type.
    #[must_use]
    pub fn into_type(self) -> Type { self.r#type }
}

#[executor(config = Config)]
async fn adt_fields_executor(
    &Key { adt_id }: &Key,
    engine: &TrackedEngine,
) -> Interned<[FieldType]> {
    let kind = engine.get_kind(adt_id).await;
    let mut results: Vec<FieldType> = Vec::new();

    match kind {
        Kind::Enum => {
            let variants = engine.get_members(adt_id).await;

            for variant_id in variants
                .member_ids_by_name
                .values()
                .copied()
                .chain(variants.unnameds.iter().copied())
            {
                let variant_id = adt_id.target_id.make_global(variant_id);

                let Some(ty) =
                    engine.get_variant_associated_type(variant_id).await
                else {
                    continue;
                };

                results.push(FieldType::new(
                    ty.deref().clone(),
                    engine.get_span(variant_id).await,
                ));
            }
        }

        Kind::Struct => {
            results.extend(
                engine
                    .get_fields(adt_id)
                    .await
                    .fields
                    .iter()
                    .map(|x| FieldType::new(x.1.r#type.clone(), x.1.span)),
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
    symbol: &Symbol,
) -> Vec<FieldType> {
    let types = self.get_adt_fields(symbol.id()).await;
    let instantiation = symbol.create_instantiation(self).await;

    let mut results = Vec::with_capacity(types.len());

    for mut ty in types.iter().cloned() {
        instantiation.instantiate(&mut ty.r#type);
        results.push(ty);
    }

    results
}
