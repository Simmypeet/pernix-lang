use std::{fmt::Debug, hash::Hash, sync::Arc};

use pernixc_extend::extend;
use pernixc_query::{
    TrackedEngine,
    runtime::executor::{self, CyclicError},
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;
use pernixc_symbol::{
    AllAdtIDKey, AllFunctionWithBodyIDKey, AllImplementsIDKey, ID,
    kind::{Key, Kind},
};
use pernixc_target::TargetID;
use pernixc_tokio::scoped;

use crate::table::{self, MapKey, get_table_of_symbol};

#[pernixc_query::executor(key(Key), name(Executor))]
#[allow(clippy::unnecessary_wraps)]
pub async fn executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Result<Kind, CyclicError> {
    let table = engine.get_table_of_symbol(key.0).await;

    Ok(table
        .kinds
        .get(&key.0.id)
        .copied()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", key.0.id)))
}

pernixc_register::register!(Key, Executor);

/// A trait for filtering symbols based on their kind.
///
/// Primarily used with [`FilterKey`] to create custom filters for symbol
/// queries.
pub trait Filter {
    /// Filters the symbol based on its kind.
    fn filter(
        &self,
        kind: Kind,
    ) -> impl std::future::Future<Output = bool> + Send + Sync + use<'_, Self>;
}

/// Queries for the all of the symbol IDs in the given target that satisfy the
/// given filter.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    Identifiable,
)]
pub struct FilterKey<T> {
    /// The target ID to collect for the symbols.
    pub target_id: TargetID,

    /// The filter on the symbol's kind.
    pub filter: T,
}

impl<
    T: Debug
        + Clone
        + Eq
        + Hash
        + StableHash
        + Identifiable
        + Filter
        + Send
        + Sync
        + 'static,
> pernixc_query::Key for FilterKey<T>
{
    type Value = Arc<[ID]>;
}

pernixc_register::register!(FilterKey<EqualsFilter>, FilterExecutor);

/// An executor implementation for the [`FilterKey`] query.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct FilterExecutor;

impl<
    T: Debug
        + Clone
        + Eq
        + Hash
        + StableHash
        + Identifiable
        + Filter
        + Send
        + Sync
        + 'static,
> executor::Executor<FilterKey<T>> for FilterExecutor
{
    async fn execute(
        &self,
        engine: &TrackedEngine,
        key: &FilterKey<T>,
    ) -> Result<<FilterKey<T> as pernixc_query::Key>::Value, CyclicError> {
        let map = engine.query(&MapKey(key.target_id)).await?;

        scoped!(|handles| async move {
            for x in map.keys_by_symbol_id.keys() {
                let map = map.clone();
                let engine = engine.clone();
                let id = *x;
                let target_id = key.target_id;
                let filter = key.filter.clone();

                handles.spawn(async move {
                    let node_key = map
                        .keys_by_symbol_id
                        .get(&id)
                        .unwrap_or_else(|| panic!("invalid symbol ID: {id:?}"))
                        .as_ref()
                        .map_or_else(
                            || table::Key::Root(target_id),
                            |x| table::Key::Submodule {
                                external_submodule: x.clone(),
                                target_id,
                            },
                        );

                    let node = engine.query(&table::TableKey(node_key)).await?;

                    if filter.filter(*node.kinds.get(&id).unwrap()).await {
                        Ok(Some(id))
                    } else {
                        Ok(None)
                    }
                });
            }

            let mut results = Vec::new();
            while let Some(symbol) = handles.next().await {
                if let Some(symbol) = symbol? {
                    results.push(symbol);
                }
            }

            Ok(Arc::from(results))
        })
    }
}

/// A struct implementing the [`Filter`] trait that filters symbols by checking
/// if their kind equals the specified kind.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    Identifiable,
)]
pub struct EqualsFilter(pub Kind);

impl Filter for EqualsFilter {
    async fn filter(&self, kind: Kind) -> bool {
        let Self(expected) = *self;

        kind == expected
    }
}

/// Retrieves all symbol IDs of the specified kind within the given target.
#[extend]
pub async fn get_all_symbols_of_kind(
    self: &TrackedEngine,
    target_id: TargetID,
    kind: Kind,
) -> Arc<[ID]> {
    self.query(&FilterKey { target_id, filter: EqualsFilter(kind) })
        .await
        .unwrap()
}

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
    Serialize,
    Deserialize,
    Identifiable,
)]
pub struct AdtFilter;

pernixc_register::register!(FilterKey<AdtFilter>, FilterExecutor);

impl Filter for AdtFilter {
    async fn filter(&self, kind: Kind) -> bool { kind.is_adt() }
}

#[pernixc_query::executor(key(AllAdtIDKey), name(AllAdtIDExecutor))]
pub async fn all_adt_ids_executor(
    &AllAdtIDKey(id): &AllAdtIDKey,
    engine: &TrackedEngine,
) -> Result<Arc<[ID]>, executor::CyclicError> {
    engine.query(&FilterKey { target_id: id, filter: AdtFilter }).await
}

pernixc_register::register!(AllAdtIDKey, AllAdtIDExecutor);

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
    Serialize,
    Deserialize,
    Identifiable,
)]
pub struct ImplementationFilter;

impl Filter for ImplementationFilter {
    async fn filter(&self, kind: pernixc_symbol::kind::Kind) -> bool {
        kind.is_implementation()
    }
}

pernixc_register::register!(FilterKey<ImplementationFilter>, FilterExecutor);

#[pernixc_query::executor(
    key(AllImplementsIDKey),
    name(AllImplementsIDExecutor)
)]
pub async fn all_implements_ids_executor(
    &AllImplementsIDKey(id): &AllImplementsIDKey,
    engine: &TrackedEngine,
) -> Result<Arc<[ID]>, executor::CyclicError> {
    engine
        .query(&FilterKey { target_id: id, filter: ImplementationFilter })
        .await
}

pernixc_register::register!(AllImplementsIDKey, AllImplementsIDExecutor);

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
    Serialize,
    Deserialize,
    Identifiable,
)]
pub struct FunctionWithBodyFilter;

impl Filter for FunctionWithBodyFilter {
    async fn filter(&self, kind: pernixc_symbol::kind::Kind) -> bool {
        kind.has_function_body()
    }
}

pernixc_register::register!(FilterKey<FunctionWithBodyFilter>, FilterExecutor);

#[pernixc_query::executor(
    key(AllFunctionWithBodyIDKey),
    name(AllFunctionIDExecutor)
)]
pub async fn all_function_ids_executor(
    &AllFunctionWithBodyIDKey(id): &AllFunctionWithBodyIDKey,
    engine: &TrackedEngine,
) -> Result<Arc<[ID]>, executor::CyclicError> {
    engine
        .query(&FilterKey { target_id: id, filter: FunctionWithBodyFilter })
        .await
}

pernixc_register::register!(AllFunctionWithBodyIDKey, AllFunctionIDExecutor);
