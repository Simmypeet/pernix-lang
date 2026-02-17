use std::{fmt::Debug, hash::Hash, sync::Arc};

use linkme::distributed_slice;
use pernixc_extend::extend;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::{
    AllAdtIDKey, AllFunctionWithBodyIDKey, AllImplementsIDKey, ID,
    kind::{Key, Kind},
};
use pernixc_target::{Global, TargetID};
use pernixc_tokio::{chunk::chunk_for_tasks, join_set::JoinSet};
use qbice::{
    Decode, Encode, Executor, Identifiable, Query, StableHash, executor,
    program::Registration,
};

use crate::table::{self, MapKey, get_table_of_symbol};

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
#[value(Option<Kind>)]
pub struct ProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn projection_executor(
    key: &ProjectionKey,
    engine: &TrackedEngine,
) -> Option<Kind> {
    let id = key.symbol_id;
    let table = engine.get_table_of_symbol(id).await?;

    table.kinds.get(&id.id).copied()
}

#[distributed_slice(PERNIX_PROGRAM)]
static PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<ProjectionKey, ProjectionExecutor>();

#[executor(config = Config)]
async fn kind_executor(key: &Key, engine: &TrackedEngine) -> Kind {
    engine.query(&ProjectionKey { symbol_id: key.symbol_id }).await.unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static KIND_EXECUTOR: Registration<Config> =
    Registration::new::<Key, KindExecutor>();

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
    Encode,
    Decode,
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
    T: Eq
        + Hash
        + Clone
        + Encode
        + Decode
        + Debug
        + Identifiable
        + StableHash
        + Send
        + Sync
        + 'static,
> Query for FilterKey<T>
{
    type Value = Arc<[ID]>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, StableHash, Default)]
struct FilterExecutor;

impl<
    T: Debug
        + Clone
        + Eq
        + Hash
        + StableHash
        + Filter
        + Encode
        + Decode
        + Send
        + Sync
        + Identifiable
        + 'static,
> Executor<FilterKey<T>, Config> for FilterExecutor
{
    async fn execute(
        &self,
        query: &FilterKey<T>,
        engine: &TrackedEngine,
    ) -> Arc<[ID]> {
        let map = engine.query(&MapKey(query.target_id)).await;

        let ids = map.keys_by_symbol_id.keys().copied().collect::<Vec<_>>();
        let mut handles = JoinSet::new();

        for id_chunk in
            ids.chunk_for_tasks().map(<[pernixc_symbol::ID]>::to_vec)
        {
            let map = map.clone();
            let engine = engine.clone();
            let target_id = query.target_id;
            let filter = query.filter.clone();

            handles.spawn(async move {
                let mut results = Vec::new();

                for id in id_chunk {
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

                    let node = engine.query(&table::KindMapKey(node_key)).await;

                    if filter
                        .filter(*node.get(&id).unwrap_or_else(|| {
                            panic!(
                                "invalid symbol ID: {id:?}\n node: \
                                 {node:#?}\n table: {map:#?}\n"
                            )
                        }))
                        .await
                    {
                        results.push(id);
                    }
                }

                results
            });
        }

        let mut results = Vec::new();
        while let Some(symbol) = handles.next().await {
            results.extend(symbol);
        }

        Arc::from(results)
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static EQUALS_FILTER_EXECUTOR: Registration<Config> =
    Registration::new::<FilterKey<EqualsFilter>, FilterExecutor>();

/// A struct implementing the [`Filter`] trait that filters symbols by checking
/// if their kind equals the specified kind.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Encode,
    Decode,
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
    self.query(&FilterKey { target_id, filter: EqualsFilter(kind) }).await
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
    Encode,
    Decode,
    Identifiable,
)]
pub struct AdtFilter;

impl Filter for AdtFilter {
    async fn filter(&self, kind: Kind) -> bool { kind.is_adt() }
}

#[distributed_slice(PERNIX_PROGRAM)]
static ADT_FILTER_EXECUTOR: Registration<Config> =
    Registration::new::<FilterKey<AdtFilter>, FilterExecutor>();

#[executor(config = Config)]
async fn all_adt_ids_executor(
    key: &AllAdtIDKey,
    engine: &TrackedEngine,
) -> Arc<[ID]> {
    engine
        .query(&FilterKey { target_id: key.target_id, filter: AdtFilter })
        .await
}

#[distributed_slice(PERNIX_PROGRAM)]
static ALL_ADT_ID_EXECUTOR: Registration<Config> =
    Registration::new::<AllAdtIDKey, AllAdtIdsExecutor>();

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
    Identifiable,
)]
pub struct ImplementationFilter;

impl Filter for ImplementationFilter {
    async fn filter(&self, kind: pernixc_symbol::kind::Kind) -> bool {
        kind.is_implementation()
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLEMENTATION_FILTER_EXECUTOR: Registration<Config> =
    Registration::new::<FilterKey<ImplementationFilter>, FilterExecutor>();

#[executor(config = Config)]
async fn all_implements_ids_executor(
    key: &AllImplementsIDKey,
    engine: &TrackedEngine,
) -> Arc<[ID]> {
    engine
        .query(&FilterKey {
            target_id: key.target_id,
            filter: ImplementationFilter,
        })
        .await
}

#[distributed_slice(PERNIX_PROGRAM)]
static ALL_IMPLEMENTS_ID_EXECUTOR: Registration<Config> =
    Registration::new::<AllImplementsIDKey, AllImplementsIdsExecutor>();

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
    Identifiable,
)]
pub struct FunctionWithBodyFilter;

impl Filter for FunctionWithBodyFilter {
    async fn filter(&self, kind: pernixc_symbol::kind::Kind) -> bool {
        kind.has_function_body()
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static FUNCTION_WITH_BODY_FILTER_EXECUTOR: Registration<Config> =
    Registration::new::<FilterKey<FunctionWithBodyFilter>, FilterExecutor>();

#[executor(config = Config)]
async fn all_function_ids_executor(
    key: &AllFunctionWithBodyIDKey,
    engine: &TrackedEngine,
) -> Arc<[ID]> {
    engine
        .query(&FilterKey {
            target_id: key.target_id,
            filter: FunctionWithBodyFilter,
        })
        .await
}

#[distributed_slice(PERNIX_PROGRAM)]
static ALL_FUNCTION_WITH_BODY_ID_EXECUTOR: Registration<Config> =
    Registration::new::<AllFunctionWithBodyIDKey, AllFunctionIdsExecutor>();
