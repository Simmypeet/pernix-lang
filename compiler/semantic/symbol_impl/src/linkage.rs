use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::linkage::{Key, Linkage};
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
};

use crate::table::get_table_of_symbol;

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
#[value(Option<Linkage>)]
pub struct ProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn projection_executor(
    key: &ProjectionKey,
    engine: &TrackedEngine,
) -> Option<Linkage> {
    let id = key.symbol_id;
    let table = engine.get_table_of_symbol(id).await?;

    table.function_linkages.get(&id.id).copied()
}

#[distributed_slice(PERNIX_PROGRAM)]
static PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<ProjectionKey, ProjectionExecutor>();

#[executor(config = Config)]
async fn linkage_executor(key: &Key, engine: &TrackedEngine) -> Linkage {
    engine.query(&ProjectionKey { symbol_id: key.symbol_id }).await.unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static LINKAGE_EXECUTOR: Registration<Config> =
    Registration::new::<Key, LinkageExecutor>();
