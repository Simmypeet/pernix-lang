use linkme::distributed_slice;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::span::Key;
use pernixc_target::{Global, TargetID};
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
#[value(Option<Option<RelativeSpan>>)]
pub struct ProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn projection_executor(
    key: &ProjectionKey,
    engine: &TrackedEngine,
) -> Option<Option<RelativeSpan>> {
    if key.symbol_id.target_id == TargetID::CORE {
        return Some(None);
    }

    let id = key.symbol_id;
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table.spans.get(&id.id).copied()
}

#[distributed_slice(PERNIX_PROGRAM)]
static PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<ProjectionKey, ProjectionExecutor>();

#[executor(config = Config)]
async fn span_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Option<RelativeSpan> {
    engine.query(&ProjectionKey { symbol_id: key.symbol_id }).await.unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static SPAN_EXECUTOR: Registration<Config> =
    Registration::new::<Key, SpanExecutor>();
