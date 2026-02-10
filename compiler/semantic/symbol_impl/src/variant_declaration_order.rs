//! Defines the query to get the declaration order of a variant in an enum.

use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::variant_declaration_order::Key;
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
#[value(Option<usize>)]
pub struct ProjectionKey {
    pub variant_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn projection_executor(
    key: &ProjectionKey,
    engine: &TrackedEngine,
) -> Option<usize> {
    let id = key.variant_id;
    let table = engine.get_table_of_symbol(key.variant_id).await?;

    table.variant_declaration_orders.get(&id.id).copied()
}

#[distributed_slice(PERNIX_PROGRAM)]
static PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<ProjectionKey, ProjectionExecutor>();

#[executor(config = Config)]
async fn variant_declaration_order_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> usize {
    engine.query(&ProjectionKey { variant_id: key.variant_id }).await.unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static VARIANT_DECLARATION_ORDER_EXECUTOR: Registration<Config> =
    Registration::new::<Key, VariantDeclarationOrderExecutor>();
