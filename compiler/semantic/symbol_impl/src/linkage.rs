use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::linkage::{Key, Linkage};
use qbice::{executor, program::Registration};

use crate::table::get_table_of_symbol;

#[executor(config = Config)]
async fn linkage_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Linkage {
    let id = key.symbol_id;
    let table = engine.get_table_of_symbol(id).await;

    table.function_linkages.get(&id.id).copied().unwrap_or_else(|| {
        panic!("invalid symbol ID or symbol is not a function: {:?}", id.id)
    })
}

#[distributed_slice(PERNIX_PROGRAM)]
static LINKAGE_EXECUTOR: Registration<Config> =
    Registration::new::<Key, LinkageExecutor>();
