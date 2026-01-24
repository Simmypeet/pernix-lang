use linkme::distributed_slice;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::scope_span::Key;
use qbice::{executor, program::Registration};

use crate::table::get_table_of_symbol;

#[executor(config = Config)]
async fn scope_span_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Option<RelativeSpan> {
    let id = key.symbol_id;
    let table = engine.get_table_of_symbol(id).await;

    table
        .scope_spans
        .get(&id.id)
        .copied()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", id.id))
}

#[distributed_slice(PERNIX_PROGRAM)]
static SCOPE_SPAN_EXECUTOR: Registration<Config> =
    Registration::new::<Key, ScopeSpanExecutor>();
