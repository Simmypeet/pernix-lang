use linkme::distributed_slice;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::span::Key;
use pernixc_target::TargetID;
use qbice::{executor, program::Registration};

use crate::table::get_table_of_symbol;

#[executor(config = Config)]
async fn span_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Option<RelativeSpan> {
    let symbol_id = key.symbol_id;
    // the core symbols don't have spans
    if symbol_id.target_id == TargetID::CORE {
        return None;
    }

    let table = engine.get_table_of_symbol(symbol_id).await;

    table
        .spans
        .get(&symbol_id.id)
        .copied()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", symbol_id.id))
}

#[distributed_slice(PERNIX_PROGRAM)]
static SPAN_EXECUTOR: Registration<Config> =
    Registration::new::<Key, SpanExecutor>();
