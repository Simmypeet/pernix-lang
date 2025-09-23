use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_symbol::span::Key;

use crate::table::get_table_of_symbol;

#[pernixc_query::executor(key(Key), name(Executor))]
#[allow(clippy::unnecessary_wraps)]
pub async fn executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Result<Option<RelativeSpan>, CyclicError> {
    let table = engine.get_table_of_symbol(key.0).await;

    Ok(table
        .spans
        .get(&key.0.id)
        .copied()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", key.0.id)))
}

pernixc_register::register!(Key, Executor);
