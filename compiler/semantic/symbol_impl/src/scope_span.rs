use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_symbol::scope_span::Key;

use crate::table::get_table_of_symbol;

#[pernixc_query::executor(key(Key), name(Executor))]
#[allow(clippy::unnecessary_wraps)]
pub async fn executor(
    &Key(id): &Key,
    engine: &TrackedEngine,
) -> Result<Option<RelativeSpan>, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;

    Ok(table
        .scope_spans
        .get(&id.id)
        .copied()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", id.id)))
}

pernixc_register::register!(Key, Executor);
