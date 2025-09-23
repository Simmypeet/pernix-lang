use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_symbol::linkage::{Key, Linkage};

use crate::table::get_table_of_symbol;

#[pernixc_query::executor(key(Key), name(Executor))]
pub async fn executor(
    &Key(id): &Key,
    engine: &TrackedEngine,
) -> Result<Linkage, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;

    Ok(table.function_linkages.get(&id.id).copied().unwrap_or_else(|| {
        panic!("invalid symbol ID or symbol is not a function: {:?}", id.id)
    }))
}

pernixc_register::register!(Key, Executor);
