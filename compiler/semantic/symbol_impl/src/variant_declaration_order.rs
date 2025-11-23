//! Defines the query to get the declaration order of a variant in an enum.

use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_symbol::variant_declaration_order::Key;

use crate::table::get_table_of_symbol;

#[pernixc_query::executor(key(Key), name(Executor))]
#[allow(clippy::unnecessary_wraps)]
pub async fn executor(
    &Key(id): &Key,
    engine: &TrackedEngine,
) -> Result<usize, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;

    Ok(table
        .variant_declaration_orders
        .get(&id.id)
        .copied()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", id.id)))
}

pernixc_register::register!(Key, Executor);
