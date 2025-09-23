//! Defines the query to get the declaration order of a variant in an enum.

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_target::Global;

use crate::{get_table_of_symbol, ID};

#[pernixc_query::query(
    key(Key),
    id(Global<ID>),
    value(usize),
    executor(Executor),
    extend(method(get_variant_declaration_order), no_cyclic)
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn executor(
    id: Global<ID>,
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
