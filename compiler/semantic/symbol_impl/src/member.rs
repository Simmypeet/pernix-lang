use std::sync::Arc;

use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_symbol::member::{Key, Member};

use crate::table::get_table_of_symbol;

#[pernixc_query::executor(key(Key), name(Executor))]
#[allow(clippy::unnecessary_wraps)]
pub async fn executor(
    &Key(id): &Key,
    engine: &TrackedEngine,
) -> Result<Arc<Member>, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;

    Ok(table
        .members
        .get(&id.id)
        .cloned()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", id.id)))
}

pernixc_register::register!(Key, Executor);
