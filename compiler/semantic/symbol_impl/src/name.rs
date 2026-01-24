use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::name::Key;
use qbice::{executor, program::Registration, storage::intern::Interned};

use crate::table::get_table_of_symbol;

#[executor(config = Config)]
async fn name_executor(key: &Key, engine: &TrackedEngine) -> Interned<str> {
    let id = key.symbol_id;
    let table = engine.get_table_of_symbol(id).await;

    table
        .names
        .get(&id.id)
        .cloned()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", id.id))
}

#[distributed_slice(PERNIX_PROGRAM)]
static NAME_EXECUTOR: Registration<Config> =
    Registration::new::<Key, NameExecutor>();
