use pernixc_query::TrackedEngine;
use pernixc_symbol::source_file_module::Key;

use crate::table::{self, MapKey, TableKey};

#[pernixc_query::executor(key(Key), name(Executor))]
pub async fn source_file_module_executor(
    Key(source_file_id): &Key,
    engine: &TrackedEngine,
) -> Result<pernixc_symbol::ID, pernixc_query::runtime::executor::CyclicError> {
    let table = engine.query(&MapKey(source_file_id.target_id)).await?;
    let (_, external_submodule) =
        table.paths_by_source_id.get(&source_file_id.id).unwrap();

    let table_key = external_submodule.as_ref().map_or_else(
        || table::Key::Root(source_file_id.target_id),
        |external_submodule| table::Key::Submodule {
            external_submodule: external_submodule.clone(),
            target_id: source_file_id.target_id,
        },
    );

    let table = engine.query(&TableKey(table_key)).await?;

    Ok(table.module_id)
}

pernixc_register::register!(Key, Executor);
