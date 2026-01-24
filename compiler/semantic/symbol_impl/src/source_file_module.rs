use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::source_file_module::Key;
use qbice::{executor, program::Registration};

use crate::table::{self, MapKey, TableKey};

#[executor(config = Config)]
async fn source_file_module_executor(
    &Key { source_file_id }: &Key,
    engine: &TrackedEngine,
) -> pernixc_symbol::ID {
    let table = engine.query(&MapKey(source_file_id.target_id)).await;
    let (_, external_submodule) =
        table.paths_by_source_id.get(&source_file_id.id).unwrap();

    let table_key = external_submodule.as_ref().map_or_else(
        || table::Key::Root(source_file_id.target_id),
        |external_submodule| table::Key::Submodule {
            external_submodule: external_submodule.clone(),
            target_id: source_file_id.target_id,
        },
    );

    let table = engine.query(&TableKey(table_key)).await;

    table.module_id
}

#[distributed_slice(PERNIX_PROGRAM)]
static SOURCE_FILE_MODULE_EXECUTOR: Registration<Config> =
    Registration::new::<Key, SourceFileModuleExecutor>();
