use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_source_file::calculate_path_id;
use pernixc_symbol::module_kind::{Key, ModuleKind};

use crate::table::get_table_of_symbol;

#[pernixc_query::executor(key(Key), name(ModuleKindExecutor))]
pub async fn module_kind_executor(
    &Key(key): &Key,
    engine: &TrackedEngine,
) -> Result<ModuleKind, CyclicError> {
    let map = engine.get_table_of_symbol(key).await;

    // if the module presents in the map.external_submodules, it is an external
    // module.
    if let Some(exteranl_submodule) = map.external_submodules.get(&key.id) {
        let path = exteranl_submodule.path.clone();

        Ok(ModuleKind::ExteranlFile(
            engine.calculate_path_id(&path, key.target_id).await.ok(),
        ))
    } else {
        Ok(ModuleKind::Inline)
    }
}

pernixc_register::register!(Key, ModuleKindExecutor);
