use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_source_file::calculate_path_id;
use pernixc_symbol::{
    module_kind::{Key, ModuleKind},
    parent::get_parent_global,
};
use pernixc_target::get_invocation_arguments;

use crate::table::get_table_of_symbol;

#[pernixc_query::executor(key(Key), name(ModuleKindExecutor))]
pub async fn module_kind_executor(
    &Key(key): &Key,
    engine: &TrackedEngine,
) -> Result<ModuleKind, CyclicError> {
    // if no parent module, the module is at the root.
    let Some(parent_module_id) = engine.get_parent_global(key).await else {
        let target_args = engine.get_invocation_arguments(key.target_id).await;
        let root_src_id = engine
            .calculate_path_id(&target_args.command.input().file, key.target_id)
            .await
            .ok();

        return Ok(ModuleKind::ExteranlFile(root_src_id));
    };

    // gets the map of the parent, which should have an external_submodules
    // containing the current module id, if it's an external module
    let map = engine.get_table_of_symbol(parent_module_id).await;

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
