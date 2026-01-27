use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_source_file::get_stable_path_id;
use pernixc_symbol::{
    module_kind::{Key, ModuleKind},
    parent::get_parent_global,
};
use pernixc_target::get_invocation_arguments;
use qbice::{executor, program::Registration};

use crate::table::get_table_of_symbol;

#[executor(config = Config)]
async fn module_kind_executor(
    &Key { module_id }: &Key,
    engine: &TrackedEngine,
) -> ModuleKind {
    // if no parent module, the module is at the root.
    let Some(parent_module_id) = engine.get_parent_global(module_id).await
    else {
        let target_args =
            engine.get_invocation_arguments(module_id.target_id).await;

        let root_src_id = engine
            .get_stable_path_id(
                engine.intern_unsized(target_args.command.input().file.clone()),
            )
            .await
            .ok();

        return ModuleKind::ExteranlFile(root_src_id);
    };

    // gets the map of the parent, which should have an external_submodules
    // containing the current module id, if it's an external module
    let map = engine.get_table_of_symbol(parent_module_id).await;

    // if the module presents in the map.external_submodules, it is an external
    // module.
    if let Some(exteranl_submodule) = map.external_submodules.get(&module_id.id)
    {
        let path = exteranl_submodule.path.clone();

        ModuleKind::ExteranlFile(engine.get_stable_path_id(path).await.ok())
    } else {
        ModuleKind::Inline
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static MODULE_KIND_EXECUTOR: Registration<Config> =
    Registration::new::<Key, ModuleKindExecutor>();
