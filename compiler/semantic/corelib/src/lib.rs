//! Setting ups the core library symbols for the compiler.

use std::sync::Arc;

use pernixc_hash::HashSet;
use pernixc_query::Engine;
use pernixc_semantic_element::import;
use pernixc_symbol::{
    accessibility, get_target_root_module_id, kind,
    member::{self, Member},
    name, parent,
};
use pernixc_target::TargetID;

pub mod copy;
pub mod drop;
pub mod intrinsics;

/// Initializes all the core library intrinsics.
pub async fn initialize_corelib(engine: &mut Arc<Engine>) {
    let root_core_module_id = {
        let tracked_engine = engine.tracked();

        TargetID::CORE.make_global(
            tracked_engine.get_target_root_module_id(TargetID::CORE).await,
        )
    };

    let copy_marker_id = copy::initialize_copy_marker(engine).await;
    let drop_trait_id =
        drop::initialize_drop_trait(engine, copy_marker_id).await;

    let intrinsic_ids = intrinsics::initialize_intrinsics(engine).await;

    let input_lock = Arc::get_mut(engine).unwrap().input_lock();

    input_lock
        .set_input(kind::Key(root_core_module_id), kind::Kind::Module)
        .await;
    input_lock
        .set_input(
            accessibility::Key(root_core_module_id),
            accessibility::Accessibility::Public,
        )
        .await;
    input_lock.set_input(parent::Key(root_core_module_id), None).await;
    input_lock.set_input(name::Key(root_core_module_id), "core".into()).await;
    input_lock
        .set_input(import::Key(root_core_module_id), Arc::default())
        .await;

    let mut member_map =
        [("Drop".into(), drop_trait_id), ("Copy".into(), copy_marker_id)]
            .into_iter()
            .collect::<pernixc_hash::HashMap<_, _>>();

    // Add intrinsics to the core module
    member_map
        .insert(intrinsics::SIZEOF_FUNCTION_NAME.into(), intrinsic_ids[0]);
    member_map
        .insert(intrinsics::ALIGNOF_FUNCTION_NAME.into(), intrinsic_ids[1]);
    member_map
        .insert(intrinsics::DROPAT_FUNCTION_NAME.into(), intrinsic_ids[2]);
    member_map.insert(intrinsics::NODROP_STRUCT_NAME.into(), intrinsic_ids[3]);
    member_map.insert(intrinsics::READ_FUNCTION_NAME.into(), intrinsic_ids[4]);

    input_lock
        .set_input(
            member::Key(root_core_module_id),
            Arc::new(Member {
                member_ids_by_name: member_map,
                unnameds: HashSet::default(),
            }),
        )
        .await;
}
