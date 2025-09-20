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

/// Initializes all the core library intrinsics.
pub async fn initialize_corelib(engine: &mut Arc<Engine>) {
    let root_core_module_id = {
        let tracked_engine = engine.tracked();

        TargetID::CORE.make_global(
            tracked_engine.get_target_root_module_id(TargetID::CORE).await,
        )
    };

    let drop_trait_id = drop::initialize_drop_trait(engine).await;

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
    input_lock
        .set_input(
            member::Key(root_core_module_id),
            Arc::new(Member {
                member_ids_by_name: std::iter::once((
                    "drop".into(),
                    drop_trait_id,
                ))
                .collect(),
                unnameds: HashSet::default(),
            }),
        )
        .await;
}
