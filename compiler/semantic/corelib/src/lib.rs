//! Setting ups the core library symbols for the compiler.

use pernixc_hash::{HashMap, HashSet};
use pernixc_qbice::InputSession;
use pernixc_semantic_element::import;
use pernixc_symbol::{
    ID, accessibility, calculate_core_root_target_module_id, kind,
    member::{self, Member},
    name, parent,
};
use pernixc_target::Global;

pub mod copy;
pub mod drop;
pub mod intrinsics;

struct CoreLibInitializer<'i> {
    input_session: &'i mut InputSession,
    root_target_module_id: Global<ID>,
    target_seed: u64,
}

/// Initializes all the core library intrinsics.
pub async fn initialize_corelib(input_session: &mut InputSession) {
    let mut initializer = CoreLibInitializer {
        input_session,
        root_target_module_id: Global::new(
            pernixc_target::TargetID::CORE,
            calculate_core_root_target_module_id(),
        ),
        target_seed: pernixc_target::CORE_TARGET_SEED,
    };

    let copy_marker_id = initializer.initialize_copy_marker().await;
    let drop_trait_id = initializer.initialize_drop_trait(copy_marker_id).await;
    let intrinsic_ids = initializer.initialize_intrinsics().await;

    initializer
        .input_session
        .set_input(
            kind::Key { symbol_id: initializer.root_target_module_id },
            kind::Kind::Module,
        )
        .await;
    initializer
        .input_session
        .set_input(
            accessibility::Key { symbol_id: initializer.root_target_module_id },
            accessibility::Accessibility::Public,
        )
        .await;
    initializer
        .input_session
        .set_input(
            parent::Key { symbol_id: initializer.root_target_module_id },
            None,
        )
        .await;
    initializer
        .input_session
        .set_input(
            name::Key { symbol_id: initializer.root_target_module_id },
            initializer.input_session.intern_unsized("core".to_owned()),
        )
        .await;
    initializer
        .input_session
        .set_input(
            import::Key { symbol_id: initializer.root_target_module_id },
            initializer.input_session.intern(HashMap::default()),
        )
        .await;

    let member_map = [
        (
            initializer.input_session.intern_unsized("Drop".to_owned()),
            drop_trait_id,
        ),
        (
            initializer.input_session.intern_unsized("Copy".to_owned()),
            copy_marker_id,
        ),
        (
            initializer
                .input_session
                .intern_unsized(intrinsics::SIZEOF_FUNCTION_NAME.to_owned()),
            intrinsic_ids.sizeof_id,
        ),
        (
            initializer
                .input_session
                .intern_unsized(intrinsics::ALIGNOF_FUNCTION_NAME.to_owned()),
            intrinsic_ids.alignof_id,
        ),
        (
            initializer
                .input_session
                .intern_unsized(intrinsics::DROPAT_FUNCTION_NAME.to_owned()),
            intrinsic_ids.drop_at_id,
        ),
        (
            initializer
                .input_session
                .intern_unsized(intrinsics::NODROP_STRUCT_NAME.to_owned()),
            intrinsic_ids.no_drop_id,
        ),
        (
            initializer
                .input_session
                .intern_unsized(intrinsics::READ_FUNCTION_NAME.to_owned()),
            intrinsic_ids.read_id,
        ),
    ]
    .into_iter()
    .collect::<pernixc_hash::HashMap<_, _>>();

    initializer
        .input_session
        .set_input(
            member::Key { symbol_id: initializer.root_target_module_id },
            initializer.input_session.intern(Member {
                member_ids_by_name: member_map,
                unnameds: HashSet::default(),
            }),
        )
        .await;
}
