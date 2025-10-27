//! Contains the definition for intrinsic functions in the core library.

use std::sync::Arc;

use pernixc_arena::Arena;
use pernixc_hash::HashSet;
use pernixc_query::Engine;
use pernixc_semantic_element::{
    elided_lifetime, fields, implemented, implied_predicate,
    parameter::{self, Parameter, Parameters},
    return_type, where_clause,
};
use pernixc_symbol::{
    accessibility::{self, Accessibility},
    calculate_qualified_name_id, get_target_root_module_id, kind,
    member::{self, Member},
    name, parent,
};
use pernixc_target::{Global, TargetID};
use pernixc_term::{
    generic_parameters::{self, GenericParameters, TypeParameter, TypeParameterID},
    r#type::{Pointer, Primitive, Type},
};

#[allow(missing_docs)]
pub const SIZEOF_FUNCTION_NAME: &str = "sizeof";
#[allow(missing_docs)]
pub const SIZEOF_FUNCTION_SEQUENCE: [&str; 2] = ["core", "sizeof"];

#[allow(missing_docs)]
pub const ALIGNOF_FUNCTION_NAME: &str = "alignof";
#[allow(missing_docs)]
pub const ALIGNOF_FUNCTION_SEQUENCE: [&str; 2] = ["core", "alignof"];

#[allow(missing_docs)]
pub const DROPAT_FUNCTION_NAME: &str = "dropAt";
#[allow(missing_docs)]
pub const DROPAT_FUNCTION_SEQUENCE: [&str; 2] = ["core", "dropAt"];

#[allow(missing_docs)]
pub const NODROP_STRUCT_NAME: &str = "NoDrop";
#[allow(missing_docs)]
pub const NODROP_STRUCT_SEQUENCE: [&str; 2] = ["core", "NoDrop"];
#[allow(missing_docs)]
pub const NODROP_VALUE_FIELD_NAME: &str = "value";

#[allow(missing_docs)]
pub const READ_FUNCTION_NAME: &str = "read";
#[allow(missing_docs)]
pub const READ_FUNCTION_SEQUENCE: [&str; 2] = ["core", "read"];

/// Creates the intrinsic functions in the core library.
///
/// This includes:
/// - `sizeof[T]() -> usize`: Returns the size in bytes of type T
/// - `alignof[T]() -> usize`: Returns the alignment in bytes of type T
/// - `dropAt[T](pointer: *mut T)`: Drops the value at the given pointer
/// - `NoDrop[T]`: A struct that holds a value without dropping it
/// - `read[T](pointer: *T) -> T`: Reads a value from the given pointer
pub async fn initialize_intrinsics(
    engine: &mut Arc<Engine>,
) -> Vec<pernixc_symbol::ID> {
    let root_target_module_id = {
        let tracked_engine = engine.tracked();
        tracked_engine.get_target_root_module_id(TargetID::CORE).await
    };

    let mut intrinsic_ids = Vec::new();

    // Initialize sizeof
    intrinsic_ids
        .push(initialize_sizeof(engine, root_target_module_id).await.id);

    // Initialize alignof
    intrinsic_ids
        .push(initialize_alignof(engine, root_target_module_id).await.id);

    // Initialize dropAt
    intrinsic_ids
        .push(initialize_drop_at(engine, root_target_module_id).await.id);

    // Initialize NoDrop
    intrinsic_ids
        .push(initialize_no_drop(engine, root_target_module_id).await.id);

    // Initialize read
    intrinsic_ids.push(initialize_read(engine, root_target_module_id).await.id);

    intrinsic_ids
}

/// Creates the `sizeof` intrinsic function.
///
/// ```txt
/// public function sizeof[T]() -> usize
/// ```
async fn initialize_sizeof(
    engine: &mut Arc<Engine>,
    root_target_module_id: pernixc_symbol::ID,
) -> Global<pernixc_symbol::ID> {
    let sizeof_id = {
        let tracked_engine = engine.tracked();
        TargetID::CORE.make_global(
            tracked_engine
                .calculate_qualified_name_id(
                    SIZEOF_FUNCTION_SEQUENCE,
                    TargetID::CORE,
                    Some(root_target_module_id),
                    0,
                )
                .await,
        )
    };

    let mut generic_params = GenericParameters::default();
    generic_params
        .add_type_parameter(TypeParameter { name: "T".into(), span: None })
        .unwrap();

    let input_lock = Arc::get_mut(engine).unwrap().input_lock();

    input_lock
        .set_input(kind::Key(sizeof_id), kind::Kind::Function)
        .await;
    input_lock
        .set_input(name::Key(sizeof_id), SIZEOF_FUNCTION_NAME.into())
        .await;
    input_lock
        .set_input(parent::Key(sizeof_id), Some(root_target_module_id))
        .await;
    input_lock
        .set_input(
            generic_parameters::Key(sizeof_id),
            Arc::new(generic_params),
        )
        .await;
    input_lock
        .set_input(accessibility::Key(sizeof_id), Accessibility::Public)
        .await;
    input_lock
        .set_input(elided_lifetime::Key(sizeof_id), Arc::default())
        .await;
    input_lock
        .set_input(implied_predicate::Key(sizeof_id), Arc::default())
        .await;
    input_lock
        .set_input(where_clause::Key(sizeof_id), Arc::default())
        .await;
    input_lock
        .set_input(
            parameter::Key(sizeof_id),
            Arc::new(Parameters {
                parameters: Arena::default(),
                parameter_order: Vec::new(),
            }),
        )
        .await;
    input_lock
        .set_input(
            return_type::Key(sizeof_id),
            Arc::new(Type::Primitive(Primitive::Usize)),
        )
        .await;
    input_lock
        .set_input(member::Key(sizeof_id), Arc::default())
        .await;

    sizeof_id
}

/// Creates the `alignof` intrinsic function.
///
/// ```txt
/// public function alignof[T]() -> usize
/// ```
async fn initialize_alignof(
    engine: &mut Arc<Engine>,
    root_target_module_id: pernixc_symbol::ID,
) -> Global<pernixc_symbol::ID> {
    let alignof_id = {
        let tracked_engine = engine.tracked();
        TargetID::CORE.make_global(
            tracked_engine
                .calculate_qualified_name_id(
                    ALIGNOF_FUNCTION_SEQUENCE,
                    TargetID::CORE,
                    Some(root_target_module_id),
                    0,
                )
                .await,
        )
    };

    let mut generic_params = GenericParameters::default();
    generic_params
        .add_type_parameter(TypeParameter { name: "T".into(), span: None })
        .unwrap();

    let input_lock = Arc::get_mut(engine).unwrap().input_lock();

    input_lock
        .set_input(kind::Key(alignof_id), kind::Kind::Function)
        .await;
    input_lock
        .set_input(name::Key(alignof_id), ALIGNOF_FUNCTION_NAME.into())
        .await;
    input_lock
        .set_input(parent::Key(alignof_id), Some(root_target_module_id))
        .await;
    input_lock
        .set_input(
            generic_parameters::Key(alignof_id),
            Arc::new(generic_params),
        )
        .await;
    input_lock
        .set_input(accessibility::Key(alignof_id), Accessibility::Public)
        .await;
    input_lock
        .set_input(elided_lifetime::Key(alignof_id), Arc::default())
        .await;
    input_lock
        .set_input(implied_predicate::Key(alignof_id), Arc::default())
        .await;
    input_lock
        .set_input(where_clause::Key(alignof_id), Arc::default())
        .await;
    input_lock
        .set_input(
            parameter::Key(alignof_id),
            Arc::new(Parameters {
                parameters: Arena::default(),
                parameter_order: Vec::new(),
            }),
        )
        .await;
    input_lock
        .set_input(
            return_type::Key(alignof_id),
            Arc::new(Type::Primitive(Primitive::Usize)),
        )
        .await;
    input_lock
        .set_input(member::Key(alignof_id), Arc::default())
        .await;

    alignof_id
}

/// Creates the `dropAt` intrinsic function.
///
/// ```txt
/// public unsafe function dropAt[T](pointer: *mut T)
/// ```
async fn initialize_drop_at(
    engine: &mut Arc<Engine>,
    root_target_module_id: pernixc_symbol::ID,
) -> Global<pernixc_symbol::ID> {
    let drop_at_id = {
        let tracked_engine = engine.tracked();
        TargetID::CORE.make_global(
            tracked_engine
                .calculate_qualified_name_id(
                    DROPAT_FUNCTION_SEQUENCE,
                    TargetID::CORE,
                    Some(root_target_module_id),
                    0,
                )
                .await,
        )
    };

    let mut generic_params = GenericParameters::default();
    let t_ty = Type::Parameter(TypeParameterID::new(
        drop_at_id,
        generic_params
            .add_type_parameter(TypeParameter { name: "T".into(), span: None })
            .unwrap(),
    ));

    let input_lock = Arc::get_mut(engine).unwrap().input_lock();

    input_lock
        .set_input(kind::Key(drop_at_id), kind::Kind::Function)
        .await;
    input_lock
        .set_input(name::Key(drop_at_id), DROPAT_FUNCTION_NAME.into())
        .await;
    input_lock
        .set_input(parent::Key(drop_at_id), Some(root_target_module_id))
        .await;
    input_lock
        .set_input(
            generic_parameters::Key(drop_at_id),
            Arc::new(generic_params),
        )
        .await;
    input_lock
        .set_input(accessibility::Key(drop_at_id), Accessibility::Public)
        .await;
    input_lock
        .set_input(elided_lifetime::Key(drop_at_id), Arc::default())
        .await;
    input_lock
        .set_input(implied_predicate::Key(drop_at_id), Arc::default())
        .await;
    input_lock
        .set_input(where_clause::Key(drop_at_id), Arc::default())
        .await;

    // Add parameter: pointer: *mut T
    let mut parameters = Arena::default();
    let param_id = parameters.insert(Parameter {
        r#type: Type::Pointer(Pointer {
            mutable: true,
            pointee: Box::new(t_ty),
        }),
        span: None,
    });

    input_lock
        .set_input(
            parameter::Key(drop_at_id),
            Arc::new(Parameters {
                parameters,
                parameter_order: vec![param_id],
            }),
        )
        .await;
    input_lock
        .set_input(return_type::Key(drop_at_id), Arc::new(Type::unit()))
        .await;
    input_lock
        .set_input(member::Key(drop_at_id), Arc::default())
        .await;

    drop_at_id
}

/// Creates the `NoDrop` struct.
///
/// ```txt
/// public struct NoDrop[T]:
///     public value: T
/// ```
async fn initialize_no_drop(
    engine: &mut Arc<Engine>,
    root_target_module_id: pernixc_symbol::ID,
) -> Global<pernixc_symbol::ID> {
    let no_drop_id = {
        let tracked_engine = engine.tracked();

        TargetID::CORE.make_global(
            tracked_engine
                .calculate_qualified_name_id(
                    NODROP_STRUCT_SEQUENCE,
                    TargetID::CORE,
                    Some(root_target_module_id),
                    0,
                )
                .await,
        )
    };

    let mut generic_params = GenericParameters::default();
    let t_ty = Type::Parameter(TypeParameterID::new(
        no_drop_id,
        generic_params
            .add_type_parameter(TypeParameter { name: "T".into(), span: None })
            .unwrap(),
    ));

    let input_lock = Arc::get_mut(engine).unwrap().input_lock();

    // Set up the struct
    input_lock
        .set_input(kind::Key(no_drop_id), kind::Kind::Struct)
        .await;
    input_lock
        .set_input(name::Key(no_drop_id), NODROP_STRUCT_NAME.into())
        .await;
    input_lock
        .set_input(parent::Key(no_drop_id), Some(root_target_module_id))
        .await;
    input_lock
        .set_input(
            generic_parameters::Key(no_drop_id),
            Arc::new(generic_params),
        )
        .await;
    input_lock
        .set_input(accessibility::Key(no_drop_id), Accessibility::Public)
        .await;
    input_lock
        .set_input(where_clause::Key(no_drop_id), Arc::default())
        .await;
    input_lock
        .set_input(member::Key(no_drop_id), Arc::default())
        .await;

    // Set up the fields
    let mut field_arena = Arena::default();
    let field_id = field_arena.insert(fields::Field {
        accessibility: Accessibility::Public,
        name: NODROP_VALUE_FIELD_NAME.into(),
        r#type: t_ty,
        span: None,
    });

    input_lock
        .set_input(
            fields::Key(no_drop_id),
            Arc::new(fields::Fields {
                fields: field_arena,
                field_ids_by_name: std::iter::once((
                    NODROP_VALUE_FIELD_NAME.into(),
                    field_id,
                ))
                .collect(),
                field_declaration_order: vec![field_id],
            }),
        )
        .await;

    no_drop_id
}

/// Creates the `read` intrinsic function.
///
/// ```txt
/// public unsafe function read[T](pointer: *T) -> T
/// ```
async fn initialize_read(
    engine: &mut Arc<Engine>,
    root_target_module_id: pernixc_symbol::ID,
) -> Global<pernixc_symbol::ID> {
    let read_id = {
        let tracked_engine = engine.tracked();
        TargetID::CORE.make_global(
            tracked_engine
                .calculate_qualified_name_id(
                    READ_FUNCTION_SEQUENCE,
                    TargetID::CORE,
                    Some(root_target_module_id),
                    0,
                )
                .await,
        )
    };

    let mut generic_params = GenericParameters::default();
    let t_ty = Type::Parameter(TypeParameterID::new(
        read_id,
        generic_params
            .add_type_parameter(TypeParameter { name: "T".into(), span: None })
            .unwrap(),
    ));

    let input_lock = Arc::get_mut(engine).unwrap().input_lock();

    input_lock
        .set_input(kind::Key(read_id), kind::Kind::Function)
        .await;
    input_lock
        .set_input(name::Key(read_id), READ_FUNCTION_NAME.into())
        .await;
    input_lock
        .set_input(parent::Key(read_id), Some(root_target_module_id))
        .await;
    input_lock
        .set_input(
            generic_parameters::Key(read_id),
            Arc::new(generic_params),
        )
        .await;
    input_lock
        .set_input(accessibility::Key(read_id), Accessibility::Public)
        .await;
    input_lock
        .set_input(elided_lifetime::Key(read_id), Arc::default())
        .await;
    input_lock
        .set_input(implied_predicate::Key(read_id), Arc::default())
        .await;
    input_lock
        .set_input(where_clause::Key(read_id), Arc::default())
        .await;

    // Add parameter: pointer: *T
    let mut parameters = Arena::default();
    let param_id = parameters.insert(Parameter {
        r#type: Type::Pointer(Pointer {
            mutable: false,
            pointee: Box::new(t_ty.clone()),
        }),
        span: None,
    });

    input_lock
        .set_input(
            parameter::Key(read_id),
            Arc::new(Parameters {
                parameters,
                parameter_order: vec![param_id],
            }),
        )
        .await;
    input_lock
        .set_input(return_type::Key(read_id), Arc::new(t_ty))
        .await;
    input_lock
        .set_input(member::Key(read_id), Arc::default())
        .await;

    read_id
}
