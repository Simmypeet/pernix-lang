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
    generic_parameters::{
        self, GenericParameters, TypeParameter, TypeParameterID,
    },
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

/// Contains the IDs of all intrinsic symbols.
#[derive(Debug, Clone, Copy)]
pub struct IntrinsicIds {
    /// The ID of the `sizeof` function.
    pub sizeof_id: pernixc_symbol::ID,
    /// The ID of the `alignof` function.
    pub alignof_id: pernixc_symbol::ID,
    /// The ID of the `dropAt` function.
    pub drop_at_id: pernixc_symbol::ID,
    /// The ID of the `NoDrop` struct.
    pub no_drop_id: pernixc_symbol::ID,
    /// The ID of the `read` function.
    pub read_id: pernixc_symbol::ID,
}

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
) -> IntrinsicIds {
    let root_target_module_id = {
        let tracked_engine = engine.tracked();
        tracked_engine.get_target_root_module_id(TargetID::CORE).await
    };

    IntrinsicIds {
        sizeof_id: initialize_sizeof(engine, root_target_module_id).await.id,
        alignof_id: initialize_alignof(engine, root_target_module_id).await.id,
        drop_at_id: initialize_drop_at(engine, root_target_module_id)
            .await
            .id,
        no_drop_id: initialize_no_drop(engine, root_target_module_id).await.id,
        read_id: initialize_read(engine, root_target_module_id).await.id,
    }
}

/// Helper function to initialize a generic intrinsic function with a single type parameter.
/// 
/// The `build_params_and_return` callback receives the function ID and the generic type parameter T,
/// and should return (parameters, return_type) for the function.
async fn initialize_generic_function<F>(
    engine: &mut Arc<Engine>,
    root_target_module_id: pernixc_symbol::ID,
    name_sequence: [&str; 2],
    name: &str,
    build_params_and_return: F,
) -> Global<pernixc_symbol::ID>
where
    F: FnOnce(Global<pernixc_symbol::ID>, Type) -> (Arc<Parameters>, Arc<Type>),
{
    let function_id = {
        let tracked_engine = engine.tracked();
        TargetID::CORE.make_global(
            tracked_engine
                .calculate_qualified_name_id(
                    name_sequence,
                    TargetID::CORE,
                    Some(root_target_module_id),
                    0,
                )
                .await,
        )
    };

    let mut generic_params = GenericParameters::default();
    let t_param_idx = generic_params
        .add_type_parameter(TypeParameter { name: "T".into(), span: None })
        .unwrap();
    let t_ty = Type::Parameter(TypeParameterID::new(function_id, t_param_idx));

    let (parameters, return_type) = build_params_and_return(function_id, t_ty);

    let input_lock = Arc::get_mut(engine).unwrap().input_lock();

    input_lock
        .set_input(kind::Key(function_id), kind::Kind::Function)
        .await;
    input_lock.set_input(name::Key(function_id), name.into()).await;
    input_lock
        .set_input(parent::Key(function_id), Some(root_target_module_id))
        .await;
    input_lock
        .set_input(
            generic_parameters::Key(function_id),
            Arc::new(generic_params),
        )
        .await;
    input_lock
        .set_input(accessibility::Key(function_id), Accessibility::Public)
        .await;
    input_lock
        .set_input(elided_lifetime::Key(function_id), Arc::default())
        .await;
    input_lock
        .set_input(implied_predicate::Key(function_id), Arc::default())
        .await;
    input_lock
        .set_input(where_clause::Key(function_id), Arc::default())
        .await;
    input_lock
        .set_input(parameter::Key(function_id), parameters)
        .await;
    input_lock
        .set_input(return_type::Key(function_id), return_type)
        .await;

    function_id
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
    initialize_generic_function(
        engine,
        root_target_module_id,
        SIZEOF_FUNCTION_SEQUENCE,
        SIZEOF_FUNCTION_NAME,
        |_function_id, _t_ty| {
            (
                Arc::new(Parameters {
                    parameters: Arena::default(),
                    parameter_order: Vec::new(),
                }),
                Arc::new(Type::Primitive(Primitive::Usize)),
            )
        },
    )
    .await
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
    initialize_generic_function(
        engine,
        root_target_module_id,
        ALIGNOF_FUNCTION_SEQUENCE,
        ALIGNOF_FUNCTION_NAME,
        |_function_id, _t_ty| {
            (
                Arc::new(Parameters {
                    parameters: Arena::default(),
                    parameter_order: Vec::new(),
                }),
                Arc::new(Type::Primitive(Primitive::Usize)),
            )
        },
    )
    .await
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
    initialize_generic_function(
        engine,
        root_target_module_id,
        DROPAT_FUNCTION_SEQUENCE,
        DROPAT_FUNCTION_NAME,
        |_function_id, t_ty| {
            let mut parameters = Arena::default();
            let param_id = parameters.insert(Parameter {
                r#type: Type::Pointer(Pointer {
                    mutable: true,
                    pointee: Box::new(t_ty),
                }),
                span: None,
            });

            (
                Arc::new(Parameters {
                    parameters,
                    parameter_order: vec![param_id],
                }),
                Arc::new(Type::unit()),
            )
        },
    )
    .await
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
    input_lock.set_input(kind::Key(no_drop_id), kind::Kind::Struct).await;
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
    input_lock.set_input(where_clause::Key(no_drop_id), Arc::default()).await;
    input_lock.set_input(member::Key(no_drop_id), Arc::default()).await;

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
    initialize_generic_function(
        engine,
        root_target_module_id,
        READ_FUNCTION_SEQUENCE,
        READ_FUNCTION_NAME,
        |_function_id, t_ty| {
            let mut parameters = Arena::default();
            let param_id = parameters.insert(Parameter {
                r#type: Type::Pointer(Pointer {
                    mutable: false,
                    pointee: Box::new(t_ty.clone()),
                }),
                span: None,
            });

            (
                Arc::new(Parameters {
                    parameters,
                    parameter_order: vec![param_id],
                }),
                Arc::new(t_ty),
            )
        },
    )
    .await
}
