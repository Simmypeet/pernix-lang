//! Contains the definition for intrinsic functions in the core library.

use pernixc_arena::{Arena, OrderedArena};
use pernixc_hash::HashSet;
use pernixc_semantic_element::{
    effect_annotation, elided_lifetime, fields, implied_predicate,
    parameter::{self, Parameter, Parameters},
    return_type, where_clause,
};
use pernixc_symbol::{
    accessibility::{self, Accessibility},
    calculate_qualified_name_id_with_given_seed, kind,
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

use crate::CoreLibInitializer;

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

impl CoreLibInitializer<'_> {
    /// Creates the intrinsic functions in the core library.
    ///
    /// This includes:
    /// - `sizeof[T]() -> usize`: Returns the size in bytes of type T
    /// - `alignof[T]() -> usize`: Returns the alignment in bytes of type T
    /// - `dropAt[T](pointer: *mut T)`: Drops the value at the given pointer
    /// - `NoDrop[T]`: A struct that holds a value without dropping it
    /// - `read[T](pointer: *T) -> T`: Reads a value from the given pointer
    pub async fn initialize_intrinsics(&mut self) -> IntrinsicIds {
        IntrinsicIds {
            sizeof_id: self.initialize_sizeof().await.id,
            alignof_id: self.initialize_alignof().await.id,
            drop_at_id: self.initialize_drop_at().await.id,
            no_drop_id: self.initialize_no_drop().await.id,
            read_id: self.initialize_read().await.id,
        }
    }

    /// Helper function to initialize a generic intrinsic function with a single
    /// type parameter.
    ///
    /// The `build_params_and_return` callback receives the function ID and the
    /// generic type parameter T, and should return (parameters, `return_type`)
    /// for the function.
    async fn initialize_generic_function<F>(
        &mut self,
        name_sequence: [&str; 2],
        name: &str,
        is_unsafe: bool,
        build_params_and_return: F,
    ) -> Global<pernixc_symbol::ID>
    where
        F: FnOnce(Global<pernixc_symbol::ID>, Type) -> (Parameters, Type),
    {
        let function_id = {
            TargetID::CORE.make_global(
                calculate_qualified_name_id_with_given_seed(
                    name_sequence,
                    Some(self.root_target_module_id.id),
                    0,
                    self.target_seed,
                ),
            )
        };

        let mut generic_params = GenericParameters::default();
        let t_param_idx = generic_params
            .add_type_parameter(TypeParameter {
                name: self.input_session.intern_unsized("T".to_owned()),
                span: None,
            })
            .unwrap();
        let t_ty =
            Type::Parameter(TypeParameterID::new(function_id, t_param_idx));

        let (parameters, return_type) =
            build_params_and_return(function_id, t_ty);

        self.input_session
            .set_input(
                kind::Key { symbol_id: function_id },
                kind::Kind::Function,
            )
            .await;
        self.input_session
            .set_input(
                name::Key { symbol_id: function_id },
                self.input_session.intern_unsized(name.to_owned()),
            )
            .await;
        self.input_session
            .set_input(
                parent::Key { symbol_id: function_id },
                Some(self.root_target_module_id.id),
            )
            .await;
        self.input_session
            .set_input(
                generic_parameters::Key { symbol_id: function_id },
                self.input_session.intern(generic_params),
            )
            .await;
        self.input_session
            .set_input(
                accessibility::Key { symbol_id: function_id },
                Accessibility::Public,
            )
            .await;
        self.input_session
            .set_input(
                elided_lifetime::Key { symbol_id: function_id },
                self.input_session.intern(Arena::default()),
            )
            .await;
        self.input_session
            .set_input(
                implied_predicate::Key { symbol_id: function_id },
                self.input_session.intern(HashSet::default()),
            )
            .await;
        self.input_session
            .set_input(
                where_clause::Key { symbol_id: function_id },
                self.input_session.intern_unsized([]),
            )
            .await;
        self.input_session
            .set_input(
                parameter::Key { symbol_id: function_id },
                self.input_session.intern(parameters),
            )
            .await;
        self.input_session
            .set_input(
                return_type::Key { symbol_id: function_id },
                self.input_session.intern(return_type),
            )
            .await;
        self.input_session
            .set_input(
                pernixc_symbol::r#unsafe::Key { symbol_id: function_id },
                is_unsafe,
            )
            .await;
        self.input_session
            .set_input(
                effect_annotation::Key { symbol_id: function_id },
                self.input_session.intern(OrderedArena::default()),
            )
            .await;

        function_id
    }

    /// Creates the `sizeof` intrinsic function.
    ///
    /// ```txt
    /// public function sizeof[T]() -> usize
    /// ```
    async fn initialize_sizeof(&mut self) -> Global<pernixc_symbol::ID> {
        self.initialize_generic_function(
            SIZEOF_FUNCTION_SEQUENCE,
            SIZEOF_FUNCTION_NAME,
            false,
            |_function_id, _t_ty| {
                (
                    Parameters {
                        parameters: Arena::default(),
                        parameter_order: Vec::new(),
                    },
                    Type::Primitive(Primitive::Usize),
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
    async fn initialize_alignof(&mut self) -> Global<pernixc_symbol::ID> {
        self.initialize_generic_function(
            ALIGNOF_FUNCTION_SEQUENCE,
            ALIGNOF_FUNCTION_NAME,
            false,
            |_function_id, _t_ty| {
                (
                    Parameters {
                        parameters: Arena::default(),
                        parameter_order: Vec::new(),
                    },
                    Type::Primitive(Primitive::Usize),
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
    async fn initialize_drop_at(&mut self) -> Global<pernixc_symbol::ID> {
        self.initialize_generic_function(
            DROPAT_FUNCTION_SEQUENCE,
            DROPAT_FUNCTION_NAME,
            true,
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
                    Parameters { parameters, parameter_order: vec![param_id] },
                    Type::unit(),
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
    async fn initialize_no_drop(&mut self) -> Global<pernixc_symbol::ID> {
        let no_drop_id = {
            TargetID::CORE.make_global(
                calculate_qualified_name_id_with_given_seed(
                    NODROP_STRUCT_SEQUENCE,
                    Some(self.root_target_module_id.id),
                    0,
                    self.target_seed,
                ),
            )
        };

        let mut generic_params = GenericParameters::default();
        let t_ty = Type::Parameter(TypeParameterID::new(
            no_drop_id,
            generic_params
                .add_type_parameter(TypeParameter {
                    name: self.input_session.intern_unsized("T".to_owned()),
                    span: None,
                })
                .unwrap(),
        ));

        // Set up the struct
        self.input_session
            .set_input(kind::Key { symbol_id: no_drop_id }, kind::Kind::Struct)
            .await;
        self.input_session
            .set_input(
                name::Key { symbol_id: no_drop_id },
                self.input_session.intern_unsized(NODROP_STRUCT_NAME),
            )
            .await;
        self.input_session
            .set_input(
                parent::Key { symbol_id: no_drop_id },
                Some(self.root_target_module_id.id),
            )
            .await;
        self.input_session
            .set_input(
                generic_parameters::Key { symbol_id: no_drop_id },
                self.input_session.intern(generic_params),
            )
            .await;
        self.input_session
            .set_input(
                accessibility::Key { symbol_id: no_drop_id },
                Accessibility::Public,
            )
            .await;
        self.input_session
            .set_input(
                where_clause::Key { symbol_id: no_drop_id },
                self.input_session.intern_unsized([]),
            )
            .await;
        self.input_session
            .set_input(
                member::Key { symbol_id: no_drop_id },
                self.input_session.intern(Member::default()),
            )
            .await;

        // Set up the fields
        let mut field_arena = Arena::default();
        let field_id = field_arena.insert(fields::Field {
            accessibility: Accessibility::Public,
            name: self
                .input_session
                .intern_unsized(NODROP_VALUE_FIELD_NAME.to_owned()),
            r#type: t_ty,
            span: None,
        });

        self.input_session
            .set_input(
                fields::Key { symbol_id: no_drop_id },
                self.input_session.intern(fields::Fields {
                    fields: field_arena,
                    field_ids_by_name: std::iter::once((
                        self.input_session
                            .intern_unsized(NODROP_VALUE_FIELD_NAME),
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
    async fn initialize_read(&mut self) -> Global<pernixc_symbol::ID> {
        self.initialize_generic_function(
            READ_FUNCTION_SEQUENCE,
            READ_FUNCTION_NAME,
            true,
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
                    Parameters { parameters, parameter_order: vec![param_id] },
                    t_ty,
                )
            },
        )
        .await
    }
}
