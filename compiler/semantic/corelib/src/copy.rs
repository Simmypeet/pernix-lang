//! Defines and implements the `Copy` marker in the core library.

use pernixc_hash::HashSet;
use pernixc_semantic_element::{
    implemented, implements, implements_arguments,
    where_clause::{self, Predicate},
};
use pernixc_symbol::{
    accessibility, calculate_implements_id_by_unique_name_with_given_seed,
    calculate_qualified_name_id_with_given_seed, kind, name, parent,
};
use pernixc_target::{Global, TargetID};
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::{
        self, GenericParameters, LifetimeParameter, LifetimeParameterID,
        TypeParameter, TypeParameterID,
    },
    lifetime::Lifetime,
    predicate,
    r#type::{Primitive, Type},
};
use qbice::storage::intern::Interned;

use crate::CoreLibInitializer;

#[allow(missing_docs)]
pub const MARKER_NAME: &str = "Copy";
#[allow(missing_docs)]
pub const MARKER_SEQUENCE: [&str; 2] = ["core", "Copy"];

impl CoreLibInitializer<'_, '_> {
    /// Creates a `Copy` marker in the core library and implements it for all
    /// primitive types, raw pointers, and immutable references.
    ///
    /// ```txt
    /// public marker Copy[T]
    ///
    /// implements Copy[{primitive types}]
    ///
    /// implements Copy[*T]
    /// implements Copy[*mut T]
    ///
    /// implements Copy[&'a T] delete:
    ///     where:
    ///         T: 'a    
    /// ```
    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    pub async fn initialize_copy_marker(&mut self) -> pernixc_symbol::ID {
        let copy_marker_id = TargetID::CORE.make_global(
            calculate_qualified_name_id_with_given_seed(
                MARKER_SEQUENCE.iter().copied(),
                Some(self.root_target_module_id.id),
                0,
                self.target_seed,
            ),
        );

        self.input_session.set_input(
            kind::Key { symbol_id: copy_marker_id },
            kind::Kind::Marker,
        );

        self.input_session.set_input(
            name::Key { symbol_id: copy_marker_id },
            self.input_session.intern_unsized(MARKER_NAME.to_owned()),
        );

        self.input_session.set_input(
            parent::Key { symbol_id: copy_marker_id },
            Some(self.root_target_module_id.id),
        );

        self.input_session.set_input(
            accessibility::Key { symbol_id: copy_marker_id },
            accessibility::Accessibility::Public,
        );

        self.input_session.set_input(
            where_clause::Key { symbol_id: copy_marker_id },
            self.input_session.intern_unsized([]),
        );

        let mut generic_params = GenericParameters::default();

        generic_params
            .add_type_parameter(
                pernixc_term::generic_parameters::TypeParameter {
                    name: self.input_session.intern_unsized("T".to_owned()),
                    span: None,
                },
            )
            .unwrap();

        self.input_session.set_input(
            generic_parameters::Key { symbol_id: copy_marker_id },
            self.input_session.intern(generic_params),
        );

        let mut implemented = HashSet::default();

        for primitive in [
            Primitive::Int8,
            Primitive::Int16,
            Primitive::Int32,
            Primitive::Int64,
            Primitive::Uint8,
            Primitive::Uint16,
            Primitive::Uint32,
            Primitive::Uint64,
            Primitive::Float32,
            Primitive::Float64,
            Primitive::Bool,
            Primitive::Isize,
            Primitive::Usize,
        ] {
            let impl_id = self.get_impl_id(&primitive.to_string());

            self.implements_copy_marker(
                kind::Kind::PositiveImplementation,
                Type::Primitive(primitive),
                impl_id,
                copy_marker_id,
                self.input_session.intern(GenericParameters::default()),
                self.input_session.intern_unsized([]),
            );

            implemented.insert(impl_id);
        }

        // immutable reference of any type can be copied
        {
            let impl_id = self.get_impl_id("&T");

            let mut generic_parameters = GenericParameters::default();
            let a_lt = Lifetime::Parameter(LifetimeParameterID::new(
                impl_id,
                generic_parameters
                    .add_lifetime_parameter(LifetimeParameter {
                        name: self.input_session.intern_unsized("a".to_owned()),
                        span: None,
                    })
                    .unwrap(),
            ));
            let t_ty = Type::Parameter(TypeParameterID::new(
                impl_id,
                generic_parameters
                    .add_type_parameter(TypeParameter {
                        name: self.input_session.intern_unsized("T".to_owned()),
                        span: None,
                    })
                    .unwrap(),
            ));
            let where_clause = self.input_session.intern_unsized([Predicate {
                predicate: predicate::Predicate::type_outlives(
                    t_ty.clone(),
                    a_lt.clone(),
                ),
                span: None,
            }]);

            self.implements_copy_marker(
                kind::Kind::PositiveImplementation,
                Type::Reference(pernixc_term::r#type::Reference {
                    qualifier: pernixc_term::r#type::Qualifier::Immutable,
                    lifetime: a_lt,
                    pointee: Box::new(t_ty),
                }),
                impl_id,
                copy_marker_id,
                self.input_session.intern(generic_parameters),
                where_clause,
            );

            implemented.insert(impl_id);
        }

        // mutable reference can't be copied
        {
            let impl_id = self.get_impl_id("&mut T");
            let mut generic_parameters = GenericParameters::default();
            let a_lt = Lifetime::Parameter(LifetimeParameterID::new(
                impl_id,
                generic_parameters
                    .add_lifetime_parameter(LifetimeParameter {
                        name: self.input_session.intern_unsized("a".to_owned()),
                        span: None,
                    })
                    .unwrap(),
            ));
            let t_ty = Type::Parameter(TypeParameterID::new(
                impl_id,
                generic_parameters
                    .add_type_parameter(TypeParameter {
                        name: self.input_session.intern_unsized("T".to_owned()),
                        span: None,
                    })
                    .unwrap(),
            ));
            let where_clause = self.input_session.intern_unsized([Predicate {
                predicate: predicate::Predicate::type_outlives(
                    t_ty.clone(),
                    a_lt.clone(),
                ),
                span: None,
            }]);

            self.implements_copy_marker(
                kind::Kind::NegativeImplementation,
                Type::Reference(pernixc_term::r#type::Reference {
                    qualifier: pernixc_term::r#type::Qualifier::Mutable,
                    lifetime: a_lt,
                    pointee: Box::new(t_ty),
                }),
                impl_id,
                copy_marker_id,
                self.input_session.intern(generic_parameters),
                where_clause,
            );

            implemented.insert(impl_id);
        }

        // mutable/immutable pointer of any type can be copied
        let mut impl_pointer = async |mutable| {
            let impl_id =
                self.get_impl_id(if mutable { "*mut T" } else { "*const T" });

            let mut generic_parameters = GenericParameters::default();
            let t_ty = Type::Parameter(TypeParameterID::new(
                impl_id,
                generic_parameters
                    .add_type_parameter(TypeParameter {
                        name: self.input_session.intern_unsized("T".to_owned()),
                        span: None,
                    })
                    .unwrap(),
            ));

            self.implements_copy_marker(
                kind::Kind::PositiveImplementation,
                Type::Pointer(pernixc_term::r#type::Pointer {
                    mutable,
                    pointee: Box::new(t_ty),
                }),
                impl_id,
                copy_marker_id,
                self.input_session.intern(generic_parameters),
                self.input_session.intern_unsized([]),
            );

            implemented.insert(impl_id);
        };

        impl_pointer(false).await;
        impl_pointer(true).await;

        self.input_session.set_input(
            implemented::InTargetKey {
                implementable_id: copy_marker_id,
                target_id: TargetID::CORE,
            },
            self.input_session.intern(implemented),
        );

        copy_marker_id.id
    }

    fn get_impl_id(&self, ty_name: &str) -> Global<pernixc_symbol::ID> {
        TargetID::CORE.make_global(
            calculate_implements_id_by_unique_name_with_given_seed(
                &format!("core::Copy[{ty_name}]"),
                self.target_seed,
            ),
        )
    }

    fn implements_copy_marker(
        &mut self,
        kind: kind::Kind,
        rt: Type,
        impl_id: Global<pernixc_symbol::ID>,
        copy_marker_id: Global<pernixc_symbol::ID>,
        generic_parameters: Interned<GenericParameters>,
        where_clause: Interned<[Predicate]>,
    ) {
        self.input_session.set_input(kind::Key { symbol_id: impl_id }, kind);
        self.input_session.set_input(
            generic_parameters::Key { symbol_id: impl_id },
            generic_parameters,
        );
        self.input_session.set_input(
            implements::Key { symbol_id: impl_id },
            Some(copy_marker_id),
        );
        self.input_session
            .set_input(where_clause::Key { symbol_id: impl_id }, where_clause);
        self.input_session.set_input(
            implements_arguments::Key { symbol_id: impl_id },
            {
                let mut args = GenericArguments::default();
                args.types.push(rt);
                Some(self.input_session.intern(args))
            },
        );
    }
}
