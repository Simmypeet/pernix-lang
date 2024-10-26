//! Contains the logic for initializing the core symbols to the symbol table.

use strum::IntoEnumIterator;

use crate::{
    arena::ID,
    symbol::{
        self,
        table::representation::{
            Container, IndexMut, Insertion, Representation,
        },
        Accessibility, FunctionTemplate, GenericDeclaration, LifetimeParameter,
        LifetimeParameterID, Marker, MarkerDefinition, MemberID, Module,
        NegativeMarkerImplementationDefinition, Parameter,
        PositiveMarkerImplementationDefinition, TraitDefinition,
        TraitFunctionDefinition, TypeParameter, TypeParameterID,
    },
    type_system::{
        model::Default,
        predicate::{self, Outlives},
        term::{
            lifetime::Lifetime,
            r#type::{Primitive, Qualifier, Reference, Type},
            GenericArguments, Tuple,
        },
    },
};

impl<C: Container> Representation<C> {
    /// Initializehjs the core symbols to the symbol table.
    pub(in crate::symbol::table::representation) fn initialize_core(&mut self) {
        let Insertion { id: core_module_id, duplication } =
            self.create_root_module("core".to_string());

        let copy_marker_id = self.initialize_copy_marker(core_module_id);
        self.initialize_drop_trait(core_module_id);

        // initialize the clone implementations for primitive types
        for primitive_ty in Primitive::iter() {
            self.initialize_copy_implementation(
                core_module_id,
                copy_marker_id,
                Type::Primitive(primitive_ty),
            );
        }

        // mutable reference can't be copied
        self.initialize_mutable_reference_negative_marker(
            core_module_id,
            copy_marker_id,
        );

        assert!(duplication.is_none());
    }

    /// Initializes the `Drop` trait of the core module.
    ///
    /// Synopsis:
    ///
    /// ``` txt
    /// public trait Drop[T] {
    ///     public function drop['a](value: &'a mutable T)
    ///     where
    ///         T: 'a;
    /// }
    /// ```
    fn initialize_drop_trait(&mut self, core_module_id: ID<Module>) {
        // initialize drop trait
        let (trait_generic_declaration, type_parameter_id) = {
            let mut generic_declaration = GenericDeclaration::default();

            let type_parameter_id = generic_declaration
                .parameters
                .add_type_parameter(TypeParameter { name: None, span: None })
                .unwrap();

            (generic_declaration, type_parameter_id)
        };

        let Insertion { id: drop_trait_id, duplication } = self
            .insert_member(
                "Drop".to_string(),
                Accessibility::Public,
                core_module_id,
                None,
                trait_generic_declaration,
                TraitDefinition::default(),
            )
            .unwrap();

        assert!(duplication.is_none());

        // the drop function
        {
            let Insertion { id: drop_function_id, duplication } = self
                .insert_member(
                    "drop".to_string(),
                    Accessibility::Public,
                    drop_trait_id,
                    None,
                    GenericDeclaration::default(),
                    FunctionTemplate::<TraitFunctionDefinition>::default(),
                )
                .unwrap();

            assert!(duplication.is_none());

            let mut drop_function = self.get_mut(drop_function_id).unwrap();

            // lifetime parameter for the parameter
            let lifetime_parameter_id = drop_function
                .generic_declaration
                .parameters
                .add_lifetime_parameter(LifetimeParameter {
                    name: Some("a".to_string()),
                    span: None,
                })
                .unwrap();
            let lifetime_parameter_term = Lifetime::Parameter(MemberID {
                parent: drop_function_id.into(),
                id: lifetime_parameter_id,
            });
            let type_parameter_term = Type::Parameter(MemberID {
                parent: drop_trait_id.into(),
                id: type_parameter_id,
            });

            // add type parameter id
            drop_function.insert_parameter(Parameter {
                r#type: Type::Reference(Reference {
                    qualifier: Qualifier::Mutable,
                    lifetime: lifetime_parameter_term.clone(),
                    pointee: Box::new(type_parameter_term.clone()),
                }),
                span: None,
            });

            drop_function.generic_declaration.predicates.push(
                symbol::Predicate {
                    predicate: predicate::Predicate::TypeOutlives(Outlives {
                        operand: type_parameter_term,
                        bound: lifetime_parameter_term,
                    }),
                    span: None,
                },
            );

            drop_function.return_type =
                Type::Tuple(Tuple { elements: Vec::new() });
        };
    }

    // mutable reference can't be copied
    fn initialize_mutable_reference_negative_marker(
        &mut self,
        core_module_id: ID<Module>,
        copy_marker_id: ID<Marker>,
    ) {
        let id = self
            .insert_implementation(
                copy_marker_id,
                core_module_id,
                GenericDeclaration::default(),
                None,
                GenericArguments::default(),
                NegativeMarkerImplementationDefinition,
            )
            .unwrap();

        let mut implementation = self.get_mut(id).unwrap();

        let lifetime_parameter = Lifetime::Parameter(LifetimeParameterID {
            parent: id.into(),
            id: implementation
                .generic_declaration
                .parameters
                .add_lifetime_parameter(LifetimeParameter {
                    name: None,
                    span: None,
                })
                .unwrap(),
        });

        let type_parameter = Type::Parameter(TypeParameterID {
            parent: id.into(),
            id: implementation
                .generic_declaration
                .parameters
                .add_type_parameter(TypeParameter { name: None, span: None })
                .unwrap(),
        });

        implementation.arguments.types.push(Type::Reference(Reference {
            qualifier: Qualifier::Mutable,
            lifetime: lifetime_parameter,
            pointee: Box::new(type_parameter),
        }));
    }

    fn initialize_copy_implementation(
        &mut self,
        core_module_id: ID<Module>,
        copy_marker_id: ID<Marker>,
        ty: Type<Default>,
    ) {
        // create the implementation
        self.insert_implementation(
            copy_marker_id,
            core_module_id,
            GenericDeclaration::default(),
            None,
            GenericArguments {
                lifetimes: Vec::new(),
                types: vec![ty],
                constants: Vec::new(),
            },
            PositiveMarkerImplementationDefinition,
        )
        .unwrap();
    }

    /// Initializes the `Copy` marker of the core module.
    ///
    /// Synopsis:
    ///
    /// ``` txt
    /// public marker Copy[T];
    /// ```
    fn initialize_copy_marker(
        &mut self,
        core_module_id: ID<Module>,
    ) -> ID<Marker> {
        // initialize clone trait
        let Insertion { id: copy_marker_id, duplication } = self
            .insert_member(
                "Copy".to_string(),
                Accessibility::Public,
                core_module_id,
                None,
                {
                    let mut generic_declaration = GenericDeclaration::default();

                    generic_declaration
                        .parameters
                        .add_type_parameter(TypeParameter {
                            name: None,
                            span: None,
                        })
                        .unwrap();

                    generic_declaration
                },
                MarkerDefinition::default(),
            )
            .unwrap();

        assert!(duplication.is_none());

        copy_marker_id
    }
}

#[cfg(test)]
mod tests;
