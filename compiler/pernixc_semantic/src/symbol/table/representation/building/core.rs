//! Contains the logic for initializing the core symbols to the symbol table.

use std::collections::HashMap;

use strum::IntoEnumIterator;

use crate::{
    arena::ID,
    symbol::{
        self,
        table::representation::{
            Container, Element, IndexMut, Insertion, Representation,
        },
        Accessibility, ConstantParameter, FunctionIR, FunctionTemplate,
        GenericDeclaration, GenericID, GenericTemplate, Intrinsics,
        LifetimeParameter, MemberID, Module, Parameter, ParentSealed,
        PositiveTraitImplementationDefinition, PredicateKind, Trait,
        TraitDefinition, TraitFunctionDefinition,
        TraitImplementationFunctionDefinition, TypeParameter,
    },
    type_system::{
        model::Default,
        predicate::{self, Outlives},
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Array, Primitive, Qualifier, Reference, Type},
            GenericArguments, Tuple, TupleElement,
        },
    },
};

impl<C: Container> Representation<C> {
    /// Initializehjs the core symbols to the symbol table.
    pub(in crate::symbol::table::representation) fn initialize_core(&mut self) {
        let Insertion { id: core_module_id, duplication } =
            self.create_root_module("core".to_string());

        let copy_trait_id = self.initialize_copy_trait(core_module_id);
        self.initialize_drop_trait(core_module_id);

        // initialize the copy implementations for primitive types
        for primitive_ty in Primitive::iter() {
            self.initialize_primitive_copy_implementation(
                core_module_id,
                copy_trait_id,
                Type::Primitive(primitive_ty),
            );
        }

        self.initialize_array_copy_implementation(
            core_module_id,
            copy_trait_id,
        );
        self.initialize_tuple_copy_implementation(
            core_module_id,
            copy_trait_id,
        );
        self.initialize_reference_copy_implementation(
            core_module_id,
            copy_trait_id,
            Qualifier::Immutable,
        );
        self.initialize_reference_copy_implementation(
            core_module_id,
            copy_trait_id,
            Qualifier::Mutable,
        );

        assert!(duplication.is_none());
    }

    /// Initializes the `Drop` trait of the core module.
    ///
    /// Synopsis:
    ///
    /// ``` txt
    /// public trait Drop[T] {
    ///     public function drop['a](value: &'a unique T)
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
                    name: None,
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
                    qualifier: Qualifier::Unique,
                    lifetime: lifetime_parameter_term,
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
                    kind: PredicateKind::Explicit(None),
                },
            );

            drop_function.return_type =
                Type::Tuple(Tuple { elements: Vec::new() });
        };
    }

    /// Creates an implementation of the `Copy` trait for the reference types.
    ///
    /// Synopsis:
    ///
    /// ``` txt
    /// implements['a, T] const Copy[&'a T]
    /// where
    ///     T: 'a
    /// {
    ///     public function copy['b](value: &'b &'a T): T
    ///     where
    ///         &'a T: 'b
    ///     { ... }
    /// }
    /// ```
    fn initialize_reference_copy_implementation(
        &mut self,
        core_module_id: ID<Module>,
        copy_trait_id: ID<Trait>,
        qualifier: Qualifier,
    ) {
        let (trait_generic_declaration, ty_parameter_id, lt_parameter_id) = {
            let mut generic_declaration = GenericDeclaration::default();

            let t_parameter_id = generic_declaration
                .parameters
                .add_type_parameter(TypeParameter { name: None, span: None });

            let a_parameter_id =
                generic_declaration.parameters.add_lifetime_parameter(
                    LifetimeParameter { name: None, span: None },
                );

            (generic_declaration, t_parameter_id, a_parameter_id)
        };

        let implementation_id = self
            .insert_implementation(
                copy_trait_id,
                core_module_id,
                trait_generic_declaration,
                None,
                GenericArguments::default(),
                PositiveTraitImplementationDefinition::default(),
            )
            .unwrap();

        let mut implementation = self.get_mut(implementation_id).unwrap();

        let implementation_ty_parameter_term = Type::Parameter(MemberID {
            parent: implementation_id.into(),
            id: ty_parameter_id.unwrap(),
        });
        let implementation_lt_parameter_term = Lifetime::Parameter(MemberID {
            parent: implementation_id.into(),
            id: lt_parameter_id.unwrap(),
        });

        let reference_type = Type::Reference(Reference {
            qualifier,
            lifetime: implementation_lt_parameter_term,
            pointee: Box::new(implementation_ty_parameter_term.clone()),
        });

        // insert the type arguments
        implementation.arguments = GenericArguments {
            lifetimes: Vec::new(),
            types: vec![reference_type.clone()],
            constants: Vec::new(),
        };

        // insert the T: 'a predicate
        implementation.generic_declaration.predicates.push(symbol::Predicate {
            predicate: predicate::Predicate::TypeOutlives(Outlives {
                operand: implementation_ty_parameter_term,
                bound: implementation_lt_parameter_term,
            }),
            kind: PredicateKind::Explicit(None),
        });

        drop(implementation);

        // create the copy function
        let copy_function_id =
            self.insert_copy_function(implementation_id, reference_type);

        // set the intrinsics
        let mut copy_function = self.get_mut(copy_function_id).unwrap();
        copy_function.ir = FunctionIR::Intrinsics(Intrinsics::Memcpy);
    }

    /// Creates an implementation of the `Copy` trait that uses the `memcpy`
    /// function to copy the memory of the value.
    ///
    /// Synopsis:
    ///
    /// ``` txt
    /// implements const Copy[T] {
    ///     public function copy['a](value: &'a T): T
    ///     where
    ///         T: 'a
    ///     { ... }
    /// }
    /// ```
    fn initialize_primitive_copy_implementation(
        &mut self,
        core_module_id: ID<Module>,
        copy_trait_id: ID<Trait>,
        ty: Type<Default>,
    ) {
        // create the implementation
        let implementation_id = self
            .insert_implementation(
                copy_trait_id,
                core_module_id,
                GenericDeclaration::default(),
                None,
                GenericArguments {
                    lifetimes: Vec::new(),
                    types: vec![ty.clone()],
                    constants: Vec::new(),
                },
                PositiveTraitImplementationDefinition {
                    is_const: true,
                    member_ids_by_name: HashMap::default(),
                },
            )
            .unwrap();

        // create the copy function
        let copy_function_id = self
            .insert_copy_function::<_, TraitImplementationFunctionDefinition>(
                implementation_id,
                ty,
            );

        // set the intrinsics
        let mut copy_function = self.get_mut(copy_function_id).unwrap();
        copy_function.ir = FunctionIR::Intrinsics(Intrinsics::Memcpy);
    }

    /// Creates an implementation of the `Copy` trait for the array types.
    ///
    /// Synopsis:
    ///
    /// ``` txt
    /// implements[T, const N: usize] const Copy[[T: N]]
    /// where
    ///     const trait Copy[T] // `T` is copyable
    /// {
    ///     public function copy['a'](value: &'a [T: N]): [T: N]
    ///     where
    ///         [T: N]: 'a
    ///     { ... }
    /// }
    /// ```
    fn initialize_array_copy_implementation(
        &mut self,
        core_module_id: ID<Module>,
        copy_trait_id: ID<Trait>,
    ) {
        let (trait_generic_declaration, t_parameter_id, n_parameter_id) = {
            let mut generic_declaration = GenericDeclaration::default();

            let t_parameter_id = generic_declaration
                .parameters
                .add_type_parameter(TypeParameter { name: None, span: None })
                .unwrap();

            let n_parameter_id = generic_declaration
                .parameters
                .add_constant_parameter(ConstantParameter {
                    name: None,
                    r#type: Type::Primitive(Primitive::Usize),
                    span: None,
                })
                .unwrap();

            (generic_declaration, t_parameter_id, n_parameter_id)
        };

        let implementation_id = self
            .insert_implementation(
                copy_trait_id,
                core_module_id,
                trait_generic_declaration,
                None,
                GenericArguments::default(),
                PositiveTraitImplementationDefinition {
                    is_const: true,
                    member_ids_by_name: HashMap::default(),
                },
            )
            .unwrap();

        let mut implementation = self.get_mut(implementation_id).unwrap();
        let array_type = Type::Array(Array {
            length: Constant::Parameter(MemberID {
                parent: implementation_id.into(),
                id: n_parameter_id,
            }),
            r#type: Box::new(Type::Parameter(MemberID {
                parent: implementation_id.into(),
                id: t_parameter_id,
            })),
        });

        implementation.arguments = GenericArguments {
            lifetimes: Vec::new(),
            types: vec![array_type.clone()],
            constants: Vec::new(),
        };

        drop(implementation);

        let copy_function_id = self
            .insert_copy_function::<_, TraitImplementationFunctionDefinition>(
                implementation_id,
                array_type,
            );

        let mut copy_function = self.get_mut(copy_function_id).unwrap();
        copy_function.ir = FunctionIR::Intrinsics(Intrinsics::ArrayCopy);
    }

    /// Creates implementations of the `Copy` trait for the tuple types.
    ///
    /// Synopsis:
    ///
    /// ```txt
    /// implements const Copy[()] {
    ///     public function copy['a](value: &'a ()): ())
    ///     where
    ///         (): 'a
    ///     {
    ///         return ();
    ///     }
    /// }
    ///
    /// implements[T, Rest] const Copy[(T, ...Rest)]
    /// where
    ///     const trait Copy[T] + Copy[Rest], // `T` and `Rest` are copyable
    ///     tuple Rest                        // `Rest` is a tuple
    /// {
    ///     public function copy['a](value: &'a (T, ...Rest)): (T, ...Rest)
    ///     where
    ///         (T, ...Rest): 'a
    ///     { ... }
    /// }
    /// ```
    #[allow(clippy::too_many_lines)]
    fn initialize_tuple_copy_implementation(
        &mut self,
        core_module_id: ID<Module>,
        copy_trait_id: ID<Trait>,
    ) {
        // empty tuple implementation
        {
            let implementation_id = self
                .insert_implementation(
                    copy_trait_id,
                    core_module_id,
                    GenericDeclaration::default(),
                    None,
                    GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Tuple(Tuple {
                            elements: Vec::new(),
                        })],
                        constants: Vec::new(),
                    },
                    PositiveTraitImplementationDefinition {
                        is_const: true,
                        member_ids_by_name: HashMap::default(),
                    },
                )
                .unwrap();

            let copy_function_id =
                self.insert_copy_function::<_, TraitImplementationFunctionDefinition>(
                    implementation_id,
                    Type::Tuple(Tuple { elements: Vec::new() }),
                );

            let mut copy_function = self.get_mut(copy_function_id).unwrap();
            copy_function.ir = FunctionIR::Intrinsics(Intrinsics::TupleCopy);
        }

        // full tuple copy implementation
        {
            let (generic_declaration, t_parameter_id, rest_parameter_id) = {
                let mut generic_declaration = GenericDeclaration::default();

                let t_parameter_id = generic_declaration
                    .parameters
                    .add_type_parameter(TypeParameter {
                        name: None,
                        span: None,
                    })
                    .unwrap();

                let rest_parameter_id = generic_declaration
                    .parameters
                    .add_type_parameter(TypeParameter {
                        name: None,
                        span: None,
                    })
                    .unwrap();

                (generic_declaration, t_parameter_id, rest_parameter_id)
            };

            let implementation_id = self
                .insert_implementation(
                    copy_trait_id,
                    core_module_id,
                    generic_declaration,
                    None,
                    GenericArguments::default(),
                    PositiveTraitImplementationDefinition {
                        is_const: true,
                        member_ids_by_name: HashMap::default(),
                    },
                )
                .unwrap();

            let t_type = Type::Parameter(MemberID {
                parent: implementation_id.into(),
                id: t_parameter_id,
            });
            let rest_type = Type::Parameter(MemberID {
                parent: implementation_id.into(),
                id: rest_parameter_id,
            });

            let tuple_type = Type::Tuple(Tuple {
                elements: vec![
                    TupleElement { term: t_type.clone(), is_unpacked: false },
                    TupleElement { term: rest_type.clone(), is_unpacked: true },
                ],
            });

            let mut implementation = self.get_mut(implementation_id).unwrap();
            implementation.arguments = GenericArguments {
                lifetimes: Vec::new(),
                types: vec![tuple_type.clone()],
                constants: Vec::new(),
            };

            // const trait Copy[T] + Copy[Rest]
            implementation.generic_declaration.predicates.push(
                symbol::Predicate {
                    predicate: predicate::Predicate::Trait(predicate::Trait {
                        id: copy_trait_id,
                        is_const: true,
                        generic_arguments: GenericArguments {
                            lifetimes: Vec::new(),
                            types: vec![t_type],
                            constants: Vec::new(),
                        },
                    }),
                    kind: PredicateKind::Explicit(None),
                },
            );
            implementation.generic_declaration.predicates.push(
                symbol::Predicate {
                    predicate: predicate::Predicate::Trait(predicate::Trait {
                        id: copy_trait_id,
                        is_const: true,
                        generic_arguments: GenericArguments {
                            lifetimes: Vec::new(),
                            types: vec![rest_type.clone()],
                            constants: Vec::new(),
                        },
                    }),
                    kind: PredicateKind::Explicit(None),
                },
            );

            // tuple Rest
            implementation.generic_declaration.predicates.push(
                symbol::Predicate {
                    predicate: predicate::Predicate::TupleType(
                        predicate::Tuple(rest_type),
                    ),
                    kind: PredicateKind::Explicit(None),
                },
            );

            drop(implementation);

            let copy_function_id = self
                .insert_copy_function::<_, TraitImplementationFunctionDefinition>(
                    implementation_id,
                    tuple_type,
                );

            let mut copy_function = self.get_mut(copy_function_id).unwrap();
            copy_function.ir = FunctionIR::Intrinsics(Intrinsics::TupleCopy);
        }
    }

    fn insert_copy_function<
        Parent: ParentSealed + Element,
        Definition: std::default::Default,
    >(
        &mut self,
        parent_id: ID<Parent>,
        copy_type: Type<Default>,
    ) -> ID<GenericTemplate<ID<Parent>, FunctionTemplate<Definition>>>
    where
        ID<GenericTemplate<ID<Parent>, FunctionTemplate<Definition>>>:
            Into<Parent::MemberID> + Into<GenericID>,

        GenericTemplate<ID<Parent>, FunctionTemplate<Definition>>: Element,
    {
        // create the generic declaration and lifetime parameter
        let (generic_declaration, lifetime_parameter_id) = {
            let mut generic_declaration = GenericDeclaration::default();

            let lt_parameter_id = generic_declaration
                .parameters
                .add_lifetime_parameter(LifetimeParameter {
                    name: None,
                    span: None,
                })
                .unwrap();

            (generic_declaration, lt_parameter_id)
        };
        let Insertion { id: copy_function_id, duplication } = self
            .insert_member(
                "copy".to_string(),
                Accessibility::Public,
                parent_id,
                None,
                generic_declaration,
                FunctionTemplate::<Definition>::default(),
            )
            .unwrap();

        assert!(duplication.is_none());

        let mut function = self.get_mut(copy_function_id).unwrap();
        let function_lifetime_parameter = Lifetime::Parameter(MemberID {
            parent: copy_function_id.into(),
            id: lifetime_parameter_id,
        });

        // add the parameter
        function.insert_parameter(Parameter {
            r#type: Type::Reference(Reference {
                qualifier: Qualifier::Immutable,
                lifetime: function_lifetime_parameter,
                pointee: Box::new(copy_type.clone()),
            }),
            span: None,
        });
        // add the bound
        function.generic_declaration.predicates.push(symbol::Predicate {
            predicate: predicate::Predicate::TypeOutlives(Outlives {
                operand: copy_type.clone(),
                bound: function_lifetime_parameter,
            }),
            kind: PredicateKind::Explicit(None),
        });

        // set the return type
        function.return_type = copy_type;

        copy_function_id
    }

    /// Initializes the `Copy` trait of the core module.
    ///
    /// Synopsis:
    ///
    /// ``` txt
    /// public trait Copy[T] {
    ///     public function copy['a](value: &'a T): T
    ///     where
    ///         T: 'a;
    /// }
    /// ```
    fn initialize_copy_trait(
        &mut self,
        core_module_id: ID<Module>,
    ) -> ID<Trait> {
        // initialize copy trait
        let (trait_generic_declaration, type_parameter_id) = {
            let mut generic_declaration = GenericDeclaration::default();

            let type_parameter_id = generic_declaration
                .parameters
                .add_type_parameter(TypeParameter { name: None, span: None })
                .unwrap();

            (generic_declaration, type_parameter_id)
        };

        let Insertion { id: copy_trait_id, duplication } = self
            .insert_member(
                "Copy".to_string(),
                Accessibility::Public,
                core_module_id,
                None,
                trait_generic_declaration,
                TraitDefinition::default(),
            )
            .unwrap();

        assert!(duplication.is_none());

        // the copy function
        self.insert_copy_function::<_, TraitFunctionDefinition>(
            copy_trait_id,
            Type::Parameter(MemberID {
                parent: copy_trait_id.into(),
                id: type_parameter_id,
            }),
        );

        copy_trait_id
    }
}

#[cfg(test)]
mod tests;
