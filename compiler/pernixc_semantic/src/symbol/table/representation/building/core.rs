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
        GenericDeclaration, GenericID, GenericTemplate, Intrinsic,
        LifetimeParameter, MemberID, Module, Parameter, ParentSealed,
        PositiveTraitImplementationDefinition, Trait, TraitDefinition,
        TraitFunctionDefinition, TraitImplementationFunctionDefinition,
        TypeParameter,
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

        let clone_trait_id = self.initialize_clone_trait(core_module_id);
        self.initialize_drop_trait(core_module_id);

        // initialize the clone implementations for primitive types
        for primitive_ty in Primitive::iter() {
            self.initialize_primitive_clone_implementation(
                core_module_id,
                clone_trait_id,
                Type::Primitive(primitive_ty),
            );
        }

        self.initialize_array_clone_implementation(
            core_module_id,
            clone_trait_id,
        );
        self.initialize_tuple_clone_implementation(
            core_module_id,
            clone_trait_id,
        );
        self.initialize_reference_clone_implementation(
            core_module_id,
            clone_trait_id,
            Qualifier::Immutable,
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

    /// Creates an implementation of the `Clone` trait for the reference types.
    ///
    /// Synopsis:
    ///
    /// ``` txt
    /// implements['a, T] const Clone[&'a T]
    /// where
    ///     T: 'a
    /// {
    ///     public function clone['b](value: &'b &'a T): T
    ///     where
    ///         &'a T: 'b
    ///     { ... }
    /// }
    /// ```
    fn initialize_reference_clone_implementation(
        &mut self,
        core_module_id: ID<Module>,
        clone_trait_id: ID<Trait>,
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
                clone_trait_id,
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
            lifetime: implementation_lt_parameter_term.clone(),
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
            span: None,
        });

        drop(implementation);

        // create the clone function
        let clone_function_id =
            self.insert_clone_function(implementation_id, reference_type);

        // set the intrinsics
        let mut clone_function = self.get_mut(clone_function_id).unwrap();
        clone_function.ir = FunctionIR::Intrinsic(Intrinsic::Memcpy);
    }

    /// Creates an implementation of the `Clone` trait that uses the `memcpy`
    /// function to clone the memory of the value.
    ///
    /// Synopsis:
    ///
    /// ``` txt
    /// implements const Clone[T] {
    ///     public function clone['a](value: &'a T): T
    ///     where
    ///         T: 'a
    ///     { ... }
    /// }
    /// ```
    fn initialize_primitive_clone_implementation(
        &mut self,
        core_module_id: ID<Module>,
        clone_trait_id: ID<Trait>,
        ty: Type<Default>,
    ) {
        // create the implementation
        let implementation_id = self
            .insert_implementation(
                clone_trait_id,
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
                    is_final: true,
                    member_ids_by_name: HashMap::default(),
                },
            )
            .unwrap();

        // create the clone function
        let clone_function_id = self
            .insert_clone_function::<_, TraitImplementationFunctionDefinition>(
                implementation_id,
                ty,
            );

        // set the intrinsics
        let mut clone_function = self.get_mut(clone_function_id).unwrap();
        clone_function.ir = FunctionIR::Intrinsic(Intrinsic::Memcpy);
    }

    /// Creates an implementation of the `Clone` trait for the array types.
    ///
    /// Synopsis:
    ///
    /// ``` txt
    /// implements[T, const N: usize] const Clone[[T: N]]
    /// where
    ///     const trait Clone[T] // `T` is cloneable
    /// {
    ///     public function clone['a'](value: &'a [T: N]): [T: N]
    ///     where
    ///         [T: N]: 'a
    ///     { ... }
    /// }
    /// ```
    fn initialize_array_clone_implementation(
        &mut self,
        core_module_id: ID<Module>,
        clone_trait_id: ID<Trait>,
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
                clone_trait_id,
                core_module_id,
                trait_generic_declaration,
                None,
                GenericArguments::default(),
                PositiveTraitImplementationDefinition {
                    is_const: true,
                    is_final: true,
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

        let clone_function_id = self
            .insert_clone_function::<_, TraitImplementationFunctionDefinition>(
                implementation_id,
                array_type,
            );

        let mut clone_function = self.get_mut(clone_function_id).unwrap();
        clone_function.ir = FunctionIR::Intrinsic(Intrinsic::ArrayClone);
    }

    /// Creates implementations of the `Clone` trait for the tuple types.
    ///
    /// Synopsis:
    ///
    /// ```txt
    /// implements const Clone[()] {
    ///     public function clone['a](value: &'a ()): ())
    ///     where
    ///         (): 'a
    ///     {
    ///         return ();
    ///     }
    /// }
    ///
    /// implements[T, Rest] const Clone[(T, ...Rest)]
    /// where
    ///     const trait Clone[T] + Clone[Rest], // `T` and `Rest` are cloneable
    ///     tuple Rest                        // `Rest` is a tuple
    /// {
    ///     public function clone['a](value: &'a (T, ...Rest)): (T, ...Rest)
    ///     where
    ///         (T, ...Rest): 'a
    ///     { ... }
    /// }
    /// ```
    #[allow(clippy::too_many_lines)]
    fn initialize_tuple_clone_implementation(
        &mut self,
        core_module_id: ID<Module>,
        clone_trait_id: ID<Trait>,
    ) {
        // empty tuple implementation
        {
            let implementation_id = self
                .insert_implementation(
                    clone_trait_id,
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
                        is_final: true,
                        member_ids_by_name: HashMap::default(),
                    },
                )
                .unwrap();

            let clone_function_id = self
                .insert_clone_function::<_, TraitImplementationFunctionDefinition>(
                    implementation_id,
                    Type::Tuple(Tuple { elements: Vec::new() }),
                );

            let mut clone_function = self.get_mut(clone_function_id).unwrap();
            clone_function.ir = FunctionIR::Intrinsic(Intrinsic::TupleClone);
        }

        // full tuple clone implementation
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
                    clone_trait_id,
                    core_module_id,
                    generic_declaration,
                    None,
                    GenericArguments::default(),
                    PositiveTraitImplementationDefinition {
                        is_const: true,
                        is_final: true,
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

            // const trait Clone[T] + Clone[Rest]
            implementation.generic_declaration.predicates.push(
                symbol::Predicate {
                    predicate: predicate::Predicate::PositiveTrait(
                        predicate::PositiveTrait {
                            id: clone_trait_id,
                            is_const: true,
                            generic_arguments: GenericArguments {
                                lifetimes: Vec::new(),
                                types: vec![t_type],
                                constants: Vec::new(),
                            },
                        },
                    ),
                    span: None,
                },
            );
            implementation.generic_declaration.predicates.push(
                symbol::Predicate {
                    predicate: predicate::Predicate::PositiveTrait(
                        predicate::PositiveTrait {
                            id: clone_trait_id,
                            is_const: true,
                            generic_arguments: GenericArguments {
                                lifetimes: Vec::new(),
                                types: vec![rest_type.clone()],
                                constants: Vec::new(),
                            },
                        },
                    ),
                    span: None,
                },
            );

            // tuple Rest
            implementation.generic_declaration.predicates.push(
                symbol::Predicate {
                    predicate: predicate::Predicate::TupleType(
                        predicate::Tuple(rest_type),
                    ),
                    span: None,
                },
            );

            drop(implementation);

            let clone_function_id = self
                .insert_clone_function::<_, TraitImplementationFunctionDefinition>(
                    implementation_id,
                    tuple_type,
                );

            let mut clone_function = self.get_mut(clone_function_id).unwrap();
            clone_function.ir = FunctionIR::Intrinsic(Intrinsic::TupleClone);
        }
    }

    fn insert_clone_function<
        Parent: ParentSealed + Element,
        Definition: std::default::Default,
    >(
        &mut self,
        parent_id: ID<Parent>,
        clone_type: Type<Default>,
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
        let Insertion { id: clone_function_id, duplication } = self
            .insert_member(
                "clone".to_string(),
                Accessibility::Public,
                parent_id,
                None,
                generic_declaration,
                FunctionTemplate::<Definition>::default(),
            )
            .unwrap();

        assert!(duplication.is_none());

        let mut function = self.get_mut(clone_function_id).unwrap();
        let function_lifetime_parameter = Lifetime::Parameter(MemberID {
            parent: clone_function_id.into(),
            id: lifetime_parameter_id,
        });

        // add the parameter
        function.insert_parameter(Parameter {
            r#type: Type::Reference(Reference {
                qualifier: Qualifier::Immutable,
                lifetime: function_lifetime_parameter.clone(),
                pointee: Box::new(clone_type.clone()),
            }),
            span: None,
        });
        // add the bound
        function.generic_declaration.predicates.push(symbol::Predicate {
            predicate: predicate::Predicate::TypeOutlives(Outlives {
                operand: clone_type.clone(),
                bound: function_lifetime_parameter,
            }),
            span: None,
        });

        // set the return type
        function.return_type = clone_type;

        clone_function_id
    }

    /// Initializes the `Clone` trait of the core module.
    ///
    /// Synopsis:
    ///
    /// ``` txt
    /// public trait Clone[T] {
    ///     public function clone['a](value: &'a T): T
    ///     where
    ///         T: 'a;
    /// }
    /// ```
    fn initialize_clone_trait(
        &mut self,
        core_module_id: ID<Module>,
    ) -> ID<Trait> {
        // initialize clone trait
        let (trait_generic_declaration, type_parameter_id) = {
            let mut generic_declaration = GenericDeclaration::default();

            let type_parameter_id = generic_declaration
                .parameters
                .add_type_parameter(TypeParameter { name: None, span: None })
                .unwrap();

            (generic_declaration, type_parameter_id)
        };

        let Insertion { id: clone_trait_id, duplication } = self
            .insert_member(
                "Clone".to_string(),
                Accessibility::Public,
                core_module_id,
                None,
                trait_generic_declaration,
                TraitDefinition::default(),
            )
            .unwrap();

        assert!(duplication.is_none());

        // the clone function
        self.insert_clone_function::<_, TraitFunctionDefinition>(
            clone_trait_id,
            Type::Parameter(MemberID {
                parent: clone_trait_id.into(),
                id: type_parameter_id,
            }),
        );

        clone_trait_id
    }
}

#[cfg(test)]
mod tests;
