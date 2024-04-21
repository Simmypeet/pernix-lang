use std::collections::{HashMap, HashSet};

use crate::{
    semantic::{
        predicate::{self, Predicate, TraitResolveError},
        session::{self, Limit},
        term::{
            r#type::{Primitive, Type},
            GenericArguments, Tuple, TupleElement,
        },
        tests::State,
        Environment, Premise,
    },
    symbol::{
        self, Accessibility, GenericDeclaration, GenericParameters,
        ImplementationSignature, MemberID, Module, NegativeTraitImplementation,
        NegativeTraitImplementationData, Trait, TraitImplementation,
        TraitImplementationData, TypeParameter,
    },
    table::Table,
};

#[test]
#[allow(clippy::too_many_lines)]
fn single_implementation() {
    /*
    public trait Test[T] {}

    implements[T] Test[(T)] {} // <- 1

    // Test[(int32)] should match the implements 1
    */

    let name = "Test";
    let mut table = Table::<State>::default();

    let module_id = {
        let id = table.representation.modules.insert_with(|id| Module {
            id,
            name: name.to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: None,
            child_ids_by_name: HashMap::new(),
            span: None,
            usings: HashSet::new(),
        });

        table
            .representation
            .root_module_ids_by_name
            .insert(name.to_string(), id);

        id
    };

    // public trait Test[T] {}
    let trait_id = {
        let id = table.representation.traits.insert_with(|id| Trait {
            id,
            name: name.to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: module_id,
            generic_declaration: GenericDeclaration {
                parameters: {
                    let mut generic_parameters = GenericParameters::default();

                    generic_parameters
                        .add_type_parameter(TypeParameter {
                            name: "T".to_string(),
                            parent_generic_id: id.into(),
                            span: None,
                        })
                        .unwrap();

                    generic_parameters
                },
                predicates: Vec::new(),
            },
            negative_implementations: HashSet::new(),
            implementations: HashSet::new(),
            span: None,
            member_ids_by_name: HashMap::new(),
        });

        table
            .representation
            .modules
            .get_mut(module_id)
            .unwrap()
            .child_ids_by_name
            .insert(name.to_string(), id.into());

        id
    };

    // implements[T] Test[(T)] {}
    let (implementation_id, type_parameter_id) = {
        let mut type_parameter_id = None;
        let id = table.representation.trait_implementations.insert_with(|id| {
            TraitImplementation {
                id,
                span: None,
                signature: ImplementationSignature {
                    generic_declaration: GenericDeclaration {
                        parameters: {
                            let mut generic_parameters =
                                GenericParameters::default();

                            type_parameter_id = Some(MemberID {
                                parent: id.into(),
                                id: generic_parameters
                                    .add_type_parameter(TypeParameter {
                                        name: "T".to_string(),
                                        parent_generic_id: id.into(),
                                        span: None,
                                    })
                                    .unwrap(),
                            });

                            generic_parameters
                        },
                        predicates: Vec::new(),
                    },
                    arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Tuple(Tuple {
                            elements: vec![TupleElement::Regular(
                                Type::Parameter(type_parameter_id.unwrap()),
                            )],
                        })],
                        constants: Vec::new(),
                    },
                    implemented_id: trait_id,
                },
                implementation_name: name.to_string(),
                declared_in: module_id,
                data: TraitImplementationData {
                    is_const: false,
                    member_ids_by_name: HashMap::new(),
                    implementation_type_ids_by_trait_type_id: HashMap::new(),
                    implementation_function_ids_by_trait_function_id:
                        HashMap::new(),
                    implementation_constant_ids_by_trait_constant_id:
                        HashMap::new(),
                },
            }
        });

        table
            .representation
            .traits
            .get_mut(trait_id)
            .unwrap()
            .implementations
            .insert(id);

        (id, type_parameter_id.unwrap())
    };

    let premise = Premise::default();
    let mut session = session::Default::default();

    let implementation = super::resolve_implementation(
        trait_id,
        &GenericArguments {
            lifetimes: Vec::new(),
            types: vec![Type::Tuple(Tuple {
                elements: vec![TupleElement::Regular(Type::Primitive(
                    crate::semantic::term::r#type::Primitive::Int32,
                ))],
            })],
            constants: Vec::new(),
        },
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session),
    )
    .unwrap();

    assert_eq!(implementation_id, implementation.id);

    let ty = implementation
        .deduced_substitution
        .types
        .get(&Type::Parameter(type_parameter_id))
        .unwrap();
    assert_eq!(ty, &Type::Primitive(Primitive::Int32));
}

#[test]
#[allow(clippy::too_many_lines)]
fn more_specialized_implementation() {
    /*
    public trait Test[T] {}

    implements[T] Test[T] {}   // <- 1
    implements[T] Test[(T)] {} // <- 2

    // Test[(int32)] should match the implements 2
    // Test[int32]   should match the implements 1
    */

    let name = "Test";
    let mut table = Table::<State>::default();

    let module_id = {
        let id = table.representation.modules.insert_with(|id| Module {
            id,
            name: name.to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: None,
            child_ids_by_name: HashMap::new(),
            span: None,
            usings: HashSet::new(),
        });

        table
            .representation
            .root_module_ids_by_name
            .insert(name.to_string(), id);

        id
    };

    // public trait Test[T] {}
    let trait_id = {
        let id = table.representation.traits.insert_with(|id| Trait {
            id,
            name: name.to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: module_id,
            generic_declaration: GenericDeclaration {
                parameters: {
                    let mut generic_parameters = GenericParameters::default();

                    generic_parameters
                        .add_type_parameter(TypeParameter {
                            name: "T".to_string(),
                            parent_generic_id: id.into(),
                            span: None,
                        })
                        .unwrap();

                    generic_parameters
                },
                predicates: Vec::new(),
            },
            negative_implementations: HashSet::new(),
            implementations: HashSet::new(),
            span: None,
            member_ids_by_name: HashMap::new(),
        });

        table
            .representation
            .modules
            .get_mut(module_id)
            .unwrap()
            .child_ids_by_name
            .insert(name.to_string(), id.into());

        id
    };

    let (more_general_implementation_id, more_general_type_parameter_id) = {
        let mut more_general_type_parameter_id = None;
        let id = table.representation.trait_implementations.insert_with(|id| {
            TraitImplementation {
                id,
                span: None,
                signature: ImplementationSignature {
                    generic_declaration: GenericDeclaration {
                        parameters: {
                            let mut generic_parameters =
                                GenericParameters::default();

                            more_general_type_parameter_id = Some(MemberID {
                                parent: id.into(),
                                id: generic_parameters
                                    .add_type_parameter(TypeParameter {
                                        name: "T".to_string(),
                                        parent_generic_id: id.into(),
                                        span: None,
                                    })
                                    .unwrap(),
                            });

                            generic_parameters
                        },
                        predicates: Vec::new(),
                    },
                    arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Parameter(
                            more_general_type_parameter_id.unwrap(),
                        )],
                        constants: Vec::new(),
                    },
                    implemented_id: trait_id,
                },
                implementation_name: name.to_string(),
                declared_in: module_id,
                data: TraitImplementationData {
                    is_const: false,
                    member_ids_by_name: HashMap::new(),
                    implementation_type_ids_by_trait_type_id: HashMap::new(),
                    implementation_function_ids_by_trait_function_id:
                        HashMap::new(),
                    implementation_constant_ids_by_trait_constant_id:
                        HashMap::new(),
                },
            }
        });

        table
            .representation
            .traits
            .get_mut(trait_id)
            .unwrap()
            .implementations
            .insert(id);

        (id, more_general_type_parameter_id.unwrap())
    };

    let (
        more_specialized_implementation_id,
        more_specialized_type_parameter_id,
    ) = {
        let mut more_specialized_type_parameter_id = None;
        let id = table.representation.trait_implementations.insert_with(|id| {
            TraitImplementation {
                id,
                span: None,
                signature: ImplementationSignature {
                    generic_declaration: GenericDeclaration {
                        parameters: {
                            let mut generic_parameters =
                                GenericParameters::default();

                            more_specialized_type_parameter_id =
                                Some(MemberID {
                                    parent: id.into(),
                                    id: generic_parameters
                                        .add_type_parameter(TypeParameter {
                                            name: "T".to_string(),
                                            parent_generic_id: id.into(),
                                            span: None,
                                        })
                                        .unwrap(),
                                });

                            generic_parameters
                        },
                        predicates: Vec::new(),
                    },
                    arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Tuple(Tuple {
                            elements: vec![TupleElement::Regular(
                                Type::Parameter(
                                    more_specialized_type_parameter_id.unwrap(),
                                ),
                            )],
                        })],
                        constants: Vec::new(),
                    },
                    implemented_id: trait_id,
                },
                implementation_name: name.to_string(),
                declared_in: module_id,
                data: TraitImplementationData {
                    is_const: false,
                    member_ids_by_name: HashMap::new(),
                    implementation_type_ids_by_trait_type_id: HashMap::new(),
                    implementation_function_ids_by_trait_function_id:
                        HashMap::new(),
                    implementation_constant_ids_by_trait_constant_id:
                        HashMap::new(),
                },
            }
        });

        table
            .representation
            .traits
            .get_mut(trait_id)
            .unwrap()
            .implementations
            .insert(id);

        (id, more_specialized_type_parameter_id.unwrap())
    };

    let premise = Premise::default();
    let mut session = session::Default::default();

    let implementation = super::resolve_implementation(
        trait_id,
        &GenericArguments {
            lifetimes: Vec::new(),
            types: vec![Type::Tuple(Tuple {
                elements: vec![TupleElement::Regular(Type::Primitive(
                    crate::semantic::term::r#type::Primitive::Int32,
                ))],
            })],
            constants: Vec::new(),
        },
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session),
    )
    .unwrap();

    assert_eq!(more_specialized_implementation_id, implementation.id);
    let ty = implementation
        .deduced_substitution
        .types
        .get(&Type::Parameter(more_specialized_type_parameter_id))
        .unwrap();

    assert_eq!(ty, &Type::Primitive(Primitive::Int32));

    let implementation = super::resolve_implementation(
        trait_id,
        &GenericArguments {
            lifetimes: Vec::new(),
            types: vec![Type::Primitive(Primitive::Int32)],
            constants: Vec::new(),
        },
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session),
    )
    .unwrap();

    assert_eq!(more_general_implementation_id, implementation.id);

    let ty = implementation
        .deduced_substitution
        .types
        .get(&Type::Parameter(more_general_type_parameter_id))
        .unwrap();

    assert_eq!(ty, &Type::Primitive(Primitive::Int32));
}

#[test]
#[allow(clippy::too_many_lines)]
fn fallback_to_generic_implementation() {
    /*
    public trait Test[T] {}
    public trait Constraint[T] {}

    implements[T] Test[T]   {} // <- 1
    implements[T] Test[(T)]    // <- 2
    where trait Constraint[T]
    {}

    // Test[(int32)] should match the implements 1
    */

    let name = "Test";
    let mut table = Table::<State>::default();

    let module_id = {
        let id = table.representation.modules.insert_with(|id| Module {
            id,
            name: name.to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: None,
            child_ids_by_name: HashMap::new(),
            span: None,
            usings: HashSet::new(),
        });

        table
            .representation
            .root_module_ids_by_name
            .insert(name.to_string(), id);

        id
    };

    // public trait Test[T] {}
    let trait_id = {
        let id = table.representation.traits.insert_with(|id| Trait {
            id,
            name: name.to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: module_id,
            generic_declaration: GenericDeclaration {
                parameters: {
                    let mut generic_parameters = GenericParameters::default();

                    generic_parameters
                        .add_type_parameter(TypeParameter {
                            name: "T".to_string(),
                            parent_generic_id: id.into(),
                            span: None,
                        })
                        .unwrap();

                    generic_parameters
                },
                predicates: Vec::new(),
            },
            negative_implementations: HashSet::new(),
            implementations: HashSet::new(),
            span: None,
            member_ids_by_name: HashMap::new(),
        });

        table
            .representation
            .modules
            .get_mut(module_id)
            .unwrap()
            .child_ids_by_name
            .insert(name.to_string(), id.into());

        id
    };

    // public trait Constraint[T]
    let constraint_trait_id = {
        let id = table.representation.traits.insert_with(|id| Trait {
            id,
            name: "Constraint".to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: module_id,
            generic_declaration: GenericDeclaration {
                parameters: {
                    let mut generic_parameters = GenericParameters::default();

                    generic_parameters
                        .add_type_parameter(TypeParameter {
                            name: "T".to_string(),
                            parent_generic_id: id.into(),
                            span: None,
                        })
                        .unwrap();

                    generic_parameters
                },
                predicates: Vec::new(),
            },
            negative_implementations: HashSet::new(),
            implementations: HashSet::new(),
            span: None,
            member_ids_by_name: HashMap::new(),
        });

        table
            .representation
            .modules
            .get_mut(module_id)
            .unwrap()
            .child_ids_by_name
            .insert("Constraint".to_string(), id.into());

        id
    };

    let (more_general_implementation_id, more_general_type_parameter_id) = {
        let mut more_general_type_parameter_id = None;
        let id = table.representation.trait_implementations.insert_with(|id| {
            TraitImplementation {
                id,
                span: None,
                signature: ImplementationSignature {
                    generic_declaration: GenericDeclaration {
                        parameters: {
                            let mut generic_parameters =
                                GenericParameters::default();

                            more_general_type_parameter_id = Some(MemberID {
                                parent: id.into(),
                                id: generic_parameters
                                    .add_type_parameter(TypeParameter {
                                        name: "T".to_string(),
                                        parent_generic_id: id.into(),
                                        span: None,
                                    })
                                    .unwrap(),
                            });

                            generic_parameters
                        },
                        predicates: Vec::new(),
                    },
                    arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Parameter(
                            more_general_type_parameter_id.unwrap(),
                        )],
                        constants: Vec::new(),
                    },
                    implemented_id: trait_id,
                },
                implementation_name: name.to_string(),
                declared_in: module_id,
                data: TraitImplementationData {
                    is_const: false,
                    member_ids_by_name: HashMap::new(),
                    implementation_type_ids_by_trait_type_id: HashMap::new(),
                    implementation_function_ids_by_trait_function_id:
                        HashMap::new(),
                    implementation_constant_ids_by_trait_constant_id:
                        HashMap::new(),
                },
            }
        });

        table
            .representation
            .traits
            .get_mut(trait_id)
            .unwrap()
            .implementations
            .insert(id);

        (id, more_general_type_parameter_id.unwrap())
    };

    {
        let id = table.representation.trait_implementations.insert_with(|id| {
            let more_specialized_type_parameter_id;
            TraitImplementation {
                id,
                span: None,
                signature: ImplementationSignature {
                    generic_declaration: GenericDeclaration {
                        parameters: {
                            let mut generic_parameters =
                                GenericParameters::default();

                            more_specialized_type_parameter_id = MemberID {
                                parent: id.into(),
                                id: generic_parameters
                                    .add_type_parameter(TypeParameter {
                                        name: "T".to_string(),
                                        parent_generic_id: id.into(),
                                        span: None,
                                    })
                                    .unwrap(),
                            };

                            generic_parameters
                        },
                        predicates: vec![symbol::Predicate {
                            predicate: Predicate::Trait(predicate::Trait {
                                id: constraint_trait_id,
                                is_const: false,
                                generic_arguments: GenericArguments {
                                    lifetimes: Vec::new(),
                                    types: vec![Type::Parameter(
                                        more_specialized_type_parameter_id,
                                    )],
                                    constants: Vec::new(),
                                },
                            }),
                            kind: symbol::PredicateKind::Explicit(None),
                        }],
                    },
                    arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Tuple(Tuple {
                            elements: vec![TupleElement::Regular(
                                Type::Parameter(
                                    more_specialized_type_parameter_id,
                                ),
                            )],
                        })],
                        constants: Vec::new(),
                    },
                    implemented_id: trait_id,
                },
                implementation_name: name.to_string(),
                declared_in: module_id,
                data: TraitImplementationData {
                    is_const: false,
                    member_ids_by_name: HashMap::new(),
                    implementation_type_ids_by_trait_type_id: HashMap::new(),
                    implementation_function_ids_by_trait_function_id:
                        HashMap::new(),
                    implementation_constant_ids_by_trait_constant_id:
                        HashMap::new(),
                },
            }
        });

        table
            .representation
            .traits
            .get_mut(trait_id)
            .unwrap()
            .implementations
            .insert(id);
    }

    let premise = Premise::default();
    let mut session = session::Default::default();

    let implementation = super::resolve_implementation(
        trait_id,
        &GenericArguments {
            lifetimes: Vec::new(),
            types: vec![Type::Tuple(Tuple {
                elements: vec![TupleElement::Regular(Type::Primitive(
                    crate::semantic::term::r#type::Primitive::Int32,
                ))],
            })],
            constants: Vec::new(),
        },
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session),
    )
    .unwrap();

    assert_eq!(more_general_implementation_id, implementation.id);
    let ty = implementation
        .deduced_substitution
        .types
        .get(&Type::Parameter(more_general_type_parameter_id))
        .unwrap();

    assert_eq!(
        ty,
        &Type::Tuple(Tuple {
            elements: vec![TupleElement::Regular(Type::Primitive(
                Primitive::Int32
            ),)],
        })
    );

    let implementation = super::resolve_implementation(
        trait_id,
        &GenericArguments {
            lifetimes: Vec::new(),
            types: vec![Type::Primitive(Primitive::Int32)],
            constants: Vec::new(),
        },
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session),
    )
    .unwrap();

    assert_eq!(more_general_implementation_id, implementation.id);

    let ty = implementation
        .deduced_substitution
        .types
        .get(&Type::Parameter(more_general_type_parameter_id))
        .unwrap();

    assert_eq!(ty, &Type::Primitive(Primitive::Int32));
}

#[test]
#[allow(clippy::too_many_lines)]
fn negative_impleemntation() {
    /*
    public trait Test[T] {}
    public trait Constraint[T] {}

    implements[T] Test[T]   {}      // <- 1
    implements[T] Test[(T)] delete; // <- 2

    // Test[(int32)] should fail because of the negative implementation 2
    */

    let name = "Test";
    let mut table = Table::<State>::default();

    let module_id = {
        let id = table.representation.modules.insert_with(|id| Module {
            id,
            name: name.to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: None,
            child_ids_by_name: HashMap::new(),
            span: None,
            usings: HashSet::new(),
        });

        table
            .representation
            .root_module_ids_by_name
            .insert(name.to_string(), id);

        id
    };

    // public trait Test[T] {}
    let trait_id = {
        let id = table.representation.traits.insert_with(|id| Trait {
            id,
            name: name.to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: module_id,
            generic_declaration: GenericDeclaration {
                parameters: {
                    let mut generic_parameters = GenericParameters::default();

                    generic_parameters
                        .add_type_parameter(TypeParameter {
                            name: "T".to_string(),
                            parent_generic_id: id.into(),
                            span: None,
                        })
                        .unwrap();

                    generic_parameters
                },
                predicates: Vec::new(),
            },
            negative_implementations: HashSet::new(),
            implementations: HashSet::new(),
            span: None,
            member_ids_by_name: HashMap::new(),
        });

        table
            .representation
            .modules
            .get_mut(module_id)
            .unwrap()
            .child_ids_by_name
            .insert(name.to_string(), id.into());

        id
    };

    // implements[T] Test[T] {}
    {
        let mut more_general_type_parameter_id = None;
        let id = table.representation.trait_implementations.insert_with(|id| {
            TraitImplementation {
                id,
                span: None,
                signature: ImplementationSignature {
                    generic_declaration: GenericDeclaration {
                        parameters: {
                            let mut generic_parameters =
                                GenericParameters::default();

                            more_general_type_parameter_id = Some(MemberID {
                                parent: id.into(),
                                id: generic_parameters
                                    .add_type_parameter(TypeParameter {
                                        name: "T".to_string(),
                                        parent_generic_id: id.into(),
                                        span: None,
                                    })
                                    .unwrap(),
                            });

                            generic_parameters
                        },
                        predicates: Vec::new(),
                    },
                    arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Parameter(
                            more_general_type_parameter_id.unwrap(),
                        )],
                        constants: Vec::new(),
                    },
                    implemented_id: trait_id,
                },
                implementation_name: name.to_string(),
                declared_in: module_id,
                data: TraitImplementationData {
                    is_const: false,
                    member_ids_by_name: HashMap::new(),
                    implementation_type_ids_by_trait_type_id: HashMap::new(),
                    implementation_function_ids_by_trait_function_id:
                        HashMap::new(),
                    implementation_constant_ids_by_trait_constant_id:
                        HashMap::new(),
                },
            }
        });

        table
            .representation
            .traits
            .get_mut(trait_id)
            .unwrap()
            .implementations
            .insert(id);
    }

    {
        let id = table
            .representation
            .negative_trait_implementations
            .insert_with(|id| {
                let more_specialized_type_parameter_id;
                NegativeTraitImplementation {
                    id,
                    span: None,
                    signature: ImplementationSignature {
                        generic_declaration: GenericDeclaration {
                            parameters: {
                                let mut generic_parameters =
                                    GenericParameters::default();

                                more_specialized_type_parameter_id = MemberID {
                                    parent: id.into(),
                                    id: generic_parameters
                                        .add_type_parameter(TypeParameter {
                                            name: "T".to_string(),
                                            parent_generic_id: id.into(),
                                            span: None,
                                        })
                                        .unwrap(),
                                };

                                generic_parameters
                            },
                            predicates: Vec::new(),
                        },
                        arguments: GenericArguments {
                            lifetimes: Vec::new(),
                            types: vec![Type::Tuple(Tuple {
                                elements: vec![TupleElement::Regular(
                                    Type::Parameter(
                                        more_specialized_type_parameter_id,
                                    ),
                                )],
                            })],
                            constants: Vec::new(),
                        },
                        implemented_id: trait_id,
                    },
                    implementation_name: name.to_string(),
                    declared_in: module_id,
                    data: NegativeTraitImplementationData,
                }
            });

        table
            .representation
            .traits
            .get_mut(trait_id)
            .unwrap()
            .negative_implementations
            .insert(id);
    }

    let premise = Premise::default();
    let mut session = session::Default::default();

    assert_eq!(
        super::resolve_implementation(
            trait_id,
            &GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Tuple(Tuple {
                    elements: vec![TupleElement::Regular(Type::Primitive(
                        crate::semantic::term::r#type::Primitive::Int32,
                    ))],
                })],
                constants: Vec::new(),
            },
            &Environment { premise: &premise, table: &table },
            &mut Limit::new(&mut session),
        ),
        Err(TraitResolveError::NotFound)
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn failed_by_where_clause() {
    /*
    public trait Test[T] {}
    public trait Constraint[T] {}

    implements[T] Test[(T)]    // <- 2
    where trait Constraint[T]
    {}

    // Test[(int32)] fails because of the where clause
    */

    let name = "Test";
    let mut table = Table::<State>::default();

    let module_id = {
        let id = table.representation.modules.insert_with(|id| Module {
            id,
            name: name.to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: None,
            child_ids_by_name: HashMap::new(),
            span: None,
            usings: HashSet::new(),
        });

        table
            .representation
            .root_module_ids_by_name
            .insert(name.to_string(), id);

        id
    };

    // public trait Test[T] {}
    let trait_id = {
        let id = table.representation.traits.insert_with(|id| Trait {
            id,
            name: name.to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: module_id,
            generic_declaration: GenericDeclaration {
                parameters: {
                    let mut generic_parameters = GenericParameters::default();

                    generic_parameters
                        .add_type_parameter(TypeParameter {
                            name: "T".to_string(),
                            parent_generic_id: id.into(),
                            span: None,
                        })
                        .unwrap();

                    generic_parameters
                },
                predicates: Vec::new(),
            },
            negative_implementations: HashSet::new(),
            implementations: HashSet::new(),
            span: None,
            member_ids_by_name: HashMap::new(),
        });

        table
            .representation
            .modules
            .get_mut(module_id)
            .unwrap()
            .child_ids_by_name
            .insert(name.to_string(), id.into());

        id
    };

    // public trait Constraint[T]
    let constraint_trait_id = {
        let id = table.representation.traits.insert_with(|id| Trait {
            id,
            name: "Constraint".to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: module_id,
            generic_declaration: GenericDeclaration {
                parameters: {
                    let mut generic_parameters = GenericParameters::default();

                    generic_parameters
                        .add_type_parameter(TypeParameter {
                            name: "T".to_string(),
                            parent_generic_id: id.into(),
                            span: None,
                        })
                        .unwrap();

                    generic_parameters
                },
                predicates: Vec::new(),
            },
            negative_implementations: HashSet::new(),
            implementations: HashSet::new(),
            span: None,
            member_ids_by_name: HashMap::new(),
        });

        table
            .representation
            .modules
            .get_mut(module_id)
            .unwrap()
            .child_ids_by_name
            .insert("Constraint".to_string(), id.into());

        id
    };

    {
        let id = table.representation.trait_implementations.insert_with(|id| {
            let more_specialized_type_parameter_id;
            TraitImplementation {
                id,
                span: None,
                signature: ImplementationSignature {
                    generic_declaration: GenericDeclaration {
                        parameters: {
                            let mut generic_parameters =
                                GenericParameters::default();

                            more_specialized_type_parameter_id = MemberID {
                                parent: id.into(),
                                id: generic_parameters
                                    .add_type_parameter(TypeParameter {
                                        name: "T".to_string(),
                                        parent_generic_id: id.into(),
                                        span: None,
                                    })
                                    .unwrap(),
                            };

                            generic_parameters
                        },
                        predicates: vec![symbol::Predicate {
                            predicate: Predicate::Trait(predicate::Trait {
                                id: constraint_trait_id,
                                is_const: false,
                                generic_arguments: GenericArguments {
                                    lifetimes: Vec::new(),
                                    types: vec![Type::Parameter(
                                        more_specialized_type_parameter_id,
                                    )],
                                    constants: Vec::new(),
                                },
                            }),
                            kind: symbol::PredicateKind::Explicit(None),
                        }],
                    },
                    arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Tuple(Tuple {
                            elements: vec![TupleElement::Regular(
                                Type::Parameter(
                                    more_specialized_type_parameter_id,
                                ),
                            )],
                        })],
                        constants: Vec::new(),
                    },
                    implemented_id: trait_id,
                },
                implementation_name: name.to_string(),
                declared_in: module_id,
                data: TraitImplementationData {
                    is_const: false,
                    member_ids_by_name: HashMap::new(),
                    implementation_type_ids_by_trait_type_id: HashMap::new(),
                    implementation_function_ids_by_trait_function_id:
                        HashMap::new(),
                    implementation_constant_ids_by_trait_constant_id:
                        HashMap::new(),
                },
            }
        });

        table
            .representation
            .traits
            .get_mut(trait_id)
            .unwrap()
            .implementations
            .insert(id);
    }

    let premise = Premise::default();
    let mut session = session::Default::default();

    assert_eq!(
        super::resolve_implementation(
            trait_id,
            &GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Tuple(Tuple {
                    elements: vec![TupleElement::Regular(Type::Primitive(
                        crate::semantic::term::r#type::Primitive::Int32,
                    ))],
                })],
                constants: Vec::new(),
            },
            &Environment { premise: &premise, table: &table },
            &mut Limit::new(&mut session),
        ),
        Err(TraitResolveError::NotFound)
    );
}
