use std::collections::{HashMap, HashSet};

use lazy_static::lazy_static;
use proptest::{
    arbitrary::Arbitrary,
    prop_assert_eq, proptest,
    strategy::{BoxedStrategy, LazyJust, Strategy},
    test_runner::TestCaseResult,
};

use crate::{
    arena::ID,
    semantic::{
        instantiation::Instantiation,
        predicate::{self, definite, Predicate, TraitResolveError},
        session::{self, Limit},
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Primitive, Type},
            GenericArguments, Kind, Tuple, TupleElement,
        },
        tests::State,
        visitor::RecursiveIterator,
        Environment, Premise,
    },
    symbol::{
        self, Accessibility, GenericDeclaration, GenericParameters,
        ImplementationSignature, LifetimeParameter, MemberID, Module,
        NegativeTraitImplementation, NegativeTraitImplementationData, Trait,
        TraitImplementation, TraitImplementationData, TypeParameter,
    },
    table::Table,
};

lazy_static! {
    static ref TABLE: Table<State> = Table::default();
}

fn definite_lifetime() -> impl Strategy<Value = Lifetime> {
    Lifetime::arbitrary().prop_filter("filter out non-definite terms", |term| {
        matches!(
            definite::definite(
                term,
                &Environment { premise: &Premise::default(), table: &*TABLE },
                &mut Limit::new(&mut session::Default::default())
            ),
            Ok(true)
        ) && !RecursiveIterator::new(term)
            .any(|(x, _)| matches!(x, Kind::Type(Type::TraitMember(_))))
    })
}

fn definite_type() -> impl Strategy<Value = Type> {
    Type::arbitrary().prop_filter("filter out non-definite terms", |term| {
        matches!(
            definite::definite(
                term,
                &Environment { premise: &Premise::default(), table: &*TABLE },
                &mut Limit::new(&mut session::Default::default())
            ),
            Ok(true)
        ) && !RecursiveIterator::new(term)
            .any(|(x, _)| matches!(x, Kind::Type(Type::TraitMember(_))))
    })
}

fn definite_constant() -> impl Strategy<Value = Constant> {
    Constant::arbitrary().prop_filter("filter out non-definite terms", |term| {
        matches!(
            definite::definite(
                term,
                &Environment { premise: &Premise::default(), table: &*TABLE },
                &mut Limit::new(&mut session::Default::default())
            ),
            Ok(true)
        ) && !RecursiveIterator::new(term)
            .any(|(x, _)| matches!(x, Kind::Type(Type::TraitMember(_))))
    })
}

fn definite_generic_arguments() -> impl Strategy<Value = GenericArguments> {
    (
        proptest::collection::vec(definite_lifetime(), 0..3),
        proptest::collection::vec(definite_type(), 0..3),
        proptest::collection::vec(definite_constant(), 0..3),
    )
        .prop_map(|(lifetimes, types, constants)| GenericArguments {
            lifetimes,
            types,
            constants,
        })
}

#[derive(Debug)]
pub struct SingleImplementation {
    pub trait_id: ID<Trait>,
    pub target_implementation_id: ID<TraitImplementation>,
    pub generic_arguments: GenericArguments,
    pub table: Table<State>,
    pub expected_instantiation: Instantiation,
}

impl SingleImplementation {
    fn assert(&self) -> TestCaseResult {
        let premise = Premise::default();
        let mut session = session::Default::default();

        let result = super::resolve_implementation(
            self.trait_id,
            &self.generic_arguments,
            &Environment { premise: &premise, table: &self.table },
            &mut Limit::new(&mut session),
        )?;

        prop_assert_eq!(result.id, self.target_implementation_id);
        prop_assert_eq!(
            &result.deduced_substitution,
            &self.expected_instantiation
        );
        prop_assert_eq!(result.lifetime_constraints.len(), 0);

        Ok(())
    }

    fn create_table_from_generic_arguments(
        generic_arguments: &GenericArguments,
    ) -> (Table<State>, ID<Module>, ID<Trait>) {
        let mut table = Table::<State>::default();

        // the module where the trait is defined
        let module_id = {
            let id = table.representation.modules.insert_with(|id| Module {
                id,
                name: "testModule".to_string(),
                accessibility: Accessibility::Public,
                parent_module_id: None,
                child_ids_by_name: HashMap::new(),
                span: None,
                usings: HashSet::new(),
            });

            table
                .representation
                .root_module_ids_by_name
                .insert("testModule".to_string(), id);

            id
        };

        // the trait has the same size as the generic arguments
        let trait_id = {
            let id = table.representation.traits.insert_with(|id| Trait {
                id,
                name: "Test".to_string(),
                accessibility: Accessibility::Public,
                parent_module_id: module_id,
                generic_declaration: GenericDeclaration {
                    parameters: {
                        let mut generic_parameters =
                            GenericParameters::default();

                        for _ in 0..generic_arguments.lifetimes.len() {
                            generic_parameters
                                .add_lifetime_parameter(LifetimeParameter {
                                    name: None,
                                    parent_generic_id: id.into(),
                                    span: None,
                                })
                                .unwrap();
                        }

                        for i in 0..generic_arguments.types.len() {
                            generic_parameters
                                .add_type_parameter(TypeParameter {
                                    name: format!("T{i}"),
                                    parent_generic_id: id.into(),
                                    span: None,
                                })
                                .unwrap();
                        }

                        for i in 0..generic_arguments.constants.len() {
                            generic_parameters
                                .add_constant_parameter(
                                    symbol::ConstantParameter {
                                        name: format!("C{i}"),
                                        parent_generic_id: id.into(),
                                        r#type: Type::default(),
                                        span: None,
                                    },
                                )
                                .unwrap();
                        }

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
                .insert("Test".to_string(), id.into());

            id
        };

        (table, module_id, trait_id)
    }

    #[allow(clippy::too_many_lines)]
    fn create_implementation(
        table: &mut Table<State>,
        module_id: ID<Module>,
        trait_id: ID<Trait>,
        mut generic_arguments: GenericArguments,
        to_be_substituted_type: Vec<Type>,
        to_be_substituted_constant: Vec<Constant>,
    ) -> (ID<TraitImplementation>, Instantiation) {
        // the trait implementation id which will be resolved to
        let implementation_id = {
            table.representation.trait_implementations.insert_with(|id| {
                TraitImplementation {
                    id,
                    span: None,
                    signature: ImplementationSignature {
                        generic_declaration: GenericDeclaration::default(),
                        arguments: GenericArguments::default(),
                        implemented_id: trait_id,
                    },
                    implementation_name: "Test".to_string(),
                    declared_in: module_id,
                    data: TraitImplementationData {
                        is_const: false,
                        member_ids_by_name: HashMap::new(),
                        implementation_type_ids_by_trait_type_id: HashMap::new(
                        ),
                        implementation_function_ids_by_trait_function_id:
                            HashMap::new(),
                        implementation_constant_ids_by_trait_constant_id:
                            HashMap::new(),
                    },
                }
            })
        };

        assert!(table
            .representation
            .traits
            .get_mut(trait_id)
            .unwrap()
            .implementations
            .insert(implementation_id));

        let implementation = table
            .representation
            .trait_implementations
            .get_mut(implementation_id)
            .unwrap();

        let mut expected_instantiation = Instantiation::default();

        // replace the choosen types with the type parameter
        for (idx, ty) in to_be_substituted_type.into_iter().enumerate() {
            let mut all_types = generic_arguments
                .types
                .iter()
                .flat_map(RecursiveIterator::new)
                .filter_map(|(term, _)| term.as_type().copied());

            // has been overwritten
            if !all_types.any(|x| x == &ty) {
                continue;
            }

            let id = implementation
                .signature
                .generic_declaration
                .parameters
                .add_type_parameter(TypeParameter {
                    name: format!("T{idx}"),
                    parent_generic_id: implementation_id.into(),
                    span: None,
                })
                .unwrap();

            let pair = (
                ty,
                Type::Parameter(MemberID {
                    parent: implementation_id.into(),
                    id,
                }),
            );
            let instantiation = Instantiation {
                lifetimes: HashMap::new(),
                types: std::iter::once(pair.clone()).collect(),
                constants: HashMap::new(),
            };

            generic_arguments.instantiate(&instantiation);

            // add to the expected list
            expected_instantiation.types.insert(pair.1, pair.0);
        }

        // replace the choosen constants with the constant parameter
        for (idx, constant) in
            to_be_substituted_constant.into_iter().enumerate()
        {
            let mut all_constants = generic_arguments
                .constants
                .iter()
                .flat_map(RecursiveIterator::new)
                .chain(
                    generic_arguments
                        .types
                        .iter()
                        .flat_map(RecursiveIterator::new),
                )
                .filter_map(|(term, _)| term.as_constant().copied());

            // has been overwritten
            if !all_constants.any(|x| x == &constant) {
                continue;
            }

            let id = implementation
                .signature
                .generic_declaration
                .parameters
                .add_constant_parameter(symbol::ConstantParameter {
                    name: format!("C{idx}"),
                    parent_generic_id: implementation_id.into(),
                    r#type: Type::default(),
                    span: None,
                })
                .unwrap();

            let pair = (
                constant,
                Constant::Parameter(MemberID {
                    parent: implementation_id.into(),
                    id,
                }),
            );
            let instantiation = Instantiation {
                lifetimes: HashMap::new(),
                types: HashMap::new(),
                constants: std::iter::once(pair.clone()).collect(),
            };

            generic_arguments.instantiate(&instantiation);

            // add to the expected list
            expected_instantiation.constants.insert(pair.1, pair.0);
        }

        // replace all the lifetimes with the lifetime parameter
        let all_lifetimes = generic_arguments
            .lifetimes
            .iter()
            .copied()
            .chain(
                generic_arguments
                    .types
                    .iter()
                    .flat_map(RecursiveIterator::new)
                    .filter_map(|(term, _)| term.as_lifetime().map(|x| **x)),
            )
            .collect::<HashSet<_>>();

        for lt in all_lifetimes {
            // no need to check for existence because it is guaranteed to not
            // be overwritten

            let id = implementation
                .signature
                .generic_declaration
                .parameters
                .add_lifetime_parameter(LifetimeParameter {
                    name: None,
                    parent_generic_id: implementation_id.into(),
                    span: None,
                })
                .unwrap();

            let pair = (
                lt,
                Lifetime::Parameter(MemberID {
                    parent: implementation_id.into(),
                    id,
                }),
            );
            let instantiation = Instantiation {
                lifetimes: std::iter::once(pair).collect(),
                types: HashMap::new(),
                constants: HashMap::new(),
            };

            generic_arguments.instantiate(&instantiation);

            // add to the expected list
            expected_instantiation.lifetimes.insert(pair.1, pair.0);
        }

        implementation.signature.arguments = generic_arguments;

        (implementation_id, expected_instantiation)
    }
}

impl Arbitrary for SingleImplementation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    #[allow(clippy::too_many_lines)]
    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        definite_generic_arguments()
            .prop_ind_flat_map2(|generic_arguments| {
                let another_generic_arguments = generic_arguments.clone();

                let table = LazyJust::new(move || {
                    Self::create_table_from_generic_arguments(
                        &generic_arguments,
                    )
                });

                // collect all the types appearing in the generic arguments
                let all_types = another_generic_arguments
                    .types
                    .iter()
                    .flat_map(RecursiveIterator::new)
                    .filter_map(|(term, _)| {
                        term.as_type().map(|x| (*x).clone())
                    })
                    .collect::<Vec<_>>();
                // collect all the constants appearing in the generic arguments
                let all_constants = another_generic_arguments
                    .constants
                    .iter()
                    .flat_map(RecursiveIterator::new)
                    .filter_map(|(term, _)| {
                        term.as_constant().map(|x| (*x).clone())
                    })
                    .chain(
                        another_generic_arguments
                            .types
                            .iter()
                            .flat_map(RecursiveIterator::new)
                            .filter_map(|(term, _)| {
                                term.as_constant().map(|x| (*x).clone())
                            }),
                    )
                    .collect::<Vec<_>>();

                (
                    table,
                    proptest::sample::subsequence(
                        all_types.clone(),
                        0..=all_types.len(),
                    )
                    .prop_map(|mut x| {
                        x.sort();
                        x.dedup();
                        x
                    }),
                    proptest::sample::subsequence(
                        all_constants.clone(),
                        0..=all_constants.len(),
                    )
                    .prop_map(|mut x| {
                        x.sort();
                        x.dedup();
                        x
                    }),
                )
            })
            .prop_map(
                |(
                    generic_arguments,
                    (
                        (mut table, module_id, trait_id),
                        to_be_substituted_type,
                        to_be_substituted_constant,
                    ),
                )| {
                    let (target_implementation_id, expected_instantiation) =
                        Self::create_implementation(
                            &mut table,
                            module_id,
                            trait_id,
                            generic_arguments.clone(),
                            to_be_substituted_type,
                            to_be_substituted_constant,
                        );

                    Self {
                        trait_id,
                        target_implementation_id,
                        generic_arguments,
                        table,
                        expected_instantiation,
                    }
                },
            )
            .boxed()
    }
}

proptest! {
    #[test]
    fn single_implementation_test(test: SingleImplementation) {
        test.assert()?;
    }
}

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
