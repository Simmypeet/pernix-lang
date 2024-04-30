use std::{
    collections::{HashMap, HashSet},
    ops::Not,
};

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
        instantiation::{self, Instantiation},
        order::Order,
        predicate::{self, definite, TraitResolveError},
        session::{self, Limit},
        term::{
            constant::Constant, lifetime::Lifetime, r#type::Type,
            GenericArguments, Kind,
        },
        tests::State,
        visitor::RecursiveIterator,
        Environment, Premise,
    },
    symbol::{
        self, Accessibility, Generic, GenericDeclaration, GenericParameters,
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
        proptest::collection::vec(definite_lifetime(), 0..4),
        proptest::collection::vec(definite_type(), 0..4),
        proptest::collection::vec(definite_constant(), 0..4),
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
    pub defined_in_module_id: ID<Module>,
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
            let id = table.representation.modules.insert_with(|_| Module {
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
            let id = table.representation.traits.insert_with(|_| Trait {
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
                                    span: None,
                                })
                                .unwrap();
                        }

                        for _ in 0..generic_arguments.types.len() {
                            generic_parameters
                                .add_type_parameter(TypeParameter {
                                    name: None,
                                    span: None,
                                })
                                .unwrap();
                        }

                        for _ in 0..generic_arguments.constants.len() {
                            generic_parameters
                                .add_constant_parameter(
                                    symbol::ConstantParameter {
                                        name: None,
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
            table.representation.trait_implementations.insert_with(|_| {
                TraitImplementation {
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
        for ty in to_be_substituted_type {
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
                .add_type_parameter(TypeParameter { name: None, span: None })
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
        for constant in to_be_substituted_constant {
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
                    name: None,
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
                        defined_in_module_id: module_id,
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

#[derive(Debug)]
pub struct SpecializedImplementation {
    pub table: Table<State>,
    pub trait_id: ID<Trait>,
    pub defined_in_module_id: ID<Module>,

    pub general_implementation_id: ID<TraitImplementation>,

    #[allow(clippy::struct_field_names)]
    pub specialized_implementation_id: ID<TraitImplementation>,

    pub expected_specialized_instantitation: Instantiation,
    pub expected_general_instantitation: Instantiation,

    pub generic_arguments: GenericArguments,
}

impl SpecializedImplementation {
    fn assert(&self) -> TestCaseResult {
        // should match to the specialized implementation
        let premise = Premise::default();
        let mut session = session::Default::default();

        let result = super::resolve_implementation(
            self.trait_id,
            &self.generic_arguments,
            &Environment { premise: &premise, table: &self.table },
            &mut Limit::new(&mut session),
        )?;

        prop_assert_eq!(result.id, self.specialized_implementation_id);
        prop_assert_eq!(
            &result.deduced_substitution,
            &self.expected_specialized_instantitation
        );
        prop_assert_eq!(result.lifetime_constraints.len(), 0);

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn create_implementation(
        table: &mut Table<State>,
        module_id: ID<Module>,
        trait_id: ID<Trait>,
        mut generic_arguments: GenericArguments,
        expected_specialized_instantiation: &Instantiation,
        to_be_substituted_type: Vec<Type>,
        to_be_substituted_constant: Vec<Constant>,
    ) -> Option<(ID<TraitImplementation>, Instantiation)> {
        let starting_generic_arguments = generic_arguments.clone();
        let mut expected_instantitation = Instantiation::default();

        // the trait implementation id which will is more general
        let general_implementation_id = {
            table.representation.trait_implementations.insert_with(|_| {
                TraitImplementation {
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
            .insert(general_implementation_id));

        let general_implementation = table
            .representation
            .trait_implementations
            .get_mut(general_implementation_id)
            .unwrap();

        // replace the choosen types with the type parameter
        let all_type_parameters = generic_arguments
            .types
            .iter()
            .flat_map(RecursiveIterator::new)
            .filter_map(|(term, _)| term.as_type().copied())
            .filter(|x| x.is_parameter())
            .cloned()
            .collect::<HashSet<_>>();

        let all_constant_parameters = generic_arguments
            .constants
            .iter()
            .flat_map(RecursiveIterator::new)
            .chain(
                generic_arguments.types.iter().flat_map(RecursiveIterator::new),
            )
            .filter_map(|(term, _)| term.as_constant().copied())
            .filter(|x| x.is_parameter())
            .cloned()
            .collect::<HashSet<_>>();

        for ty in to_be_substituted_type.into_iter().chain(all_type_parameters)
        {
            let mut all_types = generic_arguments
                .types
                .iter()
                .flat_map(RecursiveIterator::new)
                .filter_map(|(term, _)| term.as_type().copied());

            // has been overwritten
            if !all_types.any(|x| x == &ty) {
                continue;
            }

            let id = general_implementation
                .signature
                .generic_declaration
                .parameters
                .add_type_parameter(TypeParameter { name: None, span: None })
                .unwrap();

            let mut pair = (
                ty,
                Type::Parameter(MemberID {
                    parent: general_implementation_id.into(),
                    id,
                }),
            );
            let instantiation = Instantiation {
                lifetimes: HashMap::new(),
                types: std::iter::once(pair.clone()).collect(),
                constants: HashMap::new(),
            };

            generic_arguments.instantiate(&instantiation);
            expected_instantitation.types.insert(pair.1, {
                instantiation::instantiate(
                    &mut pair.0,
                    expected_specialized_instantiation,
                );
                pair.0
            });
        }

        for constant in to_be_substituted_constant
            .into_iter()
            .chain(all_constant_parameters)
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

            let id = general_implementation
                .signature
                .generic_declaration
                .parameters
                .add_constant_parameter(symbol::ConstantParameter {
                    name: None,
                    r#type: Type::default(),
                    span: None,
                })
                .unwrap();

            let mut pair = (
                constant,
                Constant::Parameter(MemberID {
                    parent: general_implementation_id.into(),
                    id,
                }),
            );
            let instantiation = Instantiation {
                lifetimes: HashMap::new(),
                types: HashMap::new(),
                constants: std::iter::once(pair.clone()).collect(),
            };

            generic_arguments.instantiate(&instantiation);
            expected_instantitation.constants.insert(pair.1, {
                instantiation::instantiate(
                    &mut pair.0,
                    expected_specialized_instantiation,
                );
                pair.0
            });
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

            let id = general_implementation
                .signature
                .generic_declaration
                .parameters
                .add_lifetime_parameter(LifetimeParameter {
                    name: None,
                    span: None,
                })
                .unwrap();

            let mut pair = (
                lt,
                Lifetime::Parameter(MemberID {
                    parent: general_implementation_id.into(),
                    id,
                }),
            );
            let instantiation = Instantiation {
                lifetimes: std::iter::once(pair).collect(),
                types: HashMap::new(),
                constants: HashMap::new(),
            };

            generic_arguments.instantiate(&instantiation);
            expected_instantitation.lifetimes.insert(pair.1, {
                instantiation::instantiate(
                    &mut pair.0,
                    expected_specialized_instantiation,
                );
                pair.0
            });
        }

        general_implementation.signature.arguments = generic_arguments;

        let mut session = session::Default::default();
        let premise = Premise::default();

        if general_implementation.signature.arguments.clone().order(
            &starting_generic_arguments,
            &Environment { premise: &premise, table },
            &mut Limit::new(&mut session),
        ) != Ok(Order::MoreGeneral)
        {
            return None;
        }

        Some((general_implementation_id, expected_instantitation))
    }
}

impl Arbitrary for SpecializedImplementation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    #[allow(clippy::too_many_lines)]
    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        SingleImplementation::arbitrary()
            .prop_ind_flat_map2(|single_implementation| {
                let implementation_generic_arguments = single_implementation
                    .table
                    .representation
                    .trait_implementations
                    .get(single_implementation.target_implementation_id)
                    .unwrap()
                    .signature
                    .arguments
                    .clone();

                let all_types = implementation_generic_arguments
                    .types
                    .iter()
                    .flat_map(RecursiveIterator::new)
                    .filter_map(|x| {
                        // filter out type parameter terms
                        x.0.as_type().and_then(|x| {
                            x.is_parameter().not().then(|| (*x).clone())
                        })
                    })
                    .collect::<Vec<_>>();

                let all_constants = implementation_generic_arguments
                    .constants
                    .iter()
                    .flat_map(RecursiveIterator::new)
                    .chain(
                        implementation_generic_arguments
                            .types
                            .iter()
                            .flat_map(RecursiveIterator::new),
                    )
                    .filter_map(|x| {
                        // filter out constant parameter terms
                        x.0.as_constant().and_then(|x| {
                            x.is_parameter().not().then(|| (*x).clone())
                        })
                    })
                    .collect::<Vec<_>>();

                let all_types_len = all_types.len();
                let all_constants_len = all_constants.len();

                (
                    proptest::sample::subsequence(all_types, 0..=all_types_len)
                        .prop_map(|mut x| {
                            x.sort();
                            x.dedup();
                            x
                        }),
                    proptest::sample::subsequence(
                        all_constants,
                        0..=all_constants_len,
                    )
                    .prop_map(|mut x| {
                        x.sort();
                        x.dedup();
                        x
                    }),
                )
            })
            .prop_filter_map(
                "filter out non-general generic arguments",
                |(
                    mut single_implementation,
                    (selected_types, selected_constants),
                )| {
                    let specialized_generic_arguments = single_implementation
                        .table
                        .representation
                        .trait_implementations
                        .get(single_implementation.target_implementation_id)
                        .unwrap()
                        .signature
                        .arguments
                        .clone();

                    let (
                        general_implementation_id,
                        expected_general_instantitation,
                    ) = Self::create_implementation(
                        &mut single_implementation.table,
                        single_implementation.defined_in_module_id,
                        single_implementation.trait_id,
                        specialized_generic_arguments,
                        &single_implementation.expected_instantiation,
                        selected_types,
                        selected_constants,
                    )?;

                    Some(Self {
                        table: single_implementation.table,
                        trait_id: single_implementation.trait_id,
                        defined_in_module_id: single_implementation
                            .defined_in_module_id,

                        general_implementation_id,
                        specialized_implementation_id: single_implementation
                            .target_implementation_id,

                        generic_arguments: single_implementation
                            .generic_arguments,
                        expected_specialized_instantitation:
                            single_implementation.expected_instantiation,
                        expected_general_instantitation,
                    })
                },
            )
            .boxed()
    }
}

#[derive(Debug)]
pub struct FallbackToGeneralImplementation(SpecializedImplementation);

impl FallbackToGeneralImplementation {
    fn assert(&self) -> TestCaseResult {
        // should match to the general implementation
        let premise = Premise::default();
        let mut session = session::Default::default();

        let result = super::resolve_implementation(
            self.0.trait_id,
            &self.0.generic_arguments,
            &Environment { premise: &premise, table: &self.0.table },
            &mut Limit::new(&mut session),
        )?;

        prop_assert_eq!(result.id, self.0.general_implementation_id);
        prop_assert_eq!(
            &result.deduced_substitution,
            &self.0.expected_general_instantitation
        );
        prop_assert_eq!(result.lifetime_constraints.len(), 0);

        Ok(())
    }
}

impl Arbitrary for FallbackToGeneralImplementation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        SpecializedImplementation::arbitrary()
            .prop_map(|mut prop| {
                let constraint_trait_id = {
                    prop.table.representation.traits.insert_with(|_| Trait {
                        name: "Constraint".to_string(),
                        accessibility: Accessibility::Public,
                        parent_module_id: prop.defined_in_module_id,
                        generic_declaration: GenericDeclaration {
                            parameters: GenericParameters::default(),
                            predicates: Vec::new(),
                        },
                        negative_implementations: HashSet::new(),
                        implementations: HashSet::new(),
                        span: None,
                        member_ids_by_name: HashMap::new(),
                    })
                };

                prop.table
                    .representation
                    .modules
                    .get_mut(prop.defined_in_module_id)
                    .unwrap()
                    .child_ids_by_name
                    .insert(
                        "Constraint".to_string(),
                        constraint_trait_id.into(),
                    );

                // add additional constraint to the specialized
                // implementation
                prop.table
                    .representation
                    .trait_implementations
                    .get_mut(prop.specialized_implementation_id)
                    .unwrap()
                    .signature
                    .generic_declaration
                    .predicates = vec![symbol::Predicate {
                    predicate: predicate::Predicate::Trait(predicate::Trait {
                        id: constraint_trait_id,
                        is_const: false,
                        generic_arguments: GenericArguments {
                            lifetimes: Vec::new(),
                            types: Vec::new(),
                            constants: Vec::new(),
                        },
                    }),
                    kind: symbol::PredicateKind::Explicit(None),
                }];

                Self(prop)
            })
            .boxed()
    }
}

#[derive(Debug)]
pub struct NegativeImplementation {
    pub table: Table<State>,
    pub trait_id: ID<Trait>,

    pub generic_arguments: GenericArguments,
}

impl NegativeImplementation {
    fn assert(&self) -> TestCaseResult {
        let premise = Premise::default();
        let mut session = session::Default::default();

        let result = super::resolve_implementation(
            self.trait_id,
            &self.generic_arguments,
            &Environment { premise: &premise, table: &self.table },
            &mut Limit::new(&mut session),
        );

        prop_assert_eq!(result, Err(TraitResolveError::NotFound));

        Ok(())
    }
}

impl Arbitrary for NegativeImplementation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    #[allow(clippy::too_many_lines)]
    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        SpecializedImplementation::arbitrary()
            .prop_map(|mut prop| {
                let implementation = prop
                    .table
                    .representation
                    .trait_implementations
                    .get_mut(prop.specialized_implementation_id)
                    .unwrap();

                let mut generic_arguments =
                    implementation.signature.arguments.clone();

                let all_lt_parameters = implementation
                    .generic_declaration()
                    .parameters
                    .lifetime_parameters_as_order()
                    .map(|x| {
                        Lifetime::Parameter(MemberID {
                            parent: prop.specialized_implementation_id.into(),
                            id: x.0,
                        })
                    })
                    .collect::<Vec<_>>();
                let all_ty_parameters = implementation
                    .generic_declaration()
                    .parameters
                    .type_parameters_as_order()
                    .map(|x| {
                        Type::Parameter(MemberID {
                            parent: prop.specialized_implementation_id.into(),
                            id: x.0,
                        })
                    })
                    .collect::<Vec<_>>();
                let all_const_parameters = implementation
                    .generic_declaration()
                    .parameters
                    .constant_parameters_as_order()
                    .map(|x| {
                        Constant::Parameter(MemberID {
                            parent: prop.specialized_implementation_id.into(),
                            id: x.0,
                        })
                    })
                    .collect::<Vec<_>>();

                let negative_implementation_id = {
                    let id = prop
                        .table
                        .representation
                        .negative_trait_implementations
                        .insert_with(|_| NegativeTraitImplementation {
                            span: None,
                            signature: ImplementationSignature {
                                generic_declaration: GenericDeclaration {
                                    parameters: GenericParameters::default(),
                                    predicates: Vec::new(),
                                },
                                arguments: GenericArguments::default(),
                                implemented_id: prop.trait_id,
                            },
                            implementation_name: "Test".to_string(),
                            declared_in: prop.defined_in_module_id,
                            data: NegativeTraitImplementationData,
                        });

                    prop.table
                        .representation
                        .traits
                        .get_mut(prop.trait_id)
                        .unwrap()
                        .negative_implementations
                        .insert(id);

                    id
                };

                let negative_implementation = prop
                    .table
                    .representation
                    .negative_trait_implementations
                    .get_mut(negative_implementation_id)
                    .unwrap();

                // remove existing specialized implementation
                prop.table
                    .representation
                    .traits
                    .get_mut(prop.trait_id)
                    .unwrap()
                    .implementations
                    .remove(&prop.specialized_implementation_id);
                prop.table
                    .representation
                    .trait_implementations
                    .remove(prop.specialized_implementation_id)
                    .unwrap();

                let mut instantiation = Instantiation::default();

                for lt_parameter in all_lt_parameters {
                    let new_lt_parameter = negative_implementation
                        .signature
                        .generic_declaration
                        .parameters
                        .add_lifetime_parameter(LifetimeParameter {
                            name: None,
                            span: None,
                        })
                        .unwrap();

                    instantiation.lifetimes.insert(
                        Lifetime::Parameter(MemberID {
                            parent: negative_implementation_id.into(),
                            id: new_lt_parameter,
                        }),
                        lt_parameter,
                    );
                }

                for ty_parameter in all_ty_parameters {
                    let new_ty_parameter = negative_implementation
                        .signature
                        .generic_declaration
                        .parameters
                        .add_type_parameter(TypeParameter {
                            name: None,
                            span: None,
                        })
                        .unwrap();

                    instantiation.types.insert(
                        Type::Parameter(MemberID {
                            parent: negative_implementation_id.into(),
                            id: new_ty_parameter,
                        }),
                        ty_parameter,
                    );
                }

                for const_parameter in all_const_parameters {
                    let new_const_parameter = negative_implementation
                        .signature
                        .generic_declaration
                        .parameters
                        .add_constant_parameter(symbol::ConstantParameter {
                            name: None,
                            r#type: Type::default(),
                            span: None,
                        })
                        .unwrap();

                    instantiation.constants.insert(
                        Constant::Parameter(MemberID {
                            parent: negative_implementation_id.into(),
                            id: new_const_parameter,
                        }),
                        const_parameter,
                    );
                }

                generic_arguments.instantiate(&instantiation);
                negative_implementation.signature.arguments = generic_arguments;

                Self {
                    table: prop.table,
                    trait_id: prop.trait_id,
                    generic_arguments: prop.generic_arguments,
                }
            })
            .boxed()
    }
}

proptest! {
    #[test]
    fn single_implementation(test: SingleImplementation) {
        test.assert()?;
    }

    #[test]
    fn more_specialized_implementation(test: SpecializedImplementation) {
        test.assert()?;
    }

    #[test]
    fn fallback_to_general_implementation(test: FallbackToGeneralImplementation) {
        test.assert()?;
    }

    #[test]
    fn negative_implementation(test: NegativeImplementation) {
        test.assert()?;
    }
}
