use std::{
    collections::{BTreeMap, BTreeSet},
    ops::Not,
};

use lazy_static::lazy_static;
use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_assert_eq, proptest,
    strategy::{BoxedStrategy, LazyJust, Strategy},
    test_runner::TestCaseResult,
};

use crate::{
    arena::ID,
    symbol::{
        self,
        table::{
            representation::{Index, IndexMut, Insertion},
            Building, Table,
        },
        Accessibility, GenericDeclaration, GenericParameters,
        LifetimeParameter, MemberID, Module, PositiveTraitImplementation,
        PositiveTraitImplementationDefinition, Trait, TraitDefinition,
        TraitImplementationID, TypeParameter,
    },
    type_system::{
        definite::Definite,
        instantiation::{self, Instantiation},
        model::Default,
        normalizer, observer,
        order::Order,
        predicate::{self, resolve_implementation},
        term::{
            constant::Constant, lifetime::Lifetime, r#type::Type,
            GenericArguments, Kind,
        },
        visitor::RecursiveIterator,
        Compute, Environment, Premise, Succeeded,
    },
};

lazy_static! {
    static ref TABLE: Table<Building> = Table::default();
}

fn definite_lifetime() -> impl Strategy<Value = Lifetime<Default>> {
    Lifetime::arbitrary().prop_filter("filter out non-definite terms", |term| {
        matches!(
            Definite(term.clone()).query(&Environment {
                premise: Premise::default(),
                table: &*TABLE,
                normalizer: normalizer::NO_OP,
                observer: observer::NO_OP
            }),
            Ok(Some(_))
        ) && !RecursiveIterator::new(term)
            .any(|(x, _)| matches!(x, Kind::Type(Type::TraitMember(_))))
    })
}

fn definite_type() -> impl Strategy<Value = Type<Default>> {
    Type::arbitrary().prop_filter("filter out non-definite terms", |term| {
        matches!(
            Definite(term.clone()).query(&Environment {
                premise: Premise::default(),
                table: &*TABLE,
                normalizer: normalizer::NO_OP,
                observer: observer::NO_OP
            }),
            Ok(Some(_))
        ) && !RecursiveIterator::new(term)
            .any(|(x, _)| matches!(x, Kind::Type(Type::TraitMember(_))))
    })
}

fn definite_constant() -> impl Strategy<Value = Constant<Default>> {
    Constant::arbitrary().prop_filter("filter out non-definite terms", |term| {
        matches!(
            Definite(term.clone()).query(&Environment {
                premise: Premise::default(),
                table: &*TABLE,
                normalizer: normalizer::NO_OP,
                observer: observer::NO_OP
            }),
            Ok(Some(_))
        ) && !RecursiveIterator::new(term)
            .any(|(x, _)| matches!(x, Kind::Type(Type::TraitMember(_))))
    })
}

fn definite_generic_arguments(
) -> impl Strategy<Value = GenericArguments<Default>> {
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
    pub target_implementation_id: ID<PositiveTraitImplementation>,
    pub generic_arguments: GenericArguments<Default>,
    pub table: Table<Building>,
    pub expected_instantiation: Instantiation<Default>,
}

impl SingleImplementation {
    fn assert(&self) -> TestCaseResult {
        let premise = Premise::default();

        let Succeeded { result, constraints } = resolve_implementation(
            self.trait_id,
            &self.generic_arguments,
            &Environment {
                premise,
                table: &self.table,
                normalizer: normalizer::NO_OP,
                observer: observer::NO_OP,
            },
        )?;

        prop_assert!(constraints.is_empty());

        prop_assert_eq!(
            result.id,
            TraitImplementationID::Positive(self.target_implementation_id)
        );
        prop_assert_eq!(&result.instantiation, &self.expected_instantiation);

        Ok(())
    }

    fn create_table_from_generic_arguments(
        generic_arguments: &GenericArguments<Default>,
    ) -> (Table<Building>, ID<Module>, ID<Trait>) {
        let mut table = Table::<Building>::default();

        // the module where the trait is defined
        let module_id = {
            let Insertion { id: module_id, duplication } =
                table.create_root_module("test".to_string());

            assert!(duplication.is_none());

            module_id
        };

        // the trait has the same size as the generic arguments
        let trait_id = {
            let generic_declaration = GenericDeclaration {
                parameters: {
                    let mut generic_parameters = GenericParameters::default();

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
                            .add_constant_parameter(symbol::ConstantParameter {
                                name: None,
                                r#type: Type::default(),
                                span: None,
                            })
                            .unwrap();
                    }

                    generic_parameters
                },
                predicates: Vec::new(),
            };

            let Insertion { id, duplication } = table
                .insert_member(
                    "Test".to_string(),
                    Accessibility::Public,
                    module_id,
                    None,
                    generic_declaration,
                    TraitDefinition::default(),
                )
                .unwrap();

            assert!(duplication.is_none());

            id
        };

        (table, module_id, trait_id)
    }

    #[allow(clippy::too_many_lines)]
    fn create_implementation(
        table: &mut Table<Building>,
        module_id: ID<Module>,
        trait_id: ID<Trait>,
        mut generic_arguments: GenericArguments<Default>,
        to_be_substituted_type: Vec<Type<Default>>,
        to_be_substituted_constant: Vec<Constant<Default>>,
    ) -> (ID<PositiveTraitImplementation>, Instantiation<Default>) {
        // the trait implementation id which will be resolved to
        let implementation_id = table
            .insert_implementation(
                trait_id,
                module_id,
                GenericDeclaration::default(),
                None,
                GenericArguments::default(),
                PositiveTraitImplementationDefinition::default(),
            )
            .unwrap();

        let mut expected_instantiation = Instantiation::default();
        let implementation = table.get_mut(implementation_id).unwrap();

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
                lifetimes: BTreeMap::new(),
                types: std::iter::once(pair.clone()).collect(),
                constants: BTreeMap::new(),
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
                lifetimes: BTreeMap::new(),
                types: BTreeMap::new(),
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
            .cloned()
            .chain(
                generic_arguments
                    .types
                    .iter()
                    .flat_map(RecursiveIterator::new)
                    .filter_map(|(term, _)| {
                        term.as_lifetime().map(|x| (*x).clone())
                    }),
            )
            .collect::<BTreeSet<_>>();

        for lt in all_lifetimes {
            // no need to check for existence because it is guaranteed to not
            // be overwritten

            let id = implementation
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
                lifetimes: std::iter::once(pair.clone()).collect(),
                types: BTreeMap::new(),
                constants: BTreeMap::new(),
            };

            generic_arguments.instantiate(&instantiation);

            // add to the expected list
            expected_instantiation.lifetimes.insert(pair.1, pair.0);
        }

        implementation.arguments = generic_arguments;

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
    pub table: Table<Building>,
    pub trait_id: ID<Trait>,
    pub defined_in_module_id: ID<Module>,

    pub general_implementation_id: ID<PositiveTraitImplementation>,

    #[allow(clippy::struct_field_names)]
    pub specialized_implementation_id: ID<PositiveTraitImplementation>,

    pub expected_specialized_instantitation: Instantiation<Default>,
    pub expected_general_instantitation: Instantiation<Default>,

    pub generic_arguments: GenericArguments<Default>,
}

impl SpecializedImplementation {
    fn assert(&self) -> TestCaseResult {
        // should match to the specialized implementation
        let Succeeded { result, constraints } = resolve_implementation(
            self.trait_id,
            &self.generic_arguments,
            &Environment {
                premise: Premise::default(),
                table: &self.table,
                normalizer: normalizer::NO_OP,
                observer: observer::NO_OP,
            },
        )?;

        prop_assert_eq!(
            result.id,
            TraitImplementationID::Positive(self.specialized_implementation_id)
        );
        prop_assert_eq!(
            &result.instantiation,
            &self.expected_specialized_instantitation
        );
        prop_assert!(constraints.is_empty());

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn create_implementation(
        table: &mut Table<Building>,
        module_id: ID<Module>,
        trait_id: ID<Trait>,
        mut generic_arguments: GenericArguments<Default>,
        expected_specialized_instantiation: &Instantiation<Default>,
        to_be_substituted_type: Vec<Type<Default>>,
        to_be_substituted_constant: Vec<Constant<Default>>,
    ) -> Option<(ID<PositiveTraitImplementation>, Instantiation<Default>)> {
        let starting_generic_arguments = generic_arguments.clone();
        let mut expected_instantitation = Instantiation::default();

        // the trait implementation id which will is more general
        let general_implementation_id = table
            .insert_implementation(
                trait_id,
                module_id,
                GenericDeclaration::default(),
                None,
                GenericArguments::default(),
                PositiveTraitImplementationDefinition::default(),
            )
            .unwrap();

        let general_implementation =
            table.get_mut(general_implementation_id).unwrap();

        // replace the choosen types with the type parameter
        let all_type_parameters = generic_arguments
            .types
            .iter()
            .flat_map(RecursiveIterator::new)
            .filter_map(|(term, _)| term.as_type().copied())
            .filter(|x| x.is_parameter())
            .cloned()
            .collect::<BTreeSet<_>>();

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
            .collect::<BTreeSet<_>>();

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
                lifetimes: BTreeMap::new(),
                types: std::iter::once(pair.clone()).collect(),
                constants: BTreeMap::new(),
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
                lifetimes: BTreeMap::new(),
                types: BTreeMap::new(),
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
            .cloned()
            .chain(
                generic_arguments
                    .types
                    .iter()
                    .flat_map(RecursiveIterator::new)
                    .filter_map(|(term, _)| {
                        term.as_lifetime().map(|x| (*x).clone())
                    }),
            )
            .collect::<BTreeSet<_>>();

        for lt in all_lifetimes {
            // no need to check for existence because it is guaranteed to not
            // be overwritten

            let id = general_implementation
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
                lifetimes: std::iter::once(pair.clone()).collect(),
                types: BTreeMap::new(),
                constants: BTreeMap::new(),
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

        general_implementation.arguments = generic_arguments;

        if general_implementation.arguments.clone().order(
            &starting_generic_arguments,
            &Environment {
                premise: Premise::default(),
                table,
                normalizer: normalizer::NO_OP,
                observer: observer::NO_OP,
            },
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
                    .get(single_implementation.target_implementation_id)
                    .unwrap()
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
                        .get(single_implementation.target_implementation_id)
                        .unwrap()
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
        let Succeeded { result, constraints } = resolve_implementation(
            self.0.trait_id,
            &self.0.generic_arguments,
            &Environment {
                premise: Premise::default(),
                table: &self.0.table,
                normalizer: normalizer::NO_OP,
                observer: observer::NO_OP,
            },
        )?;

        prop_assert_eq!(
            result.id,
            TraitImplementationID::Positive(self.0.general_implementation_id)
        );
        prop_assert_eq!(
            &result.instantiation,
            &self.0.expected_general_instantitation
        );
        prop_assert!(constraints.is_empty());

        Ok(())
    }
}

impl Arbitrary for FallbackToGeneralImplementation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        SpecializedImplementation::arbitrary()
            .prop_map(|mut prop| {
                let Insertion { id: constraint_trait_id, duplication } = prop
                    .table
                    .insert_member(
                        "Constraint".to_string(),
                        Accessibility::Public,
                        prop.defined_in_module_id,
                        None,
                        GenericDeclaration::default(),
                        TraitDefinition::default(),
                    )
                    .unwrap();

                assert!(duplication.is_none());

                // add additional constraint to the specialized
                // implementation
                prop.table
                    .get_mut(prop.specialized_implementation_id)
                    .unwrap()
                    .generic_declaration
                    .predicates = vec![symbol::Predicate {
                    predicate: predicate::Predicate::PositiveTrait(
                        predicate::PositiveTrait {
                            id: constraint_trait_id,
                            generic_arguments: GenericArguments {
                                lifetimes: Vec::new(),
                                types: Vec::new(),
                                constants: Vec::new(),
                            },
                            is_const: false,
                        },
                    ),
                    span: None,
                }];

                Self(prop)
            })
            .boxed()
    }
}

/*
#[derive(Debug)]
pub struct NegativeImplementation {
    pub table: Table<Building>,
    pub trait_id: ID<Trait>,

    pub generic_arguments: GenericArguments<Default>,
}

impl NegativeImplementation {
    fn assert(&self) -> TestCaseResult {
        let premise = Premise::default();
        let mut session = session::Default::default();

        let result = super::resolve_implementation(
            self.trait_id,
            &self.generic_arguments,
            &Environment {
                premise: &premise,
                table: &self.table,
                normalizer: normalizer::NO_OP,
            },
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
                            definition: NegativeTraitImplementationDefinition,
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

    }
}
*/

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
}
