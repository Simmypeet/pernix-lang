use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    iter::once,
    ops::Not,
    sync::Arc,
};

use lazy_static::lazy_static;
use pernixc_component::{
    implementation::Implementation, where_clause::WhereClause,
};
use pernixc_table::{
    component::{
        Implemented, Implements, PositiveTraitImplementation, SymbolKind,
        TraitImplementation,
    },
    GlobalID, MemberID, Table, TargetID, ID,
};
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameter::{
        ConstantParameter, GenericParameters, LifetimeParameter, TypeParameter,
    },
    instantiation::{self, Instantiation},
    lifetime::Lifetime,
    predicate::{PositiveTrait, Predicate},
    r#type::Type,
    visitor::RecursiveIterator,
    Default, Kind, Tuple,
};
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy},
    prop_assert, prop_assert_eq, proptest,
    strategy::LazyJust,
    test_runner::TestCaseResult,
};

use crate::{
    definite::Definite,
    environment::{Environment, Premise},
    normalizer, Succeeded,
};

lazy_static! {
    static ref TABLE: Table = Table::new(Arc::new(pernixc_handler::Panic));
}

fn definite_lifetime() -> impl Strategy<Value = Lifetime<Default>> {
    Lifetime::arbitrary().prop_filter("filter out non-definite terms", |term| {
        let environment = Environment::with_default(&TABLE);
        matches!(environment.query(&Definite::new(term.clone())), Ok(Some(_)))
            && !RecursiveIterator::new(term)
                .any(|(x, _)| matches!(x, Kind::Type(Type::TraitMember(_))))
    })
}

fn definite_type() -> impl Strategy<Value = Type<Default>> {
    Type::arbitrary().prop_filter("filter out non-definite terms", |term| {
        let environment = Environment::with_default(&TABLE);
        matches!(environment.query(&Definite::new(term.clone())), Ok(Some(_)))
            && !RecursiveIterator::new(term)
                .any(|(x, _)| matches!(x, Kind::Type(Type::TraitMember(_))))
    })
}

fn definite_constant() -> impl Strategy<Value = Constant<Default>> {
    Constant::arbitrary().prop_filter("filter out non-definite terms", |term| {
        let environment = Environment::with_default(&TABLE);
        matches!(environment.query(&Definite::new(term.clone())), Ok(Some(_)))
            && !RecursiveIterator::new(term)
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

#[derive(Debug, Clone)]
pub struct SingleImplementation {
    pub trait_id: GlobalID,
    pub trait_generic_parameters: GenericParameters,

    pub implementation_id: GlobalID,
    pub implementation_generic_parameters: GenericParameters,
    pub implementation_generic_arguments: GenericArguments<Default>,

    pub generic_arguments: GenericArguments<Default>,
    pub expected_instantiation: Instantiation<Default>,
}

impl SingleImplementation {
    fn assert(self) -> TestCaseResult {
        let table = Table::new(Arc::new(pernixc_handler::Panic));
        let premise = Premise::default();

        assert!(
            table.add_component(self.trait_id, self.trait_generic_parameters)
        );
        assert!(table.add_component(self.trait_id, WhereClause::default()));
        assert!(table.add_component(self.trait_id, SymbolKind::Trait));
        assert!(table.add_component(
            self.trait_id,
            Implemented(once(self.implementation_id).collect()),
        ));

        assert!(table.add_component(
            self.implementation_id,
            self.implementation_generic_parameters,
        ));
        assert!(table
            .add_component(self.implementation_id, WhereClause::default(),));
        assert!(table.add_component(
            self.implementation_id,
            SymbolKind::PositiveTraitImplementation,
        ));
        assert!(table
            .add_component(self.implementation_id, Implements(self.trait_id),));
        assert!(table.add_component(self.implementation_id, Implementation {
            generic_arguments: self.implementation_generic_arguments,
        }));
        assert!(table.add_component(
            self.implementation_id,
            PositiveTraitImplementation { is_const: false },
        ));
        assert!(table
            .add_component(self.implementation_id, TraitImplementation {
                is_final: false
            }));

        let environment = Environment::new(&premise, &table, normalizer::NO_OP);

        let Succeeded { result, constraints } = environment
            .resolve_implementation(self.trait_id, &self.generic_arguments)?;

        prop_assert!(constraints.is_empty());

        prop_assert_eq!(result.id, self.implementation_id);
        prop_assert_eq!(&result.instantiation, &self.expected_instantiation);

        Ok(())
    }

    fn create_trait(
        generic_arguments: &GenericArguments<Default>,
    ) -> (GlobalID, GenericParameters) {
        let trait_id = GlobalID::new(TargetID(1), ID(1));

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
                .add_type_parameter(TypeParameter { name: None, span: None })
                .unwrap();
        }

        for _ in 0..generic_arguments.constants.len() {
            generic_parameters
                .add_constant_parameter(ConstantParameter {
                    name: None,
                    r#type: Type::Tuple(Tuple { elements: Vec::new() }),
                    span: None,
                })
                .unwrap();
        }

        (trait_id, generic_parameters)
    }

    #[allow(clippy::too_many_lines)]
    fn create_implementation(
        mut generic_arguments: GenericArguments<Default>,
        to_be_substituted_type: Vec<Type<Default>>,
        to_be_substituted_constant: Vec<Constant<Default>>,
    ) -> (
        GlobalID,
        GenericParameters,
        GenericArguments<Default>,
        Instantiation<Default>,
    ) {
        // the trait implementation id which will be resolved to
        let implementation_id = GlobalID::new(TargetID(1), ID(2));

        let mut generic_parameters = GenericParameters::default();
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

            let id = generic_parameters
                .add_type_parameter(TypeParameter { name: None, span: None })
                .unwrap();

            let pair = (
                ty,
                Type::Parameter(MemberID { parent: implementation_id, id }),
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

            let id = generic_parameters
                .add_constant_parameter(ConstantParameter {
                    name: None,
                    r#type: Type::Tuple(Tuple { elements: Vec::new() }),
                    span: None,
                })
                .unwrap();

            let pair = (
                constant,
                Constant::Parameter(MemberID { parent: implementation_id, id }),
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

            let id = generic_parameters
                .add_lifetime_parameter(LifetimeParameter {
                    name: None,
                    span: None,
                })
                .unwrap();

            let pair = (
                lt,
                Lifetime::Parameter(MemberID { parent: implementation_id, id }),
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

        (
            implementation_id,
            generic_parameters,
            generic_arguments,
            expected_instantiation,
        )
    }
}

impl Arbitrary for SingleImplementation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    #[allow(clippy::too_many_lines)]
    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        definite_generic_arguments()
            .prop_flat_map(|generic_arguments| {
                let another_generic_arguments = generic_arguments.clone();

                let trait_info = LazyJust::new(move || {
                    Self::create_trait(&generic_arguments)
                });

                // collect all the types appearing in the generic arguments
                let all_types = another_generic_arguments
                    .types
                    .iter()
                    .flat_map(RecursiveIterator::new)
                    .filter_map(|(term, _)| {
                        term.as_type().and_then(|x| {
                            x.is_parameter().not().then(|| (*x).clone())
                        })
                    })
                    .collect::<Vec<_>>();
                // collect all the constants appearing in the generic arguments
                let all_constants = another_generic_arguments
                    .constants
                    .iter()
                    .flat_map(RecursiveIterator::new)
                    .filter_map(|(term, _)| {
                        term.as_constant().and_then(|x| {
                            x.is_parameter().not().then(|| (*x).clone())
                        })
                    })
                    .chain(
                        another_generic_arguments
                            .types
                            .iter()
                            .flat_map(RecursiveIterator::new)
                            .filter_map(|(term, _)| {
                                term.as_constant().and_then(|x| {
                                    x.is_parameter().not().then(|| (*x).clone())
                                })
                            }),
                    )
                    .collect::<Vec<_>>();

                (
                    Just(another_generic_arguments),
                    (
                        trait_info,
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
                    ),
                )
            })
            .prop_map(
                |(
                    generic_arguments,
                    (
                        (trait_id, trait_generic_parameters),
                        to_be_substituted_type,
                        to_be_substituted_constant,
                    ),
                )| {
                    let (
                        implementation_id,
                        implementation_generic_parameters,
                        implementation_generic_arguments,
                        expected_instantiation,
                    ) = Self::create_implementation(
                        generic_arguments.clone(),
                        to_be_substituted_type,
                        to_be_substituted_constant,
                    );

                    Self {
                        trait_id,
                        trait_generic_parameters,
                        implementation_id,
                        implementation_generic_parameters,
                        implementation_generic_arguments,
                        generic_arguments,
                        expected_instantiation,
                    }
                },
            )
            .boxed()
    }
}

#[derive(Debug)]
#[allow(clippy::struct_field_names)]
pub struct SpecializedImplementation {
    pub trait_id: GlobalID,
    pub trait_generic_parameters: GenericParameters,

    pub specialized_implementation_id: GlobalID,
    pub specialized_implementation_generic_parameters: GenericParameters,
    pub specialized_implementation_generic_arguments: GenericArguments<Default>,

    pub general_implementation_id: GlobalID,
    pub general_implementation_generic_parameters: GenericParameters,
    pub general_implementation_generic_arguments: GenericArguments<Default>,

    pub expected_specialized_instantitation: Instantiation<Default>,
    pub expected_general_instantitation: Instantiation<Default>,

    pub generic_arguments: GenericArguments<Default>,
}

impl SpecializedImplementation {
    fn assert(self) -> TestCaseResult {
        let table = Table::new(Arc::new(pernixc_handler::Panic));
        let premise = Premise::default();

        assert!(
            table.add_component(self.trait_id, self.trait_generic_parameters)
        );
        assert!(table.add_component(self.trait_id, WhereClause::default()));
        assert!(table.add_component(self.trait_id, SymbolKind::Trait));
        assert!(table.add_component(
            self.trait_id,
            Implemented(
                [
                    self.specialized_implementation_id,
                    self.general_implementation_id
                ]
                .into_iter()
                .collect()
            ),
        ));

        assert!(table.add_component(
            self.specialized_implementation_id,
            self.specialized_implementation_generic_parameters,
        ));
        assert!(table.add_component(
            self.specialized_implementation_id,
            WhereClause::default(),
        ));
        assert!(table.add_component(
            self.specialized_implementation_id,
            SymbolKind::PositiveTraitImplementation,
        ));
        assert!(table.add_component(
            self.specialized_implementation_id,
            Implements(self.trait_id),
        ));
        assert!(table.add_component(
            self.specialized_implementation_id,
            Implementation {
                generic_arguments: self
                    .specialized_implementation_generic_arguments,
            },
        ));
        assert!(table.add_component(
            self.specialized_implementation_id,
            PositiveTraitImplementation { is_const: false },
        ));
        assert!(table.add_component(
            self.specialized_implementation_id,
            TraitImplementation { is_final: false },
        ));

        assert!(table.add_component(
            self.general_implementation_id,
            self.general_implementation_generic_parameters
        ));
        assert!(table.add_component(
            self.general_implementation_id,
            WhereClause::default()
        ));
        assert!(table.add_component(
            self.general_implementation_id,
            SymbolKind::PositiveTraitImplementation
        ));
        assert!(table.add_component(
            self.general_implementation_id,
            Implements(self.trait_id)
        ));
        assert!(table.add_component(
            self.general_implementation_id,
            Implementation {
                generic_arguments: self
                    .general_implementation_generic_arguments,
            }
        ));
        assert!(table.add_component(
            self.general_implementation_id,
            PositiveTraitImplementation { is_const: false }
        ));
        assert!(table.add_component(
            self.general_implementation_id,
            TraitImplementation { is_final: false }
        ));

        let environment = Environment::new(&premise, &table, normalizer::NO_OP);

        let Succeeded { result, constraints } = environment
            .resolve_implementation(self.trait_id, &self.generic_arguments)?;

        prop_assert!(constraints.is_empty());

        prop_assert_eq!(result.id, self.specialized_implementation_id);
        prop_assert_eq!(
            &result.instantiation,
            &self.expected_specialized_instantitation
        );

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn create_general_implementation(
        mut generic_arguments: GenericArguments<Default>,
        expected_specialized_instantiation: &Instantiation<Default>,
        to_be_substituted_type: Vec<Type<Default>>,
        to_be_substituted_constant: Vec<Constant<Default>>,
    ) -> Option<(
        GlobalID,
        GenericParameters,
        GenericArguments<Default>,
        Instantiation<Default>,
    )> {
        if to_be_substituted_type.is_empty()
            && to_be_substituted_constant.is_empty()
        {
            return None;
        }

        let mut expected_instantitation = Instantiation::default();

        // the trait implementation id which will is more general
        let general_implementation_id = GlobalID::new(TargetID(1), ID(3));
        let mut generic_parameters = GenericParameters::default();

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

            let id = generic_parameters
                .add_type_parameter(TypeParameter { name: None, span: None })
                .unwrap();

            let mut pair = (
                ty,
                Type::Parameter(MemberID {
                    parent: general_implementation_id,
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

            let id = generic_parameters
                .add_constant_parameter(ConstantParameter {
                    name: None,
                    r#type: Type::Tuple(Tuple { elements: Vec::new() }),
                    span: None,
                })
                .unwrap();

            let mut pair = (
                constant,
                Constant::Parameter(MemberID {
                    parent: general_implementation_id,
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

            let id = generic_parameters
                .add_lifetime_parameter(LifetimeParameter {
                    name: None,
                    span: None,
                })
                .unwrap();

            let mut pair = (
                lt,
                Lifetime::Parameter(MemberID {
                    parent: general_implementation_id,
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

        Some((
            general_implementation_id,
            generic_parameters,
            generic_arguments,
            expected_instantitation,
        ))
    }
}

impl Arbitrary for SpecializedImplementation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    #[allow(clippy::too_many_lines)]
    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        SingleImplementation::arbitrary()
            .prop_flat_map(|single_implementation| {
                let all_types = single_implementation
                    .implementation_generic_arguments
                    .types
                    .iter()
                    .flat_map(RecursiveIterator::new)
                    .filter_map(|x| {
                        // filter out type parameter terms
                        x.0.as_type().and_then(|x| {
                            x.is_parameter().not().then(|| (*x).clone())
                        })
                    })
                    .filter(|x| RecursiveIterator::new(x).count() > 1)
                    .collect::<Vec<_>>();

                let all_constants = single_implementation
                    .implementation_generic_arguments
                    .constants
                    .iter()
                    .flat_map(RecursiveIterator::new)
                    .chain(
                        single_implementation
                            .implementation_generic_arguments
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
                    .filter(|x| RecursiveIterator::new(x).count() > 1)
                    .collect::<Vec<_>>();

                let all_types_len = all_types.len();
                let all_constants_len = all_constants.len();

                (
                    Just(single_implementation),
                    (
                        proptest::sample::subsequence(
                            all_types,
                            0..=all_types_len,
                        )
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
                    ),
                )
            })
            .prop_filter_map(
                "filter out non-general generic arguments",
                |(
                    single_implementation,
                    (selected_types, selected_constants),
                )| {
                    let (
                        general_implementation_id,
                        general_implementation_generic_parameters,
                        general_implementation_generic_arguments,
                        expected_general_instantitation,
                    ) = Self::create_general_implementation(
                        single_implementation
                            .implementation_generic_arguments
                            .clone(),
                        &single_implementation.expected_instantiation,
                        selected_types,
                        selected_constants,
                    )?;
                    Some(Self {
                        trait_id: single_implementation.trait_id,
                        trait_generic_parameters: single_implementation
                            .trait_generic_parameters,
                        specialized_implementation_id: single_implementation
                            .implementation_id,
                        specialized_implementation_generic_parameters:
                            single_implementation
                                .implementation_generic_parameters,
                        specialized_implementation_generic_arguments:
                            single_implementation
                                .implementation_generic_arguments,
                        general_implementation_id,
                        general_implementation_generic_parameters,
                        general_implementation_generic_arguments,
                        expected_specialized_instantitation:
                            single_implementation.expected_instantiation,
                        expected_general_instantitation,
                        generic_arguments: single_implementation
                            .generic_arguments,
                    })
                },
            )
            .boxed()
    }
}

#[derive(Debug)]
pub struct FallbackToGeneralImplementation(SpecializedImplementation);

impl FallbackToGeneralImplementation {
    #[allow(clippy::too_many_lines)]
    fn assert(self) -> TestCaseResult {
        let table = Table::new(Arc::new(pernixc_handler::Panic));
        let premise = Premise::default();

        let impossible_constraint_id = GlobalID::new(TargetID(1), ID(4));

        assert!(table
            .add_component(impossible_constraint_id, WhereClause::default()));
        assert!(table.add_component(
            impossible_constraint_id,
            GenericParameters::default()
        ));
        assert!(
            table.add_component(impossible_constraint_id, SymbolKind::Trait)
        );
        assert!(table.add_component(
            impossible_constraint_id,
            Implemented(HashSet::default())
        ));

        assert!(table
            .add_component(self.0.trait_id, self.0.trait_generic_parameters));
        assert!(table.add_component(self.0.trait_id, WhereClause::default()));
        assert!(table.add_component(self.0.trait_id, SymbolKind::Trait));
        assert!(table.add_component(
            self.0.trait_id,
            Implemented(
                [
                    self.0.specialized_implementation_id,
                    self.0.general_implementation_id
                ]
                .into_iter()
                .collect()
            ),
        ));

        assert!(table.add_component(
            self.0.specialized_implementation_id,
            self.0.specialized_implementation_generic_parameters,
        ));
        assert!(table.add_component(
            self.0.specialized_implementation_id,
            WhereClause {
                predicates: once(pernixc_component::where_clause::Predicate {
                    predicate: Predicate::PositiveTrait(PositiveTrait {
                        trait_id: impossible_constraint_id,
                        is_const: false,
                        generic_arguments: GenericArguments::default()
                    }),
                    span: None
                })
                .collect()
            },
        ));
        assert!(table.add_component(
            self.0.specialized_implementation_id,
            SymbolKind::PositiveTraitImplementation,
        ));
        assert!(table.add_component(
            self.0.specialized_implementation_id,
            Implements(self.0.trait_id),
        ));
        assert!(table.add_component(
            self.0.specialized_implementation_id,
            Implementation {
                generic_arguments: self
                    .0
                    .specialized_implementation_generic_arguments,
            },
        ));
        assert!(table.add_component(
            self.0.specialized_implementation_id,
            PositiveTraitImplementation { is_const: false },
        ));
        assert!(table.add_component(
            self.0.specialized_implementation_id,
            TraitImplementation { is_final: false },
        ));

        assert!(table.add_component(
            self.0.general_implementation_id,
            self.0.general_implementation_generic_parameters
        ));
        assert!(table.add_component(
            self.0.general_implementation_id,
            WhereClause::default()
        ));
        assert!(table.add_component(
            self.0.general_implementation_id,
            SymbolKind::PositiveTraitImplementation
        ));
        assert!(table.add_component(
            self.0.general_implementation_id,
            Implements(self.0.trait_id)
        ));
        assert!(table.add_component(
            self.0.general_implementation_id,
            Implementation {
                generic_arguments: self
                    .0
                    .general_implementation_generic_arguments,
            }
        ));
        assert!(table.add_component(
            self.0.general_implementation_id,
            PositiveTraitImplementation { is_const: false }
        ));
        assert!(table.add_component(
            self.0.general_implementation_id,
            TraitImplementation { is_final: false }
        ));

        let environment = Environment::new(&premise, &table, normalizer::NO_OP);

        let Succeeded { result, constraints } = environment
            .resolve_implementation(
                self.0.trait_id,
                &self.0.generic_arguments,
            )?;

        prop_assert!(constraints.is_empty());

        prop_assert_eq!(result.id, self.0.general_implementation_id);
        prop_assert_eq!(
            &result.instantiation,
            &self.0.expected_general_instantitation
        );

        Ok(())
    }
}

impl Arbitrary for FallbackToGeneralImplementation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        SpecializedImplementation::arbitrary()
            .prop_map(FallbackToGeneralImplementation)
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
    fn fallback_to_general_implementation(
        test: FallbackToGeneralImplementation
    ) {
        test.assert()?;
    }
}
