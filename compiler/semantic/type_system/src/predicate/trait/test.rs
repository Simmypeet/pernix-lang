use std::{borrow::Cow, collections::BTreeMap};

use pernixc_semantic_element::where_clause::Predicate;
use pernixc_symbol::kind::Kind;
use pernixc_target::{Global, TargetID};
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::{
        GenericParameters, LifetimeParameterID, TypeParameter, TypeParameterID,
    },
    instantiation::Instantiation,
    lifetime::Lifetime,
    predicate::{self, PositiveTrait},
    tuple::{self, Tuple},
    r#type::{Primitive, Qualifier, Reference, Type},
};

use crate::{
    environment::{Environment, Premise},
    normalizer,
    test::create_test_engine,
};

#[tokio::test]
#[allow(clippy::too_many_lines)]
async fn single_implementation() {
    // given a trait predicate `Trait[&'static (int32,)]` with a single
    // implements `implements['a, T] Trait[&'a T]`

    let tuple_with_i32 = Type::Tuple(Tuple {
        elements: vec![tuple::Element {
            term: Type::Primitive(Primitive::Int32),
            is_unpacked: false,
        }],
    });

    let ref_tuple_with_i32 = Type::Reference(Reference {
        qualifier: Qualifier::Immutable,
        lifetime: Lifetime::Static,
        pointee: Box::new(tuple_with_i32.clone()),
    });

    let implements_id =
        Global::new(TargetID::TEST, pernixc_symbol::ID::from_u128(0));
    let trait_id =
        Global::new(TargetID::TEST, pernixc_symbol::ID::from_u128(1));

    let (engine, _dir) = create_test_engine();

    let expected_instantiation = {
        let mut input_session = engine.input_session();
        input_session.set_input(
            pernixc_symbol::kind::Key { symbol_id: implements_id },
            Kind::PositiveImplementation,
        );
        input_session.set_input(
            pernixc_symbol::kind::Key { symbol_id: trait_id },
            Kind::Trait,
        );
        input_session.set_input(
            pernixc_semantic_element::implements::Key {
                symbol_id: implements_id,
            },
            Some(trait_id),
        );
        input_session.set_input(
            pernixc_symbol::final_implements::Key { symbol_id: implements_id },
            false,
        );
        input_session.set_input(
            pernixc_semantic_element::where_clause::Key {
                symbol_id: implements_id,
            },
            engine.intern_unsized(&[] as &[Predicate]),
        );
        input_session.set_input(
            pernixc_semantic_element::implemented::Key { symbol_id: trait_id },
            engine.intern(std::iter::once(implements_id).collect()),
        );

        let mut trait_generic_param = GenericParameters::default();
        trait_generic_param
            .add_type_parameter(TypeParameter {
                name: engine.intern_unsized("T".to_owned()),
                span: None,
            })
            .unwrap();

        input_session.set_input(
            pernixc_term::generic_parameters::Key { symbol_id: trait_id },
            engine.intern(trait_generic_param),
        );

        let mut impl_generic_param = GenericParameters::default();
        let impl_a = impl_generic_param
            .add_lifetime_parameter(
                pernixc_term::generic_parameters::LifetimeParameter {
                    name: engine.intern_unsized("a".to_owned()),
                    span: None,
                },
            )
            .unwrap();
        let impl_t = impl_generic_param
            .add_type_parameter(TypeParameter {
                name: engine.intern_unsized("T".to_owned()),
                span: None,
            })
            .unwrap();

        input_session.set_input(
            pernixc_term::generic_parameters::Key { symbol_id: implements_id },
            engine.intern(impl_generic_param),
        );

        input_session.set_input(
            pernixc_semantic_element::implements_arguments::Key {
                symbol_id: implements_id,
            },
            Some(engine.intern(GenericArguments {
                lifetimes: vec![],
                types: vec![Type::Reference(Reference {
                    qualifier: Qualifier::Immutable,
                    lifetime: Lifetime::Parameter(LifetimeParameterID::new(
                        implements_id,
                        impl_a,
                    )),
                    pointee: Box::new(Type::Parameter(TypeParameterID::new(
                        trait_id, impl_t,
                    ))),
                })],
                constants: vec![],
            })),
        );

        Instantiation {
            lifetimes: std::iter::once((
                Lifetime::Parameter(LifetimeParameterID::new(
                    implements_id,
                    impl_a,
                )),
                Lifetime::Static,
            ))
            .collect(),
            types: std::iter::once((
                Type::Parameter(TypeParameterID::new(trait_id, impl_t)),
                tuple_with_i32.clone(),
            ))
            .collect(),
            constants: BTreeMap::default(),
        }
    };

    let predicate = PositiveTrait::new(trait_id, false, GenericArguments {
        lifetimes: vec![],
        types: vec![ref_tuple_with_i32],
        constants: vec![],
    });

    let environment = Environment::new(
        Cow::Owned(Premise::default()),
        Cow::Owned(engine.tracked()),
        normalizer::NO_OP,
    );

    let result = environment.query(&predicate).await.unwrap().unwrap();

    assert!(result.constraints.is_empty());

    let implementation = result.result.as_implementation().unwrap();

    assert!(!implementation.is_not_general_enough);
    assert_eq!(implementation.id, implements_id);
    assert_eq!(implementation.instantiation, expected_instantiation);

    let non_valid_predicate =
        PositiveTrait::new(trait_id, false, GenericArguments {
            lifetimes: vec![],
            types: vec![Type::Reference(Reference {
                qualifier: Qualifier::Mutable, /* it's mutable now */
                lifetime: Lifetime::Static,
                pointee: Box::new(tuple_with_i32),
            })],
            constants: vec![],
        });

    assert!(environment.query(&non_valid_predicate).await.unwrap().is_none());
}

#[allow(clippy::too_many_lines)]
async fn specialization_test_internal(case: SpecializationCase) {
    let specialized_impl_id =
        Global::new(TargetID::TEST, pernixc_symbol::ID::from_u128(0));
    let general_impl_id =
        Global::new(TargetID::TEST, pernixc_symbol::ID::from_u128(1));
    let trait_id =
        Global::new(TargetID::TEST, pernixc_symbol::ID::from_u128(2));

    let (engine, _dir) = create_test_engine();

    let expected = {
        let mut input_session = engine.input_session();
        input_session.set_input(
            pernixc_symbol::kind::Key { symbol_id: specialized_impl_id },
            Kind::PositiveImplementation,
        );
        input_session.set_input(
            pernixc_semantic_element::implements::Key {
                symbol_id: specialized_impl_id,
            },
            Some(trait_id),
        );
        input_session.set_input(
            pernixc_symbol::final_implements::Key {
                symbol_id: specialized_impl_id,
            },
            false,
        );

        let where_clause_value =
            if let SpecializationCase::SpecializedButFallback(ty) = &case {
                let impossible_trait_id = Global::new(
                    TargetID::TEST,
                    pernixc_symbol::ID::from_u128(10),
                );

                input_session.set_input(
                    pernixc_symbol::kind::Key {
                        symbol_id: impossible_trait_id,
                    },
                    Kind::Trait,
                );
                input_session.set_input(
                    pernixc_semantic_element::implemented::Key {
                        symbol_id: impossible_trait_id,
                    },
                    engine.intern(pernixc_hash::HashSet::default()),
                );

                let mut trait_generic_param = GenericParameters::default();
                trait_generic_param
                    .add_type_parameter(TypeParameter {
                        name: engine.intern_unsized("T".to_owned()),
                        span: None,
                    })
                    .unwrap();

                input_session.set_input(
                    pernixc_term::generic_parameters::Key {
                        symbol_id: impossible_trait_id,
                    },
                    engine.intern(trait_generic_param),
                );

                engine.intern_unsized(vec![Predicate {
                    predicate: predicate::Predicate::PositiveTrait(
                        PositiveTrait {
                            trait_id: impossible_trait_id,
                            is_const: false,
                            generic_arguments: GenericArguments {
                                types: vec![ty.clone()],
                                ..Default::default()
                            },
                        },
                    ),
                    span: None,
                }])
            } else {
                engine.intern_unsized(&[] as &[Predicate])
            };

        input_session.set_input(
            pernixc_semantic_element::where_clause::Key {
                symbol_id: specialized_impl_id,
            },
            where_clause_value,
        );

        let mut impl_generic_param = GenericParameters::default();
        let impl_specialized_t = impl_generic_param
            .add_type_parameter(TypeParameter {
                name: engine.intern_unsized("T".to_owned()),
                span: None,
            })
            .unwrap();

        input_session.set_input(
            pernixc_term::generic_parameters::Key {
                symbol_id: specialized_impl_id,
            },
            engine.intern(impl_generic_param),
        );

        input_session.set_input(
            pernixc_semantic_element::implements_arguments::Key {
                symbol_id: specialized_impl_id,
            },
            Some(engine.intern(GenericArguments {
                lifetimes: vec![],
                types: vec![
                    Type::Parameter(TypeParameterID::new(
                        specialized_impl_id,
                        impl_specialized_t,
                    )),
                    Type::Parameter(TypeParameterID::new(
                        specialized_impl_id,
                        impl_specialized_t,
                    )),
                ],
                constants: vec![],
            })),
        );

        input_session.set_input(
            pernixc_symbol::kind::Key { symbol_id: general_impl_id },
            Kind::PositiveImplementation,
        );
        input_session.set_input(
            pernixc_semantic_element::implements::Key {
                symbol_id: general_impl_id,
            },
            Some(trait_id),
        );
        input_session.set_input(
            pernixc_symbol::final_implements::Key {
                symbol_id: general_impl_id,
            },
            false,
        );
        input_session.set_input(
            pernixc_semantic_element::where_clause::Key {
                symbol_id: general_impl_id,
            },
            engine.intern_unsized(&[] as &[Predicate]),
        );

        let mut impl_generic_param = GenericParameters::default();
        let impl_general_t = impl_generic_param
            .add_type_parameter(TypeParameter {
                name: engine.intern_unsized("T".to_owned()),
                span: None,
            })
            .unwrap();
        let impl_general_u = impl_generic_param
            .add_type_parameter(TypeParameter {
                name: engine.intern_unsized("U".to_owned()),
                span: None,
            })
            .unwrap();

        input_session.set_input(
            pernixc_term::generic_parameters::Key {
                symbol_id: general_impl_id,
            },
            engine.intern(impl_generic_param),
        );

        input_session.set_input(
            pernixc_semantic_element::implements_arguments::Key {
                symbol_id: general_impl_id,
            },
            Some(engine.intern(GenericArguments {
                lifetimes: vec![],
                types: vec![
                    Type::Parameter(TypeParameterID::new(
                        general_impl_id,
                        impl_general_t,
                    )),
                    Type::Parameter(TypeParameterID::new(
                        general_impl_id,
                        impl_general_u,
                    )),
                ],
                constants: vec![],
            })),
        );

        input_session.set_input(
            pernixc_symbol::kind::Key { symbol_id: trait_id },
            Kind::Trait,
        );
        input_session.set_input(
            pernixc_semantic_element::implemented::Key { symbol_id: trait_id },
            engine.intern(
                [specialized_impl_id, general_impl_id].into_iter().collect(),
            ),
        );

        let mut trait_generic_param = GenericParameters::default();
        trait_generic_param
            .add_type_parameter(TypeParameter {
                name: engine.intern_unsized("T".to_owned()),
                span: None,
            })
            .unwrap();

        trait_generic_param
            .add_type_parameter(TypeParameter {
                name: engine.intern_unsized("U".to_owned()),
                span: None,
            })
            .unwrap();

        input_session.set_input(
            pernixc_term::generic_parameters::Key { symbol_id: trait_id },
            engine.intern(trait_generic_param),
        );

        match &case {
            SpecializationCase::SpecializedRewrittenable(ty, _)
            | SpecializationCase::Specialized(ty) => Instantiation {
                types: std::iter::once((
                    Type::Parameter(TypeParameterID::new(
                        specialized_impl_id,
                        impl_specialized_t,
                    )),
                    ty.clone(),
                ))
                .collect(),
                ..Default::default()
            },

            SpecializationCase::SpecializedButFallback(first) => {
                Instantiation {
                    types: std::iter::once((
                        Type::Parameter(TypeParameterID::new(
                            general_impl_id,
                            impl_general_t,
                        )),
                        first.clone(),
                    ))
                    .chain(std::iter::once((
                        Type::Parameter(TypeParameterID::new(
                            general_impl_id,
                            impl_general_u,
                        )),
                        first.clone(),
                    )))
                    .collect(),
                    ..Default::default()
                }
            }

            SpecializationCase::General(first, second) => Instantiation {
                types: std::iter::once((
                    Type::Parameter(TypeParameterID::new(
                        general_impl_id,
                        impl_general_t,
                    )),
                    first.clone(),
                ))
                .chain(std::iter::once((
                    Type::Parameter(TypeParameterID::new(
                        general_impl_id,
                        impl_general_u,
                    )),
                    second.clone(),
                )))
                .collect(),
                ..Default::default()
            },
        }
    };

    let expected_id = match &case {
        SpecializationCase::SpecializedRewrittenable(_, _)
        | SpecializationCase::Specialized(_) => specialized_impl_id,

        SpecializationCase::SpecializedButFallback(_)
        | SpecializationCase::General(_, _) => general_impl_id,
    };

    let predicate = PositiveTrait::new(trait_id, false, GenericArguments {
        types: match case {
            SpecializationCase::SpecializedButFallback(a)
            | SpecializationCase::Specialized(a) => vec![a.clone(), a],

            SpecializationCase::SpecializedRewrittenable(a, b)
            | SpecializationCase::General(a, b) => vec![a, b],
        },
        ..Default::default()
    });

    let environment = Environment::new(
        Cow::Owned(Premise::default()),
        Cow::Owned(engine.tracked()),
        normalizer::NO_OP,
    );

    let result = environment.query(&predicate).await.unwrap().unwrap();

    assert!(result.constraints.is_empty());

    let implementation = result.result.as_implementation().unwrap();

    assert!(!implementation.is_not_general_enough);
    assert_eq!(implementation.id, expected_id);
    assert_eq!(implementation.instantiation, expected);
}

enum SpecializationCase {
    Specialized(Type),
    General(Type, Type),
    SpecializedRewrittenable(Type, Type),
    SpecializedButFallback(Type),
}

#[rstest::rstest]
#[case(SpecializationCase::Specialized(Type::Primitive(Primitive::Int32)))]
#[case(SpecializationCase::General(
    Type::Primitive(Primitive::Int32),
    Type::Primitive(Primitive::Float32)
))]
#[case(SpecializationCase::SpecializedRewrittenable(
    Type::Tuple(Tuple {
        elements: vec![
            tuple::Element {
                term: Type::Primitive(Primitive::Int8),
                is_unpacked: false,
            },
            tuple::Element {
                term: Type::Primitive(Primitive::Int16),
                is_unpacked: false,
            },
            tuple::Element {
                term: Type::Primitive(Primitive::Int32),
                is_unpacked: false,
            },
            tuple::Element {
                term: Type::Primitive(Primitive::Int64),
                is_unpacked: false,
            },
        ]
    }),
    Type::Tuple(Tuple {
        elements: vec![
            tuple::Element {
                term: Type::Primitive(Primitive::Int8),
                is_unpacked: false,
            },
            tuple::Element {
                term: Type::Tuple(Tuple {
                    elements: vec![
                        tuple::Element {
                            term: Type::Primitive(Primitive::Int16),
                            is_unpacked: false,
                        },

                        tuple::Element {
                            term: Type::Primitive(Primitive::Int32),
                            is_unpacked: false,
                        },
                    ]
                }),
                is_unpacked: true,
            },
            tuple::Element {
                term: Type::Primitive(Primitive::Int64),
                is_unpacked: false,
            },
        ]
    }),
))]
#[case(SpecializationCase::SpecializedButFallback(Type::Primitive(
    Primitive::Int32
)))]
fn more_specialized_implementation(#[case] specialization: SpecializationCase) {
    tokio::runtime::Runtime::new()
        .unwrap()
        .block_on(specialization_test_internal(specialization));
}
