use std::{borrow::Cow, collections::BTreeMap, sync::Arc};

use pernixc_query::Engine;
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
    r#type::{Primitive, Qualifier, Reference, Type},
    tuple::{self, Tuple},
    where_clause::Predicate,
};

use crate::{
    environment::{Environment, Premise},
    normalizer, order,
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

    let implements_id = Global::new(TargetID::Extern(1), pernixc_symbol::ID(0));
    let trait_id = Global::new(TargetID::Extern(1), pernixc_symbol::ID(1));

    let mut engine = Arc::new(Engine::default());

    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::new(order::ImplementsOrderExecutor));

    let expected_instantiation =
        Arc::get_mut(&mut engine).unwrap().input_session(|x| {
            x.set_input(
                pernixc_symbol::kind::Key(implements_id),
                Kind::PositiveImplementation,
            );
            x.set_input(pernixc_symbol::kind::Key(trait_id), Kind::Trait);
            x.set_input(
                pernixc_term::implements::Key(implements_id),
                Some(trait_id),
            );
            x.set_input(
                pernixc_symbol::final_implements::Key(implements_id),
                false,
            );
            x.set_input(
                pernixc_term::where_clause::Key(implements_id),
                Arc::default(),
            );
            x.set_input(
                pernixc_term::implemented::Key(trait_id),
                Arc::new(std::iter::once(implements_id).collect()),
            );

            let mut trait_generic_param = GenericParameters::default();
            trait_generic_param
                .add_type_parameter(TypeParameter {
                    name: "T".to_string(),
                    span: None,
                })
                .unwrap();

            x.set_input(
                pernixc_term::generic_parameters::Key(trait_id),
                Arc::new(trait_generic_param),
            );

            let mut impl_generic_param = GenericParameters::default();
            let impl_a = impl_generic_param
                .add_lifetime_parameter(
                    pernixc_term::generic_parameters::LifetimeParameter {
                        name: "a".to_string(),
                        span: None,
                    },
                )
                .unwrap();
            let impl_t = impl_generic_param
                .add_type_parameter(TypeParameter {
                    name: "T".to_string(),
                    span: None,
                })
                .unwrap();

            x.set_input(
                pernixc_term::generic_parameters::Key(implements_id),
                Arc::new(impl_generic_param),
            );

            x.set_input(
                pernixc_term::implements_argument::Key(implements_id),
                Some(Arc::new(GenericArguments {
                    lifetimes: vec![],
                    types: vec![Type::Reference(Reference {
                        qualifier: Qualifier::Immutable,
                        lifetime: Lifetime::Parameter(
                            LifetimeParameterID::new(implements_id, impl_a),
                        ),
                        pointee: Box::new(Type::Parameter(
                            TypeParameterID::new(trait_id, impl_t),
                        )),
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
        });

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
        Global::new(TargetID::Extern(1), pernixc_symbol::ID(0));
    let general_impl_id =
        Global::new(TargetID::Extern(1), pernixc_symbol::ID(1));
    let trait_id = Global::new(TargetID::Extern(1), pernixc_symbol::ID(2));

    let mut engine = Arc::new(Engine::default());

    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::new(order::ImplementsOrderExecutor));

    let expected = Arc::get_mut(&mut engine).unwrap().input_session(|x| {
        x.set_input(
            pernixc_symbol::kind::Key(specialized_impl_id),
            Kind::PositiveImplementation,
        );
        x.set_input(
            pernixc_term::implements::Key(specialized_impl_id),
            Some(trait_id),
        );
        x.set_input(
            pernixc_symbol::final_implements::Key(specialized_impl_id),
            false,
        );
        x.set_input(
            pernixc_term::where_clause::Key(specialized_impl_id),
            if let SpecializationCase::SpecializedButFallback(ty) = &case {
                Arc::from([{
                    let impossible_trait_id = Global::new(
                        TargetID::Extern(1),
                        pernixc_symbol::ID(10),
                    );

                    x.set_input(
                        pernixc_symbol::kind::Key(impossible_trait_id),
                        Kind::Trait,
                    );
                    x.set_input(
                        pernixc_term::implemented::Key(impossible_trait_id),
                        Arc::default(),
                    );

                    let mut trait_generic_param = GenericParameters::default();
                    trait_generic_param
                        .add_type_parameter(TypeParameter {
                            name: "T".to_string(),
                            span: None,
                        })
                        .unwrap();

                    x.set_input(
                        pernixc_term::generic_parameters::Key(
                            impossible_trait_id,
                        ),
                        Arc::new(trait_generic_param),
                    );

                    // This predicate requirement will never be satisfiable
                    Predicate {
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
                    }
                }])
            } else {
                Arc::default()
            },
        );

        let mut impl_generic_param = GenericParameters::default();
        let impl_specialized_t = impl_generic_param
            .add_type_parameter(TypeParameter {
                name: "T".to_string(),
                span: None,
            })
            .unwrap();

        x.set_input(
            pernixc_term::generic_parameters::Key(specialized_impl_id),
            Arc::new(impl_generic_param),
        );

        x.set_input(
            pernixc_term::implements_argument::Key(specialized_impl_id),
            Some(Arc::new(GenericArguments {
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

        x.set_input(
            pernixc_symbol::kind::Key(general_impl_id),
            Kind::PositiveImplementation,
        );
        x.set_input(
            pernixc_term::implements::Key(general_impl_id),
            Some(trait_id),
        );
        x.set_input(
            pernixc_symbol::final_implements::Key(general_impl_id),
            false,
        );
        x.set_input(
            pernixc_term::where_clause::Key(general_impl_id),
            Arc::default(),
        );

        let mut impl_generic_param = GenericParameters::default();
        let impl_general_t = impl_generic_param
            .add_type_parameter(TypeParameter {
                name: "T".to_string(),
                span: None,
            })
            .unwrap();
        let impl_general_u = impl_generic_param
            .add_type_parameter(TypeParameter {
                name: "U".to_string(),
                span: None,
            })
            .unwrap();

        x.set_input(
            pernixc_term::generic_parameters::Key(general_impl_id),
            Arc::new(impl_generic_param),
        );

        x.set_input(
            pernixc_term::implements_argument::Key(general_impl_id),
            Some(Arc::new(GenericArguments {
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

        x.set_input(pernixc_symbol::kind::Key(trait_id), Kind::Trait);
        x.set_input(
            pernixc_term::implemented::Key(trait_id),
            Arc::new(
                [specialized_impl_id, general_impl_id].into_iter().collect(),
            ),
        );

        let mut trait_generic_param = GenericParameters::default();
        trait_generic_param
            .add_type_parameter(TypeParameter {
                name: "T".to_string(),
                span: None,
            })
            .unwrap();

        trait_generic_param
            .add_type_parameter(TypeParameter {
                name: "U".to_string(),
                span: None,
            })
            .unwrap();

        x.set_input(
            pernixc_term::generic_parameters::Key(trait_id),
            Arc::new(trait_generic_param),
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
    });

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
