use std::{borrow::Cow, sync::Arc};

use pernixc_query::Engine;
use pernixc_symbol::kind::Kind;
use pernixc_target::{Global, TargetID};
use pernixc_term::{
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::{
        GenericParameters, LifetimeParameter, LifetimeParameterID,
    },
    lifetime::Lifetime,
    predicate::Outlives,
    r#type::{Primitive, Qualifier, Reference, Type},
    variance::{Variance, Variances},
};

use crate::{
    environment::{Environment, Premise},
    normalizer,
    subtype::Subtype,
    LifetimeConstraint,
};

#[rstest::rstest]
#[case(Variance::Bivariant)]
#[case(Variance::Covariant)]
#[case(Variance::Contravariant)]
#[case(Variance::Invariant)]
fn basic_subtyping(#[case] variance: Variance) {
    let test = async move {
        // Variance::Covariant: &'a bool :> &'static bool then 'a: 'static

        let a_lt = Lifetime::Parameter(LifetimeParameterID {
            parent_id: Global::new(TargetID::Extern(1), pernixc_symbol::ID(1)),
            id: pernixc_arena::ID::new(0),
        });
        let static_lt = Lifetime::Static;

        let a_t = Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: a_lt,
            pointee: Box::new(Type::Primitive(Primitive::Bool)),
        });

        let static_t = Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: static_lt,
            pointee: Box::new(Type::Primitive(Primitive::Bool)),
        });

        let engine = Arc::new(Engine::default());
        let premise = Premise::default();

        let environment = Environment::new(
            Cow::Borrowed(&premise),
            Cow::Owned(engine.tracked()),
            normalizer::NO_OP,
        );

        let result = environment
            .query(&Subtype::new(a_t, static_t, variance))
            .await
            .unwrap()
            .unwrap();

        assert_eq!(result.constraints.len(), match variance {
            Variance::Covariant | Variance::Contravariant => 1,
            Variance::Invariant => 2,
            Variance::Bivariant => 0,
        });

        let expected_constraints = match variance {
            Variance::Covariant => {
                vec![LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: a_lt,
                    bound: static_lt,
                })]
            }
            Variance::Contravariant => {
                vec![LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: static_lt,
                    bound: a_lt,
                })]
            }
            Variance::Invariant => {
                vec![
                    LifetimeConstraint::LifetimeOutlives(Outlives::new(
                        static_lt, a_lt,
                    )),
                    LifetimeConstraint::LifetimeOutlives(Outlives::new(
                        a_lt, static_lt,
                    )),
                ]
            }
            Variance::Bivariant => {
                vec![]
            }
        };

        assert!(expected_constraints
            .iter()
            .all(|x| result.constraints.contains(x)));
    };

    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(test);
}

#[rstest::rstest]
#[case(Variance::Bivariant)]
#[case(Variance::Covariant)]
#[case(Variance::Contravariant)]
#[case(Variance::Invariant)]
fn subtyping_with_adt(#[case] variance: Variance) {
    let test = async move {
        let global_id = Global::new(TargetID::Extern(1), pernixc_symbol::ID(1));
        let adt_id = Global::new(TargetID::Extern(1), pernixc_symbol::ID(2));

        let a_lt = Lifetime::Parameter(LifetimeParameterID {
            parent_id: global_id,
            id: pernixc_arena::ID::new(0),
        });

        let b_lt = Lifetime::Parameter(LifetimeParameterID {
            parent_id: global_id,
            id: pernixc_arena::ID::new(1),
        });

        let mut engine = Arc::new(Engine::default());

        let mut generic_parameter = GenericParameters::default();
        let lifetime_id = generic_parameter
            .add_lifetime_parameter(LifetimeParameter {
                name: "a".to_string(),
                span: None,
            })
            .unwrap();

        let mut variance_map = Variances::default();
        variance_map.variances_by_lifetime_ids.insert(lifetime_id, variance);

        Arc::get_mut(&mut engine).unwrap().input_session(|x| {
            x.set_input(
                pernixc_term::generic_parameters::Key(adt_id),
                generic_parameter,
            );

            x.set_input(pernixc_term::variance::Key(adt_id), variance_map);

            x.set_input(pernixc_symbol::kind::Key(adt_id), Kind::Enum);
        });

        // Adt['a]
        let a_t = Type::Symbol(Symbol {
            id: adt_id,
            generic_arguments: GenericArguments {
                lifetimes: vec![a_lt],
                types: Vec::new(),
                constants: Vec::new(),
            },
        });

        // Adt['b]
        let b_t = Type::Symbol(Symbol {
            id: adt_id,
            generic_arguments: GenericArguments {
                lifetimes: vec![b_lt],
                types: Vec::new(),
                constants: Vec::new(),
            },
        });

        let premise = Premise::default();
        let environment = Environment::new(
            Cow::Borrowed(&premise),
            Cow::Owned(engine.tracked()),
            normalizer::NO_OP,
        );

        let result = environment
            .query(&Subtype::new(a_t, b_t, Variance::Covariant))
            .await
            .unwrap()
            .unwrap();

        assert_eq!(result.constraints.len(), match variance {
            Variance::Covariant | Variance::Contravariant => 1,
            Variance::Invariant => 2,
            Variance::Bivariant => 0,
        });

        let expected_constraint = match variance {
            Variance::Covariant => {
                vec![LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: a_lt,
                    bound: b_lt,
                })]
            }
            Variance::Contravariant => {
                vec![LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: b_lt,
                    bound: a_lt,
                })]
            }
            Variance::Invariant => vec![
                LifetimeConstraint::LifetimeOutlives(Outlives::new(a_lt, b_lt)),
                LifetimeConstraint::LifetimeOutlives(Outlives::new(b_lt, a_lt)),
            ],
            Variance::Bivariant => vec![],
        };

        assert!(expected_constraint
            .iter()
            .all(|x| result.constraints.contains(x)));
    };

    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(test);
}

#[tokio::test]
async fn subtyping_with_mutable_reference() {
    // &'a mutable &'b bool == &'c mutable &'d bool

    let global_id = Global::new(TargetID::Extern(1), pernixc_symbol::ID(1));
    let a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent_id: global_id,
        id: pernixc_arena::ID::new(0),
    });
    let b_lt = Lifetime::Parameter(LifetimeParameterID {
        parent_id: global_id,
        id: pernixc_arena::ID::new(1),
    });
    let c_lt = Lifetime::Parameter(LifetimeParameterID {
        parent_id: global_id,
        id: pernixc_arena::ID::new(2),
    });
    let d_lt = Lifetime::Parameter(LifetimeParameterID {
        parent_id: global_id,
        id: pernixc_arena::ID::new(3),
    });

    let lhs = Type::Reference(Reference {
        qualifier: Qualifier::Mutable,
        lifetime: a_lt,
        pointee: Box::new(Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: b_lt,
            pointee: Box::new(Type::Primitive(Primitive::Bool)),
        })),
    });
    let rhs = Type::Reference(Reference {
        qualifier: Qualifier::Mutable,
        lifetime: c_lt,
        pointee: Box::new(Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: d_lt,
            pointee: Box::new(Type::Primitive(Primitive::Bool)),
        })),
    });

    let engine = Arc::new(Engine::default());
    let premise = Premise::default();

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked()),
        normalizer::NO_OP,
    );

    let result = environment
        .query(&Subtype::new(lhs, rhs, Variance::Covariant))
        .await
        .unwrap()
        .unwrap();

    assert_eq!(result.constraints.len(), 3);

    let a_and_c = LifetimeConstraint::LifetimeOutlives(Outlives {
        operand: a_lt,
        bound: c_lt,
    });
    let b_and_d =
        LifetimeConstraint::LifetimeOutlives(Outlives::new(b_lt, d_lt));
    let d_and_b =
        LifetimeConstraint::LifetimeOutlives(Outlives::new(d_lt, b_lt));

    assert!(result.constraints.contains(&a_and_c));
    assert!(result.constraints.contains(&b_and_d));
    assert!(result.constraints.contains(&d_and_b));
}
