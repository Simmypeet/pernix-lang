use std::{borrow::Cow, sync::Arc};

use pernixc_semantic::{
    component::{
        derived::{
            generic_parameters::{
                GenericParameters, LifetimeParameter, LifetimeParameterID,
            },
            variances::{Variance, Variances},
        },
        input::SymbolKind,
    },
    table::{self, GlobalID, Table, TargetID},
    term::{
        generic_arguments::GenericArguments,
        lifetime::Lifetime,
        predicate::Outlives,
        r#type::{Primitive, Qualifier, Reference, Type},
        Default, Symbol,
    },
};
use proptest::{arbitrary::Arbitrary, prop_assert, proptest};

use crate::{
    environment::{Environment, Premise},
    normalizer, LifetimeConstraint,
};

proptest! {
    #[test]
    fn strict_equality_compatible(
        term in Type::<Default>::arbitrary()
    ) {
        let table = Table::new(Arc::new(pernixc_handler::Panic));
        let premise = Premise::default();

        let environment = Environment::new(
            Cow::Borrowed(&premise),
            &table,
            normalizer::NO_OP
        );

        prop_assert!(
            environment
                .compatible(&term, &term, Variance::Covariant)
                .unwrap()
                .unwrap()
                .constraints
                .is_empty()
        );

        prop_assert!(
            environment
                .compatible(&term, &term, Variance::Invariant)
                .unwrap()
                .unwrap()
                .constraints
                .is_empty()
        );

        prop_assert!(
            environment
                .compatible(&term, &term, Variance::Contravariant)
                .unwrap()
                .unwrap()
                .constraints
                .is_empty()
        );

    }
}

#[test]
#[allow(clippy::similar_names)]
fn basic_compatible() {
    // Variance::Covariant: &'a bool :> &'static bool then 'a: 'static
    let a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: GlobalID::new(TargetID(1), table::ID(1)),
        id: pernixc_arena::ID::new(0),
    });
    let static_lt = Lifetime::Static;

    let a_t = Type::<Default>::Reference(Reference {
        qualifier: Qualifier::Immutable,
        lifetime: a_lt,
        pointee: Box::new(Type::Primitive(Primitive::Bool)),
    });

    let static_t = Type::<Default>::Reference(Reference {
        qualifier: Qualifier::Immutable,
        lifetime: static_lt,
        pointee: Box::new(Type::Primitive(Primitive::Bool)),
    });

    let table = Table::new(Arc::new(pernixc_handler::Panic));
    let premise = Premise::default();

    let environment =
        Environment::new(Cow::Borrowed(&premise), &table, normalizer::NO_OP);

    let check = |variance: Variance| {
        let result =
            environment.compatible(&a_t, &static_t, variance).unwrap().unwrap();

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

    check(Variance::Covariant);
    check(Variance::Contravariant);
    check(Variance::Invariant);
    check(Variance::Bivariant);
}

#[test]
#[allow(clippy::similar_names)]
fn compatible_with_adt() {
    let global_id = GlobalID::new(TargetID(1), table::ID(1));
    let adt_id = GlobalID::new(TargetID(1), table::ID(2));

    let a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: global_id,
        id: pernixc_arena::ID::new(0),
    });

    let b_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: global_id,
        id: pernixc_arena::ID::new(1),
    });

    let check = |variance: Variance| {
        let table = Table::new(Arc::new(pernixc_handler::Panic));

        let mut generic_parameter = GenericParameters::default();
        let lifetime_id = generic_parameter
            .add_lifetime_parameter(LifetimeParameter {
                name: "a".to_string(),
                span: None,
            })
            .unwrap();

        assert!(table.add_component(adt_id, generic_parameter));

        let mut variance_map = Variances::default();
        variance_map.variances_by_lifetime_ids.insert(lifetime_id, variance);

        assert!(table.add_component(adt_id, variance_map));
        assert!(table.add_component(adt_id, SymbolKind::Enum));

        // Adt['a]
        let a_t = Type::<Default>::Symbol(Symbol {
            id: adt_id,
            generic_arguments: GenericArguments {
                lifetimes: vec![a_lt],
                types: Vec::new(),
                constants: Vec::new(),
            },
        });

        // Adt['b]
        let b_t = Type::<Default>::Symbol(Symbol {
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
            &table,
            normalizer::NO_OP,
        );

        let result = environment
            .compatible(&a_t, &b_t, Variance::Covariant)
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

    check(Variance::Covariant);
    check(Variance::Contravariant);
    check(Variance::Invariant);
    check(Variance::Bivariant);
}

#[test]
fn compatible_with_mutable_reference() {
    // &'a mutable &'b bool == &'c mutable &'d bool

    let global_id = GlobalID::new(TargetID(1), table::ID(1));
    let a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: global_id,
        id: pernixc_arena::ID::new(0),
    });
    let b_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: global_id,
        id: pernixc_arena::ID::new(1),
    });
    let c_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: global_id,
        id: pernixc_arena::ID::new(2),
    });
    let d_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: global_id,
        id: pernixc_arena::ID::new(3),
    });

    let lhs = Type::<Default>::Reference(Reference {
        qualifier: Qualifier::Mutable,
        lifetime: a_lt,
        pointee: Box::new(Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: b_lt,
            pointee: Box::new(Type::Primitive(Primitive::Bool)),
        })),
    });
    let rhs = Type::<Default>::Reference(Reference {
        qualifier: Qualifier::Mutable,
        lifetime: c_lt,
        pointee: Box::new(Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: d_lt,
            pointee: Box::new(Type::Primitive(Primitive::Bool)),
        })),
    });

    let premise = Premise::default();
    let table = Table::new(Arc::new(pernixc_handler::Panic));

    let environment =
        Environment::new(Cow::Borrowed(&premise), &table, normalizer::NO_OP);

    let result = environment
        .compatible(&lhs, &rhs, Variance::Covariant)
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
