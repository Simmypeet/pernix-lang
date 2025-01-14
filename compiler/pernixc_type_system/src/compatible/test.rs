use std::sync::Arc;

use pernixc_table::{component::SymbolKind, GlobalID, Table, TargetID};
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameter::{
        GenericParameters, LifetimeParameter, LifetimeParameterID,
    },
    lifetime::Lifetime,
    predicate::Outlives,
    r#type::{Primitive, Qualifier, Reference, Type},
    variance::{self, Variance},
    Default, Symbol,
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

        let environment = Environment::new( &premise, &table, normalizer::NO_OP);

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
        parent: GlobalID::new(TargetID(1), pernixc_table::ID(1)),
        id: pernixc_arena::ID::new(0),
    });
    let static_lt = Lifetime::Static;

    let a_t = Type::<Default>::Reference(Reference {
        qualifier: Qualifier::Immutable,
        lifetime: a_lt.clone(),
        pointee: Box::new(Type::Primitive(Primitive::Bool)),
    });

    let static_t = Type::<Default>::Reference(Reference {
        qualifier: Qualifier::Immutable,
        lifetime: static_lt.clone(),
        pointee: Box::new(Type::Primitive(Primitive::Bool)),
    });

    let table = Table::new(Arc::new(pernixc_handler::Panic));
    let premise = Premise::default();

    let environment = Environment::new(&premise, &table, normalizer::NO_OP);

    let check = |variance: Variance| {
        let result =
            environment.compatible(&a_t, &static_t, variance).unwrap().unwrap();

        assert_eq!(
            result.constraints.len(),
            if Variance::Invariant == variance { 2 } else { 1 }
        );

        let expected_constraints = match variance {
            Variance::Covariant => {
                vec![LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: a_lt.clone(),
                    bound: static_lt.clone(),
                })]
            }
            Variance::Contravariant => {
                vec![LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: static_lt.clone(),
                    bound: a_lt.clone(),
                })]
            }
            Variance::Invariant => {
                vec![
                    LifetimeConstraint::LifetimeOutlives(Outlives::new(
                        static_lt.clone(),
                        a_lt.clone(),
                    )),
                    LifetimeConstraint::LifetimeOutlives(Outlives::new(
                        a_lt.clone(),
                        static_lt.clone(),
                    )),
                ]
            }
        };

        assert!(expected_constraints
            .iter()
            .all(|x| result.constraints.contains(x)));
    };

    check(Variance::Covariant);
    check(Variance::Contravariant);
    check(Variance::Invariant);
}

#[test]
#[allow(clippy::similar_names)]
fn compatible_with_adt() {
    let global_id = GlobalID::new(TargetID(1), pernixc_table::ID(1));
    let adt_id = GlobalID::new(TargetID(1), pernixc_table::ID(2));

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
                name: None,
                span: None,
            })
            .unwrap();

        assert!(table.add_component(adt_id, generic_parameter));

        let mut variance_map = variance::Map::default();
        variance_map.variances_by_lifetime_ids.insert(lifetime_id, variance);

        assert!(table.add_component(adt_id, variance_map));
        assert!(table.add_component(adt_id, SymbolKind::Enum));

        // Adt['a]
        let a_t = Type::<Default>::Symbol(Symbol {
            id: adt_id,
            generic_arguments: GenericArguments {
                lifetimes: vec![a_lt.clone()],
                types: Vec::new(),
                constants: Vec::new(),
            },
        });

        // Adt['b]
        let b_t = Type::<Default>::Symbol(Symbol {
            id: adt_id,
            generic_arguments: GenericArguments {
                lifetimes: vec![b_lt.clone()],
                types: Vec::new(),
                constants: Vec::new(),
            },
        });

        let premise = Premise::default();
        let environment = Environment::new(&premise, &table, normalizer::NO_OP);

        let result = environment
            .compatible(&a_t, &b_t, Variance::Covariant)
            .unwrap()
            .unwrap();

        assert_eq!(
            result.constraints.len(),
            if Variance::Invariant == variance { 2 } else { 1 }
        );

        let expected_constraint = match variance {
            Variance::Covariant => {
                vec![LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: a_lt.clone(),
                    bound: b_lt.clone(),
                })]
            }
            Variance::Contravariant => {
                vec![LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: b_lt.clone(),
                    bound: a_lt.clone(),
                })]
            }
            Variance::Invariant => vec![
                LifetimeConstraint::LifetimeOutlives(Outlives::new(
                    a_lt.clone(),
                    b_lt.clone(),
                )),
                LifetimeConstraint::LifetimeOutlives(Outlives::new(
                    b_lt.clone(),
                    a_lt.clone(),
                )),
            ],
        };

        assert!(expected_constraint
            .iter()
            .all(|x| result.constraints.contains(x)));
    };

    check(Variance::Covariant);
    check(Variance::Contravariant);
    check(Variance::Invariant);
}

#[test]
fn compatible_with_mutable_reference() {
    // &'a mutable &'b bool == &'c mutable &'d bool

    let global_id = GlobalID::new(TargetID(1), pernixc_table::ID(1));
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
        lifetime: a_lt.clone(),
        pointee: Box::new(Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: b_lt.clone(),
            pointee: Box::new(Type::Primitive(Primitive::Bool)),
        })),
    });
    let rhs = Type::<Default>::Reference(Reference {
        qualifier: Qualifier::Mutable,
        lifetime: c_lt.clone(),
        pointee: Box::new(Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: d_lt.clone(),
            pointee: Box::new(Type::Primitive(Primitive::Bool)),
        })),
    });

    let premise = Premise::default();
    let table = Table::new(Arc::new(pernixc_handler::Panic));

    let environment = Environment::new(&premise, &table, normalizer::NO_OP);

    let result = environment
        .compatible(&lhs, &rhs, Variance::Covariant)
        .unwrap()
        .unwrap();

    assert_eq!(result.constraints.len(), 3);

    let a_and_c = LifetimeConstraint::LifetimeOutlives(Outlives {
        operand: a_lt,
        bound: c_lt,
    });
    let b_and_d = LifetimeConstraint::LifetimeOutlives(Outlives::new(
        b_lt.clone(),
        d_lt.clone(),
    ));
    let d_and_b =
        LifetimeConstraint::LifetimeOutlives(Outlives::new(d_lt, b_lt));

    assert!(result.constraints.contains(&a_and_c));
    assert!(result.constraints.contains(&b_and_d));
    assert!(result.constraints.contains(&d_and_b));
}
