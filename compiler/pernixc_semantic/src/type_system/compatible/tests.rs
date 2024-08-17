use proptest::{arbitrary::Arbitrary, proptest};

use crate::{
    arena::ID,
    symbol::{
        table::{
            representation::{IndexMut, Insertion},
            Building, Table,
        },
        Accessibility, AdtTemplate, GenericDeclaration, GenericID,
        LifetimeParameter, LifetimeParameterID, StructDefinition,
    },
    type_system::{
        compatible::Compatible,
        model::Default,
        normalizer::NoOp,
        predicate::Outlives,
        term::{
            lifetime::Lifetime,
            r#type::{Primitive, Qualifier, Reference, Type},
            GenericArguments, Symbol,
        },
        variance::Variance,
        Environment, LifetimeConstraint, Premise,
    },
    unordered_pair::UnorderedPair,
};

proptest! {
    #[test]
    fn strict_equality_compatible(
        term in Type::<Default>::arbitrary()
    ) {
        let environment = Environment {
            premise: Premise::default(),
            table: &Table::<Building>::default(),
            normalizer: &NoOp
        };

         assert!(term
            .compatible(&term, Variance::Covariant, &environment)
            .unwrap()
            .unwrap()
            .constraints
            .is_empty());

         assert!(term
            .compatible(&term, Variance::Contravariant, &environment)
            .unwrap()
            .unwrap()
            .constraints
            .is_empty());

         assert!(term
            .compatible(&term, Variance::Invariant, &environment)
            .unwrap()
            .unwrap()
            .constraints
            .is_empty());
    }
}

#[test]
#[allow(clippy::similar_names)]
fn basic_compatible() {
    // Variance::Covariant: &'a bool :> &'static bool then 'a: 'static
    let a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(0),
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

    let environment = Environment {
        premise: Premise::default(),
        table: &Table::<Building>::default(),
        normalizer: &NoOp,
    };

    let check = |variance: Variance| {
        let result =
            a_t.compatible(&static_t, variance, &environment).unwrap().unwrap();

        assert_eq!(result.constraints.len(), 1);

        let expected_constraint = match variance {
            Variance::Covariant => {
                LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: a_lt.clone(),
                    bound: static_lt.clone(),
                })
            }
            Variance::Contravariant => {
                LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: static_lt.clone(),
                    bound: a_lt.clone(),
                })
            }
            Variance::Invariant => LifetimeConstraint::LifetimeMatching(
                UnorderedPair::new(static_lt.clone(), a_lt.clone()),
            ),
        };

        assert!(result.constraints.contains(&expected_constraint));
    };

    check(Variance::Covariant);
    check(Variance::Contravariant);
    check(Variance::Invariant);
}

#[test]
#[allow(clippy::similar_names)]
fn compatible_with_adt() {
    let mut table = Table::<Building>::default();

    let Insertion { id: root_module_id, duplication } =
        table.create_root_module("test".to_string());

    assert!(duplication.is_none());

    let Insertion { id: adt_sym_id, duplication } = table
        .insert_member(
            "Adt".to_string(),
            Accessibility::Public,
            root_module_id,
            None,
            GenericDeclaration::default(),
            AdtTemplate::<StructDefinition>::default(),
        )
        .unwrap();

    assert!(duplication.is_none());

    let lifetime_parameter_id = {
        let adt_sym = table.get_mut(adt_sym_id).unwrap();

        adt_sym
            .generic_declaration
            .parameters
            .add_lifetime_parameter(LifetimeParameter {
                name: None,
                span: None,
            })
            .unwrap()
    };

    let a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(0),
    });

    let b_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(1),
    });

    let mut check = |variance: Variance| {
        // update the variance of lifetime
        {
            let adt_sym = table.get_mut(adt_sym_id).unwrap();

            *adt_sym
                .generic_parameter_variances
                .variances_by_lifetime_ids
                .entry(lifetime_parameter_id)
                .or_default() = variance;
        }

        // Adt['a]
        let a_t = Type::<Default>::Symbol(Symbol {
            id: adt_sym_id.into(),
            generic_arguments: GenericArguments {
                lifetimes: vec![a_lt.clone()],
                types: Vec::new(),
                constants: Vec::new(),
            },
        });

        // Adt['b]
        let b_t = Type::<Default>::Symbol(Symbol {
            id: adt_sym_id.into(),
            generic_arguments: GenericArguments {
                lifetimes: vec![b_lt.clone()],
                types: Vec::new(),
                constants: Vec::new(),
            },
        });

        let result = a_t
            .compatible(&b_t, Variance::Covariant, &Environment {
                premise: Premise::default(),
                table: &table,
                normalizer: &NoOp,
            })
            .unwrap()
            .unwrap();

        assert_eq!(result.constraints.len(), 1);

        let expected_constraint = match variance {
            Variance::Covariant => {
                LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: a_lt.clone(),
                    bound: b_lt.clone(),
                })
            }
            Variance::Contravariant => {
                LifetimeConstraint::LifetimeOutlives(Outlives {
                    operand: b_lt.clone(),
                    bound: a_lt.clone(),
                })
            }
            Variance::Invariant => LifetimeConstraint::LifetimeMatching(
                UnorderedPair::new(a_lt.clone(), b_lt.clone()),
            ),
        };

        assert!(result.constraints.contains(&expected_constraint));
    };

    check(Variance::Covariant);
    check(Variance::Contravariant);
    check(Variance::Invariant);
}

#[test]
fn compatible_with_mutable_reference() {
    // &'a mutable &'b bool == &'c mutable &'d bool

    let a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(0),
    });
    let b_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(1),
    });
    let c_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(2),
    });
    let d_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(3),
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

    let environment = Environment {
        premise: Premise::default(),
        table: &Table::<Building>::default(),
        normalizer: &NoOp,
    };

    let result = lhs
        .compatible(&rhs, Variance::Covariant, &environment)
        .unwrap()
        .unwrap();

    assert_eq!(result.constraints.len(), 2);

    let a_and_c = LifetimeConstraint::LifetimeOutlives(Outlives {
        operand: a_lt.clone(),
        bound: c_lt.clone(),
    });
    let b_and_d = LifetimeConstraint::LifetimeMatching(UnorderedPair::new(
        b_lt.clone(),
        d_lt.clone(),
    ));

    assert!(result.constraints.contains(&a_and_c));
    assert!(result.constraints.contains(&b_and_d));
}
