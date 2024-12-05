use crate::{
    arena::ID,
    symbol::{
        table::{Building, Table},
        AdtID, GenericID, LifetimeParameterID,
    },
    type_system::{
        equality::Equality,
        model::Default,
        normalizer, observer,
        predicate::{Outlives, Predicate},
        simplify::simplify,
        term::{
            lifetime::Lifetime,
            r#type::{self, Primitive, TraitMember, Type},
            GenericArguments, Symbol,
        },
        Environment, LifetimeConstraint, Premise, Succeeded,
    },
};

#[test]
fn basic() {
    let trait_member = TraitMember::<Default> {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };

    let equivalent = Type::Primitive(Primitive::Bool);
    let table = Table::<Building>::default();

    let mut premise = Premise::default();

    premise.predicates.insert(Predicate::TraitTypeEquality(Equality {
        lhs: trait_member.clone(),
        rhs: equivalent.clone(),
    }));

    let Succeeded { result: simplified, constraints } =
        simplify(&Type::TraitMember(trait_member), &Environment {
            premise,
            table: &table,
            normalizer: normalizer::NO_OP,
            observer: observer::NO_OP,
        })
        .unwrap();

    assert_eq!(simplified, equivalent);
    assert!(constraints.is_empty());
}

#[test]
fn sub_term() {
    let trait_member = TraitMember::<Default> {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };

    let equivalent = Type::Primitive(Primitive::Bool);
    let table = Table::<Building>::default();

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::TraitTypeEquality(Equality {
        lhs: trait_member.clone(),
        rhs: equivalent.clone(),
    }));

    let Succeeded { result: simplified, constraints } = simplify(
        &Type::Symbol(Symbol {
            id: r#type::SymbolID::Adt(AdtID::Struct(ID::new(0))),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Symbol(Symbol {
                    id: r#type::SymbolID::Adt(AdtID::Struct(ID::new(0))),
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::TraitMember(trait_member)],
                        constants: Vec::new(),
                    },
                })],
                constants: Vec::new(),
            },
        }),
        &Environment {
            premise,
            table: &table,
            normalizer: normalizer::NO_OP,
            observer: observer::NO_OP,
        },
    )
    .unwrap();

    assert_eq!(
        simplified,
        Type::Symbol(Symbol {
            id: r#type::SymbolID::Adt(AdtID::Struct(ID::new(0))),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Symbol(Symbol {
                    id: r#type::SymbolID::Adt(AdtID::Struct(ID::new(0))),
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![equivalent],
                        constants: Vec::new(),
                    },
                })],
                constants: Vec::new(),
            },
        })
    );

    assert!(constraints.is_empty());
}

#[test]
fn already_simplified() {
    let trait_member = TraitMember::<Default> {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };

    let equivalent = Type::Primitive(Primitive::Bool);
    let table = Table::<Building>::default();

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::TraitTypeEquality(Equality {
        lhs: trait_member,
        rhs: equivalent.clone(),
    }));

    let Succeeded { result: simplified, constraints } =
        simplify(&equivalent, &Environment {
            premise,
            table: &table,
            normalizer: normalizer::NO_OP,
            observer: observer::NO_OP,
        })
        .unwrap();

    assert_eq!(simplified, equivalent);
    assert!(constraints.is_empty());
}

#[test]
fn with_lifetime_matching() {
    let first_lifetime = Lifetime::Parameter(LifetimeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(0),
    });
    let second_lifetime = Lifetime::Parameter(LifetimeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(1),
    });

    let to_be_simplified = TraitMember::<Default> {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments {
            lifetimes: vec![first_lifetime.clone()],
            types: Vec::new(),
            constants: Vec::new(),
        },
    };

    let trait_member = TraitMember::<Default> {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments {
            lifetimes: vec![second_lifetime.clone()],
            types: Vec::new(),
            constants: Vec::new(),
        },
    };

    let equivalent = Type::Primitive(Primitive::Bool);
    let table = Table::<Building>::default();

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::TraitTypeEquality(Equality {
        lhs: trait_member.clone(),
        rhs: equivalent.clone(),
    }));

    let Succeeded { result: simplified, constraints } =
        simplify(&Type::TraitMember(to_be_simplified), &Environment {
            premise,
            table: &table,
            normalizer: normalizer::NO_OP,
            observer: observer::NO_OP,
        })
        .unwrap();

    assert_eq!(simplified, equivalent);
    assert_eq!(constraints.len(), 2);

    assert!(constraints.contains(&LifetimeConstraint::LifetimeOutlives(
        Outlives::new(second_lifetime.clone(), first_lifetime.clone())
    )));
    assert!(constraints.contains(&LifetimeConstraint::LifetimeOutlives(
        Outlives::new(first_lifetime, second_lifetime)
    )));
}

#[test]
fn multiple_equivalences() {
    let first_trait_member = TraitMember::<Default> {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let second_trait_member = TraitMember {
        id: ID::new(1),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let equivalent = Type::Primitive(Primitive::Bool);

    let table = Table::<Building>::default();
    let mut premise = Premise::default();
    premise.predicates.extend([
        Predicate::TraitTypeEquality(Equality {
            lhs: first_trait_member.clone(),
            rhs: equivalent.clone(),
        }),
        Predicate::TraitTypeEquality(Equality {
            lhs: second_trait_member.clone(),
            rhs: equivalent.clone(),
        }),
    ]);
    let environment = Environment {
        premise,
        table: &table,
        normalizer: normalizer::NO_OP,
        observer: observer::NO_OP,
    };

    let Succeeded { result: result1, constraints: constraints1 } =
        simplify(&Type::TraitMember(first_trait_member), &environment).unwrap();

    let Succeeded { result: result2, constraints: constraints2 } =
        simplify(&Type::TraitMember(second_trait_member), &environment)
            .unwrap();

    assert_eq!(result1, result2);

    assert!(constraints1.is_empty());
    assert!(constraints2.is_empty());

    assert_eq!(result1, equivalent);
    assert_eq!(result2, equivalent);
}

#[test]
fn transitive() {
    // TraitA['a] = TraitB['b]
    // TraitB['c] = bool
    //
    // final result is TraitA['a] = bool
    // lifetime constraint is 'b = 'c

    let trait_member = |idx, lifetime| r#type::TraitMember::<Default> {
        id: ID::new(idx),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments {
            lifetimes: vec![lifetime],
            types: Vec::new(),
            constants: Vec::new(),
        },
    };

    let lt = |idx| {
        Lifetime::<Default>::Parameter(LifetimeParameterID {
            parent: GenericID::Struct(ID::new(0)),
            id: ID::new(idx),
        })
    };

    let a_lt = lt(0);
    let b_lt = lt(1);
    let c_lt = lt(2);

    let trait_a = trait_member(0, a_lt);
    let trait_b_b = trait_member(1, b_lt.clone());
    let trait_b_c = trait_member(1, c_lt.clone());
    let equivalent = Type::Primitive(Primitive::Bool);

    let table = Table::<Building>::default();

    let premise = Premise {
        predicates: [
            Predicate::TraitTypeEquality(Equality {
                lhs: trait_a.clone(),
                rhs: Type::TraitMember(trait_b_b),
            }),
            Predicate::TraitTypeEquality(Equality {
                lhs: trait_b_c,
                rhs: equivalent.clone(),
            }),
        ]
        .into_iter()
        .collect(),
        query_site: None,
    };

    let environment = Environment {
        premise,
        table: &table,
        normalizer: normalizer::NO_OP,
        observer: observer::NO_OP,
    };

    let Succeeded { result: simplified, constraints } =
        simplify(&Type::TraitMember(trait_a), &environment).unwrap();

    assert_eq!(simplified, equivalent);
    assert_eq!(constraints.len(), 2);

    assert!(constraints.contains(&LifetimeConstraint::LifetimeOutlives(
        Outlives::new(b_lt.clone(), c_lt.clone())
    )));
    assert!(constraints.contains(&LifetimeConstraint::LifetimeOutlives(
        Outlives::new(c_lt, b_lt)
    )));
}
