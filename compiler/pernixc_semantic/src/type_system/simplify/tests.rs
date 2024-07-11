use crate::{
    arena::ID,
    symbol::{
        table::{Building, Table},
        GenericID, LifetimeParameterID,
    },
    type_system::{
        equality::Equality,
        model::Default,
        normalizer::NoOp,
        predicate::Predicate,
        simplify::simplify,
        term::{
            lifetime::Lifetime,
            r#type::{Primitive, SymbolID, TraitMember, Type},
            GenericArguments, Symbol,
        },
        Environment, LifetimeConstraint, Premise, Succeeded,
    },
    unordered_pair::UnorderedPair,
};

#[test]
fn basic_case() {
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
            normalizer: &NoOp,
        });

    assert_eq!(simplified, equivalent);
    assert!(constraints.is_empty());
}

#[test]
fn sub_term_case() {
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
            id: SymbolID::Struct(ID::new(0)),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Symbol(Symbol {
                    id: SymbolID::Struct(ID::new(0)),
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::TraitMember(trait_member)],
                        constants: Vec::new(),
                    },
                })],
                constants: Vec::new(),
            },
        }),
        &Environment { premise, table: &table, normalizer: &NoOp },
    );

    assert_eq!(
        simplified,
        Type::Symbol(Symbol {
            id: SymbolID::Struct(ID::new(0)),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Symbol(Symbol {
                    id: SymbolID::Struct(ID::new(0)),
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
fn already_simplified_case() {
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
            normalizer: &NoOp,
        });

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
            normalizer: &NoOp,
        });

    assert_eq!(simplified, equivalent);
    assert_eq!(constraints.len(), 1);
    assert!(constraints.contains(&LifetimeConstraint::LifetimeMatching(
        UnorderedPair::new(first_lifetime, second_lifetime)
    )));
}

/*

#[test]
fn transitive_case() {
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

    let Succeeded { result: result1, constraints: constraints1 } =
        simplify(&Type::TraitMember(first_trait_member), &Environment {
            premise: &premise,
            table: &table,
            normalizer: &NoOp,
        });

    let Succeeded { result: result2, constraints: constraints2 } =
        simplify(&Type::TraitMember(second_trait_member), &Environment {
            premise: &premise,
            table: &table,
            normalizer: &NoOp,
        });

    assert_eq!(result1, result2);

    assert!(constraints1.is_empty());
    assert!(constraints2.is_empty());

    assert_eq!(result1, equivalent);
    assert_eq!(result2, equivalent);
}


#[test]
fn multiple_equivalent_but_same_case() {
    let starting_trait_member = TraitMember::<Default> {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let first_trait_member = TraitMember {
        id: ID::new(1),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let second_trait_member = TraitMember {
        id: ID::new(2),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let equivalent = Type::Primitive(Primitive::Bool);

    let table = Table::<Building>::default();
    let premise = Premise::from_predicates(
        [
            Predicate::TraitTypeEquality(Equality {
                lhs: starting_trait_member.clone(),
                rhs: Type::TraitMember(first_trait_member.clone()),
            }),
            Predicate::TraitTypeEquality(Equality {
                lhs: starting_trait_member.clone(),
                rhs: Type::TraitMember(second_trait_member.clone()),
            }),
            Predicate::TraitTypeEquality(Equality {
                lhs: first_trait_member.clone(),
                rhs: equivalent.clone(),
            }),
            Predicate::TraitTypeEquality(Equality {
                lhs: second_trait_member.clone(),
                rhs: equivalent.clone(),
            }),
        ]
        .into_iter(),
    );

    let result_from_starting =
        simplify(&Type::TraitMember(starting_trait_member), &Environment {
            premise: &premise,
            table: &table,
            normalizer: &NoOp,
        })
        .unwrap();

    let result_from_first =
        simplify(&Type::TraitMember(first_trait_member), &Environment {
            premise: &premise,
            table: &table,
            normalizer: &NoOp,
        })
        .unwrap();

    let result_from_second =
        simplify(&Type::TraitMember(second_trait_member), &Environment {
            premise: &premise,
            table: &table,
            normalizer: &NoOp,
        })
        .unwrap();

    assert_eq!(result_from_starting, result_from_first);
    assert_eq!(result_from_starting, result_from_second);
    assert_eq!(result_from_starting, equivalent);
}

#[test]
fn ambiguous_case() {
    let starting_trait_member = TraitMember::<Default> {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let first_trait_member = TraitMember {
        id: ID::new(1),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let second_trait_member = TraitMember {
        id: ID::new(2),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let first_equivalent = Type::Primitive(Primitive::Float32);
    let second_equivalent = Type::Primitive(Primitive::Float64);

    let table = Table::<Building>::default();
    let premise = Premise::from_predicates(
        [
            Predicate::TraitTypeEquality(Equality {
                lhs: starting_trait_member.clone(),
                rhs: Type::TraitMember(first_trait_member.clone()),
            }),
            Predicate::TraitTypeEquality(Equality {
                lhs: starting_trait_member.clone(),
                rhs: Type::TraitMember(second_trait_member.clone()),
            }),
            Predicate::TraitTypeEquality(Equality {
                lhs: first_trait_member.clone(),
                rhs: first_equivalent,
            }),
            Predicate::TraitTypeEquality(Equality {
                lhs: second_trait_member.clone(),
                rhs: second_equivalent,
            }),
        ]
        .into_iter(),
    );

    let result_from_starting = simplify(
        &Type::TraitMember(starting_trait_member.clone()),
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
    )
    .unwrap();

    let result_from_first = simplify(
        &Type::TraitMember(first_trait_member.clone()),
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
    )
    .unwrap();

    let result_from_second = simplify(
        &Type::TraitMember(second_trait_member.clone()),
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
    )
    .unwrap();

    assert_eq!(result_from_first, Type::TraitMember(first_trait_member));
    assert_eq!(result_from_second, Type::TraitMember(second_trait_member));

    assert_eq!(result_from_starting, Type::TraitMember(starting_trait_member));
}
*/
