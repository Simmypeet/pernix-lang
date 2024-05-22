use crate::{
    arena::ID,
    semantic::{
        model::Default,
        normalizer::NoOp,
        predicate::{Equality, Predicate},
        session::{self, Limit},
        simplify::simplify,
        term::{
            r#type::{Primitive, SymbolID, TraitMember, Type},
            GenericArguments, Symbol,
        },
        Environment, Premise,
    },
    symbol::table::{Building, Table},
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

    let premise = Premise::from_predicates(std::iter::once(
        Predicate::TraitTypeEquality(Equality {
            lhs: trait_member.clone(),
            rhs: equivalent.clone(),
        }),
    ));

    let simplified = simplify(
        &Type::TraitMember(trait_member),
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    assert_eq!(simplified, equivalent);
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

    let premise = Premise::from_predicates(std::iter::once(
        Predicate::TraitTypeEquality(Equality {
            lhs: trait_member.clone(),
            rhs: equivalent.clone(),
        }),
    ));

    let simplified = simplify(
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
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

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

    let premise = Premise::from_predicates(std::iter::once(
        Predicate::TraitTypeEquality(Equality {
            lhs: trait_member,
            rhs: equivalent.clone(),
        }),
    ));

    let simplified = simplify(
        &equivalent,
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    assert_eq!(simplified, equivalent);
}

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
    let premise = Premise::from_predicates(
        [
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

    let result1 = simplify(
        &Type::TraitMember(first_trait_member),
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    let result2 = simplify(
        &Type::TraitMember(second_trait_member),
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    assert_eq!(result1, result2);
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

    let result_from_starting = simplify(
        &Type::TraitMember(starting_trait_member),
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    let result_from_first = simplify(
        &Type::TraitMember(first_trait_member),
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    let result_from_second = simplify(
        &Type::TraitMember(second_trait_member),
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
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
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    let result_from_first = simplify(
        &Type::TraitMember(first_trait_member.clone()),
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    let result_from_second = simplify(
        &Type::TraitMember(second_trait_member.clone()),
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    assert_eq!(result_from_first, Type::TraitMember(first_trait_member));
    assert_eq!(result_from_second, Type::TraitMember(second_trait_member));

    assert_eq!(result_from_starting, Type::TraitMember(starting_trait_member));
}
