use crate::{
    arena::ID,
    semantic::{
        predicate::{Predicate, TraitMemberEquality},
        session::{self, Limit},
        simplify::simplify,
        term::{
            r#type::{Primitive, SymbolID, TraitMember, Type},
            GenericArguments, Symbol,
        },
        tests::State,
        Environment, Premise,
    },
    table::Table,
};

#[test]
fn basic_case() {
    let trait_member = TraitMember {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };

    let equivalent = Type::Primitive(Primitive::Bool);
    let table = Table::<State>::default();

    let premise = Premise::from_predicates(std::iter::once(
        Predicate::TraitTypeEquality(TraitMemberEquality {
            trait_member: trait_member.clone(),
            equivalent: equivalent.clone(),
        }),
    ));

    let simplified = simplify(
        &Type::TraitMember(trait_member),
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    assert_eq!(simplified, equivalent);
}

#[test]
fn sub_term_case() {
    let trait_member = TraitMember {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };

    let equivalent = Type::Primitive(Primitive::Bool);
    let table = Table::<State>::default();

    let premise = Premise::from_predicates(std::iter::once(
        Predicate::TraitTypeEquality(TraitMemberEquality {
            trait_member: trait_member.clone(),
            equivalent: equivalent.clone(),
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
        &Environment { premise: &premise, table: &table },
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
    let trait_member = TraitMember {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };

    let equivalent = Type::Primitive(Primitive::Bool);
    let table = Table::<State>::default();

    let premise = Premise::from_predicates(std::iter::once(
        Predicate::TraitTypeEquality(TraitMemberEquality {
            trait_member,
            equivalent: equivalent.clone(),
        }),
    ));

    let simplified = simplify(
        &equivalent,
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    assert_eq!(simplified, equivalent);
}

#[test]
fn transitive_case() {
    let first_trait_member = TraitMember {
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

    let table = Table::<State>::default();
    let premise = Premise::from_predicates(
        [
            Predicate::TraitTypeEquality(TraitMemberEquality {
                trait_member: first_trait_member.clone(),
                equivalent: equivalent.clone(),
            }),
            Predicate::TraitTypeEquality(TraitMemberEquality {
                trait_member: second_trait_member.clone(),
                equivalent: equivalent.clone(),
            }),
        ]
        .into_iter(),
    );

    let result1 = simplify(
        &Type::TraitMember(first_trait_member),
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    let result2 = simplify(
        &Type::TraitMember(second_trait_member),
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    assert_eq!(result1, result2);
    assert_eq!(result1, equivalent);
    assert_eq!(result2, equivalent);
}

#[test]
fn multiple_equivalent_but_same_case() {
    let starting_trait_member = TraitMember {
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

    let table = Table::<State>::default();
    let premise = Premise::from_predicates(
        [
            Predicate::TraitTypeEquality(TraitMemberEquality {
                trait_member: starting_trait_member.clone(),
                equivalent: Type::TraitMember(first_trait_member.clone()),
            }),
            Predicate::TraitTypeEquality(TraitMemberEquality {
                trait_member: starting_trait_member.clone(),
                equivalent: Type::TraitMember(second_trait_member.clone()),
            }),
            Predicate::TraitTypeEquality(TraitMemberEquality {
                trait_member: first_trait_member.clone(),
                equivalent: equivalent.clone(),
            }),
            Predicate::TraitTypeEquality(TraitMemberEquality {
                trait_member: second_trait_member.clone(),
                equivalent: equivalent.clone(),
            }),
        ]
        .into_iter(),
    );

    let result_from_starting = simplify(
        &Type::TraitMember(starting_trait_member),
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    let result_from_first = simplify(
        &Type::TraitMember(first_trait_member),
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    let result_from_second = simplify(
        &Type::TraitMember(second_trait_member),
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    assert_eq!(result_from_starting, result_from_first);
    assert_eq!(result_from_starting, result_from_second);
    assert_eq!(result_from_starting, equivalent);
}

#[test]
fn ambiguous_case() {
    let starting_trait_member = TraitMember {
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

    let table = Table::<State>::default();
    let premise = Premise::from_predicates(
        [
            Predicate::TraitTypeEquality(TraitMemberEquality {
                trait_member: starting_trait_member.clone(),
                equivalent: Type::TraitMember(first_trait_member.clone()),
            }),
            Predicate::TraitTypeEquality(TraitMemberEquality {
                trait_member: starting_trait_member.clone(),
                equivalent: Type::TraitMember(second_trait_member.clone()),
            }),
            Predicate::TraitTypeEquality(TraitMemberEquality {
                trait_member: first_trait_member.clone(),
                equivalent: first_equivalent,
            }),
            Predicate::TraitTypeEquality(TraitMemberEquality {
                trait_member: second_trait_member.clone(),
                equivalent: second_equivalent,
            }),
        ]
        .into_iter(),
    );

    let result_from_starting = simplify(
        &Type::TraitMember(starting_trait_member.clone()),
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    let result_from_first = simplify(
        &Type::TraitMember(first_trait_member.clone()),
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    let result_from_second = simplify(
        &Type::TraitMember(second_trait_member.clone()),
        &Environment { premise: &premise, table: &table },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap();

    assert_eq!(result_from_first, Type::TraitMember(first_trait_member));
    assert_eq!(result_from_second, Type::TraitMember(second_trait_member));

    assert_eq!(result_from_starting, Type::TraitMember(starting_trait_member));
}
