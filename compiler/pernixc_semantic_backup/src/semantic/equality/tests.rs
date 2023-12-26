use crate::{
    arena::ID,
    semantic::{
        self,
        map::Mapping,
        predicate::Premises,
        session,
        term::{
            r#type::{SymbolKindID, Type},
            GenericArguments, Symbol, Term,
        },
    },
    symbol::{semantic::Symbolic, GenericID, TypeParameterID},
    table::{Success, Table},
};

/*

suppose:
a = c(a, b)

then:
 a = c(a, b) = c(c(a, b), b) = c(c(c(a, b), b), b) = ...
*/

#[test]
fn recursive_term_test() {
    let pair = (
        (Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(0),
        })),
        (Type::Symbol(Symbol {
            id: SymbolKindID::Enum(ID::new(0)),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![
                    Type::Parameter(TypeParameterID {
                        parent: GenericID::Enum(ID::new(0)),
                        id: ID::new(0),
                    }),
                    Type::Parameter(TypeParameterID {
                        parent: GenericID::Enum(ID::new(0)),
                        id: ID::new(1),
                    }),
                ],
                constants: Vec::new(),
            },
        })),
    );

    let premises: Premises<Symbolic> = Premises {
        non_equality_predicates: Vec::new(),
        mapping: Mapping::from_pairs(
            std::iter::empty(),
            std::iter::once(pair),
            std::iter::empty(),
        ),
    };

    // ?0 = Enum0(?0, ?1)

    let table = Table::<Success>::default();

    // will 0? = Enum0(Enum0(0?, 1?), 1?) ?

    let lhs: Type<Symbolic> = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(0),
    });
    let rhs: Type<Symbolic> = Type::Symbol(Symbol {
        id: SymbolKindID::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![
                Type::Symbol(Symbol {
                    id: SymbolKindID::Enum(ID::new(0)),
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![
                            Type::Parameter(TypeParameterID {
                                parent: GenericID::Enum(ID::new(0)),
                                id: ID::new(0),
                            }),
                            Type::Parameter(TypeParameterID {
                                parent: GenericID::Enum(ID::new(0)),
                                id: ID::new(1),
                            }),
                        ],
                        constants: Vec::new(),
                    },
                }),
                Type::Parameter(TypeParameterID {
                    parent: GenericID::Enum(ID::new(0)),
                    id: ID::new(1),
                }),
            ],
            constants: Vec::new(),
        },
    });

    assert!(lhs.equals(
        &rhs,
        &premises,
        &table,
        &semantic::Default,
        &mut session::Default::default()
    ));
    assert!(rhs.equals(
        &lhs,
        &premises,
        &table,
        &semantic::Default,
        &mut session::Default::default()
    )); // symmetric
}

#[test]
fn by_unification_test() {
    let pairs = [
        (
            Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(1),
            }),
            Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(12),
            }),
        ),
        (
            Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(12),
            }),
            Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(14),
            }),
        ),
        (
            Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(2),
            }),
            Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(3),
            }),
        ),
        (
            Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(3),
            }),
            Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(4),
            }),
        ),
    ];

    let premises: Premises<Symbolic> = Premises {
        non_equality_predicates: Vec::new(),
        mapping: Mapping::from_pairs(std::iter::empty(), pairs, std::iter::empty()),
    };

    let table = Table::<Success>::default();

    let lhs: Type<Symbolic> = Type::Symbol(Symbol {
        id: SymbolKindID::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(2),
            })],
            constants: Vec::new(),
        },
    });

    let rhs: Type<Symbolic> = Type::Symbol(Symbol {
        id: SymbolKindID::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(4),
            })],
            constants: Vec::new(),
        },
    });

    assert!(lhs.equals(
        &rhs,
        &premises,
        &table,
        &semantic::Default,
        &mut session::Default::default()
    ));
    assert!(rhs.equals(
        &lhs,
        &premises,
        &table,
        &semantic::Default,
        &mut session::Default::default()
    )); // symmetric
}

/*
Suppose:
0? = enum0(1?, 2?)
0? = enum0(3?, 4?)

Then:
1? != 3?
2? != 4?
*/

#[test]
#[allow(clippy::too_many_lines)]
fn fallacy_test() {
    let equalities = [
        (
            Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(0),
            }),
            Type::Symbol(Symbol {
                id: SymbolKindID::Enum(ID::new(0)),
                generic_arguments: GenericArguments {
                    lifetimes: vec![],
                    types: vec![
                        Type::Parameter(TypeParameterID {
                            parent: GenericID::Enum(ID::new(0)),
                            id: ID::new(1),
                        }),
                        Type::Parameter(TypeParameterID {
                            parent: GenericID::Enum(ID::new(0)),
                            id: ID::new(2),
                        }),
                    ],
                    constants: vec![],
                },
            }),
        ),
        (
            Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(0),
            }),
            Type::Symbol(Symbol {
                id: SymbolKindID::Enum(ID::new(0)),
                generic_arguments: GenericArguments {
                    lifetimes: vec![],
                    types: vec![
                        Type::Parameter(TypeParameterID {
                            parent: GenericID::Enum(ID::new(0)),
                            id: ID::new(3),
                        }),
                        Type::Parameter(TypeParameterID {
                            parent: GenericID::Enum(ID::new(0)),
                            id: ID::new(4),
                        }),
                    ],
                    constants: vec![],
                },
            }),
        ),
    ];

    let premises: Premises<Symbolic> = Premises {
        non_equality_predicates: Vec::new(),
        mapping: Mapping::from_pairs(
            std::iter::empty(),
            equalities.iter().cloned(),
            std::iter::empty(),
        ),
    };

    let table = Table::<Success>::default();

    let lhs: Type<Symbolic> = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(1),
    });
    let rhs: Type<Symbolic> = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(3),
    });

    assert!(!lhs.equals(
        &rhs,
        &premises,
        &table,
        &semantic::Default,
        &mut session::Default::default()
    ));
    assert!(!rhs.equals(
        &lhs,
        &premises,
        &table,
        &semantic::Default,
        &mut session::Default::default()
    )); // symmetric

    let lhs: Type<Symbolic> = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(2),
    });

    let rhs: Type<Symbolic> = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(4),
    });

    assert!(!lhs.equals(
        &rhs,
        &premises,
        &table,
        &semantic::Default,
        &mut session::Default::default()
    ));
    assert!(!rhs.equals(
        &lhs,
        &premises,
        &table,
        &semantic::Default,
        &mut session::Default::default()
    ));
}
