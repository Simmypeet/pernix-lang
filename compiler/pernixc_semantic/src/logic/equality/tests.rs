use crate::{
    arena::ID,
    entity::{
        r#type::{Algebraic, AlgebraicKind, Primitive, Type},
        GenericArguments,
    },
    logic::Mapping,
    symbol::{GenericID, Symbolic, TypeParameterID},
    table::Table,
};

/*
0? != 1?
 */
#[test]
fn not_equal_test() {
    let pair = (
        &Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(0),
        }),
        &Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(1),
        }),
    );
    let mapping: Mapping<'_, Symbolic> = Mapping::from_pairs(
        std::iter::empty(),
        std::iter::once(pair),
        std::iter::empty(),
    );

    let table = Table::default();

    let lhs: Type<Symbolic> = Type::Primitive(Primitive::Bool);
    let rhs: Type<Symbolic> = Type::Primitive(Primitive::Float32);

    assert!(!lhs.equals(&rhs, &mapping, &table));
}

/*
Suppose:
10? = 20?
1? = 10?
2? = 20?

Then:
1? = 2?
 */
#[test]
fn transitivity_test() {
    let pairs = [
        (
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(10),
            }),
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(20),
            }),
        ),
        (
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(1),
            }),
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(10),
            }),
        ),
        (
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(2),
            }),
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(20),
            }),
        ),
    ];
    let mapping: Mapping<'_, Symbolic> =
        Mapping::from_pairs(std::iter::empty(), pairs, std::iter::empty());

    let table = Table::default();

    let lhs: Type<Symbolic> = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(1),
    });
    let rhs: Type<Symbolic> = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(2),
    });

    assert!(lhs.equals(&rhs, &mapping, &table));
}

/*

suppose:
a = c(a, b)

then:
 a = c(a, b) = c(c(a, b), b) = c(c(c(a, b), b), b) = ...
*/

#[test]
fn recursive_term_test() {
    let pair = (
        &(Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(0),
        })),
        &(Type::Algebraic(Algebraic {
            kind: AlgebraicKind::Enum(ID::new(0)),
            generic_arguments: GenericArguments {
                regions: Vec::new(),
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

    let mapping: Mapping<'_, Symbolic> = Mapping::from_pairs(
        std::iter::empty(),
        std::iter::once(pair),
        std::iter::empty(),
    );

    // ?0 = Enum0(?0, ?1)

    let table = Table::default();

    // will 0? = Enum0(Enum0(0?, 1?), 1?) ?

    let lhs: Type<Symbolic> = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(0),
    });
    let rhs: Type<Symbolic> = Type::Algebraic(Algebraic {
        kind: AlgebraicKind::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            regions: Vec::new(),
            types: vec![
                Type::Algebraic(Algebraic {
                    kind: AlgebraicKind::Enum(ID::new(0)),
                    generic_arguments: GenericArguments {
                        regions: Vec::new(),
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

    assert!(lhs.equals(&rhs, &mapping, &table));
}

/*
Suppose:
0? = 1? = 2? = 3?
1? = 4?

Then:
 0? = 2?
 */

#[test]
fn more_transitivity_test() {
    let pairs = [
        (
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(0),
            }),
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(1),
            }),
        ),
        (
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(1),
            }),
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(2),
            }),
        ),
        (
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(1),
            }),
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(4),
            }),
        ),
        (
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(2),
            }),
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(3),
            }),
        ),
    ];

    let mapping: Mapping<'_, Symbolic> =
        Mapping::from_pairs(std::iter::empty(), pairs, std::iter::empty());

    let table = Table::default();

    let lhs: Type<Symbolic> = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(0),
    });

    let rhs: Type<Symbolic> = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(3),
    });

    assert!(lhs.equals(&rhs, &mapping, &table));

    let lhs: Type<Symbolic> = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(0),
    });

    let rhs: Type<Symbolic> = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(3),
    });

    assert!(lhs.equals(&rhs, &mapping, &table));
}

/*
Suppose:
0? = 1?
2? = 3?

Then:
Enum(0?, 2?) = Enum(1?, 3?)

*/
#[test]
fn by_unification_test() {
    let pairs = [
        (
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(0),
            }),
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(1),
            }),
        ),
        (
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(2),
            }),
            &Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(3),
            }),
        ),
    ];

    let mapping: Mapping<'_, Symbolic> =
        Mapping::from_pairs(std::iter::empty(), pairs, std::iter::empty());

    let table = Table::default();

    let lhs: Type<Symbolic> = Type::Algebraic(Algebraic {
        kind: AlgebraicKind::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            regions: Vec::new(),
            types: vec![
                Type::Parameter(TypeParameterID {
                    parent: GenericID::Enum(ID::new(0)),
                    id: ID::new(0),
                }),
                Type::Parameter(TypeParameterID {
                    parent: GenericID::Enum(ID::new(0)),
                    id: ID::new(2),
                }),
            ],
            constants: Vec::new(),
        },
    });

    let rhs: Type<Symbolic> = Type::Algebraic(Algebraic {
        kind: AlgebraicKind::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            regions: Vec::new(),
            types: vec![
                Type::Parameter(TypeParameterID {
                    parent: GenericID::Enum(ID::new(0)),
                    id: ID::new(1),
                }),
                Type::Parameter(TypeParameterID {
                    parent: GenericID::Enum(ID::new(0)),
                    id: ID::new(3),
                }),
            ],
            constants: Vec::new(),
        },
    });

    assert!(lhs.equals(&rhs, &mapping, &table));
}
