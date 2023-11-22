use super::Config;
use crate::{
    arena::ID,
    entity::{
        constant::Constant,
        predicate::Premises,
        r#type::{self, Algebraic, AlgebraicKind, Tuple, TupleElement, Type},
        region::Region,
        Entity, GenericArguments, Model,
    },
    logic::Mapping,
    symbol::{GenericID, Symbolic, TypeParameterID},
    table::Table,
};

#[test]
fn tuple_test() {
    /*
    0? = (int32, float32, float64)
     */
    let equalities = [(
        Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(0),
        }),
        Type::Tuple(Tuple {
            elements: vec![
                TupleElement::Regular(Type::Primitive(r#type::Primitive::Int32)),
                TupleElement::Regular(Type::Primitive(r#type::Primitive::Float32)),
                TupleElement::Regular(Type::Primitive(r#type::Primitive::Float64)),
            ],
        }),
    )];

    let premises: Premises<Symbolic> = Premises {
        non_equality_predicates: Vec::new(),
        mapping: Mapping::from_pairs(std::iter::empty(), equalities, std::iter::empty()),
    };

    // unify( (1?, 2?...), (0?...) )
    //
    // expected:
    // 1? -> int32
    // 2? -> (float32, float64)

    let lhs = Type::Tuple(Tuple {
        elements: vec![
            TupleElement::Regular(Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(1),
            })),
            TupleElement::Unpacked(r#type::Unpacked::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(2),
            })),
        ],
    });
    let rhs = Type::Tuple(Tuple {
        elements: vec![TupleElement::Unpacked(r#type::Unpacked::Parameter(
            TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(0),
            },
        ))],
    });

    let table = Table::default();

    let sub = Type::unify(&lhs, &rhs, &premises, &table, &VariableConfig).unwrap();

    assert!(sub
        .types
        .get(&Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(1),
        }))
        .unwrap()
        .equals(
            &Type::Primitive(r#type::Primitive::Int32),
            &premises,
            &table,
        ));

    assert!(sub
        .types
        .get(&Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(2),
        }))
        .unwrap()
        .equals(
            &Type::Tuple(Tuple {
                elements: vec![
                    TupleElement::Regular(Type::Primitive(r#type::Primitive::Float32)),
                    TupleElement::Regular(Type::Primitive(r#type::Primitive::Float64)),
                ],
            }),
            &premises,
            &table,
        ));
}

#[test]
fn recursive_term_test() {
    /*
    ?0 = Enum0(?0)
    ?0 = bool
     */

    let equalities = [
        (
            (Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(0),
            })),
            (Type::Algebraic(Algebraic {
                kind: AlgebraicKind::Enum(ID::new(0)),
                generic_arguments: GenericArguments {
                    regions: Vec::new(),
                    types: vec![Type::Parameter(TypeParameterID {
                        parent: GenericID::Enum(ID::new(0)),
                        id: ID::new(0),
                    })],
                    constants: Vec::new(),
                },
            })),
        ),
        (
            (Type::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(0),
            })),
            (Type::Primitive(r#type::Primitive::Bool)),
        ),
    ];

    let premises: Premises<Symbolic> = Premises {
        non_equality_predicates: Vec::new(),
        mapping: Mapping::from_pairs(std::iter::empty(), equalities, std::iter::empty()),
    };

    let table = Table::default();

    let mut lhs = Type::Algebraic(Algebraic {
        kind: AlgebraicKind::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            regions: Vec::new(),
            types: vec![Type::Algebraic(Algebraic {
                kind: AlgebraicKind::Enum(ID::new(0)),
                generic_arguments: GenericArguments {
                    regions: Vec::new(),
                    types: vec![Type::Parameter(TypeParameterID {
                        parent: GenericID::Enum(ID::new(0)),
                        id: ID::new(1),
                    })],
                    constants: Vec::new(),
                },
            })],
            constants: Vec::new(),
        },
    });

    let rhs = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(0),
    });

    let sub = Type::unify(&lhs, &rhs, &premises, &table, &VariableConfig).unwrap();

    let mapped = sub
        .types
        .get(&Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(1),
        }))
        .unwrap();

    assert!(mapped.equals(&Type::Primitive(r#type::Primitive::Bool), &premises, &table));

    assert!(!lhs.equals(&rhs, &premises, &table));

    // after applying the substitution, the lhs should be equal to the rhs
    lhs.apply(&sub);

    assert!(lhs.equals(&rhs, &premises, &table));
}

#[test]
fn unification_conflict() {
    let equalities: [(r#type::Type<Symbolic>, r#type::Type<Symbolic>); 1] = [(
        Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(0),
        }),
        Type::Algebraic(Algebraic {
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
                        id: ID::new(2),
                    }),
                ],
                constants: Vec::new(),
            },
        }),
    )];

    let premises = Premises {
        non_equality_predicates: Vec::new(),
        mapping: Mapping::from_pairs(std::iter::empty(), equalities, std::iter::empty()),
    };

    let table = Table::default();

    let lhs = Type::Algebraic(Algebraic {
        kind: AlgebraicKind::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            regions: Vec::new(),
            types: vec![
                Type::Parameter(TypeParameterID {
                    parent: GenericID::Enum(ID::new(1)),
                    id: ID::new(1),
                }),
                Type::Parameter(TypeParameterID {
                    parent: GenericID::Enum(ID::new(1)),
                    id: ID::new(1),
                }),
            ],
            constants: Vec::new(),
        },
    });
    let rhs = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(0),
    });

    assert!(Type::unify(&lhs, &rhs, &premises, &table, &VariableConfig).is_err());
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct VariableConfig;

impl<S: Model> Config<S> for VariableConfig {
    fn type_mappable(&self, unifier: &Type<S>, _: &Type<S>) -> bool { unifier.is_parameter() }

    fn constant_mappable(&self, unifier: &Constant<S>, _: &Constant<S>) -> bool {
        unifier.is_parameter()
    }

    fn region_mappable(&self, unifier: &Region<S>, _: &Region<S>) -> bool { unifier.is_named() }
}
