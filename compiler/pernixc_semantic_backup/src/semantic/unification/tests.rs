use super::Config;
use crate::{
    arena::ID,
    semantic::{
        self,
        map::Mapping,
        model::Model,
        predicate::Premises,
        session,
        substitution::Substitute,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Primitive, SymbolKindID, Tuple, Type},
            GenericArguments, Symbol, Term, TupleElement, Unpacked,
        },
    },
    symbol::{semantic::Symbolic, GenericID, TypeParameterID},
    table::{Success, Table},
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
                TupleElement::Regular(Type::Primitive(Primitive::Int32)),
                TupleElement::Regular(Type::Primitive(Primitive::Float32)),
                TupleElement::Regular(Type::Primitive(Primitive::Float64)),
            ],
        }),
    )];

    let premises: Premises<Symbolic> = Premises {
        non_equality_predicates: Vec::new(),
        mapping: Mapping::from_pairs(
            std::iter::empty(),
            equalities,
            std::iter::empty(),
        ),
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
            TupleElement::Unpacked(Unpacked::Parameter(TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(2),
            })),
        ],
    });
    let rhs = Type::Tuple(Tuple {
        elements: vec![TupleElement::Unpacked(Unpacked::Parameter(
            TypeParameterID {
                parent: GenericID::Enum(ID::new(0)),
                id: ID::new(0),
            },
        ))],
    });

    let table = Table::<Success>::default();

    let sub = lhs
        .unify(
            &rhs,
            &premises,
            &table,
            &semantic::Default,
            &mut session::Default::default(),
            &mut VariableConfig,
        )
        .unwrap();

    assert!(sub
        .types
        .get(&Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(1),
        }))
        .unwrap()
        .equals(
            &Type::Primitive(Primitive::Int32),
            &premises,
            &table,
            &semantic::Default,
            &mut session::Default::default(),
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
                    TupleElement::Regular(Type::Primitive(Primitive::Float32)),
                    TupleElement::Regular(Type::Primitive(Primitive::Float64)),
                ],
            }),
            &premises,
            &table,
            &semantic::Default,
            &mut session::Default::default(),
        ));
}

#[test]
fn infinite_recrusion_test() {
    /*
    ?0 = Enum0(?0)
     */

    let equalities = [(
        (Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(0),
        })),
        (Type::Symbol(Symbol {
            id: SymbolKindID::Enum(ID::new(0)),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Parameter(TypeParameterID {
                    parent: GenericID::Enum(ID::new(0)),
                    id: ID::new(0),
                })],
                constants: Vec::new(),
            },
        })),
    )];

    let premises: Premises<Symbolic> = Premises {
        non_equality_predicates: Vec::new(),
        mapping: Mapping::from_pairs(
            std::iter::empty(),
            equalities,
            std::iter::empty(),
        ),
    };

    let table = Table::<Success>::default();

    let lhs = Type::Symbol(Symbol {
        id: SymbolKindID::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![Type::Symbol(Symbol {
                id: SymbolKindID::Enum(ID::new(0)),
                generic_arguments: GenericArguments {
                    lifetimes: Vec::new(),
                    types: vec![Type::Primitive(Primitive::Bool)],
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

    assert!(lhs
        .unify(
            &rhs,
            &premises,
            &table,
            &semantic::Default,
            &mut session::Default::default(),
            &mut VariableConfig,
        )
        .is_none());
}

#[test]
fn recursive_term_test() {
    /*
    ?0 = Enum0(?0)
    ?0 = bool
     */

    let equalities = [(
        (Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(0),
        })),
        (Type::Symbol(Symbol {
            id: SymbolKindID::Enum(ID::new(0)),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Parameter(TypeParameterID {
                    parent: GenericID::Enum(ID::new(0)),
                    id: ID::new(0),
                })],
                constants: Vec::new(),
            },
        })),
    )];

    let premises: Premises<Symbolic> = Premises {
        non_equality_predicates: Vec::new(),
        mapping: Mapping::from_pairs(
            std::iter::empty(),
            equalities,
            std::iter::empty(),
        ),
    };

    let table = Table::<Success>::default();

    let mut lhs = Type::Symbol(Symbol {
        id: SymbolKindID::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![Type::Symbol(Symbol {
                id: SymbolKindID::Enum(ID::new(0)),
                generic_arguments: GenericArguments {
                    lifetimes: Vec::new(),
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

    let sub = lhs
        .unify(
            &rhs,
            &premises,
            &table,
            &semantic::Default,
            &mut session::Default::default(),
            &mut VariableConfig,
        )
        .unwrap();

    let mapped = sub
        .types
        .get(&Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(1),
        }))
        .unwrap();

    assert!(mapped.equals(
        &Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(0),
        }),
        &premises,
        &table,
        &semantic::Default,
        &mut session::Default::default(),
    ));

    assert!(!lhs.equals(
        &rhs,
        &premises,
        &table,
        &semantic::Default,
        &mut session::Default::default(),
    ));

    // after applying the substitution, the lhs should be equal to the rhs
    lhs.apply(&sub);

    assert!(lhs.equals(
        &rhs,
        &premises,
        &table,
        &semantic::Default,
        &mut session::Default::default(),
    ));
}

#[test]
fn unification_conflict() {
    let equalities: [(Type<Symbolic>, Type<Symbolic>); 1] = [(
        Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(0),
        }),
        Type::Symbol(Symbol {
            id: SymbolKindID::Enum(ID::new(0)),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
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
        mapping: Mapping::from_pairs(
            std::iter::empty(),
            equalities,
            std::iter::empty(),
        ),
    };

    let table = Table::<Success>::default();

    let lhs = Type::Symbol(Symbol {
        id: SymbolKindID::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![
                Type::Parameter(TypeParameterID {
                    parent: GenericID::Enum(ID::new(0)),
                    id: ID::new(3),
                }),
                Type::Parameter(TypeParameterID {
                    parent: GenericID::Enum(ID::new(0)),
                    id: ID::new(3),
                }),
            ],
            constants: Vec::new(),
        },
    });
    let rhs = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(0),
    });

    assert!(lhs
        .unify(
            &rhs,
            &premises,
            &table,
            &semantic::Default,
            &mut session::Default::default(),
            &mut VariableConfig
        )
        .is_none());
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct VariableConfig;

impl<S: Model> Config<Type<S>> for VariableConfig {
    fn unifiable(&mut self, lhs: &Type<S>, _: &Type<S>) -> bool {
        lhs.is_parameter()
    }
}

impl<S: Model> Config<Constant<S>> for VariableConfig {
    fn unifiable(&mut self, lhs: &Constant<S>, _: &Constant<S>) -> bool {
        lhs.is_parameter()
    }
}

impl<S: Model> Config<Lifetime<S>> for VariableConfig {
    fn unifiable(&mut self, _: &Lifetime<S>, _: &Lifetime<S>) -> bool { false }
}
