use super::Config;
use crate::{
    arena::ID,
    entity::{
        constant::Constant,
        r#type::{self, Algebraic, AlgebraicKind, Type},
        region::Region,
        GenericArguments, Model,
    },
    logic::Mapping,
    symbol::{GenericID, Symbolic, TypeParameterID},
    table::Table,
};

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

    let mapping: Mapping<Symbolic> =
        Mapping::from_pairs(std::iter::empty(), equalities, std::iter::empty());

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

    let sub = Type::unify(&lhs, &rhs, &mapping, &table, &VariableConfig).unwrap();

    let mapped = sub
        .types
        .get(&Type::Parameter(TypeParameterID {
            parent: GenericID::Enum(ID::new(0)),
            id: ID::new(1),
        }))
        .unwrap();

    assert!(mapped.equals(&Type::Primitive(r#type::Primitive::Bool), &mapping, &table));

    assert!(!lhs.equals(&rhs, &mapping, &table));

    // after applying the substitution, the lhs should be equal to the rhs
    lhs.apply(&sub);

    assert!(lhs.equals(&rhs, &mapping, &table));
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

    let premise_mapping = Mapping::from_pairs(std::iter::empty(), equalities, std::iter::empty());
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

    assert!(Type::unify(&lhs, &rhs, &premise_mapping, &table, &VariableConfig).is_err());
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
