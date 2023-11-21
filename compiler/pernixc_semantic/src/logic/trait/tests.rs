use std::{fmt::Display, sync::RwLock};

use pernixc_base::{
    diagnostic::{Handler, Storage},
    source_file::SourceFile,
};
use pernixc_syntax::syntax_tree::target::Target;

use crate::{
    arena::ID,
    entity::{
        r#type::{AlgebraicKind, Primitive, Type},
        GenericArguments,
    },
    error::{self, WithTable},
    logic::Mapping,
    symbol::{GenericID, Symbolic, TypeParameterID},
    table::{Index, Table},
};

#[derive(Debug)]
struct ErrorCheck {
    has_error: RwLock<bool>,
}

impl<T: Display> Handler<T> for ErrorCheck {
    fn receive(&self, error: T) {
        *self.has_error.write().unwrap() = true;
        eprintln!("{error}");
    }
}

fn parse_table(input: impl Display) -> Table {
    let storage: Storage<error::Error> = Storage::default();
    let check = ErrorCheck {
        has_error: RwLock::new(false),
    };
    let result = Table::build(
        rayon::iter::once(Target::parse(
            &SourceFile::temp(input).unwrap(),
            "test".to_string(),
            &check,
        )),
        &storage,
    )
    .unwrap();

    assert!(!*check.has_error.read().unwrap());

    let storage = storage.into_vec();
    let has_error = !storage.is_empty();

    for error in storage {
        eprintln!("{}", WithTable {
            table: &result,
            error: &error
        });
    }

    assert!(!has_error);

    result
}

const TRIVIAL_TRAIT: &str = r"
public trait Example<T, U> {}

implements Example<int32, float32> {}
";

#[test]
fn trivial_trait_resolution() {
    let table = parse_table(TRIVIAL_TRAIT);
    let target_implementation_arguments: GenericArguments<Symbolic> = GenericArguments {
        regions: Vec::new(),
        types: vec![
            Type::Primitive(Primitive::Int32),
            Type::Primitive(Primitive::Float32),
        ],
        constants: Vec::new(),
    };

    // gets the trait id
    let trait_id = table
        .get_by_qualified_name(["test", "Example"].into_iter())
        .unwrap()
        .into_trait()
        .unwrap();

    #[allow(clippy::redundant_clone)]
    let mut generic_arguments: GenericArguments<Symbolic> = target_implementation_arguments.clone();

    let implementations =
        table.resolve_implementation(trait_id, &generic_arguments, &Mapping::default());

    assert_eq!(implementations.len(), 1);

    assert_eq!(implementations[0].deduced_unification.types.len(), 0);
    assert_eq!(implementations[0].deduced_unification.regions.len(), 0);
    assert_eq!(implementations[0].deduced_unification.constants.len(), 0);

    assert!(table
        .get(implementations[0].implementation_id)
        .unwrap()
        .signature
        .arguments
        .equals(
            &target_implementation_arguments,
            &Mapping::default(),
            &table
        ));

    let parameter = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(0),
    });
    let float32 = Type::Primitive(Primitive::Float32);

    generic_arguments = GenericArguments {
        regions: Vec::new(),
        types: vec![Type::Primitive(Primitive::Int32), parameter.clone()],
        constants: Vec::new(),
    };

    let implementations =
        table.resolve_implementation(trait_id, &generic_arguments, &Mapping::default());

    assert_eq!(implementations.len(), 0);

    let implementations = table.resolve_implementation(
        trait_id,
        &generic_arguments,
        &Mapping::from_pairs(
            std::iter::empty(),
            std::iter::once((parameter, float32)),
            std::iter::empty(),
        ),
    );

    assert_eq!(implementations.len(), 1);

    assert_eq!(implementations[0].deduced_unification.types.len(), 0);
    assert_eq!(implementations[0].deduced_unification.regions.len(), 0);
    assert_eq!(implementations[0].deduced_unification.constants.len(), 0);

    assert!(table
        .get(implementations[0].implementation_id)
        .unwrap()
        .signature
        .arguments
        .equals(
            &target_implementation_arguments,
            &Mapping::default(),
            &table
        ));
}

const TRAIT_WITH_SPECIALIZATION: &str = r"
public enum Option<T> {
    Some(T),
    None
}

public trait Example<T, U> {}

implements<T, U> Example<Option<T>, Option<U>> {}     // call this implementation 1
implements<T, U> Example<T, U> {}                     // call this implementation 2
implements<T, U> Example<Option<T>, U> {}             // call this implementation 3
implements Example<Option<int32>, Option<float32>> {} // call this implementation 4
implements<T> Example<T, T> {}                        // call this implementation 5
";

#[test]
#[allow(clippy::significant_drop_tightening, clippy::too_many_lines)]
fn trait_resolution_with_specialization_implementation_one() {
    let table = parse_table(TRAIT_WITH_SPECIALIZATION);
    let trait_symbol = table
        .get(
            table
                .get_by_qualified_name(["test", "Example"].into_iter())
                .unwrap()
                .into_trait()
                .unwrap(),
        )
        .unwrap();
    let enum_symbol = table
        .get(
            table
                .get_by_qualified_name(["test", "Option"].into_iter())
                .unwrap()
                .into_enum()
                .unwrap(),
        )
        .unwrap();

    let (first_implementation_id, t_type_parameter, u_type_parameter) = 'a: {
        for &implementation in &trait_symbol.implementations {
            let t_type_parameter = Type::Parameter(TypeParameterID {
                parent: implementation.into(),
                id: ID::new(0),
            });
            let u_type_parameter = Type::Parameter(TypeParameterID {
                parent: implementation.into(),
                id: ID::new(1),
            });

            let expected_generic_arguments = GenericArguments {
                regions: Vec::new(),
                types: vec![
                    Type::Algebraic(crate::entity::r#type::Algebraic {
                        kind: AlgebraicKind::Enum(enum_symbol.id),
                        generic_arguments: GenericArguments {
                            regions: Vec::new(),
                            types: vec![t_type_parameter.clone()],
                            constants: Vec::new(),
                        },
                    }),
                    Type::Algebraic(crate::entity::r#type::Algebraic {
                        kind: AlgebraicKind::Enum(enum_symbol.id),
                        generic_arguments: GenericArguments {
                            regions: Vec::new(),
                            types: vec![u_type_parameter.clone()],
                            constants: Vec::new(),
                        },
                    }),
                ],
                constants: Vec::new(),
            };

            if expected_generic_arguments == table.get(implementation).unwrap().signature.arguments
            {
                break 'a (implementation, t_type_parameter, u_type_parameter);
            }
        }

        panic!("Implementation not found")
    };

    // Example<Option<uint32>, Option<uint64>> should resolve to one
    let generic_arguments: GenericArguments<Symbolic> = GenericArguments {
        regions: Vec::new(),
        types: vec![
            Type::Algebraic(crate::entity::r#type::Algebraic {
                kind: AlgebraicKind::Enum(enum_symbol.id),
                generic_arguments: GenericArguments {
                    regions: Vec::new(),
                    types: vec![Type::Primitive(Primitive::Uint32)],
                    constants: Vec::new(),
                },
            }),
            Type::Algebraic(crate::entity::r#type::Algebraic {
                kind: AlgebraicKind::Enum(enum_symbol.id),
                generic_arguments: GenericArguments {
                    regions: Vec::new(),
                    types: vec![Type::Primitive(Primitive::Uint64)],
                    constants: Vec::new(),
                },
            }),
        ],
        constants: Vec::new(),
    };

    let implementations =
        table.resolve_implementation(trait_symbol.id, &generic_arguments, &Mapping::default());

    // no type variance
    assert_eq!(implementations.len(), 1);

    assert_eq!(
        implementations[0].implementation_id,
        first_implementation_id
    );

    assert_eq!(implementations[0].deduced_unification.types.len(), 2);

    // T => uint32
    assert_eq!(
        implementations[0].deduced_unification.types[&t_type_parameter],
        Type::Primitive(Primitive::Uint32)
    );
    // U => uint64
    assert_eq!(
        implementations[0].deduced_unification.types[&u_type_parameter],
        Type::Primitive(Primitive::Uint64)
    );
}

#[test]
#[allow(clippy::significant_drop_tightening, clippy::too_many_lines)]
fn trait_resolution_with_specialization_implementation_second() {
    let table = parse_table(TRAIT_WITH_SPECIALIZATION);
    let trait_symbol = table
        .get(
            table
                .get_by_qualified_name(["test", "Example"].into_iter())
                .unwrap()
                .into_trait()
                .unwrap(),
        )
        .unwrap();
    let enum_symbol = table
        .get(
            table
                .get_by_qualified_name(["test", "Option"].into_iter())
                .unwrap()
                .into_enum()
                .unwrap(),
        )
        .unwrap();

    let (second_implementation_id, t_type_parameter, u_type_parameter) = 'a: {
        for &implementation in &trait_symbol.implementations {
            let t_type_parameter = Type::Parameter(TypeParameterID {
                parent: implementation.into(),
                id: ID::new(0),
            });
            let u_type_parameter = Type::Parameter(TypeParameterID {
                parent: implementation.into(),
                id: ID::new(1),
            });

            let expected_generic_arguments = GenericArguments {
                regions: Vec::new(),
                types: vec![t_type_parameter.clone(), u_type_parameter.clone()],
                constants: Vec::new(),
            };

            if expected_generic_arguments == table.get(implementation).unwrap().signature.arguments
            {
                break 'a (implementation, t_type_parameter, u_type_parameter);
            }
        }

        panic!("Implementation not found")
    };

    // Example<int32, Option<uint64>> should resolve to two
    let generic_arguments: GenericArguments<Symbolic> = GenericArguments {
        regions: Vec::new(),
        types: vec![
            Type::Primitive(Primitive::Int32),
            Type::Algebraic(crate::entity::r#type::Algebraic {
                kind: AlgebraicKind::Enum(enum_symbol.id),
                generic_arguments: GenericArguments {
                    regions: Vec::new(),
                    types: vec![Type::Primitive(Primitive::Uint64)],
                    constants: Vec::new(),
                },
            }),
        ],
        constants: Vec::new(),
    };

    let implementations =
        table.resolve_implementation(trait_symbol.id, &generic_arguments, &Mapping::default());

    // no type variance
    assert_eq!(implementations.len(), 1);

    assert_eq!(
        implementations[0].implementation_id,
        second_implementation_id
    );

    assert_eq!(implementations[0].deduced_unification.types.len(), 2);

    // T => int32
    assert_eq!(
        implementations[0].deduced_unification.types[&t_type_parameter],
        Type::Primitive(Primitive::Int32)
    );

    // U => Option<uint64>
    assert_eq!(
        implementations[0].deduced_unification.types[&u_type_parameter],
        Type::Algebraic(crate::entity::r#type::Algebraic {
            kind: AlgebraicKind::Enum(enum_symbol.id),
            generic_arguments: GenericArguments {
                regions: Vec::new(),
                types: vec![Type::Primitive(Primitive::Uint64)],
                constants: Vec::new(),
            },
        })
    );

    // Example<int32, uint64> should resolve to two
    let generic_arguments: GenericArguments<Symbolic> = GenericArguments {
        regions: Vec::new(),
        types: vec![
            Type::Primitive(Primitive::Int32),
            Type::Primitive(Primitive::Uint64),
        ],
        constants: Vec::new(),
    };

    let implementations =
        table.resolve_implementation(trait_symbol.id, &generic_arguments, &Mapping::default());

    // no type variance
    assert_eq!(implementations.len(), 1);

    assert_eq!(
        implementations[0].implementation_id,
        second_implementation_id
    );

    assert_eq!(implementations[0].deduced_unification.types.len(), 2);

    // T => int32
    assert_eq!(
        implementations[0].deduced_unification.types[&t_type_parameter],
        Type::Primitive(Primitive::Int32)
    );

    // U => uint64
    assert_eq!(
        implementations[0].deduced_unification.types[&u_type_parameter],
        Type::Primitive(Primitive::Uint64)
    );
}

#[test]
#[allow(clippy::significant_drop_tightening, clippy::too_many_lines)]
fn trait_resolution_with_specialization_implementation_three() {
    let table = parse_table(TRAIT_WITH_SPECIALIZATION);
    let trait_symbol = table
        .get(
            table
                .get_by_qualified_name(["test", "Example"].into_iter())
                .unwrap()
                .into_trait()
                .unwrap(),
        )
        .unwrap();
    let enum_symbol = table
        .get(
            table
                .get_by_qualified_name(["test", "Option"].into_iter())
                .unwrap()
                .into_enum()
                .unwrap(),
        )
        .unwrap();

    let (third_implementation_id, t_type_parameter, u_type_parameter) = 'a: {
        for &implementation in &trait_symbol.implementations {
            let t_type_parameter = Type::Parameter(TypeParameterID {
                parent: implementation.into(),
                id: ID::new(0),
            });
            let u_type_parameter = Type::Parameter(TypeParameterID {
                parent: implementation.into(),
                id: ID::new(1),
            });

            let expected_generic_arguments = GenericArguments {
                regions: Vec::new(),
                types: vec![
                    Type::Algebraic(crate::entity::r#type::Algebraic {
                        kind: AlgebraicKind::Enum(enum_symbol.id),
                        generic_arguments: GenericArguments {
                            regions: Vec::new(),
                            types: vec![t_type_parameter.clone()],
                            constants: Vec::new(),
                        },
                    }),
                    u_type_parameter.clone(),
                ],
                constants: Vec::new(),
            };

            if expected_generic_arguments == table.get(implementation).unwrap().signature.arguments
            {
                break 'a (implementation, t_type_parameter, u_type_parameter);
            }
        }

        panic!("Implementation not found")
    };

    // Example<Option<uint32>, uint32> should resolve to three
    let generic_arguments: GenericArguments<Symbolic> = GenericArguments {
        regions: Vec::new(),
        types: vec![
            Type::Algebraic(crate::entity::r#type::Algebraic {
                kind: AlgebraicKind::Enum(enum_symbol.id),
                generic_arguments: GenericArguments {
                    regions: Vec::new(),
                    types: vec![Type::Primitive(Primitive::Uint32)],
                    constants: Vec::new(),
                },
            }),
            Type::Primitive(Primitive::Uint32),
        ],
        constants: Vec::new(),
    };

    let implementations =
        table.resolve_implementation(trait_symbol.id, &generic_arguments, &Mapping::default());

    // no type variance
    assert_eq!(implementations.len(), 1);

    assert_eq!(
        implementations[0].implementation_id,
        third_implementation_id
    );

    assert_eq!(implementations[0].deduced_unification.types.len(), 2);

    // T => uint32
    assert_eq!(
        implementations[0].deduced_unification.types[&t_type_parameter],
        Type::Primitive(Primitive::Uint32)
    );

    // U => uint32
    assert_eq!(
        implementations[0].deduced_unification.types[&u_type_parameter],
        Type::Primitive(Primitive::Uint32)
    );
}

#[test]
#[allow(clippy::significant_drop_tightening, clippy::too_many_lines)]
fn trait_resolution_with_specialization_implementation_four() {
    let table = parse_table(TRAIT_WITH_SPECIALIZATION);
    let trait_symbol = table
        .get(
            table
                .get_by_qualified_name(["test", "Example"].into_iter())
                .unwrap()
                .into_trait()
                .unwrap(),
        )
        .unwrap();
    let enum_symbol = table
        .get(
            table
                .get_by_qualified_name(["test", "Option"].into_iter())
                .unwrap()
                .into_enum()
                .unwrap(),
        )
        .unwrap();

    let (fourth_implementation, expected_generic_arguments) = 'a: {
        for &implementation in &trait_symbol.implementations {
            let expected_generic_arguments = GenericArguments {
                regions: Vec::new(),
                types: vec![
                    Type::Algebraic(crate::entity::r#type::Algebraic {
                        kind: AlgebraicKind::Enum(enum_symbol.id),
                        generic_arguments: GenericArguments {
                            regions: Vec::new(),
                            types: vec![Type::Primitive(Primitive::Int32)],
                            constants: Vec::new(),
                        },
                    }),
                    Type::Algebraic(crate::entity::r#type::Algebraic {
                        kind: AlgebraicKind::Enum(enum_symbol.id),
                        generic_arguments: GenericArguments {
                            regions: Vec::new(),
                            types: vec![Type::Primitive(Primitive::Float32)],
                            constants: Vec::new(),
                        },
                    }),
                ],
                constants: Vec::new(),
            };

            if expected_generic_arguments == table.get(implementation).unwrap().signature.arguments
            {
                break 'a (implementation, expected_generic_arguments);
            }
        }

        panic!("Implementation not found")
    };

    let implementation = table.resolve_implementation(
        trait_symbol.id,
        &expected_generic_arguments,
        &Mapping::default(),
    );

    assert_eq!(implementation.len(), 1);

    assert_eq!(implementation[0].implementation_id, fourth_implementation);

    assert_eq!(implementation[0].deduced_unification.types.len(), 0);
    assert_eq!(implementation[0].deduced_unification.regions.len(), 0);
    assert_eq!(implementation[0].deduced_unification.constants.len(), 0);
}

#[test]
#[allow(clippy::significant_drop_tightening, clippy::too_many_lines)]
fn trait_resolution_with_specialization_implementation_five() {
    let table = parse_table(TRAIT_WITH_SPECIALIZATION);
    let trait_symbol = table
        .get(
            table
                .get_by_qualified_name(["test", "Example"].into_iter())
                .unwrap()
                .into_trait()
                .unwrap(),
        )
        .unwrap();

    let (fifth_implementation, t_type_parameter) = 'a: {
        for &implementation in &trait_symbol.implementations {
            let t_type_parameter = Type::Parameter(TypeParameterID {
                parent: implementation.into(),
                id: ID::new(0),
            });

            let expected_generic_arguments = GenericArguments {
                regions: Vec::new(),
                types: vec![t_type_parameter.clone(), t_type_parameter.clone()],
                constants: Vec::new(),
            };

            if expected_generic_arguments == table.get(implementation).unwrap().signature.arguments
            {
                break 'a (implementation, t_type_parameter);
            }
        }

        panic!("Implementation not found")
    };

    // Example<int32, int32> should resolve to five
    let generic_arguments: GenericArguments<Symbolic> = GenericArguments {
        regions: Vec::new(),
        types: vec![
            Type::Primitive(Primitive::Int32),
            Type::Primitive(Primitive::Int32),
        ],
        constants: Vec::new(),
    };

    let implementations =
        table.resolve_implementation(trait_symbol.id, &generic_arguments, &Mapping::default());

    // no type variance
    assert_eq!(implementations.len(), 1);

    assert_eq!(implementations[0].implementation_id, fifth_implementation);

    assert_eq!(implementations[0].deduced_unification.types.len(), 1);

    // T => int32
    assert_eq!(
        implementations[0].deduced_unification.types[&t_type_parameter],
        Type::Primitive(Primitive::Int32)
    );
}
