use crate::{
    arena::ID,
    semantic::{
        self,
        map::Mapping,
        predicate::Premises,
        session,
        term::{
            r#type::{Primitive, SymbolKindID, Type},
            GenericArguments, Symbol, Term,
        },
    },
    symbol::{semantic::Symbolic, GenericID, TypeParameterID},
    table::{Success, Table},
};

#[test]
fn trivially_definite() {
    // primitve type is surely definite
    let primitive: Type<Symbolic> = Type::Primitive(Primitive::Bool);
    let semantic = semantic::Default;
    let premises = Premises::default();
    let table = Table::<Success>::default();

    assert!(primitive.definite(
        &premises,
        &table,
        &semantic,
        &mut session::Default::default()
    ));
}

#[test]
fn trivially_indefinite() {
    // type parameter is surely indefinite
    let inference: Type<Symbolic> = Type::Parameter(TypeParameterID {
        parent: GenericID::Enum(ID::new(0)),
        id: ID::new(0),
    });
    let semantic = semantic::Default;
    let premises = Premises::default();
    let table = Table::<Success>::default();

    assert!(!inference.definite(
        &premises,
        &table,
        &semantic,
        &mut session::Default::default()
    ));
}

#[test]
fn adt_definite() {
    // Enum0(bool, Enum0(bool))
    let enum_ty: Type<Symbolic> = Type::Symbol(Symbol {
        id: SymbolKindID::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![
                Type::Primitive(Primitive::Bool),
                Type::Symbol(Symbol {
                    id: SymbolKindID::Enum(ID::new(0)),
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Primitive(Primitive::Bool)],
                        constants: Vec::new(),
                    },
                }),
            ],
            constants: Vec::new(),
        },
    });
    let semantic = semantic::Default;
    let premises = Premises::default();
    let table = Table::<Success>::default();

    assert!(enum_ty.definite(
        &premises,
        &table,
        &semantic,
        &mut session::Default::default()
    ));
}

#[test]
fn adt_indefinite() {
    // Enum0(bool, Enum0(?T))
    let enum_ty: Type<Symbolic> = Type::Symbol(Symbol {
        id: SymbolKindID::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![
                Type::Primitive(Primitive::Bool),
                Type::Symbol(Symbol {
                    id: SymbolKindID::Enum(ID::new(0)),
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Parameter(TypeParameterID {
                            parent: GenericID::Enum(ID::new(0)),
                            id: ID::new(0),
                        })],
                        constants: Vec::new(),
                    },
                }),
            ],
            constants: Vec::new(),
        },
    });

    let semantic = semantic::Default;
    let premises = Premises::default();
    let table = Table::<Success>::default();

    assert!(!enum_ty.definite(
        &premises,
        &table,
        &semantic,
        &mut session::Default::default()
    ));
}

#[test]
fn definite_by_equlity() {
    // Enum0(bool, Enum0(?T))
    let enum_ty: Type<Symbolic> = Type::Symbol(Symbol {
        id: SymbolKindID::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![
                Type::Primitive(Primitive::Bool),
                Type::Symbol(Symbol {
                    id: SymbolKindID::Enum(ID::new(0)),
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Parameter(TypeParameterID {
                            parent: GenericID::Enum(ID::new(0)),
                            id: ID::new(0),
                        })],
                        constants: Vec::new(),
                    },
                }),
            ],
            constants: Vec::new(),
        },
    });

    let semantic = semantic::Default;
    let premises = Premises {
        non_equality_predicates: vec![],
        mapping: {
            Mapping::from_pairs(
                std::iter::empty(),
                std::iter::once((
                    Type::Parameter(TypeParameterID {
                        parent: GenericID::Enum(ID::new(0)),
                        id: ID::new(0),
                    }),
                    Type::Primitive(Primitive::Bool),
                )),
                std::iter::empty(),
            )
        },
    };
    let table = Table::<Success>::default();

    assert!(enum_ty.definite(
        &premises,
        &table,
        &semantic,
        &mut session::Default::default()
    ));
}
