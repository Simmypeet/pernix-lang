use crate::{
    arena::ID,
    semantic::{
        self,
        predicate::Premises,
        session,
        term::{
            r#type::{Algebraic, Primitive, Type},
            GenericArguments, Term,
        },
    },
    symbol::{semantic::Symbolic, AlgebraicKind, GenericID, TypeParameterID},
    table::Table,
};

#[test]
fn trivially_definite() {
    // primitve type is surely definite
    let primitive: Type<Symbolic> = Type::Primitive(Primitive::Bool);
    let mut semantic = semantic::Default;
    let premises = Premises::default();
    let table = Table::default();

    assert!(primitive.definite(
        &premises,
        &table,
        &mut semantic,
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
    let mut semantic = semantic::Default;
    let premises = Premises::default();
    let table = Table::default();

    assert!(!inference.definite(
        &premises,
        &table,
        &mut semantic,
        &mut session::Default::default()
    ));
}

#[test]
fn adt_definite() {
    // Enum0(bool, Enum0(bool))
    let enum_ty: Type<Symbolic> = Type::Algebraic(Algebraic {
        kind: AlgebraicKind::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![
                Type::Primitive(Primitive::Bool),
                Type::Algebraic(Algebraic {
                    kind: AlgebraicKind::Enum(ID::new(0)),
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
    let mut semantic = semantic::Default;
    let premises = Premises::default();
    let table = Table::default();

    assert!(enum_ty.definite(
        &premises,
        &table,
        &mut semantic,
        &mut session::Default::default()
    ));
}

#[test]
fn adt_indefinite() {
    // Enum0(bool, Enum0(?T))
    let enum_ty: Type<Symbolic> = Type::Algebraic(Algebraic {
        kind: AlgebraicKind::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![
                Type::Primitive(Primitive::Bool),
                Type::Algebraic(Algebraic {
                    kind: AlgebraicKind::Enum(ID::new(0)),
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

    let mut semantic = semantic::Default;
    let premises = Premises::default();
    let table = Table::default();

    assert!(!enum_ty.definite(
        &premises,
        &table,
        &mut semantic,
        &mut session::Default::default()
    ));
}
