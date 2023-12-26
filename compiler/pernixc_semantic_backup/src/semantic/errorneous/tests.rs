use crate::{
    arena::ID,
    semantic::term::{
        constant::{self, Constant},
        lifetime::Lifetime,
        r#type::{self, Algebraic, Type},
        GenericArguments, Term,
    },
    symbol::{semantic::Symbolic, AlgebraicKind},
};

#[test]
fn non_errorneous() {
    let ty: Type<Symbolic> = Type::Algebraic(Algebraic {
        kind: AlgebraicKind::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: vec![Lifetime::Static],
            types: vec![Type::Primitive(r#type::Primitive::Bool)],
            constants: vec![Constant::Primitive(constant::Primitive::Bool(true))],
        },
    });

    assert!(!ty.errorneous());
}

#[test]
fn trivially_errorneous() {
    let ty: Type<Symbolic> = Type::Error;

    assert!(ty.errorneous());
}

#[test]
fn sub_term_errorneous() {
    let ty: Type<Symbolic> = Type::Algebraic(Algebraic {
        kind: AlgebraicKind::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: vec![Lifetime::Static, Lifetime::Error],
            types: vec![Type::Primitive(r#type::Primitive::Bool), Type::Error],
            constants: vec![Constant::Primitive(constant::Primitive::Bool(true))],
        },
    });

    assert!(ty.errorneous());
}
