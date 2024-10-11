use crate::{
    arena::ID,
    symbol::{
        table::{representation::NoContainer, Building, Table},
        GenericID, TypeParameterID,
    },
    type_system::{
        environment::Environment,
        model::Default,
        order::Order,
        term::{
            r#type::{Primitive, Type},
            GenericArguments,
        },
        Premise,
    },
};

#[test]
fn ambiguous() {
    let t_parameter = Type::<Default>::Parameter(TypeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(0),
    });
    let u_parameter = Type::<Default>::Parameter(TypeParameterID {
        parent: GenericID::Struct(ID::new(1)),
        id: ID::new(0),
    });

    let lhs = GenericArguments {
        lifetimes: Vec::new(),
        types: vec![Type::Primitive(Primitive::Int32), t_parameter],
        constants: Vec::new(),
    };

    let rhs = GenericArguments {
        lifetimes: Vec::new(),
        types: vec![u_parameter, Type::Primitive(Primitive::Int32)],
        constants: Vec::new(),
    };

    let table = Table::<Building<NoContainer, ()>>::default();

    assert_eq!(
        lhs.order(&rhs, &Environment::new(Premise::default(), &table).0)
            .unwrap(),
        Order::Ambiguous
    );
}

#[test]
fn more_general() {
    let t_parameter = Type::<Default>::Parameter(TypeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(0),
    });

    let lhs = GenericArguments {
        lifetimes: Vec::new(),
        types: vec![Type::Primitive(Primitive::Int32), t_parameter.clone()],
        constants: Vec::new(),
    };

    let rhs = GenericArguments {
        lifetimes: Vec::new(),
        types: vec![
            Type::Primitive(Primitive::Int32),
            Type::Primitive(Primitive::Int32),
        ],
        constants: Vec::new(),
    };

    let table = Table::<Building<NoContainer, ()>>::default();

    assert_eq!(
        lhs.order(&rhs, &Environment::new(Premise::default(), &table).0)
            .unwrap(),
        Order::MoreGeneral
    );
}

#[test]
fn incompatible() {
    let t_parameter = Type::<Default>::Parameter(TypeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(0),
    });

    let lhs = GenericArguments {
        lifetimes: Vec::new(),
        types: vec![Type::Primitive(Primitive::Int32), t_parameter.clone()],
        constants: Vec::new(),
    };

    let rhs = GenericArguments {
        lifetimes: Vec::new(),
        types: vec![
            Type::Primitive(Primitive::Bool),
            Type::Primitive(Primitive::Int32),
        ],
        constants: Vec::new(),
    };

    let table = Table::<Building<NoContainer, ()>>::default();

    assert_eq!(
        lhs.order(&rhs, &Environment::new(Premise::default(), &table).0)
            .unwrap(),
        Order::Incompatible
    );
}
