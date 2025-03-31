use std::{borrow::Cow, sync::Arc};

use pernixc_semantic::{
    component::derived::generic_parameters::TypeParameterID,
    table::{self, GlobalID, Table, TargetID},
    term::{
        generic_arguments::GenericArguments,
        r#type::{Primitive, Type},
        Default,
    },
};

use crate::{
    environment::{Environment, Premise},
    normalizer,
    order::Order,
};

#[test]
fn ambiguous() {
    let t_parameter = Type::<Default>::Parameter(TypeParameterID {
        parent: GlobalID::new(TargetID(1), table::ID(1)),
        id: pernixc_arena::ID::new(0),
    });
    let u_parameter = Type::<Default>::Parameter(TypeParameterID {
        parent: GlobalID::new(TargetID(1), table::ID(2)),
        id: pernixc_arena::ID::new(0),
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

    let table = Table::new(Arc::new(pernixc_handler::Panic));
    let premise = Premise::default();

    let environment =
        Environment::new(Cow::Borrowed(&premise), &table, normalizer::NO_OP);

    assert_eq!(environment.order(&lhs, &rhs).unwrap(), Order::Ambiguous);
}

#[test]
fn more_general() {
    let t_parameter = Type::<Default>::Parameter(TypeParameterID {
        parent: GlobalID::new(TargetID(1), table::ID(1)),
        id: pernixc_arena::ID::new(0),
    });

    let lhs = GenericArguments {
        lifetimes: Vec::new(),
        types: vec![Type::Primitive(Primitive::Int32), t_parameter],
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

    let table = Table::new(Arc::new(pernixc_handler::Panic));
    let premise = Premise::default();

    let environment =
        Environment::new(Cow::Borrowed(&premise), &table, normalizer::NO_OP);

    assert_eq!(environment.order(&lhs, &rhs).unwrap(), Order::MoreGeneral);
}

#[test]
fn incompatible() {
    let t_parameter = Type::<Default>::Parameter(TypeParameterID {
        parent: GlobalID::new(TargetID(1), table::ID(1)),
        id: pernixc_arena::ID::new(0),
    });

    let lhs = GenericArguments {
        lifetimes: Vec::new(),
        types: vec![Type::Primitive(Primitive::Int32), t_parameter],
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

    let table = Table::new(Arc::new(pernixc_handler::Panic));
    let premise = Premise::default();

    let environment =
        Environment::new(Cow::Borrowed(&premise), &table, normalizer::NO_OP);

    assert_eq!(environment.order(&lhs, &rhs).unwrap(), Order::Incompatible);
}
