use std::{borrow::Cow, sync::Arc};

use pernixc_query::Engine;
use pernixc_target::{Global, TargetID};
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::TypeParameterID,
    r#type::{Primitive, Type},
};

use crate::{
    environment::{Environment, Premise},
    normalizer,
    order::Order,
};

#[tokio::test]
async fn ambiguous() {
    let t_parameter = Type::Parameter(TypeParameterID {
        parent_id: Global::new(TargetID::Extern(1), pernixc_symbol::ID(1)),
        id: pernixc_arena::ID::new(0),
    });
    let u_parameter = Type::Parameter(TypeParameterID {
        parent_id: Global::new(TargetID::Extern(1), pernixc_symbol::ID(2)),
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

    let engine = Arc::new(Engine::default());
    let premise = Premise::default();

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked()),
        normalizer::NO_OP,
    );

    assert_eq!(environment.order(&lhs, &rhs).await.unwrap(), Order::Ambiguous);
}

#[tokio::test]
async fn more_general() {
    let t_parameter = Type::Parameter(TypeParameterID {
        parent_id: Global::new(TargetID::Extern(1), pernixc_symbol::ID(1)),
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

    let engine = Arc::new(Engine::default());
    let premise = Premise::default();

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked()),
        normalizer::NO_OP,
    );

    assert_eq!(
        environment.order(&lhs, &rhs).await.unwrap(),
        Order::MoreGeneral
    );
}

#[tokio::test]
async fn incompatible() {
    let t_parameter = Type::Parameter(TypeParameterID {
        parent_id: Global::new(TargetID::Extern(1), pernixc_symbol::ID(1)),
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

    let engine = Arc::new(Engine::default());
    let premise = Premise::default();

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked()),
        normalizer::NO_OP,
    );

    assert_eq!(
        environment.order(&lhs, &rhs).await.unwrap(),
        Order::Incompatible
    );
}
