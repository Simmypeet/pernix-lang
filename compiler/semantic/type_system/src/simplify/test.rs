use std::borrow::Cow;

use pernixc_target::{Global, TargetID};
use pernixc_term::{
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::{InstanceParameterID, LifetimeParameterID},
    instance::{Instance, InstanceAssociated},
    lifetime::Lifetime,
    predicate::{Compatible, Outlives, Predicate},
    r#type::{Primitive, Type},
};

use crate::{
    environment::{Environment, Premise},
    lifetime_constraint::LifetimeConstraint,
    normalizer,
    simplify::Simplify,
    test::create_test_engine,
};

#[tokio::test]
async fn basic() {
    let instance_associated = InstanceAssociated::new(
        Box::new(Instance::Parameter(InstanceParameterID::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
            pernixc_arena::ID::new(0),
        ))),
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(2)),
        GenericArguments::default(),
    );

    let equivalent = Type::Primitive(Primitive::Bool);

    let (engine, _dir) = create_test_engine().await;

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::InstanceAssociatedTypeEquality(
        Compatible::new(instance_associated.clone(), equivalent.clone()),
    ));

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );

    let result = environment
        .query(&Simplify(Type::InstanceAssociated(instance_associated)))
        .await
        .unwrap()
        .unwrap();

    assert_eq!(result.result, equivalent);
    assert!(result.constraints.is_empty());
}

#[tokio::test]
async fn sub_term() {
    let instance_associated = InstanceAssociated::new(
        Box::new(Instance::Parameter(InstanceParameterID::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
            pernixc_arena::ID::new(0),
        ))),
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(2)),
        GenericArguments::default(),
    );

    let equivalent = Type::Primitive(Primitive::Bool);
    let (engine, _dir) = create_test_engine().await;

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::InstanceAssociatedTypeEquality(
        Compatible::new(instance_associated.clone(), equivalent.clone()),
    ));

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );

    let result = environment
        .query(&Simplify(Type::Symbol(Symbol::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(3)),
            GenericArguments::new(
                Vec::new(),
                vec![Type::Symbol(Symbol::new(
                    TargetID::TEST
                        .make_global(pernixc_symbol::ID::from_u128(3)),
                    GenericArguments::new(
                        Vec::new(),
                        vec![Type::InstanceAssociated(instance_associated)],
                        Vec::new(),
                        Vec::new(),
                    ),
                ))],
                Vec::new(),
                Vec::new(),
            ),
        ))))
        .await
        .unwrap()
        .unwrap();

    assert_eq!(
        result.result,
        Type::Symbol(Symbol::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(3)),
            GenericArguments::new(
                Vec::new(),
                vec![Type::Symbol(Symbol::new(
                    TargetID::TEST
                        .make_global(pernixc_symbol::ID::from_u128(3)),
                    GenericArguments::new(
                        Vec::new(),
                        vec![equivalent],
                        Vec::new(),
                        Vec::new(),
                    ),
                ))],
                Vec::new(),
                Vec::new(),
            ),
        ))
    );

    assert!(result.constraints.is_empty());
}

#[tokio::test]
async fn already_simplified() {
    let instance_associated = InstanceAssociated::new(
        Box::new(Instance::Parameter(InstanceParameterID::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
            pernixc_arena::ID::new(0),
        ))),
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(2)),
        GenericArguments::default(),
    );

    let equivalent = Type::Primitive(Primitive::Bool);
    let (engine, _dir) = create_test_engine().await;

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::InstanceAssociatedTypeEquality(
        Compatible::new(instance_associated, equivalent.clone()),
    ));

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );
    let result = environment
        .query(&Simplify(equivalent.clone()))
        .await
        .unwrap()
        .unwrap();

    assert!(result.result == equivalent);
    assert!(result.constraints.is_empty());
}

#[tokio::test]
async fn with_lifetime_matching() {
    let first_lifetime = Lifetime::Parameter(LifetimeParameterID {
        parent_id: Global::new(
            TargetID::TEST,
            pernixc_symbol::ID::from_u128(1),
        ),
        id: pernixc_arena::ID::new(0),
    });
    let second_lifetime = Lifetime::Parameter(LifetimeParameterID {
        parent_id: Global::new(
            TargetID::TEST,
            pernixc_symbol::ID::from_u128(1),
        ),
        id: pernixc_arena::ID::new(1),
    });

    let to_be_simplified = InstanceAssociated::new(
        Box::new(Instance::Parameter(InstanceParameterID::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
            pernixc_arena::ID::new(0),
        ))),
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(2)),
        GenericArguments::new(
            vec![first_lifetime.clone()],
            Vec::new(),
            Vec::new(),
            Vec::new(),
        ),
    );

    let instance_associated = InstanceAssociated::new(
        Box::new(Instance::Parameter(InstanceParameterID::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
            pernixc_arena::ID::new(0),
        ))),
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(2)),
        GenericArguments::new(
            vec![second_lifetime.clone()],
            Vec::new(),
            Vec::new(),
            Vec::new(),
        ),
    );

    let equivalent = Type::Primitive(Primitive::Bool);
    let (engine, _dir) = create_test_engine().await;

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::InstanceAssociatedTypeEquality(
        Compatible::new(instance_associated, equivalent.clone()),
    ));

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );
    let result = environment
        .query(&Simplify(Type::InstanceAssociated(to_be_simplified)))
        .await
        .unwrap()
        .unwrap();

    assert_eq!(result.result, equivalent);
    assert_eq!(result.constraints.len(), 2);

    assert!(result.constraints.contains(
        &LifetimeConstraint::LifetimeOutlives(Outlives::new(
            second_lifetime.clone(),
            first_lifetime.clone()
        ))
    ));
    assert!(result.constraints.contains(
        &LifetimeConstraint::LifetimeOutlives(Outlives::new(
            first_lifetime,
            second_lifetime
        ))
    ));
}

#[tokio::test]
async fn multiple_equivalences() {
    let first_instance_associated = InstanceAssociated::new(
        Box::new(Instance::Parameter(InstanceParameterID::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
            pernixc_arena::ID::new(0),
        ))),
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(2)),
        GenericArguments::default(),
    );
    let second_instance_associated = InstanceAssociated::new(
        Box::new(Instance::Parameter(InstanceParameterID::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
            pernixc_arena::ID::new(1),
        ))),
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(2)),
        GenericArguments::default(),
    );
    let equivalent = Type::Primitive(Primitive::Bool);

    let (engine, _dir) = create_test_engine().await;

    let mut premise = Premise::default();
    premise.predicates.extend([
        Predicate::InstanceAssociatedTypeEquality(Compatible::new(
            first_instance_associated.clone(),
            equivalent.clone(),
        )),
        Predicate::InstanceAssociatedTypeEquality(Compatible::new(
            second_instance_associated.clone(),
            equivalent.clone(),
        )),
    ]);

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );

    let result1 = environment
        .query(&Simplify(Type::InstanceAssociated(first_instance_associated)))
        .await
        .unwrap()
        .unwrap();

    let result2 = environment
        .query(&Simplify(Type::InstanceAssociated(second_instance_associated)))
        .await
        .unwrap()
        .unwrap();

    assert!(result1.result == equivalent);
    assert!(result1.constraints.is_empty());

    assert!(result2.result == equivalent);
    assert!(result2.constraints.is_empty());
}

#[tokio::test]
#[allow(clippy::too_many_lines)]
async fn transitive() {
    // I::AssocA['a] = I::AssocB['b]
    // I::AssocB['c] = bool
    //
    // final result is I::AssocA['a] = bool
    // lifetime constraint is 'b = 'c

    let instance_associated = |idx, lifetime| {
        InstanceAssociated::new(
            Box::new(Instance::Parameter(InstanceParameterID::new(
                TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
                pernixc_arena::ID::new(0),
            ))),
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(idx)),
            GenericArguments::new(
                vec![lifetime],
                Vec::new(),
                Vec::new(),
                Vec::new(),
            ),
        )
    };

    let lt = |idx| {
        Lifetime::Parameter(LifetimeParameterID {
            parent_id: Global::new(
                TargetID::TEST,
                pernixc_symbol::ID::from_u128(1),
            ),
            id: pernixc_arena::ID::new(idx),
        })
    };

    let a_lt = lt(0);
    let b_lt = lt(1);
    let c_lt = lt(2);

    let assoc_a = instance_associated(2, a_lt.clone());
    let assoc_b_b = instance_associated(3, b_lt.clone());
    let assoc_b_c = instance_associated(3, c_lt.clone());
    let equivalent = Type::Primitive(Primitive::Bool);

    let (engine, _dir) = create_test_engine().await;

    let premise = Premise {
        predicates: [
            Predicate::InstanceAssociatedTypeEquality(Compatible::new(
                assoc_a.clone(),
                Type::InstanceAssociated(assoc_b_b),
            )),
            Predicate::InstanceAssociatedTypeEquality(Compatible::new(
                assoc_b_c,
                equivalent.clone(),
            )),
        ]
        .into_iter()
        .collect(),
        query_site: None,
    };

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );
    let result = environment
        .query(&Simplify(Type::InstanceAssociated(assoc_a)))
        .await
        .unwrap()
        .unwrap();

    assert_eq!(result.result, equivalent);
    assert_eq!(result.constraints.len(), 2);

    assert!(result.constraints.contains(
        &LifetimeConstraint::LifetimeOutlives(Outlives::new(
            b_lt.clone(),
            c_lt.clone()
        ))
    ));
    assert!(result.constraints.contains(
        &LifetimeConstraint::LifetimeOutlives(Outlives::new(c_lt, b_lt))
    ));
}
