use std::{borrow::Cow, sync::Arc};

use pernixc_query::Engine;
use pernixc_symbol::kind::Kind;
use pernixc_target::{Global, TargetID};
use pernixc_term::{
    generic_arguments::{GenericArguments, MemberSymbol, Symbol, TraitMember},
    generic_parameters::LifetimeParameterID,
    lifetime::Lifetime,
    predicate::{Compatible, Outlives, Predicate},
    r#type::{Primitive, Type},
};

use crate::{
    environment::{Environment, Premise},
    lifetime_constraint::LifetimeConstraint,
    normalizer,
    simplify::Simplify,
};

#[tokio::test]
async fn basic() {
    let trait_member = TraitMember(MemberSymbol {
        id: Global::new(TargetID::TEST, pernixc_symbol::ID(1)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });

    let equivalent = Type::Primitive(Primitive::Bool);

    let engine = Arc::new(Engine::default());

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::TraitTypeCompatible(Compatible {
        lhs: trait_member.clone(),
        rhs: equivalent.clone(),
    }));

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked()),
        normalizer::NO_OP,
    );

    let result = environment
        .query(&Simplify(Type::TraitMember(trait_member)))
        .await
        .unwrap()
        .unwrap();

    assert_eq!(result.result, equivalent);
    assert!(result.constraints.is_empty());
}

#[tokio::test]
async fn sub_term() {
    let trait_member = TraitMember(MemberSymbol {
        id: Global::new(TargetID::TEST, pernixc_symbol::ID(1)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });

    let equivalent = Type::Primitive(Primitive::Bool);
    let engine = Arc::new(Engine::default());

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::TraitTypeCompatible(Compatible {
        lhs: trait_member.clone(),
        rhs: equivalent.clone(),
    }));

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked()),
        normalizer::NO_OP,
    );

    let result = environment
        .query(&Simplify(Type::Symbol(Symbol {
            id: Global::new(TargetID::TEST, pernixc_symbol::ID(2)),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Symbol(Symbol {
                    id: Global::new(TargetID::TEST, pernixc_symbol::ID(2)),
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::TraitMember(trait_member)],
                        constants: Vec::new(),
                    },
                })],
                constants: Vec::new(),
            },
        })))
        .await
        .unwrap()
        .unwrap();

    assert_eq!(
        result.result,
        Type::Symbol(Symbol {
            id: Global::new(TargetID::TEST, pernixc_symbol::ID(2)),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Symbol(Symbol {
                    id: Global::new(TargetID::TEST, pernixc_symbol::ID(2)),
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![equivalent],
                        constants: Vec::new(),
                    },
                })],
                constants: Vec::new(),
            },
        })
    );

    assert!(result.constraints.is_empty());
}

#[tokio::test]
async fn already_simplified() {
    let trait_member = TraitMember(MemberSymbol {
        id: Global::new(TargetID::TEST, pernixc_symbol::ID(1)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });

    let equivalent = Type::Primitive(Primitive::Bool);
    let engine = Arc::new(Engine::default());

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::TraitTypeCompatible(Compatible {
        lhs: trait_member,
        rhs: equivalent.clone(),
    }));

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked()),
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
        parent_id: Global::new(TargetID::TEST, pernixc_symbol::ID(1)),
        id: pernixc_arena::ID::new(0),
    });
    let second_lifetime = Lifetime::Parameter(LifetimeParameterID {
        parent_id: Global::new(TargetID::TEST, pernixc_symbol::ID(1)),
        id: pernixc_arena::ID::new(1),
    });

    let to_be_simplified = TraitMember(MemberSymbol {
        id: Global::new(TargetID::TEST, pernixc_symbol::ID(2)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments {
            lifetimes: vec![first_lifetime],
            types: Vec::new(),
            constants: Vec::new(),
        },
    });

    let trait_member = TraitMember(MemberSymbol {
        id: Global::new(TargetID::TEST, pernixc_symbol::ID(2)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments {
            lifetimes: vec![second_lifetime],
            types: Vec::new(),
            constants: Vec::new(),
        },
    });

    let equivalent = Type::Primitive(Primitive::Bool);
    let mut engine = Arc::new(Engine::default());
    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(
                pernixc_symbol::parent::Key(Global::new(
                    TargetID::TEST,
                    pernixc_symbol::ID(2),
                )),
                Some(pernixc_symbol::ID(3)),
            )
            .await;

            x.set_input(
                pernixc_symbol::kind::Key(Global::new(
                    TargetID::TEST,
                    pernixc_symbol::ID(3),
                )),
                Kind::Trait,
            )
            .await;

            x.set_input(
                pernixc_term::implemented::Key(Global::new(
                    TargetID::TEST,
                    pernixc_symbol::ID(3),
                )),
                Arc::default(),
            )
            .await;
        })
        .await;

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::TraitTypeCompatible(Compatible {
        lhs: trait_member,
        rhs: equivalent.clone(),
    }));

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked()),
        normalizer::NO_OP,
    );
    let result = environment
        .query(&Simplify(Type::TraitMember(to_be_simplified)))
        .await
        .unwrap()
        .unwrap();

    assert_eq!(result.result, equivalent);
    assert_eq!(result.constraints.len(), 2);

    assert!(result.constraints.contains(
        &LifetimeConstraint::LifetimeOutlives(Outlives::new(
            second_lifetime,
            first_lifetime
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
    let first_trait_member = TraitMember(MemberSymbol {
        id: Global::new(TargetID::TEST, pernixc_symbol::ID(1)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });
    let second_trait_member = TraitMember(MemberSymbol {
        id: Global::new(TargetID::TEST, pernixc_symbol::ID(2)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });
    let equivalent = Type::Primitive(Primitive::Bool);

    let mut engine = Arc::new(Engine::default());

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |session| {
            session
                .set_input(
                    pernixc_symbol::parent::Key(Global::new(
                        TargetID::TEST,
                        pernixc_symbol::ID(1),
                    )),
                    Some(pernixc_symbol::ID(3)),
                )
                .await;
            session
                .set_input(
                    pernixc_symbol::parent::Key(Global::new(
                        TargetID::TEST,
                        pernixc_symbol::ID(2),
                    )),
                    Some(pernixc_symbol::ID(3)),
                )
                .await;
            session
                .set_input(
                    pernixc_symbol::kind::Key(Global::new(
                        TargetID::TEST,
                        pernixc_symbol::ID(3),
                    )),
                    Kind::Trait,
                )
                .await;
            session
                .set_input(
                    pernixc_term::implemented::Key(Global::new(
                        TargetID::TEST,
                        pernixc_symbol::ID(3),
                    )),
                    Arc::default(),
                )
                .await;
        })
        .await;

    let mut premise = Premise::default();
    premise.predicates.extend([
        Predicate::TraitTypeCompatible(Compatible {
            lhs: first_trait_member.clone(),
            rhs: equivalent.clone(),
        }),
        Predicate::TraitTypeCompatible(Compatible {
            lhs: second_trait_member.clone(),
            rhs: equivalent.clone(),
        }),
    ]);

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked()),
        normalizer::NO_OP,
    );

    let result1 = environment
        .query(&Simplify(Type::TraitMember(first_trait_member)))
        .await
        .unwrap()
        .unwrap();

    let result2 = environment
        .query(&Simplify(Type::TraitMember(second_trait_member)))
        .await
        .unwrap()
        .unwrap();

    assert!(result1.result == equivalent);
    assert!(result1.constraints.is_empty());

    assert!(result2.result == equivalent);
    assert!(result2.constraints.is_empty());
}

#[tokio::test]
async fn transitive() {
    // TraitA['a] = TraitB['b]
    // TraitB['c] = bool
    //
    // final result is TraitA['a] = bool
    // lifetime constraint is 'b = 'c

    let trait_member = |idx, lifetime| {
        TraitMember(MemberSymbol {
            id: Global::new(TargetID::TEST, pernixc_symbol::ID(idx)),
            member_generic_arguments: GenericArguments::default(),
            parent_generic_arguments: GenericArguments {
                lifetimes: vec![lifetime],
                types: Vec::new(),
                constants: Vec::new(),
            },
        })
    };

    let lt = |idx| {
        Lifetime::Parameter(LifetimeParameterID {
            parent_id: Global::new(TargetID::TEST, pernixc_symbol::ID(1)),
            id: pernixc_arena::ID::new(idx),
        })
    };

    let a_lt = lt(0);
    let b_lt = lt(1);
    let c_lt = lt(2);

    let trait_a = trait_member(2, a_lt);
    let trait_b_b = trait_member(3, b_lt);
    let trait_b_c = trait_member(3, c_lt);
    let equivalent = Type::Primitive(Primitive::Bool);

    let mut engine = Arc::new(Engine::default());

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |table| {
            table
                .set_input(
                    pernixc_symbol::parent::Key(Global::new(
                        TargetID::TEST,
                        pernixc_symbol::ID(2),
                    )),
                    Some(pernixc_symbol::ID(4)),
                )
                .await;
            table
                .set_input(
                    pernixc_symbol::parent::Key(Global::new(
                        TargetID::TEST,
                        pernixc_symbol::ID(3),
                    )),
                    Some(pernixc_symbol::ID(4)),
                )
                .await;

            table
                .set_input(
                    pernixc_symbol::kind::Key(Global::new(
                        TargetID::TEST,
                        pernixc_symbol::ID(4),
                    )),
                    Kind::Trait,
                )
                .await;
            table
                .set_input(
                    pernixc_term::implemented::Key(Global::new(
                        TargetID::TEST,
                        pernixc_symbol::ID(4),
                    )),
                    Arc::default(),
                )
                .await;
        })
        .await;

    let premise = Premise {
        predicates: [
            Predicate::TraitTypeCompatible(Compatible {
                lhs: trait_a.clone(),
                rhs: Type::TraitMember(trait_b_b),
            }),
            Predicate::TraitTypeCompatible(Compatible {
                lhs: trait_b_c,
                rhs: equivalent.clone(),
            }),
        ]
        .into_iter()
        .collect(),
        query_site: None,
    };

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked()),
        normalizer::NO_OP,
    );
    let result = environment
        .query(&Simplify(Type::TraitMember(trait_a)))
        .await
        .unwrap()
        .unwrap();

    assert_eq!(result.result, equivalent);
    assert_eq!(result.constraints.len(), 2);

    assert!(result.constraints.contains(
        &LifetimeConstraint::LifetimeOutlives(Outlives::new(b_lt, c_lt))
    ));
    assert!(result.constraints.contains(
        &LifetimeConstraint::LifetimeOutlives(Outlives::new(c_lt, b_lt))
    ));
}
