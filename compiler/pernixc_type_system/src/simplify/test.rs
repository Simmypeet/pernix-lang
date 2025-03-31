use std::{borrow::Cow, collections::HashSet, sync::Arc};

use pernixc_semantic::{
    component::{Implemented, Parent, SymbolKind},
    GlobalID, Table, TargetID,
};
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameter::LifetimeParameterID,
    lifetime::Lifetime,
    predicate::{Compatible, Outlives, Predicate},
    r#type::{Primitive, TraitMember, Type},
    Default, MemberSymbol, Symbol,
};

use crate::{
    environment::{Environment, Premise},
    normalizer,
    simplify::Simplify,
    LifetimeConstraint,
};

#[test]
fn basic() {
    let trait_member = TraitMember::<Default>(MemberSymbol {
        id: GlobalID::new(TargetID(1), pernixc_semantic::ID(1)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });

    let equivalent = Type::Primitive(Primitive::Bool);

    let table = Table::new(Arc::new(pernixc_handler::Panic));

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::TraitTypeCompatible(Compatible {
        lhs: trait_member.clone(),
        rhs: equivalent.clone(),
    }));

    let environment =
        Environment::new(Cow::Borrowed(&premise), &table, normalizer::NO_OP);

    let result = environment
        .query(&Simplify(Type::TraitMember(trait_member)))
        .unwrap()
        .unwrap();

    assert_eq!(result.result, equivalent);
    assert!(result.constraints.is_empty());
}

#[test]
fn sub_term() {
    let trait_member = TraitMember::<Default>(MemberSymbol {
        id: GlobalID::new(TargetID(1), pernixc_semantic::ID(1)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });

    let equivalent = Type::Primitive(Primitive::Bool);
    let table = Table::new(Arc::new(pernixc_handler::Panic));

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::TraitTypeCompatible(Compatible {
        lhs: trait_member.clone(),
        rhs: equivalent.clone(),
    }));

    let environment =
        Environment::new(Cow::Borrowed(&premise), &table, normalizer::NO_OP);

    let result = environment
        .query(&Simplify(Type::Symbol(Symbol {
            id: GlobalID::new(TargetID(1), pernixc_semantic::ID(2)),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Symbol(Symbol {
                    id: GlobalID::new(TargetID(1), pernixc_semantic::ID(2)),
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::TraitMember(trait_member)],
                        constants: Vec::new(),
                    },
                })],
                constants: Vec::new(),
            },
        })))
        .unwrap()
        .unwrap();

    assert_eq!(
        result.result,
        Type::Symbol(Symbol {
            id: GlobalID::new(TargetID(1), pernixc_semantic::ID(2)),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Symbol(Symbol {
                    id: GlobalID::new(TargetID(1), pernixc_semantic::ID(2)),
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

#[test]
fn already_simplified() {
    let trait_member = TraitMember::<Default>(MemberSymbol {
        id: GlobalID::new(TargetID(1), pernixc_semantic::ID(1)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });

    let equivalent = Type::Primitive(Primitive::Bool);
    let table = Table::new(Arc::new(pernixc_handler::Panic));

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::TraitTypeCompatible(Compatible {
        lhs: trait_member,
        rhs: equivalent.clone(),
    }));

    let environment =
        Environment::new(Cow::Borrowed(&premise), &table, normalizer::NO_OP);
    let result =
        environment.query(&Simplify(equivalent.clone())).unwrap().unwrap();

    assert!(result.result == equivalent);
    assert!(result.constraints.is_empty());
}

#[test]
fn with_lifetime_matching() {
    let first_lifetime = Lifetime::Parameter(LifetimeParameterID {
        parent: GlobalID::new(TargetID(1), pernixc_semantic::ID(1)),
        id: pernixc_arena::ID::new(0),
    });
    let second_lifetime = Lifetime::Parameter(LifetimeParameterID {
        parent: GlobalID::new(TargetID(1), pernixc_semantic::ID(1)),
        id: pernixc_arena::ID::new(1),
    });

    let to_be_simplified = TraitMember::<Default>(MemberSymbol {
        id: GlobalID::new(TargetID(1), pernixc_semantic::ID(2)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments {
            lifetimes: vec![first_lifetime],
            types: Vec::new(),
            constants: Vec::new(),
        },
    });

    let trait_member = TraitMember::<Default>(MemberSymbol {
        id: GlobalID::new(TargetID(1), pernixc_semantic::ID(2)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments {
            lifetimes: vec![second_lifetime],
            types: Vec::new(),
            constants: Vec::new(),
        },
    });

    let equivalent = Type::Primitive(Primitive::Bool);
    let table = Table::new(Arc::new(pernixc_handler::Panic));

    assert!(table.add_component(
        GlobalID::new(TargetID(1), pernixc_semantic::ID(2)),
        Parent { parent: Some(pernixc_semantic::ID(3)) }
    ));
    assert!(table.add_component(
        GlobalID::new(TargetID(1), pernixc_semantic::ID(3)),
        SymbolKind::Trait
    ));
    assert!(table.add_component(
        GlobalID::new(TargetID(1), pernixc_semantic::ID(3)),
        Implemented(HashSet::new())
    ));

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::TraitTypeCompatible(Compatible {
        lhs: trait_member,
        rhs: equivalent.clone(),
    }));

    let environment =
        Environment::new(Cow::Borrowed(&premise), &table, normalizer::NO_OP);
    let result = environment
        .query(&Simplify(Type::TraitMember(to_be_simplified)))
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

#[test]
fn multiple_equivalences() {
    let first_trait_member = TraitMember::<Default>(MemberSymbol {
        id: GlobalID::new(TargetID(1), pernixc_semantic::ID(1)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });
    let second_trait_member = TraitMember::<Default>(MemberSymbol {
        id: GlobalID::new(TargetID(1), pernixc_semantic::ID(2)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });
    let equivalent = Type::Primitive(Primitive::Bool);

    let table = Table::new(Arc::new(pernixc_handler::Panic));
    assert!(table.add_component(
        GlobalID::new(TargetID(1), pernixc_semantic::ID(1)),
        Parent { parent: Some(pernixc_semantic::ID(3)) }
    ));
    assert!(table.add_component(
        GlobalID::new(TargetID(1), pernixc_semantic::ID(2)),
        Parent { parent: Some(pernixc_semantic::ID(3)) }
    ));
    assert!(table.add_component(
        GlobalID::new(TargetID(1), pernixc_semantic::ID(3)),
        SymbolKind::Trait
    ));
    assert!(table.add_component(
        GlobalID::new(TargetID(1), pernixc_semantic::ID(3)),
        Implemented(HashSet::new())
    ));
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

    let environment =
        Environment::new(Cow::Borrowed(&premise), &table, normalizer::NO_OP);

    let result1 = environment
        .query(&Simplify(Type::TraitMember(first_trait_member)))
        .unwrap()
        .unwrap();

    let result2 = environment
        .query(&Simplify(Type::TraitMember(second_trait_member)))
        .unwrap()
        .unwrap();

    assert!(result1.result == equivalent);
    assert!(result1.constraints.is_empty());

    assert!(result2.result == equivalent);
    assert!(result2.constraints.is_empty());
}

#[test]
fn transitive() {
    // TraitA['a] = TraitB['b]
    // TraitB['c] = bool
    //
    // final result is TraitA['a] = bool
    // lifetime constraint is 'b = 'c

    let trait_member = |idx, lifetime| {
        TraitMember::<Default>(MemberSymbol {
            id: GlobalID::new(TargetID(1), pernixc_semantic::ID(idx)),
            member_generic_arguments: GenericArguments::default(),
            parent_generic_arguments: GenericArguments {
                lifetimes: vec![lifetime],
                types: Vec::new(),
                constants: Vec::new(),
            },
        })
    };

    let lt = |idx| {
        Lifetime::<Default>::Parameter(LifetimeParameterID {
            parent: GlobalID::new(TargetID(1), pernixc_semantic::ID(1)),
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

    let table = Table::new(Arc::new(pernixc_handler::Panic));

    assert!(table.add_component(
        GlobalID::new(TargetID(1), pernixc_semantic::ID(2)),
        Parent { parent: Some(pernixc_semantic::ID(4)) }
    ));
    assert!(table.add_component(
        GlobalID::new(TargetID(1), pernixc_semantic::ID(3)),
        Parent { parent: Some(pernixc_semantic::ID(4)) }
    ));

    assert!(table.add_component(
        GlobalID::new(TargetID(1), pernixc_semantic::ID(4)),
        SymbolKind::Trait,
    ));
    assert!(table.add_component(
        GlobalID::new(TargetID(1), pernixc_semantic::ID(4)),
        Implemented(HashSet::new()),
    ));

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

    let environment =
        Environment::new(Cow::Borrowed(&premise), &table, normalizer::NO_OP);
    let result = environment
        .query(&Simplify(Type::TraitMember(trait_a)))
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
