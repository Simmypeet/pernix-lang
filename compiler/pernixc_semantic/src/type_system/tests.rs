use super::{
    equality::Equality,
    model::Default,
    normalizer::NoOp,
    predicate::{self, Predicate},
    term::{
        lifetime::Lifetime,
        r#type::{self, Primitive, Qualifier, Reference, Type},
        GenericArguments,
    },
    Environment, Premise, TraitContext,
};
use crate::{
    arena::ID,
    symbol::{
        table::{Building, Table},
        GenericID, LifetimeParameterID, TypeParameterID,
    },
    type_system::NewEnvironmentError,
};

#[test]
fn check_ambiguous_without_equality() {
    /*
     * const trait Test['a, &'b int32], trait Test['a, &'static int32],
     * trait Test['static, &'static int32]
     *
     * const type &'a bool, const type &'static bool
     *
     * const type &'a int32, const type &'static int32
     *
     * tuple type T
     * tuple type U
     */

    let generic_struct = GenericID::Struct(ID::new(0));

    let first_trait = Predicate::Trait(predicate::Trait {
        id: ID::new(0),
        is_const: true,
        generic_arguments: GenericArguments {
            lifetimes: vec![Lifetime::Parameter(LifetimeParameterID {
                parent: generic_struct,
                id: ID::new(0),
            })],
            types: vec![Type::Reference(Reference {
                qualifier: Qualifier::Immutable,
                lifetime: Lifetime::Parameter(LifetimeParameterID {
                    parent: generic_struct,
                    id: ID::new(1),
                }),
                pointee: Box::new(Type::Primitive(Primitive::Bool)),
            })],
            constants: Vec::new(),
        },
    });
    let second_trait = Predicate::Trait(predicate::Trait {
        id: ID::new(0),
        is_const: false,
        generic_arguments: GenericArguments {
            lifetimes: vec![Lifetime::Parameter(LifetimeParameterID {
                parent: generic_struct,
                id: ID::new(0),
            })],
            types: vec![Type::Reference(Reference {
                qualifier: Qualifier::Immutable,
                lifetime: Lifetime::Static,
                pointee: Box::new(Type::Primitive(Primitive::Bool)),
            })],
            constants: Vec::new(),
        },
    });
    let third_trait = Predicate::Trait(predicate::Trait {
        id: ID::new(0),
        is_const: false,
        generic_arguments: GenericArguments {
            lifetimes: vec![Lifetime::Static],
            types: vec![Type::Reference(Reference {
                qualifier: Qualifier::Immutable,
                lifetime: Lifetime::Static,
                pointee: Box::new(Type::Primitive(Primitive::Bool)),
            })],
            constants: Vec::new(),
        },
    });

    let first_constant_type = Predicate::ConstantType(predicate::ConstantType(
        Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: Lifetime::Parameter(LifetimeParameterID {
                parent: generic_struct,
                id: ID::new(0),
            }),
            pointee: Box::new(Type::Primitive(Primitive::Bool)),
        }),
    ));
    let second_constant_type = Predicate::ConstantType(
        predicate::ConstantType(Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: Lifetime::Static,
            pointee: Box::new(Type::Primitive(Primitive::Bool)),
        })),
    );
    let third_constant_type = Predicate::ConstantType(predicate::ConstantType(
        Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: Lifetime::Parameter(LifetimeParameterID {
                parent: generic_struct,
                id: ID::new(0),
            }),
            pointee: Box::new(Type::Primitive(Primitive::Int32)),
        }),
    ));
    let fourth_constant_type = Predicate::ConstantType(
        predicate::ConstantType(Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: Lifetime::Static,
            pointee: Box::new(Type::Primitive(Primitive::Int32)),
        })),
    );

    let first_tuple_type =
        Predicate::TupleType(predicate::Tuple(Type::Parameter(
            TypeParameterID { parent: generic_struct, id: ID::new(0) },
        )));
    let second_tuple_type =
        Predicate::TupleType(predicate::Tuple(Type::Parameter(
            TypeParameterID { parent: generic_struct, id: ID::new(1) },
        )));

    let premise: Premise<Default> = Premise {
        predicates: [
            first_trait.clone(),
            second_trait.clone(),
            third_trait.clone(),
            first_constant_type.clone(),
            second_constant_type.clone(),
            third_constant_type.clone(),
            fourth_constant_type.clone(),
            first_tuple_type.clone(),
            second_tuple_type.clone(),
        ]
        .into_iter()
        .collect(),
        trait_context: TraitContext::Normal,
    };

    let table = &Table::<Building>::default();
    let normalizer = NoOp;
    let (environment, errors) = Environment::new(premise, &table, &normalizer);

    assert_eq!(errors.len(), 3);

    assert!(errors.iter().any(|error| {
        let NewEnvironmentError::AmbiguousTraitPredicates(error) = error else {
            return false;
        };

        error.len() == 3
            && error.contains(first_trait.as_trait().unwrap())
            && error.contains(second_trait.as_trait().unwrap())
            && error.contains(third_trait.as_trait().unwrap())
    }));

    assert!(errors.iter().any(|error| {
        let NewEnvironmentError::AmbiguousConstantTypePredicates(error) = error
        else {
            return false;
        };

        error.len() == 2
            && error.contains(first_constant_type.as_constant_type().unwrap())
            && error.contains(second_constant_type.as_constant_type().unwrap())
    }));

    assert!(errors.iter().any(|error| {
        let NewEnvironmentError::AmbiguousConstantTypePredicates(error) = error
        else {
            return false;
        };

        error.len() == 2
            && error.contains(third_constant_type.as_constant_type().unwrap())
            && error.contains(fourth_constant_type.as_constant_type().unwrap())
    }));

    assert_eq!(environment.premise.predicates.len(), 2);
    assert!(environment.premise.predicates.contains(&first_tuple_type));
    assert!(environment.premise.predicates.contains(&second_tuple_type));
}

#[test]
fn check_non_ambiguous_equality() {
    /*
     * TraitA['a] = bool
     * TraitB['a] = bool
     */

    let first_trait_type_equality = Predicate::TraitTypeEquality(Equality {
        lhs: r#type::TraitMember {
            id: ID::new(0),
            member_generic_arguments: GenericArguments::default(),
            parent_generic_arguments: GenericArguments::default(),
        },
        rhs: Type::Primitive(Primitive::Bool),
    });
    let second_trait_type_equality = Predicate::TraitTypeEquality(Equality {
        lhs: r#type::TraitMember {
            id: ID::new(1),
            member_generic_arguments: GenericArguments::default(),
            parent_generic_arguments: GenericArguments::default(),
        },
        rhs: Type::Primitive(Primitive::Bool),
    });

    let premise = Premise::<Default> {
        predicates: [
            first_trait_type_equality.clone(),
            second_trait_type_equality.clone(),
        ]
        .into_iter()
        .collect(),
        trait_context: TraitContext::Normal,
    };

    let table = Table::<Building>::default();
    let normalizer = NoOp;

    let (environment, errors) = Environment::new(premise, &table, &normalizer);

    assert!(errors.is_empty());
    assert_eq!(environment.premise.predicates.len(), 2);
    assert!(environment
        .premise
        .predicates
        .contains(&first_trait_type_equality));
    assert!(environment
        .premise
        .predicates
        .contains(&second_trait_type_equality));
}

#[test]
fn check_ambiguous_equality() {
    /*
     * TraitA['a] = bool
     *
     * TraitB[TraitA['static]] = int32
     * TraitB[bool] = int32
     */

    let a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(0),
    });
    let trait_b = |ty| r#type::TraitMember {
        id: ID::new(1),
        member_generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![ty],
            constants: Vec::new(),
        },
        parent_generic_arguments: GenericArguments::default(),
    };
    let trait_a = |lifetime| r#type::TraitMember {
        id: ID::new(0),
        member_generic_arguments: GenericArguments {
            lifetimes: vec![lifetime],
            types: Vec::new(),
            constants: Vec::new(),
        },
        parent_generic_arguments: GenericArguments::default(),
    };

    let first_trait_type_equality = Predicate::TraitTypeEquality(Equality {
        lhs: trait_a(a_lt),
        rhs: Type::Primitive(Primitive::Bool),
    });

    let second_trait_type_equality = Predicate::TraitTypeEquality(Equality {
        lhs: trait_b(Type::TraitMember(trait_a(Lifetime::Static))),
        rhs: Type::Primitive(Primitive::Int32),
    });

    let third_trait_type_equality = Predicate::TraitTypeEquality(Equality {
        lhs: trait_b(Type::Primitive(Primitive::Bool)),
        rhs: Type::Primitive(Primitive::Int32),
    });

    let premise = Premise::<Default> {
        predicates: [
            first_trait_type_equality.clone(),
            second_trait_type_equality.clone(),
            third_trait_type_equality.clone(),
        ]
        .into_iter()
        .collect(),
        trait_context: TraitContext::Normal,
    };

    let table = Table::<Building>::default();
    let normalizer = NoOp;

    let (environment, errors) = Environment::new(premise, &table, &normalizer);

    assert!(environment.premise.predicates.len() == 1);
    assert!(environment
        .premise
        .predicates
        .contains(&first_trait_type_equality));

    assert_eq!(errors.len(), 1);
    assert!(errors.iter().any(|error| {
        let NewEnvironmentError::AmbiguousTraitTypeEqualityPredicates(error) =
            error
        else {
            return false;
        };

        error.len() == 2
            && error.contains(
                second_trait_type_equality.as_trait_type_equality().unwrap(),
            )
            && error.contains(
                third_trait_type_equality.as_trait_type_equality().unwrap(),
            )
    }));
}

#[test]
fn check_recursive_equality() {
    /*
     * TraitA['a] = bool
     *
     * TraitB[bool] = TraitB[TraitA['static']]
     */

    let trait_a = |lifetime| r#type::TraitMember {
        id: ID::new(0),
        member_generic_arguments: GenericArguments {
            lifetimes: vec![lifetime],
            types: Vec::new(),
            constants: Vec::new(),
        },
        parent_generic_arguments: GenericArguments::default(),
    };
    let trait_b = |ty| r#type::TraitMember {
        id: ID::new(1),
        member_generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![ty],
            constants: Vec::new(),
        },
        parent_generic_arguments: GenericArguments::default(),
    };

    let first_trait_type_equality = Predicate::TraitTypeEquality(Equality {
        lhs: trait_a(Lifetime::Parameter(LifetimeParameterID {
            parent: GenericID::Struct(ID::new(0)),
            id: ID::new(0),
        })),
        rhs: Type::Primitive(Primitive::Bool),
    });
    let second_trait_type_equality = Predicate::TraitTypeEquality(Equality {
        lhs: trait_b(Type::Primitive(Primitive::Bool)),
        rhs: Type::TraitMember(trait_b(Type::TraitMember(trait_a(
            Lifetime::Static,
        )))),
    });
    let premise = Premise::<Default> {
        predicates: [
            first_trait_type_equality.clone(),
            second_trait_type_equality.clone(),
        ]
        .into_iter()
        .collect(),
        trait_context: TraitContext::Normal,
    };

    let table = Table::<Building>::default();
    let normalizer = NoOp;

    let (environment, errors) = Environment::new(premise, &table, &normalizer);

    assert_eq!(errors.len(), 1);
    assert_eq!(environment.premise.predicates.len(), 1);

    assert!(environment
        .premise
        .predicates
        .contains(&first_trait_type_equality));

    assert!(errors.iter().any(|error| {
        let NewEnvironmentError::RecursiveTraitTypeEqualityPredicate(error) =
            error
        else {
            return false;
        };

        error == second_trait_type_equality.as_trait_type_equality().unwrap()
    }));
}
