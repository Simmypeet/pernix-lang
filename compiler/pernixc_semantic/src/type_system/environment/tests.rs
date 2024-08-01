use crate::{
    arena::ID,
    symbol::{
        table::{Building, Table},
        GenericID, LifetimeParameterID, TypeParameterID,
    },
    type_system::{
        environment::{Environment, NewEnvironmentError},
        equality::Equality,
        model::Default,
        normalizer::NoOp,
        predicate::{self, Predicate},
        term::{
            lifetime::Lifetime,
            r#type::{self, Primitive, Qualifier, Reference, Type},
            GenericArguments,
        },
        Premise, TraitContext,
    },
};

#[test]
fn check_ambiguous_without_equality() {
    /*
     * const trait Test['a, &'b T], trait Test['a, &'static T],
     * trait Test['static, &'static T]
     *
     * const type &'a T, const type &'static T
     *
     * const type &'a U, const type &'static U
     *
     * tuple type T
     * tuple type U
     */

    let t_ty_parameter = Type::Parameter(TypeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(0),
    });
    let u_ty_parameter = Type::Parameter(TypeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(1),
    });

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
                pointee: Box::new(t_ty_parameter.clone()),
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
                pointee: Box::new(t_ty_parameter.clone()),
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
                pointee: Box::new(t_ty_parameter.clone()),
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
            pointee: Box::new(t_ty_parameter.clone()),
        }),
    ));
    let second_constant_type = Predicate::ConstantType(
        predicate::ConstantType(Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: Lifetime::Static,
            pointee: Box::new(t_ty_parameter.clone()),
        })),
    );
    let third_constant_type = Predicate::ConstantType(predicate::ConstantType(
        Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: Lifetime::Parameter(LifetimeParameterID {
                parent: generic_struct,
                id: ID::new(0),
            }),
            pointee: Box::new(u_ty_parameter.clone()),
        }),
    ));
    let fourth_constant_type = Predicate::ConstantType(
        predicate::ConstantType(Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: Lifetime::Static,
            pointee: Box::new(u_ty_parameter.clone()),
        })),
    );

    let first_tuple_type =
        Predicate::TupleType(predicate::Tuple(t_ty_parameter));
    let second_tuple_type =
        Predicate::TupleType(predicate::Tuple(u_ty_parameter));

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
        let NewEnvironmentError::AmbiguousPredicates(error) = error else {
            return false;
        };

        error.len() == 3
            && error.contains(&first_trait)
            && error.contains(&second_trait)
            && error.contains(&third_trait)
    }));

    assert!(errors.iter().any(|error| {
        let NewEnvironmentError::AmbiguousPredicates(error) = error else {
            return false;
        };

        error.len() == 2
            && error.contains(&first_constant_type)
            && error.contains(&second_constant_type)
    }));

    assert!(errors.iter().any(|error| {
        let NewEnvironmentError::AmbiguousPredicates(error) = error else {
            return false;
        };

        error.len() == 2
            && error.contains(&third_constant_type)
            && error.contains(&fourth_constant_type)
    }));

    assert_eq!(environment.premise.predicates.len(), 2);
    assert!(environment.premise.predicates.contains(&first_tuple_type));
    assert!(environment.premise.predicates.contains(&second_tuple_type));
}

#[test]
fn check_non_ambiguous_equality() {
    /*
     * TraitA[T] = bool
     * TraitB[T] = bool
     */

    let t_ty_parameter = Type::Parameter(TypeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(0),
    });

    let first_trait_type_equality = Predicate::TraitTypeEquality(Equality {
        lhs: r#type::TraitMember {
            id: ID::new(0),
            member_generic_arguments: GenericArguments::default(),
            parent_generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![t_ty_parameter.clone()],
                constants: Vec::new(),
            },
        },
        rhs: Type::Primitive(Primitive::Bool),
    });
    let second_trait_type_equality = Predicate::TraitTypeEquality(Equality {
        lhs: r#type::TraitMember {
            id: ID::new(1),
            member_generic_arguments: GenericArguments::default(),
            parent_generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![t_ty_parameter],
                constants: Vec::new(),
            },
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
     * TraitA['a, T] = T
     *
     * TraitB[TraitA['static, T]] = U
     * TraitB[T] = U
     */

    let t_ty_parameter = Type::Parameter(TypeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(0),
    });
    let u_ty_parameter = Type::Parameter(TypeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(1),
    });

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
            types: vec![t_ty_parameter.clone()],
            constants: Vec::new(),
        },
        parent_generic_arguments: GenericArguments::default(),
    };

    let first_trait_type_equality = Predicate::TraitTypeEquality(Equality {
        lhs: trait_a(a_lt),
        rhs: t_ty_parameter.clone(),
    });

    let second_trait_type_equality = Predicate::TraitTypeEquality(Equality {
        lhs: trait_b(Type::TraitMember(trait_a(Lifetime::Static))),
        rhs: u_ty_parameter.clone(),
    });

    let third_trait_type_equality = Predicate::TraitTypeEquality(Equality {
        lhs: trait_b(t_ty_parameter),
        rhs: u_ty_parameter.clone(),
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
        let NewEnvironmentError::AmbiguousPredicates(error) = error else {
            return false;
        };

        error.len() == 2
            && error.contains(&second_trait_type_equality)
            && error.contains(&third_trait_type_equality)
    }));
}

#[test]
fn check_recursive_equality() {
    /*
     * TraitA['a, T] = bool
     *
     * TraitB[bool, T] = TraitB[TraitA['static', T], T]
     */

    let t_ty_parameter = Type::Parameter(TypeParameterID {
        parent: GenericID::Struct(ID::new(0)),
        id: ID::new(0),
    });

    let trait_a = |lifetime| r#type::TraitMember {
        id: ID::new(0),
        member_generic_arguments: GenericArguments {
            lifetimes: vec![lifetime],
            types: vec![t_ty_parameter.clone()],
            constants: Vec::new(),
        },
        parent_generic_arguments: GenericArguments::default(),
    };
    let trait_b = |ty| r#type::TraitMember {
        id: ID::new(1),
        member_generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![ty, t_ty_parameter.clone()],
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
