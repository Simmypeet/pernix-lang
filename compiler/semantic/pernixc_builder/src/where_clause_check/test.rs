use pernixc_semantic::{
    component::derived::generic_parameters::{
        GenericParameters, LifetimeParameterID, TypeParameterID,
    },
    term::{
        generic_arguments::GenericArguments,
        lifetime::Lifetime,
        predicate::{Compatible, ConstantType, PositiveTrait, Predicate},
        r#type::{self, Primitive, Qualifier, Reference, Type},
        MemberSymbol,
    },
};

use crate::{
    utility::build_table,
    where_clause_check::diagnostic::{
        AmbiguousPredicates, RecursiveTraitTypeEquality,
    },
};

const CHECK_AMBIGUOUS_WITHOUT_EQUALITY: &str = r"
public trait Test['a, T]:
    pass

public type Fizz['a, 'b, T, U] = int32:
    where:
        trait const Test['a, &'b T] + Test['a, &'static T] 
        trait Test['static, &'static T]

        &'a T: const
        &'static T: const
        &'a U: const
        &'static U: const

        T: tuple + 'static
        U: tuple + 'static
";

#[test]
fn check_ambiguous_without_equality() {
    let (table, errors) = build_table(CHECK_AMBIGUOUS_WITHOUT_EQUALITY);

    assert_eq!(errors.len(), 3);

    let fizz = table.get_by_qualified_name(["test", "Fizz"]).unwrap();
    let test = table.get_by_qualified_name(["test", "Test"]).unwrap();

    let generic_parameters = table.query::<GenericParameters>(fizz).unwrap();

    let a_lt = Lifetime::Parameter(LifetimeParameterID::new(
        fizz,
        generic_parameters.lifetime_parameter_ids_by_name()["a"],
    ));
    let b_lt = Lifetime::Parameter(LifetimeParameterID::new(
        fizz,
        generic_parameters.lifetime_parameter_ids_by_name()["b"],
    ));

    let t_ty = Type::Parameter(TypeParameterID::new(
        fizz,
        generic_parameters.type_parameter_ids_by_name()["T"],
    ));
    let u_ty = Type::Parameter(TypeParameterID::new(
        fizz,
        generic_parameters.type_parameter_ids_by_name()["U"],
    ));

    let create_ref = |lt, ty| {
        Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: lt,
            pointee: Box::new(ty),
        })
    };
    let create_trait = |lt, ref_lt, ref_ty, is_const| {
        Predicate::PositiveTrait(PositiveTrait::new(
            test,
            is_const,
            GenericArguments {
                lifetimes: vec![lt],
                types: vec![Type::Reference(Reference {
                    qualifier: Qualifier::Immutable,
                    lifetime: ref_lt,
                    pointee: Box::new(ref_ty),
                })],
                constants: Vec::new(),
            },
        ))
    };

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<AmbiguousPredicates>()
        .is_some_and(|x| {
            x.predicates.len() == 3
                && x.predicates.contains(&create_trait(
                    a_lt,
                    b_lt,
                    t_ty.clone(),
                    true,
                ))
                && x.predicates.contains(&create_trait(
                    a_lt,
                    Lifetime::Static,
                    t_ty.clone(),
                    false,
                ))
                && x.predicates.contains(&create_trait(
                    Lifetime::Static,
                    Lifetime::Static,
                    t_ty.clone(),
                    false,
                ))
        })));

    let create_const_type =
        |lt, ty| Predicate::ConstantType(ConstantType(create_ref(lt, ty)));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<AmbiguousPredicates>()
        .is_some_and(|x| {
            x.predicates.len() == 2
                && x.predicates.contains(&create_const_type(a_lt, t_ty.clone()))
                && x.predicates.contains(&create_const_type(
                    Lifetime::Static,
                    t_ty.clone(),
                ))
        })));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<AmbiguousPredicates>()
        .is_some_and(|x| {
            x.predicates.len() == 2
                && x.predicates.contains(&create_const_type(a_lt, u_ty.clone()))
                && x.predicates.contains(&create_const_type(
                    Lifetime::Static,
                    u_ty.clone(),
                ))
        })));
}

const CHECK_NON_AMBIGUOUS_EQUALITY: &str = r"
public trait Test[T]:
    public type Output


public type Fizz[T, U] = int32:
    where:
        trait Test[T] + Test[U]
        Test[T]::Output = bool
        Test[U]::Output = bool 
";

#[test]
fn check_non_ambiguous_equality() {
    let (_, errors) = build_table(CHECK_NON_AMBIGUOUS_EQUALITY);

    assert!(errors.is_empty());
}

const CHECK_AMBIGUOUS_EQUALITY: &str = r"
public trait A['a, T]:
    public type Output


public trait B[T]:
    public type Output


public type Fizz['a, T, U] = int32:
    where:
        'a: 'static

        A['a, T]::Output = T

        // these two predicates are the same
        B[A['static, T]::Output]::Output = U
        B[T]::Output = U

        trait A['a, T] + B[T]
";

#[test]
#[allow(clippy::similar_names)]
fn check_ambiguous_equality() {
    let (table, errors) = build_table(CHECK_AMBIGUOUS_EQUALITY);

    let fizz = table.get_by_qualified_name(["test", "Fizz"]).unwrap();
    let a = table.get_by_qualified_name(["test", "A", "Output"]).unwrap();
    let b = table.get_by_qualified_name(["test", "B", "Output"]).unwrap();

    // consequently, there might be overflow error in the type system, which is
    // plausible due to the ambiguity in the where clause
    assert!(!errors.is_empty());

    let generic_parameters = table.query::<GenericParameters>(fizz).unwrap();

    let t_ty = Type::Parameter(TypeParameterID::new(
        fizz,
        generic_parameters.type_parameter_ids_by_name()["T"],
    ));
    let u_ty = Type::Parameter(TypeParameterID::new(
        fizz,
        generic_parameters.type_parameter_ids_by_name()["U"],
    ));

    let create_b_output = |ty| {
        r#type::TraitMember(MemberSymbol {
            id: b,
            member_generic_arguments: GenericArguments::default(),
            parent_generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![ty],
                constants: Vec::new(),
            },
        })
    };

    let create_a_output = |lt, ty| {
        r#type::TraitMember(MemberSymbol {
            id: a,
            member_generic_arguments: GenericArguments::default(),
            parent_generic_arguments: GenericArguments {
                lifetimes: vec![lt],
                types: vec![ty],
                constants: Vec::new(),
            },
        })
    };

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<AmbiguousPredicates>()
        .is_some_and(|x| {
            x.predicates.len() == 2
                && x.predicates.contains(&Predicate::TraitTypeCompatible(
                    Compatible::new(
                        create_b_output(Type::TraitMember(create_a_output(
                            Lifetime::Static,
                            t_ty.clone(),
                        ))),
                        u_ty.clone(),
                    ),
                ))
                && x.predicates.contains(&Predicate::TraitTypeCompatible(
                    Compatible::new(
                        create_b_output(t_ty.clone()),
                        u_ty.clone(),
                    ),
                ))
        })));
}

const CHECK_RECURSIVE_EQUALITY: &str = r"
public trait A['a, T]:
    public type Output


public trait B[T, U]:
    public type Output


public type Fizz['a, T] = int32:
    where:
        'a: 'static

        A['a, T]::Output = bool
        B[bool, T]::Output = B[A['static, T]::Output, T]::Output

        trait A['a, T] + B[bool, T]
";

#[test]
#[allow(clippy::similar_names)]
fn check_recursive_equality() {
    let (table, errors) = build_table(CHECK_RECURSIVE_EQUALITY);

    let fizz = table.get_by_qualified_name(["test", "Fizz"]).unwrap();
    let a = table.get_by_qualified_name(["test", "A", "Output"]).unwrap();
    let b = table.get_by_qualified_name(["test", "B", "Output"]).unwrap();

    assert!(!errors.is_empty());

    let generic_parameters = table.query::<GenericParameters>(fizz).unwrap();

    let t_ty = Type::Parameter(TypeParameterID::new(
        fizz,
        generic_parameters.type_parameter_ids_by_name()["T"],
    ));

    let create_a_output = |lt, ty| {
        r#type::TraitMember(MemberSymbol {
            id: a,
            member_generic_arguments: GenericArguments::default(),
            parent_generic_arguments: GenericArguments {
                lifetimes: vec![lt],
                types: vec![ty],
                constants: Vec::new(),
            },
        })
    };

    let create_b_output = |t_ty, u_ty| {
        r#type::TraitMember(MemberSymbol {
            id: b,
            parent_generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![t_ty, u_ty],
                constants: Vec::new(),
            },
            member_generic_arguments: GenericArguments::default(),
        })
    };

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<RecursiveTraitTypeEquality>()
        .is_some_and(|x| {
            x.trait_type_equality
                == Compatible::new(
                    create_b_output(
                        Type::Primitive(Primitive::Bool),
                        t_ty.clone(),
                    ),
                    Type::TraitMember(create_b_output(
                        Type::TraitMember(create_a_output(
                            Lifetime::Static,
                            t_ty.clone(),
                        )),
                        t_ty.clone(),
                    )),
                )
        })));
}
