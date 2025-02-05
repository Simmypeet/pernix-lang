use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameter::{
        GenericParameters, LifetimeParameterID, TypeParameterID,
    },
    lifetime::Lifetime,
    r#type::{Primitive, TraitMember, Type},
    where_clause::WhereClause,
    MemberSymbol,
};

use crate::{
    utility::build_table,
    where_clause::diagnostic::{
        HigherRankedLifetimeRedefinition, PredicateKind,
        UnexpectedSymbolInPredicate,
    },
};

const WHERE_CLAUSE: &str = r"
public trait FirstTrait[T] {
    public type Member[U];
}

public trait SecondTrait[T] {
    public type Member[U];
}

public trait ThirdTrait[T] {
    public type Member[U];
}

public marker FirstMarker[T];

public marker SecondMarker[T];

public type Test['a, 'b, 'c, T, U, V] = usize
where
    FirstTrait[T]::Member[U] = usize,
    trait FirstTrait[T] + const SecondTrait[U] + !ThirdTrait[V],
    marker FirstMarker[T] + !SecondMarker[U],
    tuple T + U,
    const V + U,
    T: 'a + 'b,
    'a: 'b + 'c; 
";

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn where_clause() {
    let (table, _) = build_table(WHERE_CLAUSE);

    let first_trait_id =
        table.get_by_qualified_name(["test", "FirstTrait"]).unwrap();
    let second_trait_id =
        table.get_by_qualified_name(["test", "SecondTrait"]).unwrap();
    let third_trait_id =
        table.get_by_qualified_name(["test", "ThirdTrait"]).unwrap();
    let first_trait_member_id =
        table.get_by_qualified_name(["test", "FirstTrait", "Member"]).unwrap();

    let first_marker_id =
        table.get_by_qualified_name(["test", "FirstMarker"]).unwrap();
    let second_marker_id =
        table.get_by_qualified_name(["test", "SecondMarker"]).unwrap();

    let test_id = table.get_by_qualified_name(["test", "Test"]).unwrap();

    let generic_parameters = table.query::<GenericParameters>(test_id).unwrap();

    let t_ty = generic_parameters.type_parameter_ids_by_name()["T"];
    let u_ty = generic_parameters.type_parameter_ids_by_name()["U"];
    let v_ty = generic_parameters.type_parameter_ids_by_name()["V"];

    let a_lt = generic_parameters.lifetime_parameter_ids_by_name()["a"];
    let b_lt = generic_parameters.lifetime_parameter_ids_by_name()["b"];
    let c_lt = generic_parameters.lifetime_parameter_ids_by_name()["c"];

    let where_clause = table.query::<WhereClause>(test_id).unwrap();

    assert_eq!(where_clause.predicates.len(), 14);

    let trait_type_compatible = where_clause
        .predicates
        .iter()
        .filter_map(|x| x.predicate.as_trait_type_compatible())
        .collect::<Vec<_>>();

    assert_eq!(trait_type_compatible.len(), 1);
    assert_eq!(
        trait_type_compatible[0].lhs,
        TraitMember(MemberSymbol {
            id: first_trait_member_id,
            member_generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Parameter(TypeParameterID {
                    parent: test_id,
                    id: u_ty
                })],
                constants: Vec::new(),
            },
            parent_generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Parameter(TypeParameterID {
                    parent: test_id,
                    id: t_ty
                })],
                constants: Vec::new(),
            },
        })
    );
    assert_eq!(trait_type_compatible[0].rhs, Type::Primitive(Primitive::Usize));

    let positive_trait = where_clause
        .predicates
        .iter()
        .filter_map(|x| x.predicate.as_positive_trait())
        .collect::<Vec<_>>();
    assert_eq!(positive_trait.len(), 2);
    assert!(positive_trait.iter().any(|x| x.trait_id == first_trait_id
        && !x.is_const
        && x.generic_arguments
            == GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Parameter(TypeParameterID {
                    parent: test_id,
                    id: t_ty
                })],
                constants: Vec::new()
            }));
    assert!(positive_trait.iter().any(|x| x.trait_id == second_trait_id
        && x.is_const
        && x.generic_arguments
            == GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Parameter(TypeParameterID {
                    parent: test_id,
                    id: u_ty
                })],
                constants: Vec::new()
            }));

    let negative_trait = where_clause
        .predicates
        .iter()
        .filter_map(|x| x.predicate.as_negative_trait())
        .collect::<Vec<_>>();

    assert_eq!(negative_trait.len(), 1);
    assert_eq!(negative_trait[0].trait_id, third_trait_id);
    assert_eq!(negative_trait[0].generic_arguments, GenericArguments {
        lifetimes: Vec::new(),
        types: vec![Type::Parameter(TypeParameterID {
            parent: test_id,
            id: v_ty
        })],
        constants: Vec::new()
    });

    let constant_type = where_clause
        .predicates
        .iter()
        .filter_map(|x| x.predicate.as_constant_type())
        .collect::<Vec<_>>();

    assert_eq!(constant_type.len(), 2);
    assert!(constant_type.iter().any(|x| x.0
        == Type::Parameter(TypeParameterID { parent: test_id, id: v_ty })));
    assert!(constant_type.iter().any(|x| x.0
        == Type::Parameter(TypeParameterID { parent: test_id, id: u_ty })));

    let tuple_type = where_clause
        .predicates
        .iter()
        .filter_map(|x| x.predicate.as_tuple_type())
        .collect::<Vec<_>>();

    assert_eq!(tuple_type.len(), 2);
    assert!(tuple_type.iter().any(|x| x.0
        == Type::Parameter(TypeParameterID { parent: test_id, id: t_ty })));
    assert!(tuple_type.iter().any(|x| x.0
        == Type::Parameter(TypeParameterID { parent: test_id, id: u_ty })));

    let positive_marker = where_clause
        .predicates
        .iter()
        .filter_map(|x| x.predicate.as_positive_marker())
        .collect::<Vec<_>>();

    assert_eq!(positive_marker.len(), 1);
    assert_eq!(positive_marker[0].marker_id, first_marker_id);
    assert_eq!(positive_marker[0].generic_arguments, GenericArguments {
        lifetimes: Vec::new(),
        types: vec![Type::Parameter(TypeParameterID {
            parent: test_id,
            id: t_ty
        })],
        constants: Vec::new()
    });

    let negative_marker = where_clause
        .predicates
        .iter()
        .filter_map(|x| x.predicate.as_negative_marker())
        .collect::<Vec<_>>();

    assert_eq!(negative_marker.len(), 1);
    assert_eq!(negative_marker[0].marker_id, second_marker_id);
    assert_eq!(negative_marker[0].generic_arguments, GenericArguments {
        lifetimes: Vec::new(),
        types: vec![Type::Parameter(TypeParameterID {
            parent: test_id,
            id: u_ty
        })],
        constants: Vec::new()
    });

    let lifetime_outlives = where_clause
        .predicates
        .iter()
        .filter_map(|x| x.predicate.as_lifetime_outlives())
        .collect::<Vec<_>>();

    assert_eq!(lifetime_outlives.len(), 2);
    assert!(lifetime_outlives.iter().any(|x| x.operand
        == Lifetime::Parameter(LifetimeParameterID {
            parent: test_id,
            id: a_lt
        })
        && x.bound
            == Lifetime::Parameter(LifetimeParameterID {
                parent: test_id,
                id: b_lt
            })));
    assert!(lifetime_outlives.iter().any(|x| x.operand
        == Lifetime::Parameter(LifetimeParameterID {
            parent: test_id,
            id: a_lt
        })
        && x.bound
            == Lifetime::Parameter(LifetimeParameterID {
                parent: test_id,
                id: c_lt
            })));

    let type_outlives = where_clause
        .predicates
        .iter()
        .filter_map(|x| x.predicate.as_type_outlives())
        .collect::<Vec<_>>();

    assert_eq!(type_outlives.len(), 2);

    assert!(type_outlives.iter().any(|x| x.operand
        == Type::Parameter(TypeParameterID { parent: test_id, id: t_ty })
        && x.bound
            == Lifetime::Parameter(LifetimeParameterID {
                parent: test_id,
                id: a_lt
            })));

    assert!(type_outlives.iter().any(|x| x.operand
        == Type::Parameter(TypeParameterID { parent: test_id, id: t_ty })
        && x.bound
            == Lifetime::Parameter(LifetimeParameterID {
                parent: test_id,
                id: b_lt
            })));
}

const HIGHER_RANKED_LIFETIME_REDEFINITION: &str = r"
public trait SomeTrait['a, T] {}

public type Test[T] = T
where
    trait for['x, 'x] SomeTrait['x, T];
";

#[test]
fn higher_ranked_lifetime_redefinition() {
    let (_, errors) = build_table(HIGHER_RANKED_LIFETIME_REDEFINITION);

    assert_eq!(errors.len(), 1);

    let error = errors[0]
        .as_any()
        .downcast_ref::<HigherRankedLifetimeRedefinition>()
        .unwrap();

    assert_eq!(error.redefinition_span.str(), "'x");
}

const UNEXPECTED_SYMBOL_IN_PREDICATE: &str = r"
public struct Struct[T] { public x: T }

public type Test[T] = T
where
    trait Struct[(T,)],
    marker Struct[(T, T)],
    Struct[(T, T, T)] = usize;
";

#[test]
fn unexpected_symbol_in_predicate() {
    let (table, errors) = build_table(UNEXPECTED_SYMBOL_IN_PREDICATE);

    assert_eq!(errors.len(), 3);

    let struct_id = table.get_by_qualified_name(["test", "Struct"]).unwrap();

    assert!(errors.iter().any(|x| {
        x.as_any().downcast_ref::<UnexpectedSymbolInPredicate>().map_or(
            false,
            |x| {
                x.predicate_kind == PredicateKind::Trait
                    && x.found_id == struct_id
                    && x.qualified_identifier_span.str() == "Struct[(T,)]"
            },
        )
    }));
    assert!(errors.iter().any(|x| {
        x.as_any().downcast_ref::<UnexpectedSymbolInPredicate>().map_or(
            false,
            |x| {
                x.predicate_kind == PredicateKind::Marker
                    && x.found_id == struct_id
                    && x.qualified_identifier_span.str() == "Struct[(T, T)]"
            },
        )
    }));
    assert!(errors.iter().any(|x| {
        x.as_any().downcast_ref::<UnexpectedSymbolInPredicate>().map_or(
            false,
            |x| {
                x.predicate_kind == PredicateKind::TraitTypeEquality
                    && x.found_id == struct_id
                    && x.qualified_identifier_span.str() == "Struct[(T, T, T)]"
            },
        )
    }));
}
