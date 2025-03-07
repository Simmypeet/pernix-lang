use pernixc_component::{
    function_signature::FunctionSignature,
    implied_predicates::{ImpliedPredicate, ImpliedPredicates},
    late_bound::LateBound,
};
use pernixc_resolution::diagnostic::UnexpectedInference;
use pernixc_term::{
    elided_lifetimes::{ElidedLifetimeID, ElidedLifetimes},
    generic_parameter::{GenericKind, GenericParameters, TypeParameterID},
    lifetime::Lifetime,
    predicate::Outlives,
    r#type::{Qualifier, Reference, Type},
};

use crate::utility::build_table;

const IMPLIED_PREDICATES_AND_ELIDED_LIFETIMES: &str = r"
public function test[T](x: &T, y: T) -> &T:
    panic
";

#[test]
fn implied_predicates_and_elided_lifetimes() {
    let (table, errors) = build_table(IMPLIED_PREDICATES_AND_ELIDED_LIFETIMES);

    assert_eq!(errors.len(), 0);

    let test_function_id =
        table.get_by_qualified_name(["test", "test"]).unwrap();

    let generic_parameters =
        table.query::<GenericParameters>(test_function_id).unwrap();

    let t_ty = Type::Parameter(TypeParameterID {
        parent: test_function_id,
        id: generic_parameters.type_parameter_ids_by_name()["T"],
    });

    let elided_lifetimes =
        table.query::<ElidedLifetimes>(test_function_id).unwrap();

    let elided_lifetime = Lifetime::Elided(ElidedLifetimeID {
        parent: test_function_id,
        id: elided_lifetimes.elided_lifetimes.ids().next().unwrap(),
    });

    let implied_predicates =
        table.query::<ImpliedPredicates>(test_function_id).unwrap();

    let ref_t_ty = Type::Reference(Reference {
        qualifier: Qualifier::Immutable,
        lifetime: elided_lifetime,
        pointee: Box::new(t_ty.clone()),
    });

    assert_eq!(implied_predicates.implied_predicates.len(), 1);
    assert!(implied_predicates.implied_predicates.contains(
        &ImpliedPredicate::TypeOutlives(Outlives::new(
            t_ty.clone(),
            elided_lifetime,
        ))
    ));

    let function_signature =
        table.query::<FunctionSignature>(test_function_id).unwrap();

    assert_eq!(function_signature.parameters.len(), 2);
    assert_eq!(function_signature.parameter_order.len(), 2);
    assert_eq!(function_signature.return_type, ref_t_ty);

    let first_parameter =
        &function_signature.parameters[function_signature.parameter_order[0]];
    let second_parameter =
        &function_signature.parameters[function_signature.parameter_order[1]];

    assert_eq!(first_parameter.r#type, ref_t_ty);
    assert_eq!(second_parameter.r#type, t_ty);
}

const ELIDED_LIFETIME_NOT_ALLOWED_IN_RETURN_TYPE: &str = r"
public function test(first: &int32, second: &int32) -> &int32:
    panic
";

#[test]
fn elided_lifetime_not_allowed_in_return_type() {
    let (_, errors) = build_table(ELIDED_LIFETIME_NOT_ALLOWED_IN_RETURN_TYPE);

    assert_eq!(errors.len(), 1, "{errors:?}");

    let error = errors
        .first()
        .unwrap()
        .as_any()
        .downcast_ref::<UnexpectedInference>()
        .unwrap();

    assert_eq!(error.generic_kind, GenericKind::Lifetime);
    assert_eq!(error.unexpected_span.str(), "&");
}

const LATE_BOUND_LIFETIMES: &str = r"
public trait Trait['a, T]:
    pass

public function test['a, 'b, 'c, T](
    x: &'a int32, 
    y: &'b T, 
    z: &'a T
) -> &'c int32:
    where:
        trait Trait['b, T]
        'c: 'c 

    panic

";

#[test]
fn late_bound_lifetimes() {
    let (table, errors) = build_table(LATE_BOUND_LIFETIMES);

    assert_eq!(errors.len(), 0);

    let test_function_id =
        table.get_by_qualified_name(["test", "test"]).unwrap();

    let generic_parameters =
        table.query::<GenericParameters>(test_function_id).unwrap();

    let a = generic_parameters.lifetime_parameter_ids_by_name()["a"];

    let late_bound = table.query::<LateBound>(test_function_id).unwrap();

    assert!(late_bound.contains(&a));
    assert_eq!(late_bound.len(), 1);
}
