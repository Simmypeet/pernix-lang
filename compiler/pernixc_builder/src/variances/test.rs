use pernixc_term::{
    generic_parameter::GenericParameters,
    variance::{Variance, Variances},
};

use crate::utility::build_table;

const BASIC_VARIANCES: &str = r"
public struct Test[Co, In]:
    public covariant: (Co,)
    public invariant: &'static mut In
";

#[test]
fn basic_variances() {
    let (table, _) = build_table(BASIC_VARIANCES);

    let test_id = table.get_by_qualified_name(["test", "Test"]).unwrap();

    let generic_parameters = table.query::<GenericParameters>(test_id).unwrap();

    let co_ty = generic_parameters.type_parameter_ids_by_name()["Co"];
    let in_ty = generic_parameters.type_parameter_ids_by_name()["In"];

    let variance_map = table.query::<Variances>(test_id).unwrap();

    assert_eq!(variance_map.variances_by_type_ids[&co_ty], Variance::Covariant);
    assert_eq!(variance_map.variances_by_type_ids[&in_ty], Variance::Invariant);
}

const RECURSIVE_INVARIANT: &str = r"
public enum A['a, T: 'a]: 
    Invariant(&'a mut T)
    Covariant(B['a, T])

public struct B['a, T: 'a]:
    public a: A['a, T]
";

#[test]
fn recurisve_invariant() {
    let (table, _) = build_table(RECURSIVE_INVARIANT);

    let a_id = table.get_by_qualified_name(["test", "A"]).unwrap();
    let b_id = table.get_by_qualified_name(["test", "B"]).unwrap();

    let a_generic_parameters = table.query::<GenericParameters>(a_id).unwrap();
    let b_generic_parameters = table.query::<GenericParameters>(b_id).unwrap();

    let a_a_ty = a_generic_parameters.type_parameter_ids_by_name()["T"];
    let b_a_ty = b_generic_parameters.type_parameter_ids_by_name()["T"];

    let a_a_lt = a_generic_parameters.lifetime_parameter_ids_by_name()["a"];
    let b_a_lt = b_generic_parameters.lifetime_parameter_ids_by_name()["a"];

    let variance_map_a = table.query::<Variances>(a_id).unwrap();
    let variance_map_b = table.query::<Variances>(b_id).unwrap();

    assert_eq!(
        variance_map_a.variances_by_type_ids[&a_a_ty],
        Variance::Invariant
    );
    assert_eq!(
        variance_map_b.variances_by_type_ids[&b_a_ty],
        Variance::Invariant
    );

    assert_eq!(
        variance_map_a.variances_by_lifetime_ids[&a_a_lt],
        Variance::Covariant
    );
    assert_eq!(
        variance_map_b.variances_by_lifetime_ids[&b_a_lt],
        Variance::Covariant
    );
}

const LIFETIME_INVARIANT: &str = r"
public struct Ref['a, T: 'a]:
    public value: &'a T


public struct Test['a, 'b, T: 'a]:
    public test: &'a mut Ref['b, T]
";

#[test]
#[allow(clippy::similar_names)]
fn lifetime_invariant() {
    let (table, _) = build_table(LIFETIME_INVARIANT);

    let test_id = table.get_by_qualified_name(["test", "Test"]).unwrap();

    let generic_parameters = table.query::<GenericParameters>(test_id).unwrap();

    let test_a_lt = generic_parameters.lifetime_parameter_ids_by_name()["a"];
    let test_b_lt = generic_parameters.lifetime_parameter_ids_by_name()["b"];
    let test_t_ty = generic_parameters.type_parameter_ids_by_name()["T"];

    let variance_map = table.query::<Variances>(test_id).unwrap();

    assert_eq!(
        variance_map.variances_by_lifetime_ids[&test_a_lt],
        Variance::Covariant
    );
    assert_eq!(
        variance_map.variances_by_lifetime_ids[&test_b_lt],
        Variance::Invariant
    );
    assert_eq!(
        variance_map.variances_by_type_ids[&test_t_ty],
        Variance::Invariant
    );

    let ref_id = table.get_by_qualified_name(["test", "Ref"]).unwrap();

    let ref_generic_parameters =
        table.query::<GenericParameters>(ref_id).unwrap();

    let ref_a_lt = ref_generic_parameters.lifetime_parameter_ids_by_name()["a"];
    let ref_t_ty = ref_generic_parameters.type_parameter_ids_by_name()["T"];

    let ref_variance_map = table.query::<Variances>(ref_id).unwrap();

    assert_eq!(
        ref_variance_map.variances_by_lifetime_ids[&ref_a_lt],
        Variance::Covariant
    );
    assert_eq!(
        ref_variance_map.variances_by_type_ids[&ref_t_ty],
        Variance::Covariant
    );
}
