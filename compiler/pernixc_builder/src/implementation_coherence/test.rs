use pernixc_source_file::Span;
use pernixc_table::component::Implemented;
use pernixc_term::{
    generic_parameter::{
        GenericParameters, LifetimeParameter, LifetimeParameterID,
        TypeParameter, TypeParameterID,
    },
    lifetime::Lifetime,
    predicate::{Outlives, Predicate},
    r#type::Type,
    Default,
};

use crate::{
    implementation_coherence::diagnostic::UnusedGenericParameterInImplementation,
    test::build_table, type_system::diagnostic::UnsatisfiedPredicate,
};

const UNUSED_GENERIC_PARAMETERS: &str = r"
public trait Trait['a, 'b, T, U] {}

public trait Another['a, T] {
    public type Output;
}

// 'c, U, V are unused
implements['a, 'b, 'c, T, U, V] Trait['a, 'b, T, Another['c, U]::Output]
where
    trait Another['c, U]
{}
";

#[test]
fn unused_generic_parameters() {
    use UnusedGenericParameterInImplementation as Error;

    let (table, errors) = build_table(UNUSED_GENERIC_PARAMETERS);

    assert_eq!(errors.len(), 3);

    let trait_id = table.get_by_qualified_name(["test", "Trait"]).unwrap();
    let implementation_id =
        table.get::<Implemented>(trait_id).0.iter().copied().next().unwrap();

    let generic_parameters =
        table.query::<GenericParameters>(implementation_id).unwrap();

    let c_lt = generic_parameters.lifetime_parameter_ids_by_name()["c"];
    let u_ty = generic_parameters.type_parameter_ids_by_name()["U"];
    let v_ty = generic_parameters.type_parameter_ids_by_name()["V"];

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<Error<LifetimeParameter>>()
        .map_or(false, |x| x.generic_parameter_id.id == c_lt
            && x.generic_parameter_id.parent == implementation_id)));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<Error<TypeParameter>>()
        .map_or(false, |x| x.generic_parameter_id.id == u_ty
            && x.generic_parameter_id.parent == implementation_id)));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<Error<TypeParameter>>()
        .map_or(false, |x| x.generic_parameter_id.id == v_ty
            && x.generic_parameter_id.parent == implementation_id)));
}

const CHECK_INSTANTIATION_REQUIREMENTS_OF_IMPLEMENTED: &str = r"
public trait Trait['a, T]
where
    T: 'a
{}

implements['a, T] Trait['a, T] {}
";

#[test]
fn check_instantiation_requirements_of_implemented() {
    let (table, errors) =
        build_table(CHECK_INSTANTIATION_REQUIREMENTS_OF_IMPLEMENTED);

    assert_eq!(errors.len(), 1);

    let trait_id = table.get_by_qualified_name(["test", "Trait"]).unwrap();
    let implementation_id =
        table.get::<Implemented>(trait_id).0.iter().copied().next().unwrap();

    let generic_parameters =
        table.query::<GenericParameters>(implementation_id).unwrap();

    let t_ty = generic_parameters.type_parameter_ids_by_name()["T"];
    let a_lt = generic_parameters.lifetime_parameter_ids_by_name()["a"];

    let error = errors[0]
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<Default>>()
        .unwrap();

    assert_eq!(
        error.predicate,
        Predicate::TypeOutlives(Outlives::new(
            Type::Parameter(TypeParameterID::new(implementation_id, t_ty),),
            Lifetime::Parameter(LifetimeParameterID::new(
                implementation_id,
                a_lt
            ))
        ))
    );

    assert_eq!(
        error.predicate_declaration_span.as_ref().map(Span::str),
        Some("T: 'a")
    );
}
