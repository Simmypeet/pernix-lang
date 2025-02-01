use std::sync::Arc;

use pernixc_handler::Panic;
use pernixc_source_file::Span;
use pernixc_table::{
    component::{Implemented, Implements},
    Table,
};
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
    implementation_coherence::diagnostic::{
        FinalImplementationCannotBeOverriden, ImplementedForeignAdt,
        OrphanRuleViolation, UnusedGenericParameterInImplementation,
    },
    test::{add_target, build_table},
    type_system::diagnostic::UnsatisfiedPredicate,
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

const FOREIGN_ADT: &str = r"
public struct Foreign {}
";

const IMPLEMENTED_FOREIGN_ADT: &str = r"
using {Foreign} from foreign;

implements Foreign {}
";

#[test]
fn implemented_foreign_adt() {
    let mut table = Table::new(Arc::new(Panic));

    let (foreign_id, _) = add_target(
        &mut table,
        std::iter::empty(),
        "foreign".to_string(),
        FOREIGN_ADT,
    );

    let (_, errors) = add_target(
        &mut table,
        std::iter::once(foreign_id),
        "test".to_string(),
        IMPLEMENTED_FOREIGN_ADT,
    );

    assert_eq!(errors.len(), 1);

    let foreign_struct_id =
        table.get_by_qualified_name(["foreign", "Foreign"]).unwrap();

    let error =
        errors[0].as_any().downcast_ref::<ImplementedForeignAdt>().unwrap();

    let implemented_id = table.get::<Implements>(error.adt_implementation_id).0;

    assert_eq!(implemented_id, foreign_struct_id);

    let implemented = table.get::<Implemented>(foreign_struct_id);

    assert_eq!(implemented.len(), 1);
    assert!(implemented.contains(&error.adt_implementation_id));
}

const FOREGIN_TRAIT: &str = r"
public trait ForeignTrait[T, U, V] {}

public struct ForeignStruct {}
";

const IMPLEMENTED_FOREIGN_TRAIT: &str = r"
using {ForeignTrait, ForeignStruct} from foreign;

public trait CurrentTrait[T, U] {
    public type Output;
}

public struct CurrentStruct {}

implements[T] ForeignTrait[
    T, 
    ForeignStruct, 
    CurrentTrait[T, CurrentStruct]::Output
] 
where
    trait CurrentTrait[T, CurrentStruct]
{}

// this is fine
implements[T] ForeignTrait[T, ForeignStruct, CurrentStruct] {}
";

#[test]
fn implemented_foreign_trait() {
    let mut table = Table::new(Arc::new(Panic));

    let (foreign_id, _) = add_target(
        &mut table,
        std::iter::empty(),
        "foreign".to_string(),
        FOREGIN_TRAIT,
    );

    let (_, errors) = add_target(
        &mut table,
        std::iter::once(foreign_id),
        "test".to_string(),
        IMPLEMENTED_FOREIGN_TRAIT,
    );

    assert_eq!(errors.len(), 1);

    let foreign_trait_id =
        table.get_by_qualified_name(["foreign", "ForeignTrait"]).unwrap();

    let error =
        errors[0].as_any().downcast_ref::<OrphanRuleViolation>().unwrap();

    let implemented_id = table.get::<Implements>(error.implementation_id).0;

    assert_eq!(implemented_id, foreign_trait_id);

    let implemented = table.get::<Implemented>(foreign_trait_id);

    assert_eq!(implemented.len(), 2);
    assert!(implemented.contains(&error.implementation_id));
}

const FINAL_IMPLEMENTATION_OVERRIDE: &str = r"
public trait Trait[T, U] {}

final implements[T, U] Trait[T, U] {}

implements[T] Trait[T, T] {}
";

#[test]
fn final_implementation_override() {
    let (table, errors) = build_table(FINAL_IMPLEMENTATION_OVERRIDE);

    assert_eq!(errors.len(), 1);

    let trait_id = table.get_by_qualified_name(["test", "Trait"]).unwrap();
    let implementations = table.get::<Implemented>(trait_id);

    assert_eq!(implementations.len(), 2);

    let error = errors[0]
        .as_any()
        .downcast_ref::<FinalImplementationCannotBeOverriden>()
        .unwrap();

    assert!(implementations.contains(&error.final_implementation_id));
    assert!(implementations.contains(&error.overriden_implementation_id));
}
