use std::sync::Arc;

use pernixc_handler::Panic;
use pernixc_source_file::Span;
use pernixc_table::{
    component::{Implemented, Implements, Member, Parent},
    GlobalID, Table,
};
use pernixc_term::{
    generic_parameter::{
        ConstantParameterID, GenericKind, GenericParameters, LifetimeParameter,
        LifetimeParameterID, TypeParameter, TypeParameterID,
    },
    lifetime::Lifetime,
    predicate::{Outlives, Predicate},
    r#type::Type,
    Default,
};

use crate::{
    implementation_coherence::diagnostic::{
        AmbiguousImplementation, FinalImplementationCannotBeOverriden,
        ImplementedForeignAdt, MismatchedGenericParameterCountInImplementation,
        MismatchedImplementationConstantTypeParameter, OrphanRuleViolation,
        UnusedGenericParameterInImplementation,
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

const AMBIGUOUS_IMPLEMENTATION: &str = r"
public trait Trait[T, U] {}

implements[T] Trait[T, int32] {}
implements[T] Trait[int32, T] {}
";

#[test]
fn ambiguous_implementation() {
    let (table, errors) = build_table(AMBIGUOUS_IMPLEMENTATION);

    assert_eq!(errors.len(), 1);

    let trait_id = table.get_by_qualified_name(["test", "Trait"]).unwrap();
    let implementations = table.get::<Implemented>(trait_id);

    let error =
        errors[0].as_any().downcast_ref::<AmbiguousImplementation>().unwrap();

    assert_eq!(implementations.len(), 2);

    assert!(implementations.contains(&error.first_implementation_id));
    assert!(implementations.contains(&error.second_implementation_id));
}

const GENERIC_PARAMETER_COUNT_MISMATCHED: &str = r"
public trait Trait[V] {
    public type Output['a, 'b, T, U, const X: T];

    public function foo['a, 'b, T, U, const X: T]();
}

implements Trait[int32] {
    public type Output['a, T] = int32;

    public function foo['a, T, U, const X: T]() {}
}
";

#[test]
fn generic_parameter_count_mismatched() {
    let (table, errors) = build_table(GENERIC_PARAMETER_COUNT_MISMATCHED);

    assert_eq!(errors.len(), 3);

    let output_id =
        table.get_by_qualified_name(["test", "Trait", "Output"]).unwrap();

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<MismatchedGenericParameterCountInImplementation>()
        .map_or(false, |x| {
            x.expected_count == 2
                && x.declared_count == 1
                && x.generic_kind == GenericKind::Lifetime
                && x.trait_member_id == output_id
        })));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<MismatchedGenericParameterCountInImplementation>()
        .map_or(false, |x| {
            x.expected_count == 2
                && x.declared_count == 1
                && x.generic_kind == GenericKind::Type
                && x.trait_member_id == output_id
        })));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<MismatchedGenericParameterCountInImplementation>()
        .map_or(false, |x| {
            x.expected_count == 1
                && x.declared_count == 0
                && x.generic_kind == GenericKind::Constant
                && x.trait_member_id == output_id
        })));
}

const CONSTANT_PARAMETER_TYPE_MISMATCHED: &str = r"
public trait Test[A, B] {
    public type Output[
        T, 
        U, 
        const V: A, 
        const W: B, 
        const X: T, 
        const Y: U
    ];
}

implements Test[int32, bool] {
    public type Output[
        T, 
        U, 
        const V: int32,
        const W: (), // mismatched
        const X: T,
        const Y: T  // mismatched
    ] = ();
}
";

#[test]
#[allow(clippy::similar_names)]
fn constant_parameter_type_mismatched() {
    let (table, errors) = build_table(CONSTANT_PARAMETER_TYPE_MISMATCHED);

    assert_eq!(errors.len(), 2);

    let trait_output_id =
        table.get_by_qualified_name(["test", "Test", "Output"]).unwrap();
    let trait_output_generic_params =
        table.query::<GenericParameters>(trait_output_id).unwrap();
    let trait_id = GlobalID::new(
        trait_output_id.target_id,
        table.get::<Parent>(trait_output_id).parent.unwrap(),
    );
    let trait_w_const_id = ConstantParameterID::new(
        trait_output_id,
        trait_output_generic_params.constant_parameter_ids_by_name()["W"],
    );
    let trait_y_const_id = ConstantParameterID::new(
        trait_output_id,
        trait_output_generic_params.constant_parameter_ids_by_name()["Y"],
    );

    let impl_id =
        table.get::<Implemented>(trait_id).iter().copied().next().unwrap();
    let impl_output_id = GlobalID::new(
        impl_id.target_id,
        table.get::<Member>(impl_id)["Output"],
    );
    let impl_output_generic_params =
        table.query::<GenericParameters>(impl_output_id).unwrap();
    let impl_w_const_id = ConstantParameterID::new(
        impl_output_id,
        impl_output_generic_params.constant_parameter_ids_by_name()["W"],
    );
    let impl_y_const_id = ConstantParameterID::new(
        impl_output_id,
        impl_output_generic_params.constant_parameter_ids_by_name()["Y"],
    );

    dbg!(&errors, trait_w_const_id, impl_w_const_id);

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<MismatchedImplementationConstantTypeParameter>()
        .map_or(false, |_| true)));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<MismatchedImplementationConstantTypeParameter>()
        .map_or(false, |x| {
            x.implementation_member_constant_parameter_id == impl_y_const_id
                && x.trait_member_constant_parameter_id == trait_y_const_id
        })));
    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<MismatchedImplementationConstantTypeParameter>()
        .map_or(false, |x| {
            x.implementation_member_constant_parameter_id == impl_w_const_id
                && x.trait_member_constant_parameter_id == trait_w_const_id
        })));
}
