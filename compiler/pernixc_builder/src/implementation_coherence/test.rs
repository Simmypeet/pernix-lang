use std::sync::Arc;

use pernixc_handler::Panic;
use pernixc_semantic::{
    component::{
        derived::generic_parameters::{
            ConstantParameterID, GenericKind, GenericParameters,
            LifetimeParameter, LifetimeParameterID, TypeParameter,
            TypeParameterID,
        },
        input::{Implemented, Implements, Member, Parent},
    },
    table::{GlobalID, Table},
    term::{
        generic_arguments::GenericArguments,
        lifetime::Lifetime,
        predicate::{Outlives, PositiveTrait, Predicate},
        r#type::{Primitive, Type},
        Default,
    },
};
use pernixc_source_file::GlobalSpan;
use pernixc_type_system::diagnostic::UnsatisfiedPredicate;

use crate::{
    implementation_coherence::diagnostic::{
        AmbiguousImplementation, ExtraneousImplementationMemberPredicate,
        FinalImplementationCannotBeOverriden, ImplementedForeignAdt,
        MismatchedGenericParameterCountInImplementation,
        MismatchedImplementationConstantTypeParameter, OrphanRuleViolation,
        UnusedGenericParameterInImplementation,
    },
    utility::{add_target, build_table},
};

const UNUSED_GENERIC_PARAMETERS: &str = r"
public trait Trait['a, 'b, T, U]:
    pass


public trait Another['a, T]:
    public type Output


// 'c, U, V are unused
implements['a, 'b, 'c, T, U, V] Trait['a, 'b, T, Another['c, U]::Output]:
    where:
        trait Another['c, U]
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
        .is_some_and(|x| x.generic_parameter_id.id == c_lt
            && x.generic_parameter_id.parent == implementation_id)));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<Error<TypeParameter>>()
        .is_some_and(|x| x.generic_parameter_id.id == u_ty
            && x.generic_parameter_id.parent == implementation_id)));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<Error<TypeParameter>>()
        .is_some_and(|x| x.generic_parameter_id.id == v_ty
            && x.generic_parameter_id.parent == implementation_id)));
}

const CHECK_INSTANTIATION_REQUIREMENTS_OF_IMPLEMENTED: &str = r"
public trait Trait['a, T]:
    where:
        T: 'a

implements['a, T] Trait['a, T]:
    pass
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
        error.predicate_declaration_span.as_ref().map(GlobalSpan::str),
        Some("'a")
    );
}

const FOREIGN_ADT: &str = r"
public struct Foreign:
    pass
";

const IMPLEMENTED_FOREIGN_ADT: &str = r"
from foreign import Foreign


implements Foreign:
    pass
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
public trait ForeignTrait[T, U, V]:
    pass


public struct ForeignStruct:
    pass
";

const IMPLEMENTED_FOREIGN_TRAIT: &str = r"
from foreign import (ForeignTrait, ForeignStruct)


public trait CurrentTrait[T, U]:
    public type Output


public struct CurrentStruct:
    pass

implements[T] ForeignTrait[
    T, 
    ForeignStruct, 
    CurrentTrait[T, CurrentStruct]::Output
]:
    where:
        trait CurrentTrait[T, CurrentStruct]

// this is fine
implements[T] ForeignTrait[T, ForeignStruct, CurrentStruct]:
    pass
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
public trait Trait[T, U]:
    pass

final implements[T, U] Trait[T, U]:
    pass

implements[T] Trait[T, T]:
    pass
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
public trait Trait[T, U]:
    pass


implements[T] Trait[T, int32]:
    pass


implements[T] Trait[int32, T]:
    pass

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
public trait Trait[V]:
    public type Output['a, 'b, T, U, const X: T]

    public function foo['a, 'b, T, U, const X: T]()


implements Trait[int32]:
    type Output['a, T] = int32

    function foo['a, T, U, const X: T]():
        pass

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
        .is_some_and(|x| {
            x.expected_count == 2
                && x.declared_count == 1
                && x.generic_kind == GenericKind::Lifetime
                && x.trait_member_id == output_id
        })));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<MismatchedGenericParameterCountInImplementation>()
        .is_some_and(|x| {
            x.expected_count == 2
                && x.declared_count == 1
                && x.generic_kind == GenericKind::Type
                && x.trait_member_id == output_id
        })));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<MismatchedGenericParameterCountInImplementation>()
        .is_some_and(|x| {
            x.expected_count == 1
                && x.declared_count == 0
                && x.generic_kind == GenericKind::Constant
                && x.trait_member_id == output_id
        })));
}

const CONSTANT_PARAMETER_TYPE_MISMATCHED: &str = r"
public trait Test[A, B]:
    public type Output[
        T, 
        U, 
        const V: A, 
        const W: B, 
        const X: T, 
        const Y: U
    ]


implements Test[int32, bool]:
    type Output[
        T, 
        U, 
        const V: int32,
        const W: (), // mismatched
        const X: T,
        const Y: T  // mismatched
    ] = ()
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
        table.get::<Parent>(trait_output_id).unwrap(),
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

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<MismatchedImplementationConstantTypeParameter>()
        .is_some_and(|_| true)));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<MismatchedImplementationConstantTypeParameter>()
        .is_some_and(|x| {
            x.implementation_member_constant_parameter_id == impl_y_const_id
                && x.trait_member_constant_parameter_id == trait_y_const_id
        })));
    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<MismatchedImplementationConstantTypeParameter>()
        .is_some_and(|x| {
            x.implementation_member_constant_parameter_id == impl_w_const_id
                && x.trait_member_constant_parameter_id == trait_w_const_id
        })));
}

const IMPLEMENTED_PREDICATE_CHECK: &str = r"
public trait Require[T]:
    pass

public trait Test['a, 'b, T, U] :
    where:
        T: 'a
        U: 'a


    public type Output['c, 'd, X, Y]:
        where:
            X: 'c
            Y: 'd
            trait Require[U]


implements['a, 'b, T] Test['a, 'b, T, int32]:
    where:
        T: 'a

    /*
    missing:
        - Y: 'd
        - trait Require[int32]
    */

    type Output['c, 'd, X, Y] = ():
        where:
            X: 'c
";

#[test]
fn implemented_predicate_check() {
    let (table, errors) = build_table(IMPLEMENTED_PREDICATE_CHECK);

    assert_eq!(errors.len(), 2);

    let test_trait_id = table.get_by_qualified_name(["test", "Test"]).unwrap();
    let test_impl_id =
        table.get::<Implemented>(test_trait_id).iter().copied().next().unwrap();

    let output_impl_id = GlobalID::new(
        test_impl_id.target_id,
        table.get::<Member>(test_impl_id)["Output"],
    );
    let output_impl_generic_params =
        table.query::<GenericParameters>(output_impl_id).unwrap();

    let output_impl_y_ty = Type::Parameter(TypeParameterID::new(
        output_impl_id,
        output_impl_generic_params.type_parameter_ids_by_name()["Y"],
    ));
    let output_impl_d_lt = Lifetime::Parameter(LifetimeParameterID::new(
        output_impl_id,
        output_impl_generic_params.lifetime_parameter_ids_by_name()["d"],
    ));

    let require_trait_id =
        table.get_by_qualified_name(["test", "Require"]).unwrap();

    let expected_trait = Predicate::PositiveTrait(PositiveTrait {
        trait_id: require_trait_id,
        is_const: false,
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![Type::Primitive(Primitive::Int32)],
            constants: Vec::new(),
        },
    });
    let expected_outlives = Predicate::TypeOutlives(Outlives::new(
        output_impl_y_ty,
        output_impl_d_lt,
    ));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<Default>>()
        .is_some_and(|x| {
            x.predicate_declaration_span.as_ref().map(GlobalSpan::str)
                == Some("Require[U]")
                && x.predicate == expected_trait
        })));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<Default>>()
        .is_some_and(|x| {
            x.predicate_declaration_span.as_ref().map(GlobalSpan::str) == Some("'d")
                && x.predicate == expected_outlives
        })));
}

const EXTRANEOUS_PREDICATE_CHECK: &str = r"
public trait Require[T]:
    pass


public trait Test['a, 'b, T, U]:
    public type Output['c, 'd, X, Y]:
        where:
            T: 'a
            X: 'c


implements['a, 'b, T] Test['a, 'b, T, int32]:
    /*
    extraneous:
        - Y: 'd
        - trait Require[Y]
    */

    type Output['c, 'd, X, Y] = ():
        where:
            T: 'a
            X: 'c
            Y: 'd
            trait Require[Y]

";

#[test]
fn extraneous_predicate_check() {
    let (table, errors) = build_table(EXTRANEOUS_PREDICATE_CHECK);

    assert_eq!(errors.len(), 2);

    let test_trait_id = table.get_by_qualified_name(["test", "Test"]).unwrap();
    let test_impl_id =
        table.get::<Implemented>(test_trait_id).iter().copied().next().unwrap();

    let output_impl_id = GlobalID::new(
        test_impl_id.target_id,
        table.get::<Member>(test_impl_id)["Output"],
    );
    let output_impl_generic_params =
        table.query::<GenericParameters>(output_impl_id).unwrap();

    let output_impl_y_ty = Type::Parameter(TypeParameterID::new(
        output_impl_id,
        output_impl_generic_params.type_parameter_ids_by_name()["Y"],
    ));
    let output_impl_d_lt = Lifetime::Parameter(LifetimeParameterID::new(
        output_impl_id,
        output_impl_generic_params.lifetime_parameter_ids_by_name()["d"],
    ));

    let require_trait_id =
        table.get_by_qualified_name(["test", "Require"]).unwrap();

    let expected_trait = Predicate::PositiveTrait(PositiveTrait {
        trait_id: require_trait_id,
        is_const: false,
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![output_impl_y_ty.clone()],
            constants: Vec::new(),
        },
    });
    let expected_outlives = Predicate::TypeOutlives(Outlives::new(
        output_impl_y_ty,
        output_impl_d_lt,
    ));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<ExtraneousImplementationMemberPredicate>()
        .is_some_and(|x| {
            x.trait_implementation_member_id == output_impl_id
                && x.predicate == expected_trait
                && x.predicate_span.str() == "Require[Y]"
        })));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<ExtraneousImplementationMemberPredicate>()
        .is_some_and(|x| {
            x.trait_implementation_member_id == output_impl_id
                && x.predicate == expected_outlives
                && x.predicate_span.str() == "'d"
        })));
}
