use pernixc_term::{
    generic_parameter::{
        ConstantParameter, GenericKind, GenericParameters, LifetimeParameter,
        TypeParameter, TypeParameterID,
    },
    r#type::{Primitive, Type},
};

use crate::{
    generic_parameters::diagnostic::{
        DefaultGenericParameterMustBeTrailing, DuplicatedGenericParameter,
        MisOrderedGenericParameter,
    },
    utility::build_table,
};

const GENERIC_PARAMETERS: &str = r"
public type Test['a, 'b, T, U, const N: usize, const X: T] = T:
    where:
        T: const
";

#[test]
fn generic_parameters() {
    let (table, errors) = build_table(GENERIC_PARAMETERS);

    assert!(errors.is_empty());

    let id = table.get_by_qualified_name(["test", "Test"]).unwrap();
    let generic_parameters = table.query::<GenericParameters>(id).unwrap();

    assert_eq!(generic_parameters.lifetimes().len(), 2);
    assert_eq!(generic_parameters.types().len(), 2);
    assert_eq!(generic_parameters.constants().len(), 2);

    let a_lt = generic_parameters.lifetime_parameter_ids_by_name()["a"];
    let b_lt = generic_parameters.lifetime_parameter_ids_by_name()["b"];

    let t_ty = generic_parameters.type_parameter_ids_by_name()["T"];
    let u_ty = generic_parameters.type_parameter_ids_by_name()["U"];

    let n_const = generic_parameters.constant_parameter_ids_by_name()["N"];
    let x_const = generic_parameters.constant_parameter_ids_by_name()["X"];

    assert_eq!(*generic_parameters.lifetime_order(), [a_lt, b_lt]);
    assert_eq!(*generic_parameters.type_order(), [t_ty, u_ty]);
    assert_eq!(*generic_parameters.constant_order(), [n_const, x_const]);

    assert_eq!(
        generic_parameters.constants()[n_const].r#type,
        Type::Primitive(Primitive::Usize)
    );
    assert_eq!(
        generic_parameters.constants()[x_const].r#type,
        Type::Parameter(TypeParameterID { parent: id, id: t_ty })
    );
}

const MIS_ORDERED_GENERIC_PARAMETER: &str = r"
public type Test['a, const U: usize, T, 'b] = usize
";

#[test]
fn mis_ordered_generic_parameter() {
    let (_, errors) = build_table(MIS_ORDERED_GENERIC_PARAMETER);

    assert_eq!(errors.len(), 2);

    assert!(errors.iter().any(|x| {
        x.as_any().downcast_ref::<MisOrderedGenericParameter>().is_some_and(
            |x| {
                x.generic_kind == GenericKind::Type
                    && x.generic_parameter_span.str() == "T"
            },
        )
    }));

    assert!(errors.iter().any(|x| {
        x.as_any().downcast_ref::<MisOrderedGenericParameter>().is_some_and(
            |x| {
                x.generic_kind == GenericKind::Lifetime
                    && x.generic_parameter_span.str() == "'b"
            },
        )
    }));
}

const DEFAULT_GENERIC_PARAMETER_MUST_BE_TRAILING: &str = r"
public type Test[T = usize, U] = usize
";

#[test]
fn default_generic_parameter_must_be_trailing() {
    let (_, errors) = build_table(DEFAULT_GENERIC_PARAMETER_MUST_BE_TRAILING);

    assert_eq!(errors.len(), 1);

    let error = errors[0]
        .as_any()
        .downcast_ref::<DefaultGenericParameterMustBeTrailing>()
        .unwrap();

    assert_eq!(error.invalid_generic_default_parameter_span.str(), "= usize");
}

const DUPLICATED_GENERIC_PARAMETER: &str = r"
public type Test['a, 'a, T, T, const N: usize, const N: usize] = usize
";

#[test]
fn duplicated_generic_parameter() {
    let (table, errors) = build_table(DUPLICATED_GENERIC_PARAMETER);

    assert_eq!(errors.len(), 3);

    let id = table.get_by_qualified_name(["test", "Test"]).unwrap();
    let generic_parameters = table.query::<GenericParameters>(id).unwrap();

    assert!(errors.iter().any(|x| {
        x.as_any()
            .downcast_ref::<DuplicatedGenericParameter<LifetimeParameter>>()
            .is_some_and(|x| {
                x.duplicating_generic_parameter_span.str() == "a"
                    && generic_parameters.lifetimes()
                        [x.existing_generic_parameter_id.id]
                        .name
                        == "a"
            })
    }));
    assert!(errors.iter().any(|x| {
        x.as_any()
            .downcast_ref::<DuplicatedGenericParameter<TypeParameter>>()
            .is_some_and(|x| {
                x.duplicating_generic_parameter_span.str() == "T"
                    && generic_parameters.types()
                        [x.existing_generic_parameter_id.id]
                        .name
                        == "T"
            })
    }));
    assert!(errors.iter().any(|x| {
        x.as_any()
            .downcast_ref::<DuplicatedGenericParameter<ConstantParameter>>()
            .is_some_and(|x| {
                x.duplicating_generic_parameter_span.str() == "N"
                    && generic_parameters.constants()
                        [x.existing_generic_parameter_id.id]
                        .name
                        == "N"
            })
    }));
}
