use pernixc_syntax::{syntax_tree, utility::parse};
use pernixc_term::r#type::{Primitive, Type};
use pernixc_type_system::equality::Equality;

use crate::{
    binding::{
        diagnostic::MismatchedType,
        infer,
        test::{BindExt, Template},
    },
    model::{self, Constraint},
    value::Value,
};

#[test]
fn zero_element_array() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let register_id = binder
        .bind_as_rvalue_success(&parse::<syntax_tree::expression::Array>("[]"))
        .into_register()
        .unwrap();

    let array_assignment = binder
        .intermediate_representation
        .values
        .registers
        .get(register_id)
        .unwrap()
        .assignment
        .as_array()
        .unwrap();

    assert!(array_assignment.elements.is_empty());

    let inference =
        array_assignment.element_type.clone().into_inference().unwrap();

    let constraint_id = binder
        .inference_context
        .get_inference(inference)
        .cloned()
        .unwrap()
        .into_inferring()
        .unwrap();

    assert_eq!(
        *binder
            .inference_context
            .get_constraint::<Type<_>>(constraint_id)
            .unwrap(),
        Constraint::All(false)
    );
}

#[test]
fn array() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let register_id = binder
        .bind_as_rvalue_success(&parse::<syntax_tree::expression::Array>(
            "[1, 2, 4us]",
        ))
        .into_register()
        .unwrap();

    let array_assignment = binder
        .intermediate_representation
        .values
        .registers
        .get(register_id)
        .unwrap()
        .assignment
        .as_array()
        .unwrap();

    assert_eq!(array_assignment.elements.len(), 3);

    let expected_type = Type::Primitive(Primitive::Usize);
    let environment = binder.create_environment();

    let check = |value: &Value<infer::Model>, string: &str| {
        let numeric = value.as_literal().unwrap().as_numeric().unwrap();

        assert_eq!(numeric.integer_string, string);
        assert!(numeric.decimal_stirng.is_none());

        let result = environment
            .query(&Equality::new(
                numeric.r#type.clone(),
                expected_type.clone(),
            ))
            .unwrap()
            .unwrap();

        assert!(result.constraints.is_empty());
    };

    check(array_assignment.elements.first().unwrap(), "1");
    check(array_assignment.elements.get(1).unwrap(), "2");
    check(array_assignment.elements.get(2).unwrap(), "4");

    let result = environment
        .query(&Equality::new(
            array_assignment.element_type.clone(),
            expected_type,
        ))
        .unwrap()
        .unwrap();

    assert!(result.constraints.is_empty());
}

#[test]
fn array_type_mismatched_error() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let (value, errors) = binder.bind_as_rvalue_error(&parse::<
        syntax_tree::expression::Array,
    >("[1i32, true]"));

    let register_id = value.into_register().unwrap();

    let array_assignment = binder
        .intermediate_representation
        .values
        .registers
        .get(register_id)
        .unwrap()
        .assignment
        .as_array()
        .unwrap();

    assert_eq!(array_assignment.elements.len(), 2);

    let expected_type = Type::Primitive(Primitive::Int32);
    let environment = binder.create_environment();

    let result = environment
        .query(&Equality::new(
            array_assignment.element_type.clone(),
            expected_type,
        ))
        .unwrap()
        .unwrap();

    assert!(result.constraints.is_empty());

    assert_eq!(errors.len(), 1);
    let error = errors[0]
        .as_any()
        .downcast_ref::<MismatchedType<model::Constrained>>()
        .unwrap();

    assert_eq!(error.expected_type, Type::Primitive(Primitive::Int32));
    assert_eq!(error.found_type, Type::Primitive(Primitive::Bool));
}
