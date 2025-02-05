use pernixc_syntax::{syntax_tree, utility::parse};
use pernixc_term::{
    generic_parameter::{GenericParameters, TypeParameterID},
    r#type::{Primitive, Type},
};
use pernixc_type_system::equality::Equality;

use crate::{
    binding::{
        diagnostic::{
            ExpressionIsNotCallable, InvalidCastType, MismatchedArgumentCount,
        },
        test::{build_table, BindExt, CreateBinderAtExt, Template},
    },
    model,
};

#[test]
fn cast() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let register_id = binder
        .bind_as_rvalue_success(&parse::<syntax_tree::expression::Postfixable>(
            "32i32 as float64",
        ))
        .into_register()
        .unwrap();

    let cast = binder
        .intermediate_representation
        .values
        .registers
        .get(register_id)
        .unwrap()
        .assignment
        .as_cast()
        .unwrap();

    let numeric = cast.value.as_literal().unwrap().as_numeric().unwrap();

    assert_eq!(numeric.integer_string, "32");
    assert!(numeric.decimal_stirng.is_none());

    assert_eq!(cast.r#type, Type::Primitive(Primitive::Float64));
}

#[test]
fn invalid_cast_type() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let errors = binder.bind_as_rvalue_error_fatal(&parse::<
        syntax_tree::expression::Postfixable,
    >("32i32 as ()"));

    assert_eq!(errors.len(), 1);

    let error = errors
        .first()
        .unwrap()
        .as_any()
        .downcast_ref::<InvalidCastType<model::Constrained>>()
        .unwrap();

    assert_eq!(
        error.r#type,
        Type::Tuple(pernixc_term::Tuple { elements: Vec::new() })
    );
}

#[test]
fn expression_is_not_callable_error() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let errors = binder.bind_as_rvalue_error_fatal(&parse::<
        syntax_tree::expression::Postfixable,
    >("32()"));

    assert_eq!(errors.len(), 1);

    let _ = errors
        .first()
        .unwrap()
        .as_any()
        .downcast_ref::<ExpressionIsNotCallable>()
        .unwrap();
}

const FUNCTION_DECLARATION: &str = r"
public function fizz[T](a: T, b: T, c: int32) {}

public function test() {}
";

#[test]
fn function_call() {
    let table = build_table(FUNCTION_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    // no check
    let call_register = binder
        .bind_as_rvalue_success(&parse::<syntax_tree::expression::Postfixable>(
            "fizz(true, false, 32)",
        ))
        .into_register()
        .unwrap();

    let function_call_assignment = binder
        .intermediate_representation
        .values
        .registers
        .get(call_register)
        .unwrap()
        .assignment
        .as_function_call()
        .unwrap();

    let function_id = table.get_by_qualified_name(["test", "fizz"]).unwrap();
    let generic_params = table.query::<GenericParameters>(function_id).unwrap();

    let function_t_ty_param = generic_params.type_parameter_ids_by_name()["T"];

    assert_eq!(function_call_assignment.callable_id, function_id);
    assert_eq!(function_call_assignment.arguments.len(), 3);

    // check if the generic arguments instantiated correctly
    {
        assert!(function_call_assignment.instantiation.lifetimes.is_empty());
        assert_eq!(function_call_assignment.instantiation.types.len(), 1);
        assert!(function_call_assignment.instantiation.constants.is_empty());

        let ty_param = Type::Parameter(TypeParameterID {
            parent: function_id,
            id: function_t_ty_param,
        });
        let instantiated = function_call_assignment
            .instantiation
            .types
            .get(&ty_param)
            .unwrap()
            .clone();

        let environment = binder.create_environment();

        assert!(environment
            .query(&Equality::new(
                instantiated,
                Type::Primitive(Primitive::Bool)
            ))
            .unwrap()
            .unwrap()
            .constraints
            .is_empty());
    }

    // the first arguments is a boolean true
    {
        let first_value = function_call_assignment
            .arguments
            .first()
            .unwrap()
            .as_literal()
            .unwrap()
            .as_boolean()
            .unwrap();

        assert!(first_value.value);
    }

    // the second arguments is a boolean false
    {
        let second_value = function_call_assignment
            .arguments
            .get(1)
            .unwrap()
            .as_literal()
            .unwrap()
            .as_boolean()
            .unwrap();

        assert!(!second_value.value);
    }

    // the third arguments is an integer 32
    {
        let third_value = function_call_assignment
            .arguments
            .get(2)
            .unwrap()
            .as_literal()
            .unwrap()
            .as_numeric()
            .unwrap();

        assert_eq!(third_value.integer_string, "32");
        assert!(third_value.decimal_stirng.is_none());
    }
}

#[test]
fn function_call_mismatched_argument_count_error() {
    let table = build_table(FUNCTION_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);
    let fizz_id = table.get_by_qualified_name(["test", "fizz"]).unwrap();

    // too many arguments
    {
        let (value, errors) = binder.bind_as_rvalue_error(&parse::<
            syntax_tree::expression::Postfixable,
        >(
            "fizz(true, false, 32, 32)",
        ));

        let register_id = value.into_register().unwrap();
        let function_assignment = binder
            .intermediate_representation
            .values
            .registers
            .get(register_id)
            .unwrap()
            .assignment
            .as_function_call()
            .unwrap();

        // should be trimmed to 3
        assert_eq!(function_assignment.arguments.len(), 3);

        assert_eq!(errors.len(), 1);

        let error = errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<MismatchedArgumentCount>()
            .unwrap();

        assert_eq!(error.expected_count, 3);
        assert_eq!(error.found_count, 4);
        assert_eq!(error.called_id, fizz_id);
    }

    // to few
    {
        let (value, errors) =
            binder.bind_as_rvalue_error(&parse::<
                syntax_tree::expression::Postfixable,
            >("fizz(true, false)"));

        let register_id = value.into_register().unwrap();
        let function_assignment = binder
            .intermediate_representation
            .values
            .registers
            .get(register_id)
            .unwrap()
            .assignment
            .as_function_call()
            .unwrap();

        // should be trimmed to 3
        assert_eq!(function_assignment.arguments.len(), 3);

        assert_eq!(errors.len(), 1);

        let error = errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<MismatchedArgumentCount>()
            .unwrap();

        assert_eq!(error.expected_count, 3);
        assert_eq!(error.found_count, 2);
        assert_eq!(error.called_id, fizz_id);
    }
}
