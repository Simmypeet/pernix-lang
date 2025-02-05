use pernixc_syntax::{syntax_tree, utility::parse};
use pernixc_term::r#type::{Primitive, Type};

use crate::{
    binding::{
        diagnostic::InvalidCastType,
        test::{BindExt, Template},
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
