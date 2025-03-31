use pernixc_syntax::{syntax_tree, utility::parse};
use pernixc_semantic::term::r#type::{Primitive, Type};

use crate::{
    binding::{
        diagnostic::{
            FloatingPointLiteralHasIntegralSuffix, InvalidNumericSuffix,
        },
        test::{BindExt, Template},
    },
    model::Constraint,
};

#[test]
fn numeric_literal_suffix() {
    const SOURCE: &str = r"432u64";

    let template = Template::new();
    let mut binder = template.create_binder();

    let numeric_literal = binder
        .bind_as_rvalue_success(
            &parse::<syntax_tree::expression::unit::Numeric>(SOURCE),
        )
        .into_literal()
        .unwrap()
        .into_numeric()
        .unwrap();

    assert!(numeric_literal.decimal_stirng.is_none());
    assert_eq!(numeric_literal.integer_string, "432");
    assert_eq!(numeric_literal.r#type, Type::Primitive(Primitive::Uint64));
}

#[test]
fn numberic_literal_float_infer() {
    const SOURCE: &str = r"32.0";

    let template = Template::new();
    let mut binder = template.create_binder();

    let numeric_literal = binder
        .bind_as_rvalue_success(
            &parse::<syntax_tree::expression::unit::Numeric>(SOURCE),
        )
        .into_literal()
        .unwrap()
        .into_numeric()
        .unwrap();

    assert_eq!(numeric_literal.integer_string, "32");
    assert_eq!(
        numeric_literal.decimal_stirng.as_ref().map(AsRef::as_ref),
        Some("0")
    );
    let inference_variable = numeric_literal.r#type.as_inference().unwrap();

    let constraint_id = binder
        .inference_context
        .get_inference(*inference_variable)
        .cloned()
        .unwrap()
        .into_inferring()
        .unwrap();

    assert_eq!(
        binder.inference_context.get_constraint::<Type<_>>(constraint_id),
        Some(&Constraint::Floating)
    );
}

#[test]
fn numeric_literal_number_infer() {
    const SOURCE: &str = r"32";

    let template = Template::new();
    let mut binder = template.create_binder();

    let numeric_literal = binder
        .bind_as_rvalue_success(
            &parse::<syntax_tree::expression::unit::Numeric>(SOURCE),
        )
        .into_literal()
        .unwrap()
        .into_numeric()
        .unwrap();

    assert_eq!(numeric_literal.integer_string, "32");
    assert!(numeric_literal.decimal_stirng.is_none());

    let inference_variable = numeric_literal.r#type.as_inference().unwrap();

    let constraint_id = binder
        .inference_context
        .get_inference(*inference_variable)
        .cloned()
        .unwrap()
        .into_inferring()
        .unwrap();

    assert_eq!(
        binder.inference_context.get_constraint::<Type<_>>(constraint_id),
        Some(&Constraint::Number)
    );
}

#[test]
fn invalid_numeric_literal_suffix() {
    const SOURCE: &str = r"32goofy";

    let template = Template::new();
    let mut binder = template.create_binder();

    let errors = binder.bind_as_rvalue_error_fatal(&parse::<
        syntax_tree::expression::unit::Numeric,
    >(SOURCE));

    assert_eq!(errors.len(), 1);
    let error =
        errors[0].as_any().downcast_ref::<InvalidNumericSuffix>().unwrap();

    assert_eq!(error.suffix_span.str(), "goofy");
}

#[test]
fn floating_point_literal_has_integral_suffix() {
    const SOURCE: &str = r"32.0i64";

    let template = Template::new();
    let mut binder = template.create_binder();

    let errors = binder.bind_as_rvalue_error_fatal(&parse::<
        syntax_tree::expression::unit::Numeric,
    >(SOURCE));

    assert_eq!(errors.len(), 1);
    assert!(errors[0]
        .as_any()
        .downcast_ref::<FloatingPointLiteralHasIntegralSuffix>()
        .is_some());
}
