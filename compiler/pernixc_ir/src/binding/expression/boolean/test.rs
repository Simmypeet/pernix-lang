use pernixc_syntax::{syntax_tree, utility::parse};

use crate::binding::test::{BindExt, Template};

#[test]
fn bind_boolean_literal() {
    const TRUE_SOURCE: &str = "true";
    const FALSE_SOURCE: &str = "false";

    let template = Template::new();
    let mut binder = template.create_binder();

    let true_value = binder
        .bind_as_rvalue_success(&parse::<syntax_tree::expression::Boolean>(
            TRUE_SOURCE,
        ))
        .into_literal()
        .unwrap()
        .into_boolean()
        .unwrap();
    let false_value = binder
        .bind_as_rvalue_success(&parse::<syntax_tree::expression::Boolean>(
            FALSE_SOURCE,
        ))
        .into_literal()
        .unwrap()
        .into_boolean()
        .unwrap();

    assert!(true_value.value);
    assert!(!false_value.value);
}
