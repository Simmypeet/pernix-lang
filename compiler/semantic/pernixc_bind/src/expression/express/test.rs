use pernixc_syntax::{syntax_tree, utility::parse};

use crate::test::{BindExt, Template};

const SINGLE_EXPRESS_BLOCK: &str = r"
scope:
    express 32
";

#[test]
fn single_express_block() {
    let test_template = Template::new();
    let mut binder = test_template.create_binder();

    let numeric_literal =
        binder
            .bind_as_rvalue_success(&parse::<
                syntax_tree::expression::block::Scope,
            >(SINGLE_EXPRESS_BLOCK))
            .into_literal()
            .unwrap()
            .into_numeric()
            .unwrap();

    assert_eq!(numeric_literal.integer_string, "32");
    assert!(numeric_literal.decimal_stirng.is_none());
}
