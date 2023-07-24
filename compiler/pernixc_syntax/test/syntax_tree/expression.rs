use pernix_input::Input;
use proptest::{prelude::Arbitrary, proptest};

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn expression_test(
        expression_input in pernix_syntax_input::syntax_tree::expression::Expression::arbitrary()
    ) {
        let source = expression_input.to_string();

        let expression = super::parse(
            &source,
            |parser, handler| parser.parse_expression(handler)
        )?;

        expression_input.assert(&expression)?;
    }
}
