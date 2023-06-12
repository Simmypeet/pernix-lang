use pernixc_system::input::Input;
use proptest::{prelude::Arbitrary, proptest};

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn expression_test(
        expression_input in super::input::Expression::arbitrary()
    ) {
        let source = expression_input.to_string();

        let expression = crate::syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_expression(handler)
        )?;

        expression_input.assert(&expression)?;
    }
}
