use proptest::proptest;

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn expression_test(
        expression_input in super::strategy::expression()
    ) {
        let source = expression_input.to_string();

        let expression = crate::syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_expression(handler)
        )?;

        expression_input.validate(&expression)?;
    }
}
