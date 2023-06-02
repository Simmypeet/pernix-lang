use proptest::proptest;

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn statement_test(
        statement_input in super::strategy::statement()
    ) {
        let source = statement_input.to_string();

        let statement = crate::syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_statement(handler)
        )?;

        statement_input.validate(&statement)?;
    }
}
