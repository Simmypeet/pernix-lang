use proptest::proptest;

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn item_test(
        item_input in super::strategy::item()
    ) {
        let source = item_input.to_string();

        println!("{source}");

        let item = crate::syntax_tree::tests::parse(
            source,
            |parser, handler| parser.parse_item(handler)
        )?;

        item_input.validate(&item)?;
    }
}
