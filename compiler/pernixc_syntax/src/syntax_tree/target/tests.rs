use proptest::proptest;

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn module_path_test(
        module_path_input in super::strategy::module_path()
    ) {
        let source = module_path_input.to_string();

        println!("{source}");

        let module_path = crate::syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_module_path(handler)
        )?;

        module_path_input.validate(&module_path)?;
    }

    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn using_test(
        using_input in super::strategy::using()
    ) {
        let source = using_input.to_string();

        println!("{source}");

        let using = crate::syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_using(handler)
        )?;

        using_input.validate(&using)?;
    }

    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn module_test(
        module_input in super::strategy::module()
    ) {
        let source = module_input.to_string();

        println!("{source}");

        let module = crate::syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_module(handler)
        )?;

        module_input.validate(&module)?;
    }


    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn file_test(
        file_input in super::strategy::file()
    ) {
        println!("{file_input:#?}");
    }
}
