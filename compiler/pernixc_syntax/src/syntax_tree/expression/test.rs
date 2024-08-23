use pernixc_tests::input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{self, expression::strategy::Expression};

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 4096,
        ..proptest::test_runner::Config::default()
    })]
    #[allow(clippy::ignored_unit_patterns)]
    #[test]
    fn expression(
        expression_input in Expression::arbitrary(),
    ) {
        let source = expression_input.to_string();
        println!("{source}");
        let expression = syntax_tree::test::parse(
            &source,
            |parser, handler| parser.parse_expression(handler)
        )?;

        expression_input.assert(&expression)?;
    }
}
