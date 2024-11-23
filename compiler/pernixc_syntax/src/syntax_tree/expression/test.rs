use pernixc_tests::input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{self, expression::strategy::Expression};

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 10000,
        ..proptest::test_runner::Config::default()
    })]
    #[test]
    fn expression(
        expression_input in Expression::arbitrary(),
    ) {
        let source = expression_input.to_string();
        let expression = syntax_tree::test::parse(&source)?;

        expression_input.assert(&expression)?;
    }
}
