use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, expression::strategy::Expression, strategy::IndentDisplayItem,
};

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        cases: 2048,
        ..proptest::test_runner::Config::default()
    })]
    #[test]
    fn expression(
        expression_input in Expression::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &expression_input).to_string();
        let expression = syntax_tree::test::parse(&source)?;

        expression_input.assert(&expression)?;
    }
}
