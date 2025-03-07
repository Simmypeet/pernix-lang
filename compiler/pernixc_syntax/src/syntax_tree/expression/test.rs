use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, expression::strategy::Expression, strategy::IndentDisplayItem,
};

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        cases: 10_000,
        ..std::default::Default::default()
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
