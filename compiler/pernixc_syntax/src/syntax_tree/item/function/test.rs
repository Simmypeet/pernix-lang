use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, item::function::strategy::Function, strategy::IndentDisplayItem,
};

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 819_200,
        ..proptest::test_runner::Config::default()
    })]

    #[test]
    fn function(
        function_input in Function::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &function_input).to_string();
        let func = syntax_tree::test::parse(&source)?;

        function_input.assert(&func)?;
    }
}
