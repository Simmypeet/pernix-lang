use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, item::implements::strategy::Implements, strategy::IndentDisplayItem,
};

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 819_200,
        ..proptest::test_runner::Config::default()
    })]

    #[test]
    fn implements(
        impl_input in Implements::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &impl_input).to_string();
        let im = syntax_tree::test::parse(&source)?;

        impl_input.assert(&im)?;
    }
}
