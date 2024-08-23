use pernixc_tests::input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{self, item::strategy::Item};

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 819_200,
        ..proptest::test_runner::Config::default()
    })]
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls, clippy::ignored_unit_patterns)]
    fn item(
        item_input in Item::arbitrary()
    ) {
        let source = item_input.to_string();

        let item = syntax_tree::test::parse(
            &source,
            |parser, handler| parser.parse_item(handler)
        )?;

        item_input.assert(&item)?;
    }
}
