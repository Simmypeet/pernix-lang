use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{self, predicate::strategy::Predicate};

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 819_200,
        ..proptest::test_runner::Config::default()
    })]
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls, clippy::ignored_unit_patterns)]
    fn predicate(
        predicate_input in Predicate::arbitrary()
    ) {
        let source = predicate_input.to_string();

        let item = syntax_tree::test::parse(&source)?;

        predicate_input.assert(&item)?;
    }
}
