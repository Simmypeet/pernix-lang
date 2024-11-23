use pernixc_tests::input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self,
    pattern::strategy::{Irrefutable, Refutable},
};

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls, clippy::ignored_unit_patterns)]
    fn irrefutable_test(
        irrefutable_input in Irrefutable::arbitrary()
    ) {
        let source = irrefutable_input.to_string();

        let irrefutable = syntax_tree::test::parse(&source)?;

        irrefutable_input.assert(&irrefutable)?;
    }


    #[test]
    #[allow(clippy::redundant_closure_for_method_calls, clippy::ignored_unit_patterns)]
    fn refutable_test(
        refutable_input in Refutable::arbitrary()
    ) {
        let source = refutable_input.to_string();

        let irrefutable = syntax_tree::test::parse(&source)?;

        refutable_input.assert(&irrefutable)?;
    }
}
