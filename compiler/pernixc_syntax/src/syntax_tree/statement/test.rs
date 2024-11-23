use pernixc_tests::input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{self, statement::strategy::Statement};

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls, clippy::ignored_unit_patterns)]
    fn statement(
        statement_input in Statement::arbitrary()
    ) {
        let source = statement_input.to_string();

        let statement = syntax_tree::test::parse(&source)?;

        statement_input.assert(&statement)?;
    }
}
