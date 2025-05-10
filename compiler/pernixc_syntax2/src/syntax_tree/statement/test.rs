use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, statement::strategy::Statement, strategy::IndentDisplayItem,
};

proptest! {
    #[test]
    fn statement(
        statement_input in Statement::arbitrary()
    ) {
        let source = IndentDisplayItem(0, &statement_input).to_string();
        let statement = syntax_tree::test::parse(&source)?;

        statement_input.assert(&statement)?;
    }
}
