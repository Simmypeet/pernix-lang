use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, item::where_clause::strategy::WhereClause,
    strategy::IndentDisplayItem,
};

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 10000,
        ..proptest::test_runner::Config::default()
    })]
    #[test]
    fn where_clause(
        where_clause in WhereClause::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &where_clause).to_string();
        let expression = syntax_tree::test::parse(&source)?;

        where_clause.assert(&expression)?;
    }
}
