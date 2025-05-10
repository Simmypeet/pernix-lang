use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, item::generic_parameter::strategy::GenericParameters,
    strategy::IndentDisplayItem,
};

proptest! {
    #[test]
    fn generic_parameters(
        where_clause in GenericParameters::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &where_clause).to_string();
        let expression = syntax_tree::test::parse(&source)?;

        where_clause.assert(&expression)?;
    }
}
