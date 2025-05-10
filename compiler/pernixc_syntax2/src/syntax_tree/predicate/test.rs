use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, predicate::strategy::Predicate, strategy::IndentDisplayItem,
};

proptest! {
    #[test]
    fn predicate(
        predicate_input in Predicate::arbitrary()
    ) {
        let source = IndentDisplayItem(0, &predicate_input).to_string();
        let item = syntax_tree::test::parse(&source)?;

        predicate_input.assert(&item)?;
    }
}
