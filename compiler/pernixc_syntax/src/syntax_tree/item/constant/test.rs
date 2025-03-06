use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, item::constant::strategy::Constant, strategy::IndentDisplayItem,
};

proptest! {
    #[test]
    fn constant(
        constant_input in Constant::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &constant_input).to_string();
        let constant = syntax_tree::test::parse(&source)?;

        constant_input.assert(&constant)?;
    }
}
