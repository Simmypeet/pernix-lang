use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, item::marker::strategy::Marker, strategy::IndentDisplayItem,
};

proptest! {
    #[test]
    fn marker(
        marker_input in Marker::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &marker_input).to_string();
        let mar = syntax_tree::test::parse(&source)?;

        marker_input.assert(&mar)?;
    }
}
