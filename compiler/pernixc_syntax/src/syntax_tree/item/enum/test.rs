use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, item::r#enum::strategy::Enum, strategy::IndentDisplayItem,
};

proptest! {
    #[test]
    fn r#enum(
        enum_input in Enum::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &enum_input).to_string();
        let en = syntax_tree::test::parse(&source)?;

        enum_input.assert(&en)?;
    }
}
