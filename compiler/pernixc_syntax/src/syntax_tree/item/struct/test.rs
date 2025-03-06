use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, item::r#struct::strategy::Struct, strategy::IndentDisplayItem,
};

proptest! {
    #[test]
    fn r#struct(
        struct_input in Struct::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &struct_input).to_string();
        let st = syntax_tree::test::parse(&source)?;

        struct_input.assert(&st)?;
    }
}
