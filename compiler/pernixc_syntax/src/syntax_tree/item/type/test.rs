use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, item::r#type::strategy::Type, strategy::IndentDisplayItem,
};

proptest! {
    #[test]
    fn r#type(
        type_input in Type::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &type_input).to_string();
        let ty = syntax_tree::test::parse(&source)?;

        type_input.assert(&ty)?;
    }
}
