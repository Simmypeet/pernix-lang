use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, r#type::strategy::Type, strategy::IndentDisplayItem,
};

proptest! {
    #[test]
    fn r#type(
        type_specifier_input in Type::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &type_specifier_input).to_string();
        let type_specifier = syntax_tree::test::parse(&source)?;

        type_specifier_input.assert(&type_specifier)?;
    }
}
