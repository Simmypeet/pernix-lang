use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, item::r#trait::strategy::Trait, strategy::IndentDisplayItem,
};

proptest! {
    #[test]
    fn r#trait(
        trait_input in Trait::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &trait_input).to_string();
        let tr = syntax_tree::test::parse(&source)?;

        trait_input.assert(&tr)?;
    }
}
