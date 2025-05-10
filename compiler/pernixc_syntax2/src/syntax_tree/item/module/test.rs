use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, item::module::strategy::Module, strategy::IndentDisplayItem,
};

proptest! {
    #[test]
    fn module(
        module_input in Module::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &module_input).to_string();
        let module = syntax_tree::test::parse(&source)?;

        module_input.assert(&module)?;
    }
}
