use pernixc_test_input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{
    self, item::r#extern::strategy::Extern, strategy::IndentDisplayItem,
};

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 819_200,
        ..proptest::test_runner::Config::default()
    })]

    #[test]
    fn r#extern(
        extern_input in Extern::arbitrary(),
    ) {
        let source = IndentDisplayItem(0, &extern_input).to_string();
        let ex = syntax_tree::test::parse(&source)?;

        extern_input.assert(&ex)?;
    }
}
