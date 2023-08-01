use pernix_input::Input;
use proptest::{prelude::Arbitrary, proptest};

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn item_test(
        item_input in pernix_syntax_input::syntax_tree::item::Item::arbitrary()
    ) {
        let source = item_input.to_string();

        let item = super::parse(
            &source,
            |parser, handler| parser.parse_item(handler)
        )?;

        item_input.assert(&item)?;
    }
}