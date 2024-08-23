use pernixc_tests::input::Input;
use proptest::{prelude::Arbitrary, proptest};

use crate::syntax_tree::{self, r#type::strategy::Type};

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls, clippy::ignored_unit_patterns)]
    fn r#type(
        type_specifier_input in Type::arbitrary(),
    ) {
        let source = type_specifier_input.to_string();
        let type_specifier = syntax_tree::test::parse(
            &source,
            |parser, handler| parser.parse_type(handler)
        )?;

        type_specifier_input.assert(&type_specifier)?;
    }
}
