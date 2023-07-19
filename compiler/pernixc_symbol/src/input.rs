use std::str::FromStr;

use pernixc_lexical::token::KeywordKind;
use proptest::{
    prelude::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};

use crate::Accessibility;

impl Arbitrary for Accessibility {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Public),
            Just(Self::Private),
            Just(Self::Internal)
        ]
        .boxed()
    }
}

/// Creates a strategy for generating valid names for any kind of symbol.
pub fn name() -> impl Strategy<Value = String> {
    "[a-zA-Z_][a-zA-Z0-9_]*".prop_filter("filter out illegal names", |name| {
        !matches!(name.as_str(), "@core" | "main") && KeywordKind::from_str(name).is_err()
    })
}

/// Creates a strategy for generating valid names for modules and targets.
pub fn module_or_target_name() -> impl Strategy<Value = String> {
    "[a-zA-Z_][a-zA-Z0-9_]*".prop_filter_map("filter out illegal names", |mut name| {
        name = name.to_ascii_lowercase();
        if !matches!(name.as_str(), "@core" | "main") && KeywordKind::from_str(&name).is_err() {
            Some(name)
        } else {
            None
        }
    })
}
