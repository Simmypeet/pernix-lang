use pernixc_lexical::kind::{arbitrary, arbitrary::Identifier};
use pernixc_parser::expect;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just},
    prop_oneof,
    strategy::Strategy,
};

use crate::reference;

reference! {
    #[derive(Debug, Clone, Copy, derive_more::Display)]
    pub enum Boolean for super::Boolean {
        #{prop_assert(|x| x.kind == expect::Keyword::True)}
        True,

        #{prop_assert(|x| x.kind == expect::Keyword::False)}
        False
    }
}

impl Arbitrary for Boolean {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![Just(Self::True), Just(Self::False),].boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(".{digits}")]
    pub struct Decimal for super::Decimal {
        #{map_input_assert(digits, &digits.kind)}
        pub digits (arbitrary::Numeric)
    }
}

impl Arbitrary for Decimal {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        arbitrary::Numeric::arbitrary()
            .prop_map(|digits| Self { digits })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "{numeric}{}{}",
        decimal.as_ref().map_or_else(String::default, ToString::to_string),
        identifier.as_ref().map_or_else(String::default, ToString::to_string)
    )]
    pub struct Numeric for super::Numeric {
        #{map_input_assert(numeric, &numeric.kind)}
        pub numeric (arbitrary::Numeric),

        pub decimal (Option<Decimal>),

        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Option<Identifier>)
    }
}
