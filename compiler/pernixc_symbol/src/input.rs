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
