#![allow(missing_docs)]

use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy},
    prop_oneof,
};

use crate::TargetID;

impl Arbitrary for TargetID {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            1 => Just(Self::Core),
            1 => Just(Self::Local),
            4 => u64::arbitrary().prop_map(Self::Extern),
        ]
        .boxed()
    }
}
