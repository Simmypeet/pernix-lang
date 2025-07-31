#![allow(missing_docs)]

use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy},
    prop_oneof,
};

use crate::{Global, TargetID};

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

impl<ID: Arbitrary<Strategy = BoxedStrategy<ID>> + 'static> Arbitrary
    for Global<ID>
{
    type Parameters = Option<BoxedStrategy<ID>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (TargetID::arbitrary_with(()), args.unwrap_or_else(ID::arbitrary))
            .prop_map(|(target, id)| target.make_global(id))
            .boxed()
    }
}
