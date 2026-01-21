#![allow(missing_docs)]

use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy};

use crate::{Global, TargetID};

impl Arbitrary for TargetID {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (proptest::num::u64::ANY, proptest::num::u64::ANY)
            .prop_map(|(lo, hi)| Self { lo, hi })
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
