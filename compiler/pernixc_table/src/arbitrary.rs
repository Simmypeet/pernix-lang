//! Arbitrary implementations for the `ID` types.

use proptest::{
    num::usize,
    prelude::{Arbitrary, BoxedStrategy, Strategy},
};

use crate::{GlobalID, MemberID, TargetID, ID};

impl Arbitrary for ID {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        usize::ANY.prop_map(ID).boxed()
    }
}

impl Arbitrary for TargetID {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        u64::arbitrary().prop_map(TargetID).boxed()
    }
}

impl Arbitrary for GlobalID {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (TargetID::arbitrary(), ID::arbitrary())
            .prop_map(|(x, y)| Self::new(x, y))
            .boxed()
    }
}

impl<ChildID: Arbitrary> Arbitrary for MemberID<ChildID>
where
    ChildID::Strategy: 'static,
{
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (GlobalID::arbitrary(), ChildID::arbitrary())
            .prop_map(|(x, y)| Self::new(x, y))
            .boxed()
    }
}
