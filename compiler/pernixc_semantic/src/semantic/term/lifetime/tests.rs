use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};

use super::{Forall, Lifetime};
use crate::{
    arena::ID,
    symbol::{GenericID, LifetimeParameterID},
};

impl Arbitrary for Lifetime {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Static),
            (GenericID::arbitrary(), ID::arbitrary())
                .prop_map(|(parent, id)| Self::Parameter(LifetimeParameterID { parent, id })),
            proptest::num::usize::ANY.prop_map(|x| Self::Forall(Forall(x)))
        ]
        .boxed()
    }
}
