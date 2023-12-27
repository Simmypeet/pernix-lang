use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};

use super::Lifetime;
use crate::{
    arena::ID,
    symbol::{GenericID, LifetimeParameterID},
};

impl Arbitrary for Lifetime {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            1 => Just(Self::Static),
            3 => (GenericID::arbitrary(), ID::arbitrary())
                .prop_map(|(parent, id)| Self::Parameter(LifetimeParameterID { parent, id })),
        ]
        .boxed()
    }
}
