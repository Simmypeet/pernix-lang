//! Implements the [`Arbitrary`] trait for testing purposes.

use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};

use super::Lifetime;
use crate::{generic_parameter::LifetimeParameterID, Default};

impl Arbitrary for Lifetime<Default> {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            1 => Just(Self::Static),
            2 => LifetimeParameterID::arbitrary().prop_map(Self::Parameter),
        ]
        .boxed()
    }
}
