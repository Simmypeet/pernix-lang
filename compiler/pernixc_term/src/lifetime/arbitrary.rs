//! Implements the [`Arbitrary`] trait for testing purposes.

use pernixc_table::GlobalID;
use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};

use super::{Forall, Lifetime};
use crate::{generic_parameter::LifetimeParameterID, Default};

impl Arbitrary for Lifetime<Default> {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            1 => Just(Self::Static),
            2 => LifetimeParameterID::arbitrary().prop_map(Self::Parameter),
            2 => (GlobalID::arbitrary(), usize::arbitrary())
                .prop_map(|(parent, id)| Self::Forall(Forall {
                    global_id: parent,
                    id,
                    span: None
                }))
        ]
        .boxed()
    }
}
