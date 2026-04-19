#![allow(missing_docs)]

use pernixc_target::Global;
use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};

use super::{
    ElidedLifetime, ElidedLifetimeID, Forall, FromSemanticElement,
    GeneratedForall, Lifetime,
};
use crate::generic_parameters::LifetimeParameterID;

impl Arbitrary for GeneratedForall {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (Global::arbitrary(), Just(FromSemanticElement::DoEffect), any_usize())
            .prop_map(|(from_id, from_semantic_element, unique_counter)| {
                Self::new(from_id, from_semantic_element, unique_counter)
            })
            .boxed()
    }
}

impl Arbitrary for Forall {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        GeneratedForall::arbitrary().prop_map(Self::Generated).boxed()
    }
}

impl Arbitrary for ElidedLifetime {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        any_usize().prop_map(Self::new).boxed()
    }
}

impl Arbitrary for Lifetime {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            2 => Just(Self::Static),
            1 => Just(Self::Erased),
            2 => LifetimeParameterID::arbitrary().prop_map(Self::Parameter),
            1 => arbitrary_elided_lifetime_id().prop_map(Self::Elided),
            1 => Forall::arbitrary().prop_map(Self::Forall),
            1 => Just(Self::Error(crate::error::Error)),
        ]
        .boxed()
    }
}

fn arbitrary_elided_lifetime_id() -> BoxedStrategy<ElidedLifetimeID> {
    (Global::arbitrary(), pernixc_arena::ID::arbitrary())
        .prop_map(|(parent_id, id)| ElidedLifetimeID::new(parent_id, id))
        .boxed()
}

fn any_usize() -> BoxedStrategy<usize> { usize::arbitrary().boxed() }
