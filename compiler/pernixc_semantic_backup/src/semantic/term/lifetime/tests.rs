use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};

use super::Lifetime;
use crate::{
    arena::ID,
    semantic::{
        model::{Forall, Model},
        term::Never,
    },
    symbol::{GenericID, LifetimeParameterID},
};

impl<
        S: Model<
            TypeInference = Never,
            ConstantInference = Never,
            LifetimeInference = Never,
            ScopedLifetime = Never,
        >,
    > Arbitrary for Lifetime<S>
{
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
