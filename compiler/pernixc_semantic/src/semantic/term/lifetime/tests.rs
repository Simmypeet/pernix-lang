use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};

use super::Lifetime;
use crate::{
    arena::ID,
    semantic::term::{constant::Constant, r#type::Type},
    symbol::{GenericID, LifetimeParameterID},
};

impl Arbitrary for Lifetime {
    type Parameters =
        (Option<BoxedStrategy<Type>>, Option<BoxedStrategy<Constant>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((_, _): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            1 => Just(Self::Static),
            4 => (GenericID::arbitrary(), ID::arbitrary())
                .prop_map(|(parent, id)| Self::Parameter(LifetimeParameterID { parent, id })),
        ]
        .boxed()
    }
}
