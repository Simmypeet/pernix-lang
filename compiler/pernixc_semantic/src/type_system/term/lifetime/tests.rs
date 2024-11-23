use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
};

use super::Lifetime;
use crate::{
    arena::ID,
    symbol::{GenericID, LifetimeParameterID},
    type_system::{
        model::Default,
        term::{constant::Constant, r#type::Type},
    },
};

impl Arbitrary for Lifetime<Default> {
    type Parameters = (
        Option<BoxedStrategy<Type<Default>>>,
        Option<BoxedStrategy<Constant<Default>>>,
    );
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
