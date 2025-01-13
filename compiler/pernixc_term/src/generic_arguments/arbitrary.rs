use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy};

use super::GenericArguments;
use crate::{constant::Constant, lifetime::Lifetime, r#type::Type, Default};

impl Arbitrary for GenericArguments<Default> {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Lifetime<Default>>>,
        Option<BoxedStrategy<Type<Default>>>,
        Option<BoxedStrategy<Constant<Default>>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let lt_strat = args.0.unwrap_or_else(Lifetime::arbitrary);
        let ty_start = args.1.unwrap_or_else(Type::arbitrary);
        let const_strat = args.2.unwrap_or_else(Constant::arbitrary);

        (
            proptest::collection::vec(lt_strat, 0..=2),
            proptest::collection::vec(ty_start, 0..=2),
            proptest::collection::vec(const_strat, 0..=2),
        )
            .prop_map(|(lifetimes, types, constants)| Self {
                lifetimes,
                types,
                constants,
            })
            .boxed()
    }
}
