#![allow(missing_docs)]

use pernixc_target::Global;
use proptest::{
    num::u128,
    prelude::{Arbitrary, BoxedStrategy, Strategy},
};

impl Arbitrary for crate::ID {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        u128::ANY.prop_map(Self::from_u128).boxed()
    }
}

impl<InnerID: Arbitrary<Strategy = BoxedStrategy<InnerID>> + 'static> Arbitrary
    for crate::MemberID<InnerID>
{
    type Parameters = Option<BoxedStrategy<InnerID>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let inner_strategy = args.unwrap_or_else(InnerID::arbitrary);

        (Global::arbitrary(), inner_strategy)
            .prop_map(|(global_symbol_id, inner_id)| {
                Self::new(global_symbol_id, inner_id)
            })
            .boxed()
    }
}
