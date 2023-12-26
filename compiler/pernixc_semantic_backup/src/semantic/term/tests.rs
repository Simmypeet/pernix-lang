use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Strategy},
};

use crate::{arena::ID, symbol::GenericID};

impl<T: 'static> Arbitrary for ID<T> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::num::usize::ANY.prop_map(Self::new).boxed()
    }
}

impl Arbitrary for GenericID {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            ID::arbitrary().prop_map(Self::Struct),
            ID::arbitrary().prop_map(Self::Trait),
            ID::arbitrary().prop_map(Self::Enum),
            ID::arbitrary().prop_map(Self::Type),
            ID::arbitrary().prop_map(Self::Function),
            ID::arbitrary().prop_map(Self::Constant),
            ID::arbitrary().prop_map(Self::TraitType),
            ID::arbitrary().prop_map(Self::TraitFunction),
            ID::arbitrary().prop_map(Self::TraitConstant),
            ID::arbitrary().prop_map(Self::NegativeTraitImplementation),
            ID::arbitrary().prop_map(Self::TraitImplementation),
            ID::arbitrary().prop_map(Self::TraitImplementationFunction),
            ID::arbitrary().prop_map(Self::TraitImplementationType),
            ID::arbitrary().prop_map(Self::TraitImplementationConstant),
            ID::arbitrary().prop_map(Self::AdtImplementation),
            ID::arbitrary().prop_map(Self::AdtImplementationFunction),
            ID::arbitrary().prop_map(Self::AdtImplementationType),
            ID::arbitrary().prop_map(Self::AdtImplementationConstant),
        ]
        .boxed()
    }
}
