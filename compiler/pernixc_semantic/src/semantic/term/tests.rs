use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Strategy},
};

use super::{constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments, Symbol};
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

impl<ID: Arbitrary> Arbitrary for Symbol<ID>
where
    ID::Strategy: 'static,
{
    type Parameters = (
        Option<BoxedStrategy<Lifetime>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Constant>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((lt_strat, ty_start, const_strat): Self::Parameters) -> Self::Strategy {
        let lt_strat = lt_strat.unwrap_or_else(Lifetime::arbitrary);
        let ty_start = ty_start.unwrap_or_else(Type::arbitrary);
        let const_strat = const_strat.unwrap_or_else(Constant::arbitrary);

        (
            ID::arbitrary(),
            proptest::collection::vec(lt_strat, 0..=2),
            proptest::collection::vec(ty_start, 0..=2),
            proptest::collection::vec(const_strat, 0..=2),
        )
            .prop_map(|(id, lifetimes, types, constants)| Self {
                id,
                generic_arguments: GenericArguments {
                    lifetimes,
                    types,
                    constants,
                },
            })
            .boxed()
    }
}
