use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy};

use crate::term::{
    constant::Constant, generic_arguments::GenericArguments,
    lifetime::Lifetime, r#type::Type, table::GlobalID, Default, MemberSymbol,
    Symbol, Tuple, TupleElement,
};

impl Arbitrary for MemberSymbol<Default> {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Lifetime<Default>>>,
        Option<BoxedStrategy<Type<Default>>>,
        Option<BoxedStrategy<Constant<Default>>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            GlobalID::arbitrary(),
            GenericArguments::arbitrary_with(args.clone()),
            GenericArguments::arbitrary_with(args),
        )
            .prop_map(|(x, y, z)| Self {
                id: x,
                member_generic_arguments: y,
                parent_generic_arguments: z,
            })
            .boxed()
    }
}

impl Arbitrary for Symbol<Default> {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = (
        Option<BoxedStrategy<Lifetime<Default>>>,
        Option<BoxedStrategy<Type<Default>>>,
        Option<BoxedStrategy<Constant<Default>>>,
    );

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (GlobalID::arbitrary(), GenericArguments::arbitrary_with(args))
            .prop_map(|(x, y)| Self { id: x, generic_arguments: y })
            .boxed()
    }
}

impl<T: Arbitrary<Strategy = BoxedStrategy<T>> + 'static> Arbitrary
    for Tuple<T>
{
    type Parameters = Option<BoxedStrategy<T>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strat = args.unwrap_or_else(T::arbitrary);

        proptest::collection::vec((strat, proptest::bool::ANY), 0..=2)
            .prop_map(|elements| Self {
                elements: elements
                    .into_iter()
                    .map(|(term, is_unpacked)| TupleElement {
                        term,
                        is_unpacked,
                    })
                    .collect(),
            })
            .boxed()
    }
}
