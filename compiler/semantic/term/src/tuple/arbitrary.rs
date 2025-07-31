#![allow(missing_docs)]

use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy};

use crate::tuple::{Element, Tuple};

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
                    .map(|(term, is_unpacked)| Element { term, is_unpacked })
                    .collect(),
            })
            .boxed()
    }
}
