#![allow(missing_docs)]

use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy};
use qbice::storage::intern::Interned;

use crate::tuple::{Element, Tuple};

impl<Term: Arbitrary<Strategy = BoxedStrategy<Term>> + 'static> Arbitrary
    for Element<Term>
{
    type Parameters = Option<BoxedStrategy<Term>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let term = args.unwrap_or_else(Term::arbitrary);

        (term, proptest::bool::ANY)
            .prop_map(|(term, is_unpacked)| {
                Self::new(Interned::new_duplicating(term), is_unpacked)
            })
            .boxed()
    }
}

impl<Term: Arbitrary<Strategy = BoxedStrategy<Term>> + 'static> Arbitrary
    for Tuple<Term>
{
    type Parameters = Option<BoxedStrategy<Term>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(Element::arbitrary_with(args), 0..=2)
            .prop_map(Self::new)
            .boxed()
    }
}
