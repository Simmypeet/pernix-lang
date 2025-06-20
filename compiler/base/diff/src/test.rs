use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy, TestCaseError},
    prop_assert_eq, prop_assert_ne, proptest,
};

use crate::Comparable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct A(i32);

impl Comparable<()> for A {
    fn compare(&self, other: &Self, (): ()) -> bool { self.0 == other.0 }
}

impl Arbitrary for A {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::num::i32::ANY.prop_map(Self).boxed()
    }
}

proptest! {
    #[test]
    fn replace_equal(
        mut first in proptest::collection::vec(A::arbitrary(), 0..=20),
        second in proptest::collection::vec(A::arbitrary(), 0..=20),
    ) {
        let Some(diff) = super::single_diff(&first, &second, ()) else {
            prop_assert_eq!(first, second);
            return Ok(());
        };

        prop_assert_ne!(&first, &second);

        first.splice(
            diff.replace_original.clone(),
            second[diff.replace_with_from_new].iter().copied(),
        );

        prop_assert_eq!(first, second);
    }

    #[test]
    fn equal(
        sample in proptest::collection::vec(A::arbitrary(), 0..=20),
    ) {
        let diff = super::single_diff(&sample, &sample, ());
        prop_assert_eq!(diff, None);
    }

    #[test]
    fn prioritize_end(
        mut first in proptest::collection::vec(A::arbitrary(), 0..=20),
        mut append_first in proptest::collection::vec(A::arbitrary(), 1..=20),
    ) {
        let mut second = first.clone();
        second.append(&mut append_first);

        let Some(diff) = super::single_diff(&first, &second, ()) else {
            return Err(TestCaseError::fail("should have a diff"));
        };

        prop_assert_ne!(&first, &second);

        prop_assert_eq!(&diff.replace_original, &(first.len()..first.len()));
        prop_assert_eq!(&diff.replace_with_from_new, &(first.len()..second.len()));

        first.splice(
            diff.replace_original.clone(),
            second[diff.replace_with_from_new].iter().copied(),
        );

        prop_assert_eq!(first, second);
    }
}
