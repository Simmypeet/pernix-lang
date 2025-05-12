use proptest::prelude::Arbitrary;

use crate::{
    pattern::{arbitrary, Irrefutable, Refutable},
    test::verify_ref_display,
};

proptest::proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn refutable(
        reference in arbitrary::Refutable::arbitrary()
    ) {
        verify_ref_display::<_, Refutable>(&reference)?;
    }

    #[test]
    fn irrefutable(
        reference in arbitrary::Irrefutable::arbitrary()
    ) {
        verify_ref_display::<_, Irrefutable>(&reference)?;
    }
}
