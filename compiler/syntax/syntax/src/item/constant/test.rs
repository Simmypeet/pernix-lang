use proptest::prelude::Arbitrary;

use crate::{
    item::constant::{Constant, arbitrary},
    test::verify_ref,
};

proptest::proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..Default::default()
    })]

    #[test]
    fn constant(
        reference in arbitrary::Constant::arbitrary()
    ) {
        verify_ref::<_, Constant>(&reference)?;
    }
}
