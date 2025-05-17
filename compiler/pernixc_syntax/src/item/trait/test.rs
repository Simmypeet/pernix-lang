use proptest::prelude::Arbitrary;

use crate::{
    item::r#trait::{arbitrary, Trait},
    test::verify_ref,
};

proptest::proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..Default::default()
    })]

    #[test]
    fn r#trait(input in arbitrary::Trait::arbitrary()) {
        verify_ref::<_, Trait>(&input)?;
    }
}
