use proptest::prelude::Arbitrary;

use crate::{
    item::r#extern::{Extern, arbitrary},
    test::verify_ref,
};

proptest::proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..Default::default()
    })]

    #[test]
    fn r#extern(input in arbitrary::Extern::arbitrary()) {
        verify_ref::<_, Extern>(&input)?;
    }
}
