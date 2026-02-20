use proptest::prelude::*;

use crate::{
    item::instance::{Instance, arbitrary},
    test::verify_ref,
};

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn instance(input in arbitrary::Instance::arbitrary()) {
        verify_ref::<_, Instance>(&input)?;
    }
}
