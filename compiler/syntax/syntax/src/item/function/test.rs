use proptest::prelude::Arbitrary;

use crate::{
    item::function::{Function, arbitrary},
    test::verify_ref,
};

proptest::proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn function(
        reference in arbitrary::Function::arbitrary()
    ) {
        verify_ref::<_, Function>(&reference)?;
    }
}
