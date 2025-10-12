use proptest::prelude::Arbitrary;

use crate::{
    item::effect::{arbitrary, Effect},
    test::verify_ref,
};

proptest::proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn effect(
        reference in arbitrary::Effect::arbitrary()
    ) {
        verify_ref::<_, Effect>(&reference)?;
    }
}
