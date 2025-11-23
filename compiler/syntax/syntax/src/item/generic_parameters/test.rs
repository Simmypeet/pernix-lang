use proptest::prelude::Arbitrary;

use crate::{
    item::generic_parameters::{GenericParameters, arbitrary},
    test::verify_ref,
};

proptest::proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn generic_parameters(
        reference in arbitrary::GenericParameters::arbitrary()
    ) {
        verify_ref::<_, GenericParameters>(&reference)?;
    }
}
