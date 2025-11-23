use proptest::prelude::Arbitrary;

use crate::{
    item::module::{Module, arbitrary},
    test::verify_ref,
};

proptest::proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn module(
        reference in arbitrary::Module::arbitrary()
    ) {
        verify_ref::<_, Module>(&reference)?;
    }
}
