use proptest::prelude::Arbitrary;

use crate::{
    item::r#type::{arbitrary, Type},
    test::verify_ref,
};

proptest::proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..Default::default()
    })]

    #[test]
    fn r#type(input in arbitrary::Type::arbitrary()) {
        verify_ref::<_, Type>(&input)?;
    }
}
