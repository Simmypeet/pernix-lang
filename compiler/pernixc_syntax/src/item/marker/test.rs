use proptest::{prelude::Arbitrary, proptest};

use super::{arbitrary, Marker};
use crate::test::verify_ref;

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn marker(input in arbitrary::Marker::arbitrary()) {
        verify_ref::<_, Marker>(&input)?;
    }
}
