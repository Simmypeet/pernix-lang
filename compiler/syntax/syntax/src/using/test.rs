use proptest::prelude::Arbitrary;

use crate::{
    test::verify_ref,
    using::{UsingClause, arbitrary},
};

proptest::proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn using_clause(
        reference in arbitrary::UsingClause::arbitrary()
    ) {
        verify_ref::<_, UsingClause>(&reference)?;
    }
}
