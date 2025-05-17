use proptest::prelude::Arbitrary;

use crate::{
    item::where_clause::{arbitrary, WhereClause},
    test::verify_ref,
};

proptest::proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn where_clause(
        reference in arbitrary::WhereClause::arbitrary()
    ) {
        verify_ref::<_, WhereClause>(&reference)?;
    }
}
