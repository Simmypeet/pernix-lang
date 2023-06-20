use proptest::proptest;

use crate::table::{self, Table};

proptest! {
    #[test]
    fn draft_symbols_test(
        input_table in super::input::table_with_drafting(),
    ) {
        table::tests::verify_table(&input_table, |targets, _| {
            let mut table = Table::new();
            table.create_modules(&targets);
            Ok(table)
        })?;
    }
}
