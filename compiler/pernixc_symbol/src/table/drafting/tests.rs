use proptest::proptest;

use crate::table::{self, Table};

proptest! {
    #[test]
    fn draft_symbols_test(
        input_table in super::input::table_with_drafting(),
    ) { table::tests::verify_table(&input_table, |targets, handler| {
            let mut table = Table::new();
            table.create_modules(&targets);
            table.populate_usings_in_workspace(&targets, handler);
            table.draft_symbols(targets, handler);
            Ok(table)
        })?;
    }
}
