use proptest::proptest;

use crate::table::{self, Table};

proptest! {
    #[test]
    fn create_module_test(
        input_table in super::input::table_with_module_strategy(),
    ) {
        table::tests::verify_table(&input_table, |targets, _| {
            let mut table = Table::new();
            table.create_modules(&targets);
            Ok(table)
        })?;
    }

    #[test]
    fn populate_usings_in_workspace_test(
        input_table in super::input::table_with_usings_strategy()
    ) {
        println!("{input_table:#?}");
        table::tests::verify_table(&input_table, |targets, handler| {
            let mut table = Table::new();
            table.create_modules(&targets);
            table.populate_usings_in_workspace(&targets, handler);
            Ok(table)
        })?;
    }
}
