use std::sync::Arc;

use pernixc_handler::Panic;
use pernixc_source_file::SourceFile;
use pernixc_table::{diagnostic::Diagnostic, Table, TargetID};

use crate::Compilation;

/// Builds the table for testing purposes.
pub fn build_table(
    source: impl std::fmt::Display,
) -> (Table, Vec<Box<dyn Diagnostic>>) {
    let mut table = Table::new(Arc::new(Panic));

    let (_, errors) =
        add_target(&mut table, std::iter::empty(), "test".to_string(), source);

    (table, errors)
}

/// Adds a target to the table for testing purposes.
pub fn add_target(
    table: &mut Table,
    linked_targets: impl IntoIterator<Item = TargetID>,
    target_name: String,
    source: impl std::fmt::Display,
) -> (TargetID, Vec<Box<dyn Diagnostic>>) {
    let source_file = Arc::new(SourceFile::new(
        source.to_string(),
        target_name.clone().into(),
    ));

    // we'll panic on syntax errors
    let target = pernixc_syntax::syntax_tree::target::Target::parse(
        &source_file,
        target_name.clone(),
        &pernixc_handler::Panic,
    );

    let storage = Arc::new(pernixc_handler::Storage::new());
    table.set_handler(storage.clone());

    let target_id = table
        .add_compilation_target(
            target_name,
            linked_targets,
            target,
            &pernixc_handler::Panic,
        )
        .unwrap();

    Compilation::builder().table(table).target_id(target_id).build().run();
    let storage = std::mem::take(&mut *storage.as_vec_mut());

    (target_id, storage)
}
