use std::sync::Arc;

use pernixc_source_file::SourceFile;
use pernixc_table::{diagnostic::Diagnostic, Table};

use crate::Compilation;

/// Builds the table for testing purposes.
pub fn build_table(
    source: impl std::fmt::Display,
) -> (Table, Vec<Box<dyn Diagnostic>>) {
    let source_file =
        Arc::new(SourceFile::new(source.to_string(), "test".into()));

    // we'll panic on syntax errors
    let target = pernixc_syntax::syntax_tree::target::Target::parse(
        &source_file,
        "test".to_string(),
        &pernixc_handler::Panic,
    );

    let storage = Arc::new(pernixc_handler::Storage::new());
    let mut table = Table::new(storage.clone());
    let target_id = table
        .add_compilation_target(
            "test".to_string(),
            std::iter::empty(),
            target,
            &pernixc_handler::Panic,
        )
        .unwrap();

    Compilation::builder().table(&mut table).target_id(target_id).build().run();
    let storage = std::mem::take(&mut *storage.as_vec_mut());

    (table, storage)
}
