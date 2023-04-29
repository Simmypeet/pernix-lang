use std::{error::Error, path::PathBuf, sync::Arc};

use pernixc_source::source_file::SourceFile;
use pernixc_syntax::target_parsing;

use crate::{hir::builder::Builder, symbol::table::Table};

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn numeric_literal_binding() -> Result<(), Box<dyn Error>> {
    // creates a new source file
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/hir/numericLiteralBinding.pnx"),
        vec!["test".to_string()],
    )?;

    let (target_parsing, errors) = target_parsing::parse_target(source_file)?;
    assert!(errors.is_empty());

    let (table, errors) = Table::analyze(target_parsing);
    let table = Arc::new(table);
    assert!(errors.is_empty());

    let overload_id = table[table
        .get_global_id_by_full_name(["test", "main"].into_iter())
        .unwrap()
        .into_overload_set()
        .unwrap()]
    .overloads()[0];

    let syntax_tree = table[overload_id].syntax_tree();
    let statements = syntax_tree.block_without_label.statements();

    let mut builder = Builder::new(table, overload_id);

    {}

    todo!()
}
