use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_source::SourceFile;
use pernixc_syntax::syntax_tree::target::Target;
use pernixc_system::diagnostic::Storage;
use pernixc_tests::input::Input;
use proptest::test_runner::{TestCaseError, TestCaseResult};

use super::Table;
use crate::error::Error;

/// Enumeration of all error types that can be returned when parsing a target tree.
#[derive(Debug, EnumAsInner, From)]
enum TargetParseError {
    Lexical(pernixc_lexical::error::Error),
    Syntax(pernixc_syntax::error::Error),
    Target(pernixc_syntax::syntax_tree::target::Error),
}

pub(super) fn verify_table(
    input_table: &Table,
    f: impl FnOnce(Vec<Target>, &Storage<Error>) -> Result<Table, TestCaseError>,
) -> TestCaseResult {
    let workspace_directory = input_table.create_workspace()?;

    let mut targets = Vec::new();
    let storage: Storage<TargetParseError> = Storage::new();

    for target_name in input_table.target_root_module_ids_by_name.keys() {
        let target_root_source_file_path = workspace_directory
            .path()
            .join(target_name)
            .join("main.pnx");
        let target_root_source_file = SourceFile::load(&target_root_source_file_path)?;

        targets.push(Target::parse(
            target_root_source_file,
            target_name.clone(),
            &storage,
        ));
    }

    {
        let storage_vec = storage.as_vec();
        if !storage_vec.is_empty() {
            return Err(TestCaseError::fail(format!(
                "Failed to parse target tree: {storage_vec:?}",
            )));
        }
    }

    let storage: Storage<Error> = Storage::new();
    let output = f(targets, &storage)?;

    {
        let storage_vec = storage.as_vec();
        if !storage_vec.is_empty() {
            return Err(TestCaseError::fail(format!(
                "Failed to build the table: {storage_vec:?}",
            )));
        }
    }

    input_table.assert(&output)
}
