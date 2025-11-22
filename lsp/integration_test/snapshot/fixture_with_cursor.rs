use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use pernixc_query::runtime::executor::Executor;

/// A collection of fixture files in a directory, with a designated cursor
/// position.
///
/// This is intended to be used as a mock workspace for testing LSP operations
/// that depend on the cursor position, such as "go to definition" or
/// "find references".
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FixtureWithCursor {
    contents: HashMap<PathBuf, String>,
    cursor_file: PathBuf,
    cursor_offset: usize,
}

impl FixtureWithCursor {
    /// Recursively scans the given directory for files with `.pnx` extension.
    ///
    /// Exactly one of these files must contain a `<cursor>` marker indicating
    /// the cursor position to test for LSP operations.
    pub fn from_directory(path: &Path) -> Self {
        use walkdir::WalkDir;

        let mut contents = HashMap::new();
        let mut cursor: Option<(PathBuf, usize)> = None;

        for entry in
            WalkDir::new(path).into_iter().filter_map(Result::ok).filter(|e| {
                e.path().extension().and_then(|ext| ext.to_str()) == Some("pnx")
            })
        {
            let file_path = entry.path().to_path_buf();

            // we'll always use the canonicalized path for the cursor tracking
            let full_file_path = std::fs::canonicalize(&file_path)
                .expect("Failed to canonicalize file path");

            let mut file_contents = std::fs::read_to_string(&file_path)
                .expect("Failed to read fixture file");

            if let Some(offset) = file_contents.find("<cursor>") {
                if let Some(cursor) = &cursor {
                    panic!(
                        "Multiple files contain <cursor> marker: {:?} and {:?}",
                        cursor.0, full_file_path
                    );
                }

                cursor = Some((full_file_path.clone(), offset));
                file_contents = file_contents.replace("<cursor>", "");
            }

            contents.insert(full_file_path, file_contents);
        }

        let (cursor_file, cursor_offset) =
            cursor.expect("No file contains <cursor> marker");

        FixtureWithCursor { contents, cursor_file, cursor_offset }
    }
}

// Overrides the source file executor to provide fixture contents.
impl Executor<pernixc_source_file::Key> for FixtureWithCursor {
    async fn execute(
        &self,
        _: &pernixc_query::TrackedEngine,
        key: &pernixc_source_file::Key,
    ) -> Result<
        <pernixc_source_file::Key as pernixc_query::Key>::Value,
        pernixc_query::runtime::executor::CyclicError,
    > {
        let canonicalized_path = match std::fs::canonicalize(&key.path) {
            Ok(path) => path,
            Err(err) => {
                return Ok(Err(pernixc_source_file::Error(
                    err.to_string().into(),
                )))
            }
        };

        if let Some(contents) = self.contents.get(&canonicalized_path) {
            Ok(Ok(Arc::new(pernixc_source_file::SourceFile::new(
                contents.clone(),
                canonicalized_path,
            ))))
        } else {
            Ok(Err(pernixc_source_file::Error(
                "File not found in fixture".into(),
            )))
        }
    }
}
