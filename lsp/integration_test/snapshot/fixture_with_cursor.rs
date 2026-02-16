//! Defines a fixture with a cursor position for LSP integration tests.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use pernix_server::conversion::to_lsp_position;
use pernixc_qbice::TrackedEngine;
use pernixc_source_file::{EditorLocation, SourceFile};
use qbice::{Executor, storage::intern::Interned};
use tower_lsp::lsp_types::{
    TextDocumentIdentifier, TextDocumentPositionParams, Url,
};

/// A collection of fixture files in a directory, with a designated cursor
/// position.
///
/// This is intended to be used as a mock workspace for testing LSP operations
/// that depend on the cursor position, such as "go to definition" or
/// "find references".
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FixtureWithCursor {
    contents: HashMap<PathBuf, SourceFile>,
    cursor_file: PathBuf,
    cursor_offset: usize,
    cursor_location: EditorLocation,
}

impl FixtureWithCursor {
    /// Recursively scans the given directory for files with `.pnx` extension.
    ///
    /// Exactly one of these files must contain a `<cursor>` marker indicating
    /// the cursor position to test for LSP operations.
    pub fn from_directory(path: &Path) -> Self {
        use walkdir::WalkDir;

        let mut contents = HashMap::new();
        let mut cursor: Option<(PathBuf, usize, EditorLocation)> = None;

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
                        "Multiple files contain <cursor> marker: {} and {}",
                        cursor.0.display(),
                        full_file_path.display()
                    );
                }

                file_contents = file_contents.replace("<cursor>", "");

                let source_file = SourceFile::from_str(
                    &file_contents,
                    Interned::new_duplicating_unsized(file_path),
                );

                let editor_location = source_file
                    .get_editor_location_from_byte_index(offset)
                    .unwrap();

                cursor =
                    Some((full_file_path.clone(), offset, editor_location));
                contents.insert(full_file_path, source_file);
            } else {
                contents.insert(
                    full_file_path,
                    SourceFile::from_str(
                        &file_contents,
                        Interned::new_duplicating_unsized(file_path),
                    ),
                );
            }
        }

        let (cursor_file, cursor_offset, cursor_location) =
            cursor.expect("No file contains <cursor> marker");

        Self { contents, cursor_file, cursor_offset, cursor_location }
    }

    /// Retrieves the source file for the given path from the fixture.
    #[must_use]
    pub fn retrieve_source_file(&self, path: &Path) -> Option<SourceFile> {
        let canonicalized_path = std::fs::canonicalize(path).ok()?;
        self.contents.get(&canonicalized_path).cloned()
    }
}

// Overrides the source file executor to provide fixture contents.
impl Executor<pernixc_source_file::Key, pernixc_qbice::Config>
    for FixtureWithCursor
{
    async fn execute(
        &self,
        key: &pernixc_source_file::Key,
        engine: &TrackedEngine,
    ) -> Result<SourceFile, pernixc_source_file::Error> {
        let canonicalized_path = match std::fs::canonicalize(&key.path) {
            Ok(path) => path,
            Err(err) => {
                return Err(pernixc_source_file::Error(
                    engine.intern_unsized(err.to_string()),
                ));
            }
        };

        self.contents.get(&canonicalized_path).map_or_else(
            || {
                Err(pernixc_source_file::Error(
                    engine.intern_unsized("File not found in fixture"),
                ))
            },
            |contents| Ok(contents.clone()),
        )
    }
}

impl FixtureWithCursor {
    /// Retrieves the text document position parameters for the cursor
    /// location in the fixture.
    #[must_use]
    pub fn cursor_text_document_position_params(
        &self,
    ) -> TextDocumentPositionParams {
        TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: Url::from_file_path(&self.cursor_file).unwrap(),
            },
            position: self.cursor_location.to_lsp_position(),
        }
    }
}
