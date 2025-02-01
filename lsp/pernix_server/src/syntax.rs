//! Contains logic related to handling syntax errors.

use std::{
    collections::{hash_map::Entry, HashMap},
    sync::Arc,
};

use getset::Getters;
use parking_lot::RwLock;
use pernixc_diagnostic::Report;
use pernixc_handler::Handler;
use pernixc_lexical::token_stream::{TokenStream, Tree};
use pernixc_source_file::{Location, SourceFile};
use pernixc_syntax::{
    state_machine::parse::Parse,
    syntax_tree::{item::ModuleContent, SyntaxTree},
};
use tower_lsp::lsp_types::{self, Diagnostic, Url};

use crate::extension::DaignosticExt;

/// A struct which implements [`Handler`] for collecting diagnostics.
#[derive(Debug, Default)]
pub struct Collector {
    diagnostics: RwLock<Vec<Diagnostic>>,
}

impl Collector {
    /// Creates a collector handler.
    #[must_use]
    pub const fn new() -> Self { Self { diagnostics: RwLock::new(Vec::new()) } }

    /// Gets the list of diagnostics collected so far.
    pub fn into_diagnostic(self) -> Vec<Diagnostic> {
        self.diagnostics.into_inner()
    }
}

impl<E: Report<()>> Handler<E> for Collector {
    fn receive(&self, error: E) {
        self.diagnostics.write().push(error.report(()).into_diagnostic());
    }
}

/// Manages analysis related to the syntax of the source files.
#[derive(Debug, Default, Getters, derive_more::Index)]
pub struct Syntax {
    /// Gets the map of source files by their URL.
    source_files_by_url: HashMap<Url, Arc<SourceFile>>,
}

/// An error that can occur when updating a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
#[allow(missing_docs)]
pub enum UpdateSourceFileError {
    #[error(
        "the URL is not found within the context, probably the file hasn't \
         been registered"
    )]
    UnknownUrl,

    #[error(
        "the given range {}:{} - {}:{} is invalid to the source file", 
        .0.start.line, .0.start.character, .0.end.line, .0.end.character,
    )]
    InvalidRange(lsp_types::Range),
}

impl Syntax {
    /// Creates a new source file for the given [`Url`] and inserts it into the
    /// context.
    ///
    /// This directly cooresponds to the `textDocument/didOpen` notification.
    pub fn register_source_file(&mut self, url: Url, text: String) {
        let url_path = url.path().into();

        match self.source_files_by_url.entry(url) {
            Entry::Occupied(mut entry) => {
                Arc::get_mut(entry.get_mut())
                    .expect("should be unique")
                    .replace(text);
            }

            Entry::Vacant(entry) => {
                entry.insert(Arc::new(SourceFile::new(text, url_path)));
            }
        }
    }

    /// Updates the source file with the given URL.
    ///
    /// This directly cooresponds to the `textDocument/didChange` notification.
    ///
    /// # Errors
    ///
    /// See [`UpdateSourceFileError`] for more information.
    #[allow(clippy::significant_drop_tightening)]
    pub fn update_source_file(
        &mut self,
        url: &Url,
        text: &str,
        range: tower_lsp::lsp_types::Range,
    ) -> Result<(), UpdateSourceFileError> {
        let pernix_location_start = Location::new(
            range.start.line as usize,
            range.start.character as usize,
        );
        let pernix_location_end = Location::new(
            range.end.line as usize,
            range.end.character as usize,
        );

        let source_file = self
            .source_files_by_url
            .get_mut(url)
            .ok_or(UpdateSourceFileError::UnknownUrl)?;

        let start = source_file
            .into_byte_index_include_ending(pernix_location_start)
            .ok_or(UpdateSourceFileError::InvalidRange(range))?;
        let end = source_file
            .into_byte_index_include_ending(pernix_location_end)
            .ok_or(UpdateSourceFileError::InvalidRange(range))?;

        Arc::get_mut(source_file)
            .expect("should be unique")
            .replace_range(start..end, text);

        Ok(())
    }

    /// Diagnoses the syntax of the source file with the given URL.
    #[must_use]
    pub fn analyze(&self, url: &Url) -> Option<Vec<Diagnostic>> {
        let source_file = self.source_files_by_url.get(url)?.clone();

        let collector = Collector::new();
        let token_stream = TokenStream::tokenize(source_file, &collector);
        let tree = Tree::new(&token_stream);

        ModuleContent::parse.parse_syntax(&tree, &collector);

        Some(collector.into_diagnostic())
    }
}
