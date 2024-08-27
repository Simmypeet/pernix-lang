//! Contains logic related to handling syntax errors.

use std::sync::Arc;

use dashmap::DashMap;
use getset::Getters;
use parking_lot::RwLock;
use pernixc_base::{
    diagnostic::Report,
    handler::Handler,
    source_file::{Location, ReplaceRangeError, SourceFile},
};
use pernixc_lexical::token_stream::TokenStream;
use pernixc_syntax::parser::Parser;
use tower_lsp::lsp_types::{self, Diagnostic, Url};

use crate::extension::DaignosticExt;

/// A struct which implements [`Handler`] for collecting diagnostics.
#[derive(Debug)]
pub struct Collector {
    diagnostics: RwLock<Vec<Diagnostic>>,
}

impl Collector {
    /// Creates a collector handler.
    #[must_use]
    pub fn new() -> Self { Self { diagnostics: RwLock::new(Vec::new()) } }

    /// Gets the list of diagnostics collected so far.
    pub fn into_diagnostic(self) -> Vec<Diagnostic> {
        self.diagnostics.into_inner()
    }
}

impl<E: Report> Handler<E> for Collector
where
    E::Parameter: Default,
{
    fn receive(&self, error: E) {
        let Ok(diagnostic) = error.report(E::Parameter::default()) else {
            return;
        };

        self.diagnostics.write().push(diagnostic.into_diagnostic());
    }
}

/// Manages analysis related to the syntax of the source files.
#[derive(Debug, Default, Getters, derive_more::Index)]
pub struct Syntax {
    /// Gets the map of source files by their URL.
    source_files_by_url: DashMap<Url, Arc<SourceFile>>,
}

/// An error that can occur when updating a source file.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
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

    #[error(transparent)]
    ReplaceRange(#[from] ReplaceRangeError),
}

impl Syntax {
    /// Creates a new source file for the given [`Url`] and inserts it into the
    /// context.
    ///
    /// This directly cooresponds to the `textDocument/didOpen` notification.
    #[must_use]
    pub fn register_source_file(&self, url: Url, text: String) {
        let url_path = url.path().into();

        match self.source_files_by_url.entry(url) {
            dashmap::Entry::Occupied(mut entry) => {
                Arc::get_mut(entry.get_mut())
                    .expect("should be unique")
                    .replace(text);
            }

            dashmap::Entry::Vacant(entry) => {
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
    pub fn update_source_file(
        &self,
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

        let mut source_file = self
            .source_files_by_url
            .get_mut(url)
            .ok_or(UpdateSourceFileError::UnknownUrl)?;

        let start = source_file
            .into_byte_index_include_ending(pernix_location_start)
            .ok_or(UpdateSourceFileError::InvalidRange(range))?;
        let end = source_file
            .into_byte_index_include_ending(pernix_location_end)
            .ok_or(UpdateSourceFileError::InvalidRange(range))?;

        Ok(Arc::get_mut(&mut source_file)
            .expect("should be unique")
            .replace_range(start..end, text)?)
    }

    /// Diagnoses the syntax of the source file with the given URL.
    #[must_use]
    pub fn diagnose_syntax(&self, url: &Url) -> Option<Vec<Diagnostic>> {
        let source_file = self.source_files_by_url.get(url)?.clone();

        let collector = Collector::new();
        let token_stream = TokenStream::tokenize(&source_file, &collector);

        let mut parser = Parser::new(&token_stream, source_file);

        parser.parse_module_content(&collector);

        Some(collector.into_diagnostic())
    }
}
