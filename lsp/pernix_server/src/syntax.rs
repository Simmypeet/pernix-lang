//! Contains logic related to handling syntax errors.

use std::sync::Arc;

use dashmap::DashMap;
use getset::Getters;
use parking_lot::RwLock;
use pernixc_base::{
    diagnostic::Handler,
    log::formatting,
    source_file::{Location, ReplaceRangeError, SourceElement, SourceFile},
};
use pernixc_lexical::token_stream::TokenStream;
use pernixc_syntax::parser::Parser;
use tower_lsp::lsp_types::{self, Diagnostic, Url};

use crate::span_ext::SpanExt;

/// A struct which implements [`Handler`] for collecting diagnostics.
#[derive(Debug)]
pub struct Collector {
    target_source_file: Arc<SourceFile>,
    diagnostics: RwLock<Vec<Diagnostic>>,
}

impl Collector {
    /// Creates a collector handler.
    ///
    /// The `target_source_file` is the source file that the diagnostics will be
    /// collected for. Errors that aren't from that given source file will be
    /// ignored.
    #[must_use]
    pub fn new(target_source_file: Arc<SourceFile>) -> Self {
        Self { target_source_file, diagnostics: RwLock::new(Vec::new()) }
    }

    /// Gets the list of diagnostics collected so far.
    pub fn into_diagnostic(self) -> Vec<Diagnostic> {
        self.diagnostics.into_inner()
    }
}

fn cut_message(message: &str, string_prefix: &str) -> String {
    let result = if let Some((first, _)) = message.split_once('\n') {
        first
    } else {
        message
    };

    let result = formatting::remove_vt100_codes(result);

    result.replace(string_prefix, "")
}

impl Handler<pernixc_lexical::error::Error> for Collector {
    fn receive(&self, error: pernixc_lexical::error::Error) {
        let sp = match &error {
            pernixc_lexical::error::Error::UnterminatedDelimitedComment(sp) => {
                &sp.span
            }
            pernixc_lexical::error::Error::UndelimitedDelimiter(sp) => {
                &sp.opening_span
            }
            pernixc_lexical::error::Error::UnterminatedStringLiteral(sp) => {
                &sp.span
            }
            pernixc_lexical::error::Error::InvalidEscapeSequence(sp) => {
                &sp.span
            }
        };

        if !Arc::ptr_eq(sp.source_file(), &self.target_source_file) {
            return;
        }

        let range = sp.to_range();
        let message = cut_message(&error.to_string(), "[error]:");

        self.diagnostics.write().push(Diagnostic {
            range,
            message,
            severity: Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR),
            ..Default::default()
        });
    }
}

impl Handler<pernixc_syntax::error::Error> for Collector {
    fn receive(&self, error: pernixc_syntax::error::Error) {
        // convert pernix's span to lsp's range
        let range = {
            let span = error.found.span();

            // skip if the error is not from the target file
            if !Arc::ptr_eq(span.source_file(), &self.target_source_file) {
                return;
            }

            span.to_range()
        };

        // cutoff the error message after new line character
        let string = cut_message(&error.to_string(), "[error]:");

        self.diagnostics.write().push(Diagnostic {
            range,
            message: string,
            severity: Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR),
            ..Default::default()
        });
    }
}

impl Handler<pernixc_syntax::syntax_tree::target::Error> for Collector {
    fn receive(&self, error: pernixc_syntax::syntax_tree::target::Error) {
        let span = match &error {
            pernixc_syntax::syntax_tree::target::Error::ModuleRedefinition(d) => {
                d.redefinition_submodule_span.clone()
            }
            pernixc_syntax::syntax_tree::target::Error::RootSubmoduleConflict(d) => {
                d.submodule_span.clone()
            }
            pernixc_syntax::syntax_tree::target::Error::SourceFileLoadFail(d) => {
                d.submodule.span()
            }
        };

        if !Arc::ptr_eq(span.source_file(), &self.target_source_file) {
            return;
        }

        let range = span.to_range();
        let message = cut_message(&error.to_string(), "[error]:");

        self.diagnostics.write().push(Diagnostic {
            range,
            message,
            severity: Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR),
            ..Default::default()
        });
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

        let collector = Collector::new(source_file.clone());
        let token_stream = TokenStream::tokenize(&source_file, &collector);

        let mut parser = Parser::new(&token_stream, source_file);

        parser.parse_module_content(&collector);

        Some(collector.into_diagnostic())
    }
}
