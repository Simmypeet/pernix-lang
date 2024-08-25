//! Contains the definition of [`Collector`]

use std::sync::Arc;

use parking_lot::RwLock;
use pernixc_base::{
    diagnostic::Handler,
    log::formatting,
    source_file::{SourceElement, SourceFile, Span},
};
use tower_lsp::lsp_types::{Diagnostic, Position};

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
    pub fn new(target_source_file: Arc<SourceFile>) -> Self {
        Self { target_source_file, diagnostics: RwLock::new(Vec::new()) }
    }

    /// Gets the list of diagnostics collected so far.
    pub fn into_diagnostic(self) -> Vec<Diagnostic> {
        self.diagnostics.into_inner()
    }
}

fn convert_span(span: &Span) -> tower_lsp::lsp_types::Range {
    let start = Position::new(
        span.start_location().line as u32 - 1,
        span.start_location().column as u32 - 1,
    );
    let end = span.end_location().map_or_else(
        || {
            let mut end = start.clone();
            end.character += 1;
            end
        },
        |x| Position::new(x.line as u32 - 1, x.column as u32 - 1),
    );

    tower_lsp::lsp_types::Range { start, end }
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

        let range = convert_span(&sp);
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

            convert_span(&span)
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

        let range = convert_span(&span);
        let message = cut_message(&error.to_string(), "[error]:");

        self.diagnostics.write().push(Diagnostic {
            range,
            message,
            severity: Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR),
            ..Default::default()
        });
    }
}
