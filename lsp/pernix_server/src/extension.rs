//! Contains extension traits for the [`pernixc_base::source_file::Span`] type.

use tower_lsp::lsp_types::{Position, Url};

/// Extension trait for converting the [`pernixc_base::source_file::Span`] type
/// to the [`tower_lsp::lsp_types::Range`] type.
pub trait SpanExt {
    /// Converts the span to a range.
    fn to_range(&self) -> tower_lsp::lsp_types::Range;
}

impl SpanExt for pernixc_base::source_file::Span {
    #[allow(clippy::cast_possible_truncation)]
    fn to_range(&self) -> tower_lsp::lsp_types::Range {
        let start = Position::new(
            self.start_location().line as u32,
            self.start_location().column as u32,
        );
        let end = self.end_location().map_or_else(
            || {
                let line = self.source_file().line_coount() - 1;
                let column = self
                    .source_file()
                    .get_line(self.source_file().line_coount() - 1)
                    .unwrap()
                    .len();

                Position::new(line as u32, column as u32)
            },
            |x| Position::new(x.line as u32, x.column as u32),
        );

        tower_lsp::lsp_types::Range { start, end }
    }
}

/// Extension trait for converting the [`pernixc_base::diagnostic::Diagnostic`]
/// type to the [`tower_lsp::lsp_types::Diagnostic`] type.
pub trait DaignosticExt {
    /// Converts the diagnostic to a LSP diagnostic.
    fn into_diagnostic(self) -> tower_lsp::lsp_types::Diagnostic;
}

impl DaignosticExt for pernixc_base::diagnostic::Diagnostic {
    fn into_diagnostic(self) -> tower_lsp::lsp_types::Diagnostic {
        let related = &self.related;
        tower_lsp::lsp_types::Diagnostic {
            range: self.span.to_range(),
            severity: Some(match self.severity {
                pernixc_base::log::Severity::Error => {
                    tower_lsp::lsp_types::DiagnosticSeverity::ERROR
                }
                pernixc_base::log::Severity::Info => {
                    tower_lsp::lsp_types::DiagnosticSeverity::INFORMATION
                }
                pernixc_base::log::Severity::Warning => {
                    tower_lsp::lsp_types::DiagnosticSeverity::WARNING
                }
            }),
            code: None,
            source: None,
            message: self.message.clone(),
            related_information: {
                let result = related
                    .iter()
                    .filter_map(|x| {
                        let to_absolute_path =
                            std::fs::canonicalize(self.span.source_file().full_path()).ok()?;
                        let uri = Url::from_file_path(to_absolute_path).ok()?;

                        Some(tower_lsp::lsp_types::DiagnosticRelatedInformation {
                            location: tower_lsp::lsp_types::Location {
                                uri,
                                range: x.span.to_range(),
                            },
                            message: x.message.clone(),
                        })
                    })
                    .collect::<Vec<_>>();

                if result.is_empty() {
                    None
                } else {
                    Some(result)
                }
            },
            tags: None,
            code_description: None,
            data: None,
        }
    }
}
