//! Contains extension traits for the [`pernixc_base::source_file::Span`] type.

use pernixc_source_file::ByteIndex;
use tower_lsp::lsp_types::{Position, Url};

/// Extension trait for converting the [`pernixc_base::source_file::Span`] type
/// to the [`tower_lsp::lsp_types::Range`] type.
pub trait SpanExt {
    /// Converts the span to a range.
    fn to_range(&self) -> tower_lsp::lsp_types::Range;
}

impl SpanExt for pernixc_source_file::Span<ByteIndex> {
    #[allow(clippy::cast_possible_truncation)]
    fn to_range(&self) -> tower_lsp::lsp_types::Range {
        let start = self.start_location().map_or_else(
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

impl DaignosticExt for pernixc_diagnostic::Rendered<ByteIndex> {
    fn into_diagnostic(self) -> tower_lsp::lsp_types::Diagnostic {
        let related = &self.related;
        let related_informations = {
            let result = related
                .iter()
                .filter_map(|x| {
                    let to_absolute_path = std::fs::canonicalize(
                        self.span.source_file().full_path(),
                    )
                    .ok()?;
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
        };
        tower_lsp::lsp_types::Diagnostic {
            range: self.span.to_range(),
            severity: Some(match self.severity {
                pernixc_diagnostic::Severity::Error => {
                    tower_lsp::lsp_types::DiagnosticSeverity::ERROR
                }
                pernixc_diagnostic::Severity::Info => {
                    tower_lsp::lsp_types::DiagnosticSeverity::INFORMATION
                }
                pernixc_diagnostic::Severity::Warning => {
                    tower_lsp::lsp_types::DiagnosticSeverity::WARNING
                }
            }),
            code: None,
            source: None,
            message: self.message.clone(),
            related_information: related_informations,
            tags: None,
            code_description: None,
            data: None,
        }
    }
}
