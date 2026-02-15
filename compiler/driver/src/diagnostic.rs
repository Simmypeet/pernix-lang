//! Contains the diagnostic conversion functions for the driver.

use std::fmt;

use ansi_term::Style;
use miette::{
    Diagnostic, LabeledSpan, Severity, SourceCode, SourceSpan, SpanContents,
};
use pernixc_diagnostic::{ByteIndex, Highlight};
use pernixc_source_file::SourceFile;
use pernixc_symbol_impl::source_map::SourceMap;

/// An owned implementation of [`SpanContents`] for use with dynamic content.
///
/// This struct owns its data, allowing it to be returned from functions that
/// generate content dynamically (like extracting a portion from a rope).
#[derive(Debug)]
pub struct OwnedSpanContents {
    /// The name of the source (e.g., file path).
    name: String,
    /// The owned content data.
    data: Vec<u8>,
    /// The span within the data.
    span: SourceSpan,
    /// The 0-indexed line number where the span starts.
    line: usize,
    /// The 0-indexed column number where the span starts.
    column: usize,
    /// The total number of lines in the source.
    line_count: usize,
}

impl SpanContents for OwnedSpanContents {
    fn data(&self) -> &[u8] { &self.data }

    fn span(&self) -> &SourceSpan { &self.span }

    fn line(&self) -> usize { self.line }

    fn column(&self) -> usize { self.column }

    fn line_count(&self) -> usize { self.line_count }

    fn name(&self) -> Option<&str> { Some(&self.name) }

    fn language(&self) -> Option<&str> { None }
}

/// A wrapper around [`SourceFile`] that implements miette's [`SourceCode`]
/// trait efficiently by only materializing context lines from the rope.
///
/// This avoids materializing the entire file content - only the context
/// lines around the highlighted span are extracted from the rope.
#[derive(Debug, Clone)]
pub struct RopeSourceCode {
    /// The name of the source file (path).
    pub name: String,
    /// The source file containing the rope.
    pub source_file: SourceFile,
}

impl RopeSourceCode {
    /// Creates a new `RopeSourceCode` from a `SourceFile`.
    #[must_use]
    pub fn new(source_file: &SourceFile) -> Self {
        let name = source_file.path().display().to_string();
        Self { name, source_file: source_file.clone() }
    }
}

impl SourceCode for RopeSourceCode {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn SpanContents + 'a>, miette::MietteError> {
        let start = span.offset();
        let len = span.len();
        let end = start + len;
        let file_len = self.source_file.len_bytes();

        if end > file_len {
            return Err(miette::MietteError::OutOfBounds);
        }

        let total_lines = self.source_file.line_coount();

        // Find the line containing the start of the span (0-indexed)
        let start_line = self.source_file.byte_to_line(start);
        let end_line = if len == 0 {
            start_line
        } else {
            self.source_file.byte_to_line(end.saturating_sub(1))
        };

        // Calculate context boundaries
        let context_start_line =
            start_line.saturating_sub(context_lines_before);
        let context_end_line =
            (end_line + context_lines_after + 1).min(total_lines);

        // Get the byte range for the context
        let context_start = self.source_file.line_to_byte(context_start_line);
        let context_end = if context_end_line >= total_lines {
            file_len
        } else {
            self.source_file.line_to_byte(context_end_line)
        };

        // Only materialize the context portion - not the whole file
        let context_content =
            self.source_file.slice(context_start..context_end);
        let context_len = context_end - context_start;

        // Calculate the column (in characters, not bytes) - this is where
        // the CONTEXT starts, not the span
        let context_column = if context_lines_before == 0 {
            // If no context lines before, column is where the span starts
            self.source_file
                .slice(self.source_file.line_to_byte(start_line)..start)
                .chars()
                .count()
        } else {
            // Context starts at the beginning of a line
            0
        };

        // The span field should indicate where in the ORIGINAL file the
        // context data comes from - this allows miette to map the original
        // label offsets to the correct position in our returned data.
        Ok(Box::new(OwnedSpanContents {
            name: self.name.clone(),
            data: context_content.into_bytes(),
            // This is the span of the CONTEXT within the original file
            span: SourceSpan::new(context_start.into(), context_len),
            // Line where context starts (0-indexed)
            line: context_start_line,
            column: context_column,
            line_count: total_lines,
        }))
    }
}

/// A struct that represents a miette diagnostic with source code attached.
#[derive(Debug)]
pub struct PernixDiagnostic {
    /// The severity of the diagnostic.
    pub severity: Severity,
    /// The main message of the diagnostic.
    pub message: String,
    /// The source code that this diagnostic refers to.
    pub source_code: Option<RopeSourceCode>,
    /// The labels to display.
    pub labels: Vec<LabeledSpan>,
    /// The help message.
    pub help: Option<String>,
    /// Related diagnostics (e.g., for cross-file highlights).
    pub related_diagnostics: Vec<PernixDiagnostic>,
}

impl fmt::Display for PernixDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Style::new().bold().paint(&self.message))
    }
}

impl std::error::Error for PernixDiagnostic {}

impl Diagnostic for PernixDiagnostic {
    fn severity(&self) -> Option<Severity> { Some(self.severity) }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.help
            .as_ref()
            .map(|h| Box::new(h.clone()) as Box<dyn fmt::Display + 'a>)
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        self.source_code.as_ref().map(|s| s as &dyn miette::SourceCode)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        if self.labels.is_empty() {
            None
        } else {
            Some(Box::new(self.labels.clone().into_iter()))
        }
    }

    fn related<'a>(
        &'a self,
    ) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        if self.related_diagnostics.is_empty() {
            None
        } else {
            Some(Box::new(
                self.related_diagnostics.iter().map(|d| d as &dyn Diagnostic),
            ))
        }
    }
}

/// Converts a pernix diagnostic to a miette diagnostic.
#[must_use]
pub fn pernix_diagnostic_to_miette_diagnostic(
    diagnostic: &pernixc_diagnostic::Rendered<ByteIndex>,
    source_map: &SourceMap,
) -> PernixDiagnostic {
    let severity = match diagnostic.severity {
        pernixc_diagnostic::Severity::Error => Severity::Error,
        pernixc_diagnostic::Severity::Warning => Severity::Warning,
        pernixc_diagnostic::Severity::Info => Severity::Advice,
    };

    // Get source code if there's a primary highlight
    let (source_code, labels, related_diagnostics) = if let Some(Highlight {
        span,
        message,
    }) =
        &diagnostic.primary_highlight
    {
        // Get the source file
        let source_file = source_map.0.get(&span.source_id);

        source_file.map_or_else(
            || (None, vec![], vec![]),
            |source_file| {
                // Create RopeSourceCode which materializes the content once
                let source_code = RopeSourceCode::new(source_file);

                // Build all labels - primary label first
                let mut labels = vec![LabeledSpan::at(
                    span.start..span.end,
                    message.clone().unwrap_or_default(),
                )];

                // Collect related highlights from the same file as labels,
                // and create nested diagnostics for highlights in different
                // files
                let mut related_diagnostics = Vec::new();

                for related in &diagnostic.related {
                    if related.span.source_id == span.source_id {
                        // Same file - add as a label
                        labels.push(LabeledSpan::at(
                            related.span.start..related.span.end,
                            related.message.clone().unwrap_or_default(),
                        ));
                    } else {
                        // Different file - create a nested diagnostic
                        if let Some(related_source_file) =
                            source_map.0.get(&related.span.source_id)
                        {
                            let related_source_code =
                                RopeSourceCode::new(related_source_file);
                            let related_label = LabeledSpan::at(
                                related.span.start..related.span.end,
                                related.message.clone().unwrap_or_default(),
                            );

                            related_diagnostics.push(PernixDiagnostic {
                                severity: Severity::Advice,
                                message: related
                                    .message
                                    .clone()
                                    .unwrap_or_else(|| "related".to_string()),
                                source_code: Some(related_source_code),
                                labels: vec![related_label],
                                help: None,
                                related_diagnostics: vec![],
                            });
                        }
                    }
                }

                (Some(source_code), labels, related_diagnostics)
            },
        )
    } else {
        (None, vec![], vec![])
    };

    PernixDiagnostic {
        severity,
        message: diagnostic.message.clone(),
        source_code,
        labels,
        help: diagnostic.help_message.clone(),
        related_diagnostics,
    }
}

/// Creates a simple error diagnostic without source code.
#[must_use]
pub fn simple_error(message: impl Into<String>) -> PernixDiagnostic {
    PernixDiagnostic {
        severity: Severity::Error,
        message: message.into(),
        source_code: None,
        labels: vec![],
        help: None,
        related_diagnostics: vec![],
    }
}

/// Creates a simple warning diagnostic without source code.
#[must_use]
pub fn simple_warning(message: impl Into<String>) -> PernixDiagnostic {
    PernixDiagnostic {
        severity: Severity::Warning,
        message: message.into(),
        source_code: None,
        labels: vec![],
        help: None,
        related_diagnostics: vec![],
    }
}

/// Creates a simple note diagnostic without source code.
#[must_use]
pub fn simple_note(message: impl Into<String>) -> PernixDiagnostic {
    PernixDiagnostic {
        severity: Severity::Advice,
        message: message.into(),
        source_code: None,
        labels: vec![],
        help: None,
        related_diagnostics: vec![],
    }
}

/// Creates a simple error diagnostic with a help message.
#[must_use]
pub fn simple_error_with_help(
    message: impl Into<String>,
    help: impl Into<String>,
) -> PernixDiagnostic {
    PernixDiagnostic {
        severity: Severity::Error,
        message: message.into(),
        source_code: None,
        labels: vec![],
        help: Some(help.into()),
        related_diagnostics: vec![],
    }
}
