//! Contains the diagnostic conversion functions for the driver.

use std::fmt;

use miette::{Diagnostic, LabeledSpan, NamedSource, Severity};
use pernixc_diagnostic::{ByteIndex, Highlight};
use pernixc_symbol_impl::source_map::SourceMap;

/// A struct that represents a miette diagnostic with source code attached.
#[derive(Debug)]
pub struct PernixDiagnostic {
    /// The severity of the diagnostic.
    pub severity: Severity,
    /// The main message of the diagnostic.
    pub message: String,
    /// The source code that this diagnostic refers to.
    pub source_code: Option<NamedSource<String>>,
    /// The labels to display.
    pub labels: Vec<LabeledSpan>,
    /// The help message.
    pub help: Option<String>,
}

impl fmt::Display for PernixDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
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
    let (source_code, labels) = if let Some(Highlight { span, message }) =
        &diagnostic.primary_highlight
    {
        // Get the source file
        let source_file = source_map.0.get(&span.source_id);

        source_file.map_or_else(
            || (None, vec![]),
            |source_file| {
                let content = source_file.content();
                let name = source_file.path().display().to_string();
                let source_code = NamedSource::new(name, content);

                // Build all labels - primary label first
                let mut labels = vec![LabeledSpan::at(
                    span.start..span.end,
                    message.clone().unwrap_or_default(),
                )];

                // Add related highlights - but only from the same source file
                for related in &diagnostic.related {
                    if related.span.source_id == span.source_id {
                        labels.push(LabeledSpan::at(
                            related.span.start..related.span.end,
                            related.message.clone().unwrap_or_default(),
                        ));
                    }
                }

                (Some(source_code), labels)
            },
        )
    } else {
        (None, vec![])
    };

    PernixDiagnostic {
        severity,
        message: diagnostic.message.clone(),
        source_code,
        labels,
        help: diagnostic.help_message.clone(),
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
    }
}
