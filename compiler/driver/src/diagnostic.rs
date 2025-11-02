use codespan_reporting::diagnostic::Label;
use pernixc_diagnostic::{ByteIndex, Highlight};
use pernixc_source_file::GlobalSourceID;

pub fn pernix_diagnostic_to_codespan_diagnostic(
    diagnostic: &pernixc_diagnostic::Rendered<ByteIndex>,
) -> codespan_reporting::diagnostic::Diagnostic<GlobalSourceID> {
    let mut result = match diagnostic.severity {
        pernixc_diagnostic::Severity::Error => {
            codespan_reporting::diagnostic::Diagnostic::error()
        }
        pernixc_diagnostic::Severity::Warning => {
            codespan_reporting::diagnostic::Diagnostic::warning()
        }
        pernixc_diagnostic::Severity::Info => {
            codespan_reporting::diagnostic::Diagnostic::note()
        }
    }
    .with_message(diagnostic.message.clone());

    if let Some(Highlight { span, message }) = &diagnostic.primary_highlight {
        result = result.with_labels(
            std::iter::once({
                let mut primary = Label::primary(span.source_id, span.range());

                if let Some(label_message) = message {
                    primary = primary.with_message(label_message);
                }

                primary
            })
            .chain(diagnostic.related.iter().map(|x| {
                let mut label =
                    Label::secondary(x.span.source_id, x.span.range());

                if let Some(message) = x.message.as_ref() {
                    label = label.with_message(message.clone());
                }

                label
            }))
            .collect(),
        );
    }

    if let Some(msg) = &diagnostic.help_message {
        result.with_notes(vec![msg.clone()])
    } else {
        result
    }
}
