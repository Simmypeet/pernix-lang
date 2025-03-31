//! Contains the diagnostic related to the pattern binding.

use pernixc_diagnostic::{Related, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;

use crate::table::Table;

/// A particular name has already been bound in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AlreadyBoundName {
    /// The span of the already bound identifier.
    pub already_bound_identifier_span: Span,

    /// The span of the rebinding.
    pub new_binding_span: Span,
}

impl Report<&Table> for AlreadyBoundName {
    fn report(&self, _: &Table) -> pernixc_diagnostic::Diagnostic {
        pernixc_diagnostic::Diagnostic {
            span: self.new_binding_span.clone(),
            message: format!(
                "the name `{}` has already been bound in the scope",
                self.already_bound_identifier_span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: vec![Related {
                span: self.already_bound_identifier_span.clone(),
                message: "the name is already bound here".to_string(),
            }],
        }
    }
}
