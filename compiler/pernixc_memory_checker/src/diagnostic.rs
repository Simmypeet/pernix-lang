//! Diagnostics for the memory checker.

use pernixc_diagnostic::{Diagnostic, Related, Report};
use pernixc_log::Severity;
use pernixc_semantic::table::Table;
use pernixc_source_file::GlobalSpan;

/// The value behind the mutable reference has been moved out and needs to be
/// restored.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MovedOutValueFromMutableReference {
    /// The span of the moved out value.
    pub moved_out_value_span: GlobalSpan,

    /// The span of the mutable reference.
    pub reassignment_span: Option<GlobalSpan>,
}

impl Report<&Table> for MovedOutValueFromMutableReference {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.moved_out_value_span.clone(),
            message: "the value behind the mutable reference has been moved \
                      out and needs to be restored"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: self
                .reassignment_span
                .as_ref()
                .map(|reassignment_span| Related {
                    span: reassignment_span.clone(),
                    message: "this assignment makes the value behind mutable \
                              reference inaccessible from now on..."
                        .to_string(),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// The value is used before it has been initialized.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UseBeforeInitialization {
    /// The span where the value is used.
    pub use_span: GlobalSpan,
}

impl Report<&Table> for UseBeforeInitialization {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.use_span.clone(),
            message: "the value is used before it has been initialized"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The value is used after it has been moved.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UseAfterMove {
    /// The span where the value is used.
    pub use_span: GlobalSpan,

    /// The span where the value is moved.
    pub move_span: GlobalSpan,
}

impl Report<&Table> for UseAfterMove {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.use_span.clone(),
            message: "the value is used after it has been moved".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: vec![Related {
                span: self.move_span.clone(),
                message: "the value is moved here".to_string(),
            }],
        }
    }
}

/// The value has been moved inside the loop, which could be used in the
/// subsequent iteration.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MoveInLoop {
    /// The span of the moved value.
    pub moved_value_span: GlobalSpan,
}

impl Report<&Table> for MoveInLoop {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.moved_value_span.clone(),
            message: "the value has been moved inside the loop, which could \
                      be used in the subsequent iteration"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}
