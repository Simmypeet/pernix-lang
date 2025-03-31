//! Contains the diagnostic for the extern function check.

use pernixc_diagnostic::{Diagnostic, Report};
use pernixc_log::Severity;
use pernixc_semantic::{
    component::input::LocationSpan,
    table::{GlobalID, Table},
};

/// The extern function can't have any generic parameters other than lifetime
/// parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParametersAreNotAllowedInExternFunction {
    /// The ID of the extern function.
    pub extern_function_id: GlobalID,
}

impl Report<&Table> for GenericParametersAreNotAllowedInExternFunction {
    fn report(&self, table: &Table) -> Diagnostic {
        let location = table
            .get::<LocationSpan>(self.extern_function_id)
            .span
            .clone()
            .unwrap();

        Diagnostic {
            span: location,
            message: "generic parameters are not allowed in extern functions"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: vec![],
        }
    }
}
