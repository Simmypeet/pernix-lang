//! Diagnostics for the implementation coherence check.

use pernixc_arena::ID;
use pernixc_diagnostic::Report;
use pernixc_table::{MemberID, Table};
use pernixc_term::generic_parameter::{
    GenericKind, GenericParameter, GenericParameters,
};

/// Generic parameter is unused in the implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnusedGenericParameterInImplementation<T> {
    /// The ID of the generic parameter that is unused.
    pub generic_parameter_id: MemberID<ID<T>>,
}

impl<T: GenericParameter> Report<&Table>
    for UnusedGenericParameterInImplementation<T>
{
    fn report(&self, parameter: &Table) -> pernixc_diagnostic::Diagnostic {
        let generic_parameters = parameter
            .query::<GenericParameters>(self.generic_parameter_id.parent)
            .unwrap();

        let arena = T::get_generic_parameters_arena(&generic_parameters);

        let generic_parameter = &arena[self.generic_parameter_id.id];

        let span = T::span(generic_parameter).cloned().unwrap();
        let name = T::name(generic_parameter);
        let kind = match T::kind() {
            GenericKind::Type => "type parameter",
            GenericKind::Lifetime => "lifetime parameter",
            GenericKind::Constant => "constant parameter",
        };

        pernixc_diagnostic::Diagnostic {
            span,
            message: format!("unused {kind} `{name}` in the implementation"),
            severity: pernixc_log::Severity::Error,
            help_message: None,
            related: vec![],
        }
    }
}
