//! Diagnostics for the implementation coherence check.

use pernixc_arena::ID;
use pernixc_diagnostic::Report;
use pernixc_table::{
    component::{Implements, LocationSpan, SymbolKind},
    GlobalID, MemberID, Table,
};
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

/// Attempted to implement a foreign struct or enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementedForeignAdt {
    /// The ID of the implementation.
    pub adt_implementation_id: GlobalID,
}

impl Report<&Table> for ImplementedForeignAdt {
    fn report(&self, parameter: &Table) -> pernixc_diagnostic::Diagnostic {
        let implementation_span =
            parameter.get::<LocationSpan>(self.adt_implementation_id);
        let implemented_id =
            parameter.get::<Implements>(self.adt_implementation_id).0;

        let symbol_kind = parameter.get::<SymbolKind>(implemented_id);
        let kind_str = symbol_kind.kind_str();
        let qualified_name = parameter.get_qualified_name(implemented_id);

        pernixc_diagnostic::Diagnostic {
            span: implementation_span.span.clone().unwrap(),
            message: format!(
                "{kind_str} `{qualified_name}` is defined in another target; \
                 `implements` to foreign {kind_str} is not allowed",
            ),
            severity: pernixc_log::Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Implementing a foreign trait/marker but doesn't contain any struct or enum
/// defined in the current target in the implementation arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OrphanRuleViolation {
    /// The ID of the trait/marker implementation.
    pub implementation_id: GlobalID,
}

impl Report<&Table> for OrphanRuleViolation {
    fn report(&self, parameter: &Table) -> pernixc_diagnostic::Diagnostic {
        let implementation_span =
            parameter.get::<LocationSpan>(self.implementation_id);

        let implemented_id =
            parameter.get::<Implements>(self.implementation_id).0;

        let symbol_kind = parameter.get::<SymbolKind>(implemented_id);
        let kind_str = symbol_kind.kind_str();
        let qualified_name = parameter.get_qualified_name(implemented_id);

        pernixc_diagnostic::Diagnostic {
            span: implementation_span.span.clone().unwrap(),
            message: format!(
                "can't implement a foreign {kind_str} `{qualified_name}` \
                 without any struct or enum defined in the current target in \
                 the implementation arguments"
            ),
            severity: pernixc_log::Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}
