//! Contains the diagnostic information related to building the generic
//! parameters.

use pernixc_arena::ID;
use pernixc_diagnostic::{Diagnostic, Related, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;
use pernixc_table::{MemberID, Table};
use pernixc_term::generic_parameter::{
    GenericKind, GenericParameter, GenericParameters,
};

/// The generic parameter was declared in the wrong order.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MisOrderedGenericParameter {
    /// The kind of the mis-ordered generic parameter.
    pub generic_kind: GenericKind,

    /// The span of the generic parameter.
    pub generic_parameter_span: Span,
}

impl Report<&Table> for MisOrderedGenericParameter {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.generic_parameter_span.clone(),
            message: "the generic parameter was declared in the wrong order"
                .to_string(),
            severity: Severity::Error,
            help_message: match self.generic_kind {
                GenericKind::Type => Some(
                    "can't be declared after constant parameters".to_string(),
                ),
                GenericKind::Lifetime => Some(
                    "can't be declared after type or constant parameters"
                        .to_string(),
                ),
                GenericKind::Constant => None,
            },
            related: Vec::new(),
        }
    }
}

/// The default generic parameter must be trailing.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefaultGenericParameterMustBeTrailing {
    /// The span of the generic parameter.
    pub invalid_generic_default_parameter_span: Span,
}

impl Report<&Table> for DefaultGenericParameterMustBeTrailing {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.invalid_generic_default_parameter_span.clone(),
            message: "the default generic parameter must be trailing"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The generic parameter with the same name already exists in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DuplicatedGenericParameter<T> {
    /// The ID of the existing generic parameter.
    pub existing_generic_parameter_id: MemberID<ID<T>>,

    /// The ID of the new generic parameter.
    pub duplicating_generic_parameter_span: Span,
}

impl<T: GenericParameter> Report<&Table> for DuplicatedGenericParameter<T> {
    fn report(&self, table: &Table) -> Diagnostic {
        let generic_parameters = table
            .query::<GenericParameters>(
                self.existing_generic_parameter_id.parent,
            )
            .unwrap();
        let generic_parameter =
            T::get_generic_parameters_arena(&generic_parameters)
                .get(self.existing_generic_parameter_id.id)
                .unwrap();

        Diagnostic {
            span: self.duplicating_generic_parameter_span.clone(),
            message: format!(
                "the generic parameter named `{}` is already defined",
                generic_parameter.name()
            ),
            severity: Severity::Error,
            help_message: None,
            related: generic_parameter
                .span()
                .map(|x| Related {
                    span: x.clone(),
                    message: "previously defined here".to_string(),
                })
                .into_iter()
                .collect(),
        }
    }
}
