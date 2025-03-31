//! Diagnostic reports for the well-formedness of the occurrences.

use pernixc_diagnostic::{Related, Report};
use pernixc_log::Severity;
use pernixc_semantic::{
    component::{Implements, LocationSpan},
    DisplayObject, GlobalID, Table,
};
use pernixc_source_file::Span;
use pernixc_semantic::term::{
    constant::Constant, generic_arguments::GenericArguments, r#type::Type,
    Model,
};

/// The generic arguments are not compatible with the generic arguments defined
/// in the implementation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedImplementationArguments<M: Model> {
    /// The ID of the ADT implementation where the generic arguments are
    /// mismatched.
    pub adt_implementation_id: GlobalID,

    /// The generic arguments found in the implementation.
    pub found_generic_arguments: GenericArguments<M>,

    /// The span of the instantiation that causes the mismatch.
    pub instantiation_span: Span,
}

impl<M: Model> Report<&Table> for MismatchedImplementationArguments<M>
where
    GenericArguments<M>: pernixc_semantic::Display,
{
    fn report(&self, table: &Table) -> pernixc_diagnostic::Diagnostic {
        let span = table.get::<LocationSpan>(self.adt_implementation_id);

        pernixc_diagnostic::Diagnostic {
            span: self.instantiation_span.clone(),
            message: "the generic arguments are not compatible with the \
                      generic arguments defined in the implementation"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: span
                .span
                .as_ref()
                .map(|span| Related {
                    span: span.clone(),
                    message: "the implementation is defined here".to_string(),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// The ADT implementation is not general enough to satisfy the required forall
/// lifetimes in the generic arguments
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AdtImplementationIsNotGeneralEnough<M: Model> {
    /// The ADT implementation ID where the generic arguments are not general
    /// enough.
    pub adt_implementation_id: GlobalID,

    /// The generic arguments supplied to the ADT.
    pub generic_arguments: GenericArguments<M>,

    /// The span location of where the ADT is instantiated.
    pub instantiation_span: Span,
}

impl<M: Model> Report<&Table> for AdtImplementationIsNotGeneralEnough<M>
where
    GenericArguments<M>: pernixc_semantic::Display,
{
    fn report(&self, table: &Table) -> pernixc_diagnostic::Diagnostic {
        let span = table.get::<LocationSpan>(self.adt_implementation_id);
        let implemented_id =
            table.get::<Implements>(self.adt_implementation_id).0;

        let implemented_qualified_name =
            table.get_qualified_name(implemented_id);

        pernixc_diagnostic::Diagnostic {
            span: self.instantiation_span.clone(),
            message: format!(
                "the implementation is not general enough for {}{}",
                implemented_qualified_name,
                DisplayObject { table, display: &self.generic_arguments }
            ),
            severity: Severity::Error,
            help_message: None,
            related: span
                .span
                .as_ref()
                .map(|span| Related {
                    span: span.clone(),
                    message: "the implementation is defined here".to_string(),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// The constant argument has a mismatched type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantArgumentTypeMismatched<M: Model> {
    /// The span of the constant argument.
    pub span: Span,

    /// The expected type of the constant argument.
    pub expected_type: Type<M>,

    /// The constant argument that has a mismatched type.
    pub constant_argument: Constant<M>,
}

impl<M: Model> Report<&Table> for ConstantArgumentTypeMismatched<M>
where
    Type<M>: pernixc_semantic::Display,
    Constant<M>: pernixc_semantic::Display,
{
    fn report(&self, table: &Table) -> pernixc_diagnostic::Diagnostic {
        pernixc_diagnostic::Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the constant argument `{}` has a mismatched type: expected \
                 `{}`",
                DisplayObject { display: &self.constant_argument, table },
                DisplayObject { display: &self.expected_type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}
