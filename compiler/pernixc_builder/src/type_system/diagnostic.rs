//! Contains the diagnostic information related to the type system.

use pernixc_diagnostic::{Diagnostic, Related, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;
use pernixc_table::{DisplayObject, Table};
use pernixc_term::{predicate::Predicate, Model};

/// The bound is not satisfied upon instantiation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnsatisfiedPredicate<M: Model> {
    /// The unsatisfied bound.
    pub predicate: Predicate<M>,

    /// The span of the instantiation that causes the bound check.
    pub instantiation_span: Span,

    /// The span of the predicate declaration.
    pub predicate_declaration_span: Option<Span>,
}

impl<M: Model> Report<&Table> for UnsatisfiedPredicate<M>
where
    Predicate<M>: pernixc_table::Display,
{
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.instantiation_span.clone(),
            message: format!(
                "the predicate `{}` is not satisfied",
                DisplayObject { display: &self.predicate, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: self
                .predicate_declaration_span
                .as_ref()
                .map(|predicate_span| Related {
                    span: predicate_span.clone(),
                    message: "predicate declared here".to_string(),
                })
                .into_iter()
                .collect(),
        }
    }
}
