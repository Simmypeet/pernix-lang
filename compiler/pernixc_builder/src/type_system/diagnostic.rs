//! Contains the diagnostic information related to the type system.

use derive_new::new;
use pernixc_diagnostic::{Diagnostic, Related, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;
use pernixc_table::{DisplayObject, Table};
use pernixc_term::{predicate::Predicate, Model};
use pernixc_type_system::OverflowError;

/// Represents an enumeration of operations that can cause [`OverflowError`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OverflowOperation {
    /// Caused by calculating the type of particular entity
    TypeOf,

    /// Caused by type checking
    TypeCheck,
}

/// An [`OverflowError`] occurred while compiling the program.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error, new,
)]
#[error(
    "ecnountered an over flow error from the `type_system` module; this \
     should be reported to the user"
)]
pub struct TypeSystemOverflow {
    /// The operation that caused the overflow.
    pub operation: OverflowOperation,

    /// The span where the overflow occurred.
    pub overflow_span: Span,

    /// The [`OverflowError`] that occurred.
    pub overflow_error: OverflowError,
}

impl Report<&Table> for TypeSystemOverflow {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.overflow_span.clone(),
            message: match &self.operation {
                OverflowOperation::TypeOf => format!(
                    "overflow calculating the type of `{}`",
                    self.overflow_span.str()
                ),
                OverflowOperation::TypeCheck => format!(
                    "overflow while calculating requirements for checking the \
                     type of `{}`",
                    self.overflow_span.str()
                ),
            },
            severity: Severity::Error,
            help_message: Some(
                "try reduce the complexity of the code; this error is the \
                 limitation of the type-system/compiler"
                    .to_string(),
            ),
            related: Vec::new(),
        }
    }
}

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

/// The satisfiability of the predicate can't be decided (most likely Overflow
/// error).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UndecidablePredicate<M: Model> {
    /// The undecidable predicate.
    pub predicate: Predicate<M>,

    /// The span where the where clause predicate was declared.
    pub predicate_declaration_span: Option<Span>,

    /// The span of the instantiation that causes the bound check.
    pub instantiation_span: Span,

    /// The overflow error that occurred.
    pub overflow_error: OverflowError,
}

impl<M: Model> Report<&Table> for UndecidablePredicate<M>
where
    Predicate<M>: pernixc_table::Display,
{
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.instantiation_span.clone(),
            message: format!(
                "the predicate `{}` is undecidable",
                DisplayObject { display: &self.predicate, table }
            ),
            severity: Severity::Error,
            help_message: Some(
                "try reduce the complexity of the code; this error is the \
                 limitation of the type-system/compiler"
                    .to_string(),
            ),
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
