//! Contains the diagnostic information related to the type system overflows.

use derive_new::new;
use pernixc_diagnostic::{Diagnostic, Related, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;
use pernixc_semantic::{component::LocationSpan, DisplayObject, GlobalID, Table};
use pernixc_term::{
    generic_arguments::GenericArguments, predicate::Predicate, Model,
};

use crate::OverflowError;

/// An [`OverflowError`] occurred while calculating the type of an expression or
/// symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct TypeCalculatingOverflow {
    /// The span where the overflow occurred.
    pub overflow_span: Span,

    /// The [`OverflowError`] that occurred.
    pub overflow_error: OverflowError,
}

impl Report<&Table> for TypeCalculatingOverflow {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.overflow_span.clone(),
            message: "overflow calculating the type".to_string(),
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

/// An [`OverflowError`] occurred while performing a type check operation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct TypeCheckOverflow {
    /// The span where the overflow occurred.
    pub overflow_span: Span,

    /// The [`OverflowError`] that occurred.
    pub overflow_error: OverflowError,
}

impl Report<&Table> for TypeCheckOverflow {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.overflow_span.clone(),
            message: "overflow checking the type".to_string(),
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

/// The satisfiability of the predicate can't be decided (most likely Overflow
/// error).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
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
    Predicate<M>: pernixc_semantic::Display,
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
    Predicate<M>: pernixc_semantic::Display,
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

/// The implementation is not general enough to satisfy the required
/// predicate's forall lifetimes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementationIsNotGeneralEnough<M: Model> {
    /// The ID of the implementation where the predicate is not satisfied.
    pub resolvable_implementation_id: GlobalID,

    /// The generic arguments required by the trait predicate.
    pub generic_arguments: GenericArguments<M>,

    /// The span where the trait predicate was declared.
    pub predicate_declaration_span: Option<Span>,

    /// The span of the instantiation that cuases the error
    pub instantiation_span: Span,
}

impl<M: Model> Report<&Table> for ImplementationIsNotGeneralEnough<M>
where
    GenericArguments<M>: pernixc_semantic::Display,
{
    fn report(&self, table: &Table) -> pernixc_diagnostic::Diagnostic {
        let span = table.get::<LocationSpan>(self.resolvable_implementation_id);

        pernixc_diagnostic::Diagnostic {
            span: self.instantiation_span.clone(),
            message: format!(
                "the implementation is not general enough to satisfy the \
                 required forall lifetimes in the generic arguments: {}",
                DisplayObject { table, display: &self.generic_arguments }
            ),
            severity: Severity::Error,
            help_message: None,
            related: self
                .predicate_declaration_span
                .as_ref()
                .map(|span| Related {
                    span: span.clone(),
                    message: "the predicate is declared here".to_string(),
                })
                .into_iter()
                .chain(span.span.as_ref().map(|span| Related {
                    span: span.clone(),
                    message: "the implementation is defined here".to_string(),
                }))
                .collect(),
        }
    }
}
