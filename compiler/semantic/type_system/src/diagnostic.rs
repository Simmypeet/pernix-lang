//! Defines the diagnostic related to the type system checking

use pernixc_diagnostic::{Highlight, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_source_file::ByteIndex;
use pernixc_symbol::source_map::to_absolute_span;
use pernixc_term::predicate::Predicate;

use crate::OverflowError;

/// An [`OverflowError`] occurred while calculating the type of an expression or
/// symbol.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_new::new,
)]
pub struct TypeCalculatingOverflow {
    /// The span where the overflow occurred.
    pub overflow_span: RelativeSpan,

    /// The [`OverflowError`] that occurred.
    pub overflow_error: OverflowError,
}

impl Report<&TrackedEngine> for TypeCalculatingOverflow {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic::builder()
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.overflow_span).await)
                    .message(
                        "couldn't calculate the type of this expression/symbol",
                    )
                    .build(),
            )
            .message("overflow calculating the type")
            .help_message(
                "this is due to the limitation of the compiler/language, try \
                 reduce the complexity of the expression/symbol",
            )
            .build()
    }
}

/// An [`OverflowError`] occurred while performing a type check operation.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_new::new,
)]
pub struct TypeCheckOverflow {
    /// The span where the overflow occurred.
    pub overflow_span: RelativeSpan,

    /// The [`OverflowError`] that occurred.
    pub overflow_error: OverflowError,
}

impl Report<&TrackedEngine> for TypeCheckOverflow {
    type Location = ByteIndex;

    async fn report(
        &self,
        tracked_engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic::builder()
            .primary_highlight(
                Highlight::builder()
                    .span(
                        tracked_engine
                            .to_absolute_span(&self.overflow_span)
                            .await,
                    )
                    .message("couldn't type check of this expression/symbol")
                    .build(),
            )
            .message("overflow checking the type")
            .help_message(
                "try reduce the complexity of the code; this error is the \
                 limitation of the type-system/compiler",
            )
            .build()
    }
}

/// The satisfiability of the predicate can't be decided (most likely Overflow
/// error).
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_new::new,
)]
pub struct UndecidablePredicate {
    /// The undecidable predicate.
    pub predicate: Predicate,

    /// The span where the where clause predicate was declared.
    pub predicate_declaration_span: Option<RelativeSpan>,

    /// The span of the instantiation that causes the bound check.
    pub instantiation_span: RelativeSpan,

    /// The overflow error that occurred.
    pub overflow_error: OverflowError,
}

/*
impl Report<&TrackedEngine> for UndecidablePredicate {
    type Location = ByteIndex;

    async fn report(&self, table: &TrackedEngine) -> Diagnostic {
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
    Predicate<M>: table::Display,
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
    GenericArguments<M>: table::Display,
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
*/
