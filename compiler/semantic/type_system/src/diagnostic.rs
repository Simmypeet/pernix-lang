//! Defines the diagnostic related to the type system checking

use bon::Builder;
use pernixc_diagnostic::{Highlight, Report};
use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{source_map::to_absolute_span, span::get_span};
use pernixc_target::Global;
use pernixc_term::{
    display::Display, generic_arguments::GenericArguments, predicate::Predicate,
};

use crate::OverflowError;

/// Diagnostic messages for the type system.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Diagnostic {
    TypeCalculatingOverflow(TypeCalculatingOverflow),
    TypeCheckOverflow(TypeCheckOverflow),
    UnsatisfiedPredicate(UnsatisfiedPredicate),
    PredicateSatisfiabilityOverflow(PredicateSatisfiabilityOverflow),
    ImplementationIsNotGeneralEnough(ImplementationIsNotGeneralEnough),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        match self {
            Self::TypeCalculatingOverflow(diagnostic) => {
                diagnostic.report(parameter).await
            }
            Self::TypeCheckOverflow(diagnostic) => {
                diagnostic.report(parameter).await
            }
            Self::UnsatisfiedPredicate(diagnostic) => {
                diagnostic.report(parameter).await
            }
            Self::PredicateSatisfiabilityOverflow(diagnostic) => {
                diagnostic.report(parameter).await
            }
            Self::ImplementationIsNotGeneralEnough(diagnostic) => {
                diagnostic.report(parameter).await
            }
        }
    }
}

/// An [`OverflowError`] occurred while calculating the type of an expression or
/// symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_new::new,
    Builder,
)]
pub struct TypeCalculatingOverflow {
    /// The span where the overflow occurred.
    pub overflow_span: RelativeSpan,

    /// The [`OverflowError`] that occurred.
    pub overflow_error: OverflowError,
}

impl Report for TypeCalculatingOverflow {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Diagnostic::builder()
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
            .build())
    }
}

/// An [`OverflowError`] occurred while performing a type check operation.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_new::new,
    Builder,
)]
pub struct TypeCheckOverflow {
    /// The span where the overflow occurred.
    pub overflow_span: RelativeSpan,

    /// The [`OverflowError`] that occurred.
    pub overflow_error: OverflowError,
}

impl Report for TypeCheckOverflow {
    async fn report(
        &self,
        tracked_engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Diagnostic::builder()
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
            .build())
    }
}

/// The satisfiability of the predicate can't be decided (most likely Overflow
/// error).
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_new::new,
    Builder,
)]
pub struct PredicateSatisfiabilityOverflow {
    /// The undecidable predicate.
    pub predicate: Predicate,

    /// The span where the where clause predicate was declared.
    pub predicate_declaration_span: Option<RelativeSpan>,

    /// The span of the instantiation that causes the bound check.
    pub instantiation_span: RelativeSpan,

    /// The overflow error that occurred.
    pub overflow_error: OverflowError,
}

impl Report for PredicateSatisfiabilityOverflow {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Diagnostic::builder()
            .primary_highlight(
                Highlight::builder()
                    .span(
                        engine.to_absolute_span(&self.instantiation_span).await,
                    )
                    .message({
                        let mut message = String::new();
                        message
                            .push_str("the satisfiability of the predicate `");
                        self.predicate
                            .write_async(engine, &mut message)
                            .await
                            .unwrap();
                        message.push_str("` can't be decided");
                        message
                    })
                    .build(),
            )
            .message("overflow checking the predicate")
            .help_message(
                "try reduce the complexity of the code; this error is the \
                 limitation of the type-system/compiler",
            )
            .related(match &self.predicate_declaration_span {
                Some(span) => {
                    let declaration_span = engine.to_absolute_span(span).await;

                    vec![Highlight::builder()
                        .span(declaration_span)
                        .message("the required predicate was declared here")
                        .build()]
                }
                None => Vec::new(),
            })
            .build())
    }
}

/// The bound is not satisfied upon instantiation.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    Builder,
)]
pub struct UnsatisfiedPredicate {
    /// The unsatisfied bound.
    pub predicate: Predicate,

    /// The span of the instantiation that causes the bound check.
    pub instantiation_span: RelativeSpan,

    /// The span of the predicate declaration.
    pub predicate_declaration_span: Option<RelativeSpan>,
}

impl Report for UnsatisfiedPredicate {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Diagnostic::builder()
            .primary_highlight(
                Highlight::builder()
                    .span(
                        engine.to_absolute_span(&self.instantiation_span).await,
                    )
                    .message({
                        let mut message = String::new();
                        message.push_str("the predicate `");
                        self.predicate
                            .write_async(engine, &mut message)
                            .await
                            .unwrap();
                        message.push_str("` is not satisfied");
                        message
                    })
                    .build(),
            )
            .message("unsatisfied predicate")
            .related(match &self.predicate_declaration_span {
                Some(span) => {
                    let declaration_span = engine.to_absolute_span(span).await;

                    vec![Highlight::builder()
                        .span(declaration_span)
                        .message("the required predicate was declared here")
                        .build()]
                }
                None => Vec::new(),
            })
            .build())
    }
}

/// The implementation is not general enough to satisfy the required
/// predicate's forall lifetimes.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    Builder,
)]
pub struct ImplementationIsNotGeneralEnough {
    /// The ID of the implementation where the predicate is not satisfied.
    pub resolvable_implementation_id: Global<pernixc_symbol::ID>,

    /// The generic arguments required by the trait predicate.
    pub generic_arguments: GenericArguments,

    /// The span where the trait predicate was declared.
    pub predicate_declaration_span: Option<RelativeSpan>,

    /// The span of the instantiation that causes the error
    pub instantiation_span: RelativeSpan,
}

impl Report for ImplementationIsNotGeneralEnough {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let implementation_span = if let Some(span) =
            engine.get_span(self.resolvable_implementation_id).await
        {
            Some(engine.to_absolute_span(&span).await)
        } else {
            None
        };

        Ok(pernixc_diagnostic::Diagnostic::builder()
            .primary_highlight(
                Highlight::builder()
                    .span(
                        engine.to_absolute_span(&self.instantiation_span).await,
                    )
                    .message({
                        let mut message = "the implementation is not general \
                                           enought to satisfy the required \
                                           forall lifetimes in the generic \
                                           arguments: "
                            .to_string();

                        self.generic_arguments
                            .write_async(engine, &mut message)
                            .await
                            .unwrap();

                        message
                    })
                    .build(),
            )
            .message("implementation is not general enough")
            .related({
                let mut related = Vec::new();

                if let Some(span) = self.predicate_declaration_span.as_ref() {
                    related.push(
                        Highlight::builder()
                            .span(engine.to_absolute_span(span).await)
                            .message("the predicate is declared here")
                            .build(),
                    );
                }

                if let Some(span) = implementation_span.as_ref() {
                    related.push(
                        Highlight::builder()
                            .span(*span)
                            .message(
                                "this implementation was used but its \
                                 lifetimes are not general enough",
                            )
                            .build(),
                    );
                }

                related
            })
            .build())
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "overflow occurred during type checking, the error has been reported to \
     the user"
)]
#[allow(missing_docs)]
pub struct Reported;

impl OverflowError {
    /// Reports the [`OverflowError`] as a
    /// [`TypeCalculatingOverflow`] to be reported to the user.
    pub fn report_as_type_calculating_overflow(
        self,
        overflow_span: RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) {
        handler
            .receive(TypeCalculatingOverflow::new(overflow_span, self).into());
    }

    /// Reports the [`OverflowError`] as a [`TypeCheckOverflow`] to
    /// be reported to the user.
    pub fn report_as_type_check_overflow(
        self,
        overflow_span: RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) {
        handler.receive(TypeCheckOverflow::new(overflow_span, self).into());
    }

    /// Reports the [`OverflowError`] as a [`PredicateSatisfiabilityOverflow`]
    /// to be reported to the user.
    pub fn report_as_undecidable_predicate(
        self,
        predicate: Predicate,
        predicate_declaration_span: Option<RelativeSpan>,
        instantiation_span: RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) {
        handler.receive(
            PredicateSatisfiabilityOverflow::new(
                predicate,
                predicate_declaration_span,
                instantiation_span,
                self,
            )
            .into(),
        );
    }
}
