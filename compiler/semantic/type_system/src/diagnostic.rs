//! Defines the diagnostic related to the type system checking

use bon::Builder;
use pernixc_diagnostic::{Highlight, Report};
use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::implements_arguments::get_implements_argument;
use pernixc_source_file::ByteIndex;
use pernixc_symbol::{source_map::to_absolute_span, span::get_span};
use pernixc_target::Global;
use pernixc_term::{
    display::Display, generic_arguments::GenericArguments, predicate::Predicate,
};
use qbice::{Decode, Encode, StableHash};

use crate::{OverflowError, UnrecoverableError};

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
    Encode,
    Decode,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Diagnostic {
    TypeCalculatingOverflow(TypeCalculatingOverflow),
    TypeCheckOverflow(TypeCheckOverflow),
    UnsatisfiedPredicate(UnsatisfiedPredicate),
    PredicateSatisfiabilityOverflow(PredicateSatisfiabilityOverflow),
    ImplementationIsNotGeneralEnough(ImplementationIsNotGeneralEnough),
    MismatchedImplementationArguments(MismatchedImplementationArguments),
    AdtImplementationIsNotGeneralEnough(AdtImplementationIsNotGeneralEnough),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
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
            Self::MismatchedImplementationArguments(diagnostic) => {
                diagnostic.report(parameter).await
            }
            Self::AdtImplementationIsNotGeneralEnough(diagnostic) => {
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
    Encode,
    Decode,
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
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        pernixc_diagnostic::Rendered::builder()
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
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
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
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        pernixc_diagnostic::Rendered::builder()
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
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
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
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        pernixc_diagnostic::Rendered::builder()
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

                    vec![
                        Highlight::builder()
                            .span(declaration_span)
                            .message("the required predicate was declared here")
                            .build(),
                    ]
                }
                None => Vec::new(),
            })
            .build()
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
    Encode,
    Decode,
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
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        pernixc_diagnostic::Rendered::builder()
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

                    vec![
                        Highlight::builder()
                            .span(declaration_span)
                            .message("the required predicate was declared here")
                            .build(),
                    ]
                }
                None => Vec::new(),
            })
            .build()
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
    Encode,
    Decode,
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
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let implementation_span = if let Some(span) =
            engine.get_span(self.resolvable_implementation_id).await
        {
            Some(engine.to_absolute_span(&span).await)
        } else {
            None
        };

        pernixc_diagnostic::Rendered::builder()
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
            .build()
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
    ) -> UnrecoverableError {
        handler
            .receive(TypeCalculatingOverflow::new(overflow_span, self).into());

        UnrecoverableError::Reported
    }

    /// Reports the [`OverflowError`] as a [`TypeCheckOverflow`] to
    /// be reported to the user.
    pub fn report_as_type_check_overflow(
        self,
        overflow_span: RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> UnrecoverableError {
        handler.receive(TypeCheckOverflow::new(overflow_span, self).into());

        UnrecoverableError::Reported
    }

    /// Reports the [`OverflowError`] as a [`PredicateSatisfiabilityOverflow`]
    /// to be reported to the user.
    pub fn report_as_undecidable_predicate(
        self,
        predicate: Predicate,
        predicate_declaration_span: Option<RelativeSpan>,
        instantiation_span: RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> UnrecoverableError {
        handler.receive(
            PredicateSatisfiabilityOverflow::new(
                predicate,
                predicate_declaration_span,
                instantiation_span,
                self,
            )
            .into(),
        );

        UnrecoverableError::Reported
    }
}

/// The generic arguments are not compatible with the generic arguments defined
/// in the implementation.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct MismatchedImplementationArguments {
    /// The ID of the ADT implementation where the generic arguments are
    /// mismatched.
    pub adt_implementation_id: Global<pernixc_symbol::ID>,

    /// The generic arguments found in the implementation.
    pub found_generic_arguments: GenericArguments,

    /// The span of the instantiation that causes the mismatch.
    pub instantiation_span: RelativeSpan,
}

impl Report for MismatchedImplementationArguments {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let impl_span = if let Some(span) =
            engine.get_span(self.adt_implementation_id).await
        {
            Some(engine.to_absolute_span(&span).await)
        } else {
            None
        };

        let impl_arguments = engine
            .get_implements_argument(self.adt_implementation_id)
            .await
            .unwrap();

        pernixc_diagnostic::Rendered::builder()
            .message(
                "the generic arguments are not compatible with the generic \
                 arguments defined in the implementation",
            )
            .primary_highlight(
                Highlight::builder()
                    .span(
                        engine.to_absolute_span(&self.instantiation_span).await,
                    )
                    .message({
                        let mut string = String::new();

                        string.push_str("the generic arguments supplied was `");
                        self.found_generic_arguments
                            .write_async(engine, &mut string)
                            .await
                            .unwrap();
                        string.push_str("` aren't compatible with `");
                        impl_arguments
                            .write_async(engine, &mut string)
                            .await
                            .unwrap();
                        string.push('`');

                        string
                    })
                    .build(),
            )
            .related(
                impl_span
                    .as_ref()
                    .map(|span| {
                        Highlight::new(
                            *span,
                            Some(
                                "the implementation is defined here"
                                    .to_string(),
                            ),
                        )
                    })
                    .into_iter()
                    .collect(),
            )
            .build()
    }
}

/// The ADT implementation is not general enough to satisfy the required forall
/// lifetimes in the generic arguments
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct AdtImplementationIsNotGeneralEnough {
    /// The ADT implementation ID where the generic arguments are not general
    /// enough.
    pub adt_implementation_id: Global<pernixc_symbol::ID>,

    /// The generic arguments supplied to the ADT.
    pub generic_arguments: GenericArguments,

    /// The span location of where the ADT is instantiated.
    pub instantiation_span: RelativeSpan,
}

impl Report for AdtImplementationIsNotGeneralEnough {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let span = engine.to_absolute_span(&self.instantiation_span).await;
        let impl_span = if let Some(span) =
            engine.get_span(self.adt_implementation_id).await
        {
            Some(engine.to_absolute_span(&span).await)
        } else {
            None
        };

        pernixc_diagnostic::Rendered::builder()
            .message(
                "the struct/enum implementation is not general enough to \
                 satisfy the required forall lifetimes in the generic \
                 arguments",
            )
            .primary_highlight(Highlight::new(
                span,
                Some({
                    let mut string = String::new();

                    string.push_str("the generic arguments supplied was `");
                    self.generic_arguments
                        .write_async(engine, &mut string)
                        .await
                        .unwrap();
                    string.push('`');

                    string
                }),
            ))
            .related(
                impl_span
                    .map(|span| {
                        Highlight::new(
                            span,
                            Some(
                                "the implementation is defined here"
                                    .to_string(),
                            ),
                        )
                    })
                    .into_iter()
                    .collect(),
            )
            .build()
    }
}
