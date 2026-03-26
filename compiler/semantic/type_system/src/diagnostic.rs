//! Defines the diagnostic related to the type system checking

use bon::{Builder, bon};
use pernixc_diagnostic::{Highlight, Note, Report};
use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::implements_arguments::get_implements_argument;
use pernixc_source_file::ByteIndex;
use pernixc_symbol::{source_map::to_absolute_span, span::get_span};
use pernixc_target::Global;
use pernixc_term::{
    display::Display,
    generic_arguments::GenericArguments,
    instance::{Instance, TraitRef},
    predicate::Predicate,
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
    FoundNegativeImplementation(FoundNegativeImplementation),
    MismatchedTraitRef(MismatchedTraitRef),
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
            Self::FoundNegativeImplementation(diagnostic) => {
                diagnostic.report(parameter).await
            }
            Self::MismatchedTraitRef(diagnostic) => {
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

/// Used to attach to the [`RequiredBy`] to indicate that the required predicate
/// is required by an implementation.
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
pub struct RequiredByImplements {
    resolved_implements_id: Global<pernixc_symbol::SymbolID>,
    predicate: pernixc_term::predicate::PositiveMarker,
}

/// Used to attach to [`UnsatisfiedPredicate`] to indicate where the unsatisfied
/// predicate is required by an implementation.
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
pub struct RequiredBy {
    predicate_declaration_span: Option<RelativeSpan>,
    by_implements: Option<RequiredByImplements>,
}

impl RequiredBy {
    async fn generate_note(
        &self,
        engine: &TrackedEngine,
    ) -> Option<pernixc_diagnostic::Note<ByteIndex>> {
        if let Some(by_implements) = &self.by_implements {
            let mut message = "required for predicate ".to_string();
            by_implements
                .predicate
                .write_async(engine, &mut message)
                .await
                .unwrap();
            message.push_str(" to be implemented");

            let implements_span = if let Some(implements_span) =
                engine.get_span(by_implements.resolved_implements_id).await
            {
                Some(engine.to_absolute_span(&implements_span).await)
            } else {
                None
            };

            let primary_highlight =
                if let Some(span) = self.predicate_declaration_span.as_ref() {
                    Some(
                        Highlight::builder()
                            .span(engine.to_absolute_span(span).await)
                            .message("the required predicate is declared here")
                            .build(),
                    )
                } else {
                    None
                };

            Some(
                Note::builder()
                    .message(message)
                    .maybe_primary_highlight(primary_highlight)
                    .related(
                        implements_span
                            .map(|span| Highlight::builder().span(span).build())
                            .into_iter()
                            .collect(),
                    )
                    .build(),
            )
        } else {
            let primary_highlight = self.predicate_declaration_span.as_ref()?;

            Some(
                Note::builder()
                    .message("the required predicate is declared here")
                    .primary_highlight(
                        Highlight::builder()
                            .span(
                                engine
                                    .to_absolute_span(primary_highlight)
                                    .await,
                            )
                            .build(),
                    )
                    .build(),
            )
        }
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
)]
pub struct UnsatisfiedPredicate {
    /// The unsatisfied bound.
    predicate: Predicate,

    /// The span of the instantiation that causes the bound check.
    instantiation_span: RelativeSpan,

    /// The stack of requirements that lead to the unsatisfied predicate, used
    /// to provide more context to the user about why the predicate is not
    /// satisfied.
    requirement_stack: Vec<RequiredBy>,
}

#[bon]
impl UnsatisfiedPredicate {
    /// Returns a builder for creating [`UnsatisfiedPredicate`].
    #[builder(finish_fn = build)]
    pub fn builder(
        predicate: Predicate,
        instantiation_span: RelativeSpan,
        predicate_declaration_span: Option<RelativeSpan>,
    ) -> Self {
        Self {
            predicate,
            instantiation_span,
            requirement_stack: predicate_declaration_span
                .map(|span| RequiredBy {
                    by_implements: None,
                    predicate_declaration_span: Some(span),
                })
                .into_iter()
                .collect(),
        }
    }

    /// Returns a builder for creating [`UnsatisfiedPredicate`] with explicitly
    /// given requirement stack.
    #[builder(finish_fn = build)]
    pub const fn builder_with_required_by_stack(
        predicate: Predicate,
        instantiation_span: RelativeSpan,
        requirement_stack: Vec<RequiredBy>,
    ) -> Self {
        Self { predicate, instantiation_span, requirement_stack }
    }
}

async fn generate_notes(
    requirement_stack: &[RequiredBy],
    engine: &TrackedEngine,
) -> Vec<pernixc_diagnostic::Note<ByteIndex>> {
    let mut notes = Vec::new();

    for requirement in requirement_stack {
        if let Some(note) = requirement.generate_note(engine).await {
            notes.push(note);
        }
    }

    notes
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
                    .build(),
            )
            .message({
                let mut message = String::new();

                message.push_str("the predicate `");
                self.predicate.write_async(engine, &mut message).await.unwrap();
                message.push_str("` is not satisfied");

                message
            })
            .notes(generate_notes(&self.requirement_stack, engine).await)
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
    resolvable_implementation_id: Global<pernixc_symbol::SymbolID>,

    /// The generic arguments required by the trait predicate.
    generic_arguments: GenericArguments,

    /// The span of the instantiation that causes the error
    instantiation_span: RelativeSpan,

    /// The stack of requirements that lead to the error, used to provide more
    /// context to the user about why the implementation is not general enough.
    required_by_stack: Vec<RequiredBy>,
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
            .notes(generate_notes(&self.required_by_stack, engine).await)
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
    pub adt_implementation_id: Global<pernixc_symbol::SymbolID>,

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
    pub adt_implementation_id: Global<pernixc_symbol::SymbolID>,

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

/// A negative implementation was found that explicitly marks the predicate as
/// unsatisfiable.
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
pub struct FoundNegativeImplementation {
    /// The predicate that was found to be explicitly unsatisfiable.
    predicate: pernixc_term::predicate::PositiveMarker,

    /// The ID of the negative implementation.
    negative_implementation_id: Global<pernixc_symbol::SymbolID>,

    /// The span of the instantiation that causes the check.
    instantiation_span: RelativeSpan,

    /// The stack of requirements that lead to the negative implementation
    /// being checked, used to provide more context to the user.
    requirement_stack: Vec<RequiredBy>,
}

impl Report for FoundNegativeImplementation {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let negative_impl_span = if let Some(span) =
            engine.get_span(self.negative_implementation_id).await
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
                        let mut message = String::new();
                        message.push_str("the predicate `");
                        self.predicate
                            .write_async(engine, &mut message)
                            .await
                            .unwrap();
                        message.push_str(
                            "` is explicitly marked as unsatisfiable by a \
                             negative implementation",
                        );
                        message
                    })
                    .build(),
            )
            .message("found negative implementation")
            .related({
                let mut related = Vec::new();

                if let Some(span) = negative_impl_span.as_ref() {
                    related.push(
                        Highlight::builder()
                            .span(*span)
                            .message(
                                "this negative implementation explicitly \
                                 marks the predicate as unsatisfiable",
                            )
                            .build(),
                    );
                }

                related
            })
            .notes(generate_notes(&self.requirement_stack, engine).await)
            .build()
    }
}

/// A diagnostic generated by instance argument being supplied with a mismatched
/// trait reference.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Builder,
)]
pub struct MismatchedTraitRef {
    found_trait_ref: TraitRef,
    expected_trait_ref: TraitRef,
    instance: Instance,
    span: RelativeSpan,
}

impl Report for MismatchedTraitRef {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let found_string =
            self.found_trait_ref.write_to_string(engine).await.unwrap();
        let expected_string =
            self.expected_trait_ref.write_to_string(engine).await.unwrap();
        let instance_string =
            self.instance.write_to_string(engine).await.unwrap();

        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.span).await)
                    .message(format!(
                        "instance `{instance_string}` implements \
                         `{found_string}` but `{expected_string}` was expected"
                    ))
                    .build(),
            )
            .message("mismatched trait reference")
            .build()
    }
}
