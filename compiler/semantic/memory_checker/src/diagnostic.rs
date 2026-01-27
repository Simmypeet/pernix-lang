//! Diagnostics for the memory checker.

use pernixc_diagnostic::{ByteIndex, Highlight, Report, Severity};
use pernixc_ir::value::register::load;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::source_map::to_absolute_span;
use qbice::{Decode, Encode, StableHash};

/// Enumeration of all diagnostics that can be produced by the memory checker.
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
#[allow(clippy::large_enum_variant, missing_docs)]
pub enum Diagnostic {
    TypeSystem(pernixc_type_system::diagnostic::Diagnostic),
    MovedOutValueFromMutableReference(MovedOutValueFromMutableReference),
    UseBeforeInitialization(UseBeforeInitialization),
    UseAfterMove(UseAfterMove),
    MoveInLoop(MoveInLoop),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        match self {
            Self::TypeSystem(d) => d.report(engine).await,
            Self::MovedOutValueFromMutableReference(d) => {
                d.report(engine).await
            }
            Self::UseBeforeInitialization(d) => d.report(engine).await,
            Self::UseAfterMove(d) => d.report(engine).await,
            Self::MoveInLoop(d) => d.report(engine).await,
        }
    }
}

/// The value behind the mutable reference has been moved out and needs to be
/// restored.
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
)]
pub struct MovedOutValueFromMutableReference {
    /// The span of the moved out value.
    pub moved_out_value_span: RelativeSpan,

    /// The span of the mutable reference.
    pub reassignment_span: Option<RelativeSpan>,
}

impl Report for MovedOutValueFromMutableReference {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let mut related = Vec::new();

        if let Some(reassignment_span) = &self.reassignment_span {
            related.push(
                Highlight::builder()
                    .span(engine.to_absolute_span(reassignment_span).await)
                    .message(
                        "this assignment makes the value behind mutable \
                         reference inaccessible from now on...",
                    )
                    .build(),
            );
        }

        pernixc_diagnostic::Rendered::builder()
            .message(
                "the value behind the mutable reference has been moved out \
                 and needs to be restored",
            )
            .severity(Severity::Error)
            .primary_highlight(
                Highlight::builder()
                    .span(
                        engine
                            .to_absolute_span(&self.moved_out_value_span)
                            .await,
                    )
                    .build(),
            )
            .related(related)
            .build()
    }
}

/// The value is used before it has been initialized.
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
)]
pub struct UseBeforeInitialization {
    /// The span where the value is used.
    pub use_span: RelativeSpan,
}

impl Report for UseBeforeInitialization {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        pernixc_diagnostic::Rendered::builder()
            .message("the value is used before it has been initialized")
            .severity(Severity::Error)
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.use_span).await)
                    .build(),
            )
            .build()
    }
}

/// The value is used after it has been moved.
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
)]
pub struct UseAfterMove {
    /// The span where the value is used.
    pub use_span: RelativeSpan,

    /// The span where the value is moved.
    pub move_span: RelativeSpan,

    /// The purpose of the load/move.
    pub load_purpose: load::Purpose,
}

impl Report for UseAfterMove {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let use_span = engine.to_absolute_span(&self.use_span).await;
        let move_span = engine.to_absolute_span(&self.move_span).await;

        pernixc_diagnostic::Rendered::builder()
            .message("the value is used after it has been moved")
            .severity(Severity::Error)
            .primary_highlight(Highlight::builder().span(use_span).build())
            .related(vec![
                Highlight::builder()
                    .span(move_span)
                    .message(match self.load_purpose {
                        load::Purpose::General => {
                            "the value was moved here".to_string()
                        }
                        load::Purpose::Capture => "the value was moved here \
                                                   for closure capture"
                            .to_string(),
                    })
                    .build(),
            ])
            .build()
    }
}

/// The value has been moved inside the loop, which could be used in the
/// subsequent iteration.
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
)]
pub struct MoveInLoop {
    /// The span of the moved value.
    pub moved_value_span: RelativeSpan,

    /// The purpose of the load/move.
    pub load_purpose: load::Purpose,
}

impl Report for MoveInLoop {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let moved_value_span =
            engine.to_absolute_span(&self.moved_value_span).await;

        pernixc_diagnostic::Rendered::builder()
            .message(match self.load_purpose {
                load::Purpose::General => "the value has been moved inside \
                                           the loop, which could be used in \
                                           the subsequent iteration"
                    .to_string(),
                load::Purpose::Capture => {
                    "the value has been moved inside the loop for closure \
                     capture, which could be used in the subsequent iteration"
                        .to_string()
                }
            })
            .severity(Severity::Warning)
            .primary_highlight(
                Highlight::builder().span(moved_value_span).build(),
            )
            .build()
    }
}
