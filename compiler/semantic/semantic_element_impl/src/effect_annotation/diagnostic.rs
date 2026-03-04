use pernixc_diagnostic::{ByteIndex, Highlight, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_resolution::{
    diagnostic::ForallLifetimeRedefinition, qualified_identifier::Resolution,
};
use pernixc_symbol::source_map::to_absolute_span;
use pernixc_term::{display::Display, effect};
use qbice::{Decode, Encode, Identifiable, StableHash};

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
    Identifiable,
    derive_more::From,
)]
#[allow(clippy::large_enum_variant)]
pub enum Diagnostic {
    Resolution(pernixc_resolution::diagnostic::Diagnostic),
    TypeSystem(pernixc_type_system::diagnostic::Diagnostic),
    ForallLifetimeRedefinition(ForallLifetimeRedefinition),
    AmbiguousEffectDefinition(AmbiguousEffectDefinition),
    EffectExpected(EffectExpected),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        match self {
            Self::Resolution(d) => d.report(engine).await,
            Self::TypeSystem(d) => d.report(engine).await,
            Self::ForallLifetimeRedefinition(d) => d.report(engine).await,
            Self::AmbiguousEffectDefinition(d) => d.report(engine).await,
            Self::EffectExpected(d) => d.report(engine).await,
        }
    }
}

/// Expected an effect but found something else.
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
pub struct EffectExpected {
    /// The span of the unexpected symbol.
    pub found: Resolution,

    /// The span where the unexpected symbol was found.
    pub found_span: RelativeSpan,
}

impl Report for EffectExpected {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let found_string = self.found.found_string(engine).await;
        let message = format!("expected an effect, found `{found_string}`");
        let span = engine.to_absolute_span(&self.found_span).await;

        pernixc_diagnostic::Rendered::builder()
            .message(message)
            .primary_highlight(Highlight::builder().span(span).build())
            .severity(pernixc_diagnostic::Severity::Error)
            .build()
    }
}

/// The `do Effects` annotation contains multiple effects that have the same
/// arguments but only differ in lifetimes.
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
pub struct AmbiguousEffectDefinition {
    /// The effect that was first defined with these arguments.
    pub first_effect: effect::Unit,

    /// The spans of all the ambiguous definitions.
    pub ambiguos_spans: Vec<RelativeSpan>,
}

impl Report for AmbiguousEffectDefinition {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let effect_string =
            self.first_effect.write_to_string(engine).await.unwrap();

        let message = format!(
            "the effect `{effect_string}` is defined multiple times with \
             different lifetimes"
        );

        let mut related = Vec::with_capacity(self.ambiguos_spans.len() - 1);
        for span in &self.ambiguos_spans[1..] {
            let absolute_span = engine.to_absolute_span(span).await;

            related.push(Highlight::builder().span(absolute_span).build());
        }

        let primary_span =
            engine.to_absolute_span(&self.ambiguos_spans[0]).await;

        pernixc_diagnostic::Rendered::builder()
            .message(message)
            .primary_highlight(
                Highlight::builder()
                    .span(primary_span)
                    .message(format!(
                        "`{effect_string}` was first defined here"
                    ))
                    .build(),
            )
            .related(related)
            .severity(pernixc_diagnostic::Severity::Error)
            .build()
    }
}
