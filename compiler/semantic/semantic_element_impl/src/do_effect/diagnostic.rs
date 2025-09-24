use pernixc_diagnostic::{ByteIndex, Highlight, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_resolution::diagnostic::ForallLifetimeRedefinition;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    kind::get_kind, name::get_qualified_name, source_map::to_absolute_span,
};
use pernixc_target::Global;

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
pub enum Diagnostic {
    Resolution(pernixc_resolution::diagnostic::Diagnostic),
    ForallLifetimeRedefinition(ForallLifetimeRedefinition),
    EffectExpected(EffectExpected),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        match self {
            Self::Resolution(d) => d.report(engine).await,
            Self::ForallLifetimeRedefinition(d) => d.report(engine).await,
            Self::EffectExpected(d) => d.report(engine).await,
        }
    }
}

/// Expected an effect but found something else.
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
)]
pub struct EffectExpected {
    /// The span of the unexpected symbol.
    pub found: Global<pernixc_symbol::ID>,

    /// The span where the unexpected symbol was found.
    pub found_span: RelativeSpan,
}

impl Report for EffectExpected {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let q_name = engine.get_qualified_name(self.found).await;
        let kind = engine.get_kind(self.found).await;

        let span = engine.to_absolute_span(&self.found_span).await;

        let message = format!(
            "expected an effect, found a `{} {q_name}`",
            kind.kind_str(),
        );

        Ok(pernixc_diagnostic::Rendered::builder()
            .message(message)
            .primary_highlight(
                Highlight::builder()
                    .span(span)
                    .message(format!("`{q_name}` is a {}`", kind.kind_str()))
                    .build(),
            )
            .severity(pernixc_diagnostic::Severity::Error)
            .build())
    }
}
