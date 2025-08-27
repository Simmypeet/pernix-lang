use derive_more::From;
use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    kind::get_kind, name::get_qualified_name, source_map::to_absolute_span,
};
use pernixc_target::Global;
use pernixc_term::{display::Display, r#type::Type};

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
    From,
)]
pub enum Diagnostic {
    Resolution(pernixc_resolution::diagnostic::Diagnostic),
    InvalidSymbolForImplements(InvalidSymbolForImplements),
    InvalidTypeForImplements(InvalidTypeForImplements),
}

impl Report<&TrackedEngine> for Diagnostic {
    type Location = ByteIndex;

    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        match self {
            Self::Resolution(diag) => diag.report(parameter).await,
            Self::InvalidSymbolForImplements(diag) => {
                diag.report(parameter).await
            }
            Self::InvalidTypeForImplements(diag) => {
                diag.report(parameter).await
            }
        }
    }
}

/// The symbol kinds cannot be implemented.
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
pub struct InvalidSymbolForImplements {
    /// The qualified identifier span of the resolved symbol.
    pub qualified_identifier_span: RelativeSpan,

    /// The resolved symbol ID that's not valid for implements.
    pub symbol_id: Global<pernixc_symbol::ID>,
}

impl Report<&TrackedEngine> for InvalidSymbolForImplements {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let qualified_name = engine.get_qualified_name(self.symbol_id).await;
        let kind = engine.get_kind(self.symbol_id).await;

        let span =
            engine.to_absolute_span(&self.qualified_identifier_span).await;

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("the symbol cannot be implemented")
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "the symbol `{} {qualified_name}` cannot be \
                         implemented",
                        kind.kind_str()
                    ))
                    .span(span)
                    .build(),
            )
            .help_message(
                "only `trait`, `marker`, `enum`, or `struct` can be \
                 implemented"
                    .to_string(),
            )
            .build()
    }
}

/// The type cannot be implemented.
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
)]
pub struct InvalidTypeForImplements {
    /// The qualified identifier span of the resolved type.
    pub qualified_identifier_span: RelativeSpan,

    /// The resolved type that's not valid for implements.
    pub r#type: Type,
}

impl Report<&TrackedEngine> for InvalidTypeForImplements {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let span =
            engine.to_absolute_span(&self.qualified_identifier_span).await;

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("the type cannot be implemented")
            .primary_highlight(
                Highlight::builder()
                    .message({
                        let mut buf = String::new();
                        buf.push_str("the type `");
                        self.r#type
                            .write_async(engine, &mut buf)
                            .await
                            .unwrap();
                        buf.push_str("` cannot be implemented");

                        buf
                    })
                    .span(span)
                    .build(),
            )
            .help_message(
                "only `enum`, or `struct` can be implemented".to_string(),
            )
            .build()
    }
}
