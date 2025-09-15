use pernixc_diagnostic::{Highlight, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::source_map::to_absolute_span;

use crate::diagnostic_enum;

diagnostic_enum! {
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
    pub enum Diagnostic {
        InvalidNumericSuffix(InvalidNumericSuffix),
        FloatingPointLiteralHasIntegralSuffix(
            FloatingPointLiteralHasIntegralSuffix
        )
    }
}

/// The numeric suffix is unknown.
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
pub struct InvalidNumericSuffix {
    /// The span of the numeric suffix.
    pub suffix_span: RelativeSpan,
}

impl Report for InvalidNumericSuffix {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let span = engine.to_absolute_span(&self.suffix_span).await;

        Ok(pernixc_diagnostic::Rendered::builder()
            .message("invalid numeric suffix")
            .primary_highlight(
                Highlight::builder()
                    .span(span)
                    .message("this suffix is not recognized")
                    .build(),
            )
            .severity(pernixc_diagnostic::Severity::Error)
            .help_message(
                "valid suffixes are: i8, i16, i32, i64, u8, u16, u32, u64, \
                 f32, f64, us, is",
            )
            .build())
    }
}

/// Thee numeric expression has a suffix of an integral type but the expression
/// has decimal point.
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
pub struct FloatingPointLiteralHasIntegralSuffix {
    /// The span of the numeric literal.
    pub numeric_literal_span: RelativeSpan,
}

impl Report for FloatingPointLiteralHasIntegralSuffix {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let span = engine.to_absolute_span(&self.numeric_literal_span).await;

        Ok(pernixc_diagnostic::Rendered::builder()
            .message("floating point literal has integral suffix")
            .primary_highlight(
                Highlight::builder()
                    .span(span)
                    .message(
                        "a floating point literal cannot have an integral \
                         suffix",
                    )
                    .build(),
            )
            .severity(pernixc_diagnostic::Severity::Error)
            .help_message(
                "remove the decimal point or change the suffix to a floating \
                 point type (such as f32 or f64)",
            )
            .build())
    }
}
