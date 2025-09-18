use pernixc_diagnostic::{ByteIndex, Highlight, Rendered, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::source_map::to_absolute_span;

/// Enumeration of all operations that required unsafe scope.
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
pub enum UnsafeOperation {
    /// Casting another type to reference type.
    ReferenceTypeCast,

    /// Calling an extern function
    ExternFunctionCall,

    /// Calling an unsafe function.
    UnsafeFunctionCall,
}

/// The operation requires an unsafe scope.
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
pub struct UnsafeRequired {
    /// The span of the expression where the unsafe operation is performed.
    pub expression_span: RelativeSpan,

    /// The operation that requires an unsafe scope.
    pub operation: UnsafeOperation,
}

impl Report for UnsafeRequired {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, CyclicError> {
        let operation_string = match self.operation {
            UnsafeOperation::ReferenceTypeCast => {
                "casting to reference type".to_string()
            }

            UnsafeOperation::ExternFunctionCall => {
                "calling an `extern` function".to_string()
            }

            UnsafeOperation::UnsafeFunctionCall => {
                "calling an `unsafe` function".to_string()
            }
        };

        let message = format!(
            "the operation of {operation_string} is unsafe and requires an \
             unsafe scope"
        );

        Ok(pernixc_diagnostic::Rendered::builder()
            .message(message)
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.expression_span).await)
                    .build(),
            )
            .severity(pernixc_diagnostic::Severity::Error)
            .help_message(
                "these operations require an extra level of caution and must \
                 be performed within an `unsafe scope`",
            )
            .build())
    }
}
