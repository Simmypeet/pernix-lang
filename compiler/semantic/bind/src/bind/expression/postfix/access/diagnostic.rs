use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::source_map::to_absolute_span;
use pernixc_term::{
    constant::Constant,
    display::{Display, InferenceRenderingMap},
    r#type::Type,
};

pub use crate::pattern::bind::diagnostic::{
    FieldIsNotAccessible, FieldNotFound,
};

/// Expected a struct type to access a field member.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct ExpectedStruct {
    /// The span where the type check occurred.
    pub span: RelativeSpan,

    /// The expected struct type.
    pub found_type: Type,

    /// The inference rendering map for constants.
    pub constant_inference_map: InferenceRenderingMap<Constant>,

    /// The inference rendering map for types.
    pub type_inference_map: InferenceRenderingMap<Type>,
}

impl Report for ExpectedStruct {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let span = engine.to_absolute_span(&self.span).await;

        // Format the found type with inference rendering maps
        let mut found_type_str = String::new();
        self.found_type
            .write_async_with_mapping(
                engine,
                &mut found_type_str,
                None,
                Some(&self.type_inference_map),
                Some(&self.constant_inference_map),
            )
            .await
            .unwrap();

        Ok(pernixc_diagnostic::Rendered::builder()
            .severity(Severity::Error)
            .message("expected a struct type")
            .primary_highlight(
                Highlight::builder()
                    .message(format!("found type '{found_type_str}'",))
                    .span(span)
                    .build(),
            )
            .help_message(
                "a struct type is required to access its field members",
            )
            .build())
    }
}
