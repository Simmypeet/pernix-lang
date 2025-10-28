use flexstr::SharedStr;
use pernixc_diagnostic::{Highlight, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{name::get_qualified_name, source_map::to_absolute_span};
use pernixc_target::Global;

/// Diagnostic for when a member is redefined across different implementation
/// blocks for the same ADT.
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
pub struct AdtImplementationMemberRedefinition {
    /// The name of the member that is redefined.
    pub member_name: SharedStr,

    /// The ID of the ADT being implemented.
    pub adt_id: Global<pernixc_symbol::ID>,

    /// The ID of the first implementation block.
    pub first_implementation_id: Global<pernixc_symbol::ID>,

    /// The span of the first member definition.
    pub first_span: Option<RelativeSpan>,

    /// The ID of the second implementation block.
    pub second_implementation_id: Global<pernixc_symbol::ID>,

    /// The span of the second member definition.
    pub second_span: Option<RelativeSpan>,
}

impl Report for AdtImplementationMemberRedefinition {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let adt_name = engine.get_qualified_name(self.adt_id).await;

        let primary_highlight = if let Some(second_span) = &self.second_span {
            Some(Highlight::new(
                engine.to_absolute_span(second_span).await,
                Some(format!("redefinition here")),
            ))
        } else {
            None
        };

        let related = if let Some(first_span) = &self.first_span {
            vec![Highlight::new(
                engine.to_absolute_span(first_span).await,
                Some(format!(
                    "member `{}` is already defined here",
                    self.member_name
                )),
            )]
        } else {
            Vec::new()
        };

        Ok(pernixc_diagnostic::Rendered {
            primary_highlight,
            message: format!(
                "member `{}` is already defined in another implementation \
                 block for `{}`",
                self.member_name, adt_name
            ),
            severity: pernixc_diagnostic::Severity::Error,
            help_message: Some(
                "members cannot be redefined across different implementation \
                 blocks for the same type"
                    .to_string(),
            ),
            related,
        })
    }
}

/// Enumeration of all diagnostics for ADT implementation member checking.
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
    AdtImplementationMemberRedefinition(AdtImplementationMemberRedefinition),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        match self {
            Self::AdtImplementationMemberRedefinition(diag) => {
                diag.report(parameter).await
            }
        }
    }
}
