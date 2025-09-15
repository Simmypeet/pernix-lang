use pernixc_diagnostic::Report;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;

/// Enumeration of all diagnostics while building the function signature.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    derive_more::From,
)]
#[allow(clippy::large_enum_variant)]
pub enum Diagnostic {
    Resolution(pernixc_resolution::diagnostic::Diagnostic),
    TypeSystem(pernixc_type_system::diagnostic::Diagnostic),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        match self {
            Self::Resolution(diag) => diag.report(parameter).await,
            Self::TypeSystem(diag) => diag.report(parameter).await,
        }
    }
}
