use pernixc_diagnostic::Report;
use pernixc_qbice::TrackedEngine;
use pernixc_source_file::ByteIndex;
use qbice::{Decode, Encode, Identifiable, StableHash};

/// Enumeration of all diagnostics while building the function signature.
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
    Identifiable,
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
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        match self {
            Self::Resolution(diag) => diag.report(parameter).await,
            Self::TypeSystem(diag) => diag.report(parameter).await,
        }
    }
}
