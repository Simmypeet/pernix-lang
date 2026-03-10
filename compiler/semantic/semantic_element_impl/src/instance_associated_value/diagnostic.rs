use pernixc_diagnostic::Report;
use qbice::{Decode, Encode, Identifiable, StableHash};

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
        parameter: &pernixc_qbice::TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<pernixc_diagnostic::ByteIndex> {
        match self {
            Self::Resolution(diagnostic) => diagnostic.report(parameter).await,
            Self::TypeSystem(diagnostic) => diagnostic.report(parameter).await,
        }
    }
}
