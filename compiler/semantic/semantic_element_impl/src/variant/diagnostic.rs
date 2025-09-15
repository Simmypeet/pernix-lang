use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Identifiable,
    StableHash,
    Serialize,
    Deserialize,
    derive_more::From,
)]
#[allow(clippy::large_enum_variant)]
pub enum Diagnostic {
    Resolution(pernixc_resolution::diagnostic::Diagnostic),
    TypeSystem(pernixc_type_system::diagnostic::Diagnostic),
}

impl pernixc_diagnostic::Report for Diagnostic {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        match self {
            Self::Resolution(diagnostic) => diagnostic.report(parameter).await,
            Self::TypeSystem(diagnostic) => diagnostic.report(parameter).await,
        }
    }
}
