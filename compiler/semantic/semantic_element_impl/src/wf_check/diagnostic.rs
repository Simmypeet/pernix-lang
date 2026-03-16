use pernixc_diagnostic::Report;
use pernixc_qbice::TrackedEngine;
use pernixc_source_file::ByteIndex;
use pernixc_type_system::diagnostic::UnsatisfiedPredicate;
use qbice::{Decode, Encode, Identifiable, StableHash};

/// Enumeration of all diagnostics during the well-formedness checking.
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
    TypeSystem(pernixc_type_system::diagnostic::Diagnostic),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        match self {
            Self::TypeSystem(diag) => diag.report(parameter).await,
        }
    }
}

impl From<UnsatisfiedPredicate> for Diagnostic {
    fn from(value: UnsatisfiedPredicate) -> Self {
        Self::TypeSystem(value.into())
    }
}

impl From<pernixc_type_system::diagnostic::ImplementationIsNotGeneralEnough>
    for Diagnostic
{
    fn from(
        value: pernixc_type_system::diagnostic::ImplementationIsNotGeneralEnough,
    ) -> Self {
        Self::TypeSystem(value.into())
    }
}
