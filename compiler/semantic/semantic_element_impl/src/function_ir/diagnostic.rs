use pernixc_diagnostic::Report;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    StableHash,
    Serialize,
    Deserialize,
    derive_more::From,
)]
pub enum Diagnostic {
    Bind(pernixc_bind::diagnostic::Diagnostic),
    MemoryChecker(pernixc_memory_checker::diagnostic::Diagnostic),
    BorrowChecker(pernixc_borrow_checker::diagnostic::Diagnostic),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &pernixc_query::TrackedEngine,
    ) -> Result<
        pernixc_diagnostic::Rendered<pernixc_source_file::ByteIndex>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        match self {
            Self::Bind(d) => d.report(engine).await,
            Self::MemoryChecker(d) => d.report(engine).await,
            Self::BorrowChecker(d) => d.report(engine).await,
        }
    }
}
