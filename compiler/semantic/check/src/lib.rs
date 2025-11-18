//! Crate for collecting all semantic analysis checks.

use std::sync::Arc;

use pernixc_diagnostic::Rendered;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::TargetID;

/// The main data structure collecting all diagnostics for semantic analysis
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Default,
    Serialize,
    Deserialize,
    pernixc_query::Value,
)]
#[id(TargetID)]
#[value(Arc<Check>)]
pub struct Check {
    symbol_immpl: Arc<[Rendered<usize>]>,
    semantic_impl: Arc<[Arc<[Rendered<usize>]>]>,
    ir_impl: Arc<[Arc<[Rendered<usize>]>]>,
}

impl Check {
    /// Returns an iterator over all diagnostics in this check.
    pub fn all_diagnostics(&self) -> impl Iterator<Item = &Rendered<usize>> {
        self.symbol_immpl
            .iter()
            .chain(self.semantic_impl.iter().flat_map(|diags| diags.iter()))
            .chain(self.ir_impl.iter().flat_map(|diags| diags.iter()))
    }
}

#[pernixc_query::executor(key(Key), name(Executor))]
pub async fn check_executor(
    &key: &Key,
    engine: &pernixc_query::TrackedEngine,
) -> Result<Arc<Check>, pernixc_query::runtime::executor::CyclicError> {
    // Collect all diagnostics for the various phases.
    unsafe {
        engine.start_parallel();
    }

    let symbol_diags = {
        let engine = engine.clone();

        tokio::spawn(async move {
            engine
                .query(&pernixc_symbol_impl::diagnostic::RenderedKey(key.0))
                .await
        })
    };

    let semantic_diags = {
        let engine = engine.clone();

        tokio::spawn(async move {
            engine
                .query(
                    &pernixc_semantic_element_impl::diagnostic::AllRenderedKey(
                        key.0,
                    ),
                )
                .await
        })
    };

    let ir_diags = {
        let engine = engine.clone();

        tokio::spawn(async move {
            engine.query(&pernixc_ir_impl::AllRenderedKey(key.0)).await
        })
    };

    let check = Check {
        symbol_immpl: symbol_diags.await.unwrap()?,
        semantic_impl: semantic_diags.await.unwrap()?,
        ir_impl: ir_diags.await.unwrap()?,
    };

    unsafe {
        engine.end_parallel();
    }

    Ok(Arc::new(check))
}

pernixc_register::register!(Key, Executor);
