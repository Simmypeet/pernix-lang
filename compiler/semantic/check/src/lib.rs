//! Crate for collecting all semantic analysis checks.

use linkme::distributed_slice;
use pernixc_diagnostic::Rendered;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_tokio::panic_propagate::PanicPropagate;
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
    storage::intern::Interned,
};

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
    Encode,
    Decode,
)]
pub struct Check {
    symbol_immpl: Interned<[Rendered<usize>]>,
    semantic_impl: Interned<[Interned<[Rendered<usize>]>]>,
    ir_impl: Interned<[Interned<[Rendered<usize>]>]>,
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

/// The main query for checking all semantic errors.
///
/// This represents the `pernixc check` command.
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
    Encode,
    Decode,
    Query,
)]
#[value(Check)]
pub struct Key {
    /// The target ID for which to check semantic errors.
    pub target_id: pernixc_target::TargetID,
}

#[executor(config = Config)]
async fn check_executor(&key: &Key, engine: &TrackedEngine) -> Check {
    // PARALLEL: Collect all diagnostics for the various phases.
    unsafe {
        engine.start_unordered_callee_group();
    }

    let symbol_diags = {
        let engine = engine.clone();

        tokio::spawn(async move {
            engine
                .query(&pernixc_symbol_impl::diagnostic::RenderedKey(
                    key.target_id,
                ))
                .await
        })
    };

    let semantic_diags = {
        let engine = engine.clone();

        tokio::spawn(async move {
            engine
                .query(
                    &pernixc_semantic_element_impl::diagnostic::AllRenderedKey {
                        target_id: key.target_id,
                    },
                )
                .await
        })
    };

    let ir_diags = {
        let engine = engine.clone();

        tokio::spawn(async move {
            engine.query(&pernixc_ir_impl::AllRenderedKey(key.target_id)).await
        })
    };

    unsafe {
        engine.end_unordered_callee_group();
    }

    Check {
        symbol_immpl: symbol_diags.await.panic_propagate().unwrap(),
        semantic_impl: semantic_diags.await.panic_propagate().unwrap(),
        ir_impl: ir_diags.await.panic_propagate().unwrap(),
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static CHECK_EXECUTOR: Registration<Config> =
    Registration::new::<Key, CheckExecutor>();
