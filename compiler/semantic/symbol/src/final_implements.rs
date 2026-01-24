//! Contains the qyery definition for retrieving whether the trait
//! implementation is final or not

use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
};

use crate::syntax::get_implements_final_keyword;

/// Retrieves whether the trait implementation is marked as final.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(bool)]
#[extend(name = get_is_implements_final, by_val)]
pub struct Key {
    /// The global ID of the implements symbol.
    pub symbol_id: Global<crate::ID>,
}

#[executor(config = Config)]
async fn is_implements_final_executor(
    id: &Key,
    engine: &TrackedEngine,
) -> bool {
    let kw = engine.get_implements_final_keyword(id.symbol_id).await;
    kw.is_some()
}

#[distributed_slice(PERNIX_PROGRAM)]
static IS_IMPLEMENTS_FINAL_EXECUTOR: Registration<Config> =
    Registration::new::<Key, IsImplementsFinalExecutor>();
