//! Contains the query definition for retrieving whether the function
//! is marked as unsafe or not

use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
};

use crate::syntax::get_function_unsafe_keyword;

/// The key type used to query whether a function is unsafe.
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
#[extend(name = is_function_unsafe, by_val)]
pub struct Key {
    /// The global ID of the function symbol.
    pub symbol_id: Global<crate::ID>,
}

#[executor(config = Config)]
async fn is_function_unsafe_executor(id: &Key, engine: &TrackedEngine) -> bool {
    let kw = engine.get_function_unsafe_keyword(id.symbol_id).await;
    kw.is_some()
}

#[distributed_slice(PERNIX_PROGRAM)]
static IS_FUNCTION_UNSAFE_EXECUTOR: Registration<Config> =
    Registration::new::<Key, IsFunctionUnsafeExecutor>();
