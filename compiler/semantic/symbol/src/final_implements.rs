//! Contains the qyery definition for retrieving whether the trait
//! implementation is final or not

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_target::Global;

use crate::syntax::get_implements_final_keyword;

#[pernixc_query::query(
    key(Key),
    value(bool),
    id(Global<crate::ID>),
    executor(Executor),
    extend(method(is_implements_final), no_cyclic)
)]
pub async fn is_implements_final(
    id: Global<crate::ID>,
    engine: &TrackedEngine,
) -> Result<bool, CyclicError> {
    let kw = engine.get_implements_final_keyword(id).await;
    Ok(kw.is_some())
}
