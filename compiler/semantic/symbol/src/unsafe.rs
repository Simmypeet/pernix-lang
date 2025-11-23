//! Contains the query definition for retrieving whether the function
//! is marked as unsafe or not

use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_target::Global;

use crate::syntax::get_function_unsafe_keyword;

#[pernixc_query::query(
    key(Key),
    value(bool),
    id(Global<crate::ID>),
    executor(Executor),
    extend(method(is_function_unsafe), no_cyclic)
)]
pub async fn is_function_unsafe(
    id: Global<crate::ID>,
    engine: &TrackedEngine,
) -> Result<bool, CyclicError> {
    let kw = engine.get_function_unsafe_keyword(id).await;
    Ok(kw.is_some())
}

pernixc_register::register!(Key, Executor);
