//! Contains the implementation for checking if a function is unsafe.

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_symbol::syntax::get_function_unsafe_keyword;

pernixc_register::register!(
    pernixc_semantic_element::r#unsafe::Key,
    Executor
);

#[pernixc_query::executor(
    key(pernixc_semantic_element::r#unsafe::Key),
    name(Executor)
)]
pub async fn executor(
    &pernixc_semantic_element::r#unsafe::Key(id): &pernixc_semantic_element::r#unsafe::Key,
    engine: &TrackedEngine,
) -> Result<bool, CyclicError> {
    let unsafe_keyword = engine.get_function_unsafe_keyword(id).await;
    Ok(unsafe_keyword.is_some())
}
