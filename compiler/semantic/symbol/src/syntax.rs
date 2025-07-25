//! Contains the queries for retrieving syntax items defined to a particular
//! symbol.

use std::sync::Arc;

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_target::Global;

use crate::{get_table_of_symbol, ID};

/// Implementation of the `get_module_imports_syntax` method
#[pernixc_query::query(
    key(ImportKey),
    id(Global<ID>),
    value(Arc<[pernixc_syntax::item::module::Import]>),
    executor(ImportExecutor)
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn import_syntax_executor(
    id: Global<ID>,
    engine: &TrackedEngine,
) -> Result<Arc<[pernixc_syntax::item::module::Import]>, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;
    Ok(table
        .import_syntaxes
        .get(&id.id)
        .unwrap_or_else(|| {
            panic!("No import syntax found for symbol ID: {:?}", id.id)
        })
        .clone())
}
