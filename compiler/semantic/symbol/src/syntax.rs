//! Contains the queries for retrieving syntax items defined to a particular
//! symbol.

use std::sync::Arc;

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_syntax::QualifiedIdentifier;
use pernixc_target::Global;

use crate::{get_table_of_symbol, ID};

/// Implementation of the `get_module_imports_syntax` method
#[pernixc_query::query(
    key(ImportKey),
    id(Global<ID>),
    value(Arc<[pernixc_syntax::item::module::Import]>),
    executor(ImportExecutor),
    extend(method(get_module_imports_syntax), no_cyclic),
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

/// Implementation of the `get_module_imports_syntax` method
#[pernixc_query::query(
    key(ImplementsQualifiedIdentifierKey),
    id(Global<ID>),
    value(QualifiedIdentifier),
    executor(ImplementsQualifiedIdentifierExecutor)
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn implements_qualified_identifier_executor(
    id: Global<ID>,
    engine: &TrackedEngine,
) -> Result<QualifiedIdentifier, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;
    Ok(table
        .implements_qualified_identifier_syntaxes
        .get(&id.id)
        .unwrap_or_else(|| {
            panic!(
                "No implements qualified identifier syntax found for symbol \
                 ID: {:?}",
                id.id
            )
        })
        .clone())
}

/// Implementation of the `get_generic_parameters_syntax` method
#[pernixc_query::query(
    key(GenericParametersKey),
    id(Global<ID>),
    value(Option<pernixc_syntax::item::generic_parameters::GenericParameters>),
    executor(GenericParametersExecutor),
    extend(method(get_generic_parameters_syntax), no_cyclic),
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn generic_parameters_syntax(
    id: Global<ID>,
    engine: &TrackedEngine,
) -> Result<
    Option<pernixc_syntax::item::generic_parameters::GenericParameters>,
    CyclicError,
> {
    let table = engine.get_table_of_symbol(id).await;
    Ok(table
        .generic_parameter_syntaxes
        .get(&id.id)
        .unwrap_or_else(|| {
            panic!(
                "No generic parameter syntax found for symbol ID: {:?}",
                id.id
            )
        })
        .clone())
}
