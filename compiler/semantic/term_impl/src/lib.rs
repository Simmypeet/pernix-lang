//! Defines the executors for various term queries.

use std::sync::Arc;

use pernixc_query::runtime::persistence::serde::DynamicRegistry;
use pernixc_serialize::{de::Deserializer, ser::Serializer};

pub mod diagnostic;

mod build;
mod generic_parameters;
mod occurrences;
mod where_clause;

/// Registers all the required executors to run the queries.
pub fn register_executors(
    executor: &mut pernixc_query::runtime::executor::Registry,
) {
    executor.register(Arc::new(generic_parameters::BuildExecutor));
    executor.register(Arc::new(generic_parameters::DiagnosticExecutor));
    executor.register(Arc::new(generic_parameters::Executor));

    executor.register(Arc::new(where_clause::BuildExecutor));
    executor.register(Arc::new(where_clause::DiagnosticExecutor));
    executor.register(Arc::new(where_clause::Executor));

    executor.register(Arc::new(diagnostic::SingleRenderedExecutor));
    executor.register(Arc::new(diagnostic::AllRenderedExecutor));
}

/// Registers all the necessary runtime information for the query engine.
pub fn register_serde<
    S: Serializer<Registry>,
    D: Deserializer<Registry>,
    Registry: DynamicRegistry<S, D> + Send + Sync,
>(
    serde_registry: &mut Registry,
) where
    S::Error: Send + Sync,
{
    serde_registry.register::<generic_parameters::BuildKey>();
    serde_registry.register::<generic_parameters::DiagnosticKey>();
    serde_registry.register::<pernixc_term::generic_parameters::Key>();

    serde_registry.register::<where_clause::BuildKey>();
    serde_registry.register::<where_clause::DiagnosticKey>();
    serde_registry.register::<pernixc_term::where_clause::Key>();

    serde_registry.register::<diagnostic::SingleRenderedKey>();
    serde_registry.register::<diagnostic::AllRenderedKey>();
}

/// Registers the keys that should be skipped during serialization and
/// deserialization in the query engine's persistence layer
pub const fn skip_persistence(
    _persistence: &mut pernixc_query::runtime::persistence::Persistence,
) {
}
