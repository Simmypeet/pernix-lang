//! Defines the executors for various term queries.

use std::{fmt::Debug, sync::Arc};

use pernixc_query::{
    runtime::{executor::Executor, persistence::serde::DynamicRegistry},
    Key,
};
use pernixc_serialize::{de::Deserializer, ser::Serializer};
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;

pub mod diagnostic;

mod build;
mod generic_parameters;
mod occurrences;
mod type_alias;
mod where_clause;

fn register_term_executors<
    K: Key,
    D: Debug + Identifiable + StableHash + Send + Sync + 'static,
    BuildExecutor: Executor<build::Key<K::Value, D>> + Default,
    TermExtractExecutor: Executor<K> + Default,
>(
    executor: &mut pernixc_query::runtime::executor::Registry,
) where
    K::Value: Identifiable,
{
    executor.register(Arc::new(BuildExecutor::default()));
    executor.register::<build::DiagnosticKey<K::Value, D>, _>(Arc::new(
        build::DiagnosticExecutor,
    ));
    executor.register::<build::OccurrencesKey<K::Value, D>, _>(Arc::new(
        build::OccurrencesExecutor,
    ));
    executor.register(Arc::new(TermExtractExecutor::default()));
}

/// Registers all the required executors to run the queries.
pub fn register_executors(
    registry: &mut pernixc_query::runtime::executor::Registry,
) {
    register_term_executors::<
        pernixc_term::generic_parameters::Key,
        generic_parameters::diagnostic::Diagnostic,
        generic_parameters::BuildExecutor,
        generic_parameters::Executor,
    >(registry);

    register_term_executors::<
        pernixc_term::where_clause::Key,
        where_clause::diagnostic::Diagnostic,
        where_clause::BuildExecutor,
        where_clause::Executor,
    >(registry);

    register_term_executors::<
        pernixc_term::type_alias::Key,
        type_alias::diagnostic::Diagnostic,
        type_alias::BuildExecutor,
        type_alias::Executor,
    >(registry);

    registry.register(Arc::new(diagnostic::SingleRenderedExecutor));
    registry.register(Arc::new(diagnostic::AllRenderedExecutor));
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
    serde_registry.register::<generic_parameters::OccurrencesKey>();
    serde_registry.register::<pernixc_term::generic_parameters::Key>();

    serde_registry.register::<where_clause::BuildKey>();
    serde_registry.register::<where_clause::DiagnosticKey>();
    serde_registry.register::<where_clause::OccurrencesKey>();
    serde_registry.register::<pernixc_term::where_clause::Key>();

    serde_registry.register::<type_alias::BuildKey>();
    serde_registry.register::<type_alias::DiagnosticKey>();
    serde_registry.register::<type_alias::OccurrencesKey>();
    serde_registry.register::<pernixc_term::type_alias::Key>();

    serde_registry.register::<diagnostic::SingleRenderedKey>();
    serde_registry.register::<diagnostic::AllRenderedKey>();
}

/// Registers the keys that should be skipped during serialization and
/// deserialization in the query engine's persistence layer
pub const fn skip_persistence(
    _persistence: &mut pernixc_query::runtime::persistence::Persistence,
) {
}
