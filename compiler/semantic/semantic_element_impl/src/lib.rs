//! Defines the executors for various term queries.

use std::sync::Arc;

use pernixc_query::runtime::persistence::serde::DynamicRegistry;
use pernixc_serialize::{
    de::Deserializer, ser::Serializer, Deserialize, Serialize,
};
use pernixc_symbol::kind::FilterKey;

use crate::{build::Build, implemented::ImplementationFilter};

pub mod diagnostic;

mod build;
mod fields;
mod function_signature;
mod generic_parameters;
mod implemented;
mod implements_qualified_identifier;
mod occurrences;
mod parameters;
mod type_alias;
mod variance;
mod variant;
mod wf_check;
mod where_clause;

fn register_term_executors<K: Build>(
    executor: &mut pernixc_query::runtime::executor::Registry,
) {
    executor.register::<build::Key<K>, _>(Arc::new(build::BuildExecutor));
    executor.register::<build::DiagnosticKey<K>, _>(Arc::new(
        build::DiagnosticExecutor,
    ));
    executor.register::<build::OccurrencesKey<K>, _>(Arc::new(
        build::OccurrencesExecutor,
    ));
    executor.register::<K, _>(Arc::new(build::TermExtractExecutor));
}

fn register_term_serde<
    K: Build + Serialize<S, Registry> + Deserialize<D, Registry>,
    S: Serializer<Registry>,
    D: Deserializer<Registry>,
    Registry: DynamicRegistry<S, D> + Send + Sync,
>(
    serde_registry: &mut Registry,
) where
    S::Error: Send + Sync,
    K::Diagnostic: Serialize<S, Registry> + Deserialize<D, Registry>,
    K::Value: Serialize<S, Registry> + Deserialize<D, Registry>,
{
    serde_registry.register::<build::Key<K>>();
    serde_registry.register::<build::DiagnosticKey<K>>();
    serde_registry.register::<build::OccurrencesKey<K>>();
    serde_registry.register::<K>();
}

/// Registers all the required executors to run the queries.
pub fn register_executors(
    registry: &mut pernixc_query::runtime::executor::Registry,
) {
    register_term_executors::<pernixc_term::generic_parameters::Key>(registry);
    register_term_executors::<pernixc_semantic_element::where_clause::Key>(
        registry,
    );
    register_term_executors::<pernixc_semantic_element::type_alias::Key>(
        registry,
    );
    register_term_executors::<pernixc_semantic_element::variant::Key>(registry);
    register_term_executors::<pernixc_semantic_element::fields::Key>(registry);
    register_term_executors::<implements_qualified_identifier::Key>(registry);
    register_term_executors::<function_signature::Key>(registry);

    registry.register(Arc::new(
        implements_qualified_identifier::ExtractImplementsID,
    ));
    registry.register(Arc::new(
        implements_qualified_identifier::ExtractGenericArguments,
    ));

    registry.register(Arc::new(implemented::InTargetExecutor));
    registry.register::<FilterKey<ImplementationFilter>, _>(Arc::new(
        pernixc_symbol::kind::FilterExecutor,
    ));
    registry.register(Arc::new(implemented::Executor));

    registry.register(Arc::new(function_signature::ParametersExecutor));
    registry.register(Arc::new(function_signature::ReturnTypeExecutor));
    registry
        .register(Arc::new(function_signature::LateBooundLifetimesExecutor));
    registry.register(Arc::new(function_signature::ImpliedPredicatesExecutor));
    registry.register(Arc::new(function_signature::ElidedLifetimesExecutor));

    registry.register(Arc::new(variance::MapExecutor));
    registry.register(Arc::new(variance::Executor));

    registry.register(Arc::new(wf_check::Executor));

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
    register_term_serde::<pernixc_term::generic_parameters::Key, _, _, _>(
        serde_registry,
    );
    register_term_serde::<pernixc_semantic_element::where_clause::Key, _, _, _>(
        serde_registry,
    );
    register_term_serde::<pernixc_semantic_element::type_alias::Key, _, _, _>(
        serde_registry,
    );
    register_term_serde::<pernixc_semantic_element::variant::Key, _, _, _>(
        serde_registry,
    );
    register_term_serde::<pernixc_semantic_element::fields::Key, _, _, _>(
        serde_registry,
    );
    register_term_serde::<implements_qualified_identifier::Key, _, _, _>(
        serde_registry,
    );
    register_term_serde::<function_signature::Key, _, _, _>(serde_registry);

    serde_registry.register::<pernixc_semantic_element::implements::Key>();
    serde_registry
        .register::<pernixc_semantic_element::implements_arguments::Key>();

    serde_registry.register::<pernixc_semantic_element::implemented::Key>();
    serde_registry.register::<FilterKey<ImplementationFilter>>();
    serde_registry.register::<implemented::InTargetKey>();

    serde_registry.register::<pernixc_semantic_element::parameter::Key>();
    serde_registry.register::<pernixc_semantic_element::return_type::Key>();
    serde_registry
        .register::<pernixc_semantic_element::implied_predicate::Key>();
    serde_registry.register::<pernixc_semantic_element::elided_lifetime::Key>();
    serde_registry
        .register::<pernixc_semantic_element::late_bound_lifetime::Key>();

    serde_registry.register::<variance::MapKey>();
    serde_registry.register::<pernixc_semantic_element::variance::Key>();

    serde_registry.register::<wf_check::Key>();

    serde_registry.register::<diagnostic::SingleRenderedKey>();
    serde_registry.register::<diagnostic::AllRenderedKey>();
}

/// Registers the keys that should be skipped during serialization and
/// deserialization in the query engine's persistence layer
pub const fn skip_persistence(
    _persistence: &mut pernixc_query::runtime::persistence::Persistence,
) {
}
