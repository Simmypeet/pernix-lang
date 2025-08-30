//! Contains the query for retrieving the diagnostics from the whole compilation
//! process

use std::sync::Arc;

use pernixc_diagnostic::Report;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_semantic_element::{
    fields::Key as FieldsKey, type_alias::Key as TypeAliasKey,
    variant::Key as VariantKey, where_clause::Key as WhereClauseKey,
};
use pernixc_source_file::ByteIndex;
use pernixc_symbol::{all_symbol_ids, kind::get_kind};
use pernixc_target::{Global, TargetID};
use pernixc_term::generic_parameters::Key as GenericParametersKey;
use pernixc_tokio::scoped;

use crate::{
    build::DiagnosticKey as BuildDiagnosticKey,
    implements_qualified_identifier::Key as ImplementsQualifiedIdentifierKey,
};

#[pernixc_query::query(
    key(SingleRenderedKey),
    executor(SingleRenderedExecutor),
    value(Arc<[pernixc_diagnostic::Diagnostic<ByteIndex>]>),
    id(Global<pernixc_symbol::ID>)
)]
#[allow(clippy::cognitive_complexity)]
pub async fn single_rendered_executor(
    id: Global<pernixc_symbol::ID>,
    engine: &TrackedEngine,
) -> Result<
    Arc<[pernixc_diagnostic::Diagnostic<ByteIndex>]>,
    executor::CyclicError,
> {
    let mut final_diagnostics = Vec::new();
    let kind = engine.get_kind(id).await;

    if kind.has_generic_parameters() {
        let diags = engine
            .query(&BuildDiagnosticKey::new(GenericParametersKey(id)))
            .await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.has_where_clause() {
        let diags =
            engine.query(&BuildDiagnosticKey::new(WhereClauseKey(id))).await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.has_type_alias() {
        let diags =
            engine.query(&BuildDiagnosticKey::new(TypeAliasKey(id))).await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.has_fields() {
        let diags =
            engine.query(&BuildDiagnosticKey::new(FieldsKey(id))).await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.has_variant_associated_type() {
        let diags =
            engine.query(&BuildDiagnosticKey::new(VariantKey(id))).await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.is_implementation() {
        let diags = engine
            .query(&BuildDiagnosticKey::new(ImplementsQualifiedIdentifierKey(
                id,
            )))
            .await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    Ok(final_diagnostics.into())
}

#[pernixc_query::query(
    key(AllRenderedKey),
    executor(AllRenderedExecutor),
    value(Arc<[Arc<[pernixc_diagnostic::Diagnostic<ByteIndex>]>]>),
    id(TargetID)
)]
pub async fn all_rendered_executor(
    id: TargetID,
    engine: &TrackedEngine,
) -> Result<
    Arc<[Arc<[pernixc_diagnostic::Diagnostic<ByteIndex>]>]>,
    executor::CyclicError,
> {
    scoped!(|handles| async move {
        let mut diagnostics = Vec::new();
        let all_ids = engine.all_symbol_ids(id).await;

        // SAFETY: the spawned tasks are independent and do not access shared
        // state therefore they can be safely parallelly re-verified.
        unsafe {
            engine.start_parallel();
        }
        for id in all_ids.iter().map(|x| Global::new(id, *x)) {
            let engine = engine.clone();
            handles.spawn(
                async move { engine.query(&SingleRenderedKey(id)).await },
            );
        }

        while let Some(handle) = handles.next().await {
            diagnostics.push(handle?);
        }

        unsafe {
            engine.end_parallel();
        }

        Ok(diagnostics.into())
    })
}
