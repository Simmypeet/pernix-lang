//! Contains the query for retrieving the diagnostics from the whole compilation
//! process

use std::sync::Arc;

use pernixc_diagnostic::Report;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_source_file::ByteIndex;
use pernixc_symbol::{all_symbol_ids, kind::get_kind};
use pernixc_target::{Global, TargetID};
use pernixc_tokio::scoped;

#[pernixc_query::query(
    key(SingleRenderedKey),
    executor(SingleRenderedExecutor),
    value(Arc<[pernixc_diagnostic::Diagnostic<ByteIndex>]>),
    id(Global<pernixc_symbol::ID>)
)]
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
        let diags =
            engine.query(&crate::generic_parameters::DiagnosticKey(id)).await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.has_where_clause() {
        let diags =
            engine.query(&crate::where_clause::DiagnosticKey(id)).await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.has_type_alias() {
        let diags = engine.query(&crate::type_alias::DiagnosticKey(id)).await?;

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
