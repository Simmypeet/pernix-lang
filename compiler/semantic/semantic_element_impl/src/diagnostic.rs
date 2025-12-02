//! Contains the query for retrieving the diagnostics from the whole compilation
//! process

use std::sync::Arc;

use pernixc_diagnostic::Report;
use pernixc_query::{TrackedEngine, runtime::executor};
use pernixc_semantic_element::{
    effect_annotation::Key as DoEffectKey, fields::Key as FieldsKey,
    import::Key as ImportKey, type_alias::Key as TypeAliasKey,
    variant::Key as VariantKey, where_clause::Key as WhereClauseKey,
};
use pernixc_source_file::ByteIndex;
use pernixc_symbol::{
    get_all_symbol_ids,
    kind::{Kind, get_kind},
};
use pernixc_target::{Global, TargetID};
use pernixc_term::generic_parameters::Key as GenericParametersKey;
use pernixc_tokio::{chunk::chunk_for_tasks, scoped};

use crate::{
    build::DiagnosticKey as BuildDiagnosticKey,
    function_signature::Key as FunctionSignatureKey,
    implements_qualified_identifier::Key as ImplementsQualifiedIdentifierKey,
    wf_check,
};

#[pernixc_query::query(
    key(SingleRenderedKey),
    executor(SingleRenderedExecutor),
    value(Arc<[pernixc_diagnostic::Rendered<ByteIndex>]>),
    id(Global<pernixc_symbol::ID>)
)]
#[allow(clippy::cognitive_complexity)]
pub async fn single_rendered_executor(
    id: Global<pernixc_symbol::ID>,
    engine: &TrackedEngine,
) -> Result<Arc<[pernixc_diagnostic::Rendered<ByteIndex>]>, executor::CyclicError>
{
    let mut final_diagnostics = Vec::new();
    let kind = engine.get_kind(id).await;

    if kind == Kind::Module {
        let diags =
            engine.query(&BuildDiagnosticKey::new(ImportKey(id))).await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await?);
        }
    }

    if kind.has_generic_parameters() {
        let diags = engine
            .query(&BuildDiagnosticKey::new(GenericParametersKey(id)))
            .await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await?);
        }
    }

    if kind.has_where_clause() {
        let diags =
            engine.query(&BuildDiagnosticKey::new(WhereClauseKey(id))).await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await?);
        }
    }

    if kind.has_type_alias() {
        let diags =
            engine.query(&BuildDiagnosticKey::new(TypeAliasKey(id))).await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await?);
        }
    }

    if kind.has_fields() {
        let diags =
            engine.query(&BuildDiagnosticKey::new(FieldsKey(id))).await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await?);
        }
    }

    if kind.has_variant_associated_type() {
        let diags =
            engine.query(&BuildDiagnosticKey::new(VariantKey(id))).await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await?);
        }
    }

    if kind.is_implementation() {
        let diags = engine
            .query(&BuildDiagnosticKey::new(ImplementsQualifiedIdentifierKey(
                id,
            )))
            .await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await?);
        }
    }

    if kind.has_function_signature() {
        let diags = engine
            .query(&BuildDiagnosticKey::new(FunctionSignatureKey(id)))
            .await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await?);
        }
    }

    if kind.has_capabilities() {
        let diags =
            engine.query(&BuildDiagnosticKey::new(DoEffectKey(id))).await?;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await?);
        }
    }

    {
        let diags = engine.query(&wf_check::Key(id)).await?;
        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await?);
        }
    }

    Ok(final_diagnostics.into())
}

pernixc_register::register!(SingleRenderedKey, SingleRenderedExecutor);

#[pernixc_query::query(
    key(AllRenderedKey),
    executor(AllRenderedExecutor),
    value(Arc<[Arc<[pernixc_diagnostic::Rendered<ByteIndex>]>]>),
    id(TargetID)
)]
pub async fn all_rendered_executor(
    id: TargetID,
    engine: &TrackedEngine,
) -> Result<
    Arc<[Arc<[pernixc_diagnostic::Rendered<ByteIndex>]>]>,
    executor::CyclicError,
> {
    scoped!(|handles| async move {
        let mut diagnostics = Vec::new();
        let all_ids = engine.get_all_symbol_ids(id).await;

        // SAFETY: the spawned tasks are independent and do not access shared
        // state therefore they can be safely parallelly re-verified.
        unsafe {
            engine.start_parallel();
        }

        for chunk in all_ids
            .chunk_for_tasks()
            .map(|x| x.iter().map(|x| Global::new(id, *x)).collect::<Vec<_>>())
        {
            let engine = engine.clone();
            handles.spawn(async move {
                let mut chunk_diagnostics = Vec::new();
                for id in chunk {
                    chunk_diagnostics
                        .push(engine.query(&SingleRenderedKey(id)).await?);
                }

                Ok::<_, executor::CyclicError>(chunk_diagnostics)
            });
        }

        while let Some(handle) = handles.next().await {
            for diag in handle? {
                diagnostics.push(diag);
            }
        }

        unsafe {
            engine.end_parallel();
        }

        Ok(diagnostics.into())
    })
}

pernixc_register::register!(AllRenderedKey, AllRenderedExecutor);
