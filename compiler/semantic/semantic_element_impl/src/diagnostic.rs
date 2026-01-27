//! Contains the query for retrieving the diagnostics from the whole compilation
//! process

use linkme::distributed_slice;
use pernixc_diagnostic::Report;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
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
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
    storage::intern::Interned,
};

use crate::{
    build::DiagnosticKey as BuildDiagnosticKey,
    function_signature::Key as FunctionSignatureKey,
    implements_qualified_identifier::Key as ImplementsQualifiedIdentifierKey,
    wf_check,
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, StableHash, Encode, Decode, Query,
)]
#[value(Interned<[pernixc_diagnostic::Rendered<ByteIndex>]>)]
struct SingleRenderedKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config)]
#[allow(clippy::cognitive_complexity)]
async fn single_rendered_executor(
    &SingleRenderedKey { symbol_id }: &SingleRenderedKey,
    engine: &TrackedEngine,
) -> Interned<[pernixc_diagnostic::Rendered<ByteIndex>]> {
    let mut final_diagnostics = Vec::new();
    let kind = engine.get_kind(symbol_id).await;

    if kind == Kind::Module {
        let diags = engine
            .query(&BuildDiagnosticKey::new(ImportKey { symbol_id }))
            .await;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.has_generic_parameters() {
        let diags = engine
            .query(&BuildDiagnosticKey::new(GenericParametersKey { symbol_id }))
            .await;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.has_where_clause() {
        let diags = engine
            .query(&BuildDiagnosticKey::new(WhereClauseKey { symbol_id }))
            .await;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.has_type_alias() {
        let diags = engine
            .query(&BuildDiagnosticKey::new(TypeAliasKey { symbol_id }))
            .await;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.has_fields() {
        let diags = engine
            .query(&BuildDiagnosticKey::new(FieldsKey { symbol_id }))
            .await;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.has_variant_associated_type() {
        let diags = engine
            .query(&BuildDiagnosticKey::new(VariantKey { symbol_id }))
            .await;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.is_implementation() {
        let diags = engine
            .query(&BuildDiagnosticKey::new(ImplementsQualifiedIdentifierKey {
                symbol_id,
            }))
            .await;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.has_function_signature() {
        let diags = engine
            .query(&BuildDiagnosticKey::new(FunctionSignatureKey { symbol_id }))
            .await;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    if kind.has_capabilities() {
        let diags = engine
            .query(&BuildDiagnosticKey::new(DoEffectKey { symbol_id }))
            .await;

        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    {
        let diags = engine.query(&wf_check::Key { symbol_id }).await;
        for diag in diags.iter() {
            final_diagnostics.push(diag.report(engine).await);
        }
    }

    engine.intern_unsized(final_diagnostics)
}

#[distributed_slice(PERNIX_PROGRAM)]
static SINGLE_RENDERED_EXECUTOR: Registration<Config> =
    Registration::<Config>::new::<SingleRenderedKey, SingleRenderedExecutor>();

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, StableHash, Encode, Decode, Query,
)]
#[value(Interned<[Interned<[pernixc_diagnostic::Rendered<ByteIndex>]>]>)]
struct AllRenderedKey {
    pub target_id: TargetID,
}

#[executor(config = Config)]
async fn all_rendered_executor(
    &AllRenderedKey { target_id }: &AllRenderedKey,
    engine: &TrackedEngine,
) -> Interned<[Interned<[pernixc_diagnostic::Rendered<ByteIndex>]>]> {
    scoped!(|handles| async move {
        let mut diagnostics = Vec::new();
        let all_ids = engine.get_all_symbol_ids(target_id).await;

        // PARALLEL: the spawned tasks are independent and do not access shared
        // state therefore they can be safely parallelly re-verified.

        for chunk in all_ids.chunk_for_tasks().map(|x| {
            x.iter().map(|x| Global::new(target_id, *x)).collect::<Vec<_>>()
        }) {
            let engine = engine.clone();
            handles.spawn(async move {
                let mut chunk_diagnostics = Vec::new();
                for symbol_id in chunk {
                    chunk_diagnostics.push(
                        engine.query(&SingleRenderedKey { symbol_id }).await,
                    );
                }

                chunk_diagnostics
            });
        }

        while let Some(handle) = handles.next().await {
            for diag in handle {
                diagnostics.push(diag);
            }
        }

        engine.intern_unsized(diagnostics)
    })
}

#[distributed_slice(PERNIX_PROGRAM)]
static ALL_RENDERED_EXECUTOR: Registration<Config> =
    Registration::<Config>::new::<AllRenderedKey, AllRenderedExecutor>();
