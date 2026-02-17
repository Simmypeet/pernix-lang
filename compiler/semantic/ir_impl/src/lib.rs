//! Crate for implementing the IR queries.

use std::borrow::Cow;

use linkme::distributed_slice;
use pernixc_bind::binder::{self, Binder, UnrecoverableError};
use pernixc_diagnostic::{ByteIndex, Rendered, Report};
use pernixc_handler::Storage;
use pernixc_ir::{FunctionIR, value};
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::{
    get_all_function_with_body_ids, syntax::get_function_body_syntax,
};
use pernixc_target::{Global, TargetID};
use pernixc_tokio::{chunk::chunk_for_tasks, join_set::JoinSet};
use pernixc_type_system::environment::{Environment, get_active_premise};
use qbice::{
    Decode, Encode, Identifiable, StableHash, executor, program::Registration,
    storage::intern::Interned,
};

/// A collection of all diagnostics related to IRs.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    StableHash,
    Encode,
    Decode,
    Identifiable,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Diagnostic {
    Bind(pernixc_bind::diagnostic::Diagnostic),
    MemoryChecker(pernixc_memory_checker::diagnostic::Diagnostic),
    BorrowChecker(pernixc_borrow_checker::diagnostic::Diagnostic),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<pernixc_source_file::ByteIndex> {
        match self {
            Self::Bind(d) => d.report(engine).await,
            Self::MemoryChecker(d) => d.report(engine).await,
            Self::BorrowChecker(d) => d.report(engine).await,
        }
    }
}

/// Query key for building function IR with diagnostics.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    qbice::Query,
)]
#[value((Interned<FunctionIR>, Interned<[Diagnostic]>))]
pub struct BuildFunctionIRKey(pub Global<pernixc_symbol::ID>);

#[executor(config = Config)]
#[allow(clippy::too_many_lines)]
async fn ir_with_diagnostic_executor(
    &BuildFunctionIRKey(id): &BuildFunctionIRKey,
    engine: &TrackedEngine,
) -> (Interned<FunctionIR>, Interned<[Diagnostic]>) {
    let function_body_syntax = engine.get_function_body_syntax(id).await;

    let Some(function_body_syntax) = function_body_syntax else {
        return (
            engine.intern(pernixc_ir::FunctionIR::default()),
            engine.intern_unsized([]),
        );
    };

    let storage = Storage::<Diagnostic>::default();
    let environment = binder::Environment::new(engine, id).await;

    let mut binder =
        match Binder::new_function(engine, &environment, &storage).await {
            Ok(binder) => binder,
            Err(UnrecoverableError::Reported) => {
                return (
                    engine.intern(pernixc_ir::FunctionIR::default()),
                    engine.intern_unsized(storage.into_vec()),
                );
            }
        };

    for statement in
        function_body_syntax.members().filter_map(|x| x.into_line().ok())
    {
        match binder.bind_statement(&statement, &storage).await {
            Ok(()) => {}
            Err(UnrecoverableError::Reported) => {
                return (
                    engine.intern(pernixc_ir::FunctionIR::default()),
                    engine.intern_unsized(storage.into_vec()),
                );
            }
        }
    }

    // finalize the binder to an ir
    let mut ir = match binder.finalize_function_ir(&storage).await {
        Ok(ir) => ir,
        Err(UnrecoverableError::Reported) => {
            return (
                engine.intern(pernixc_ir::FunctionIR::default()),
                engine.intern_unsized(storage.into_vec()),
            );
        }
    };

    // if there's an error from binding, do not proceed to analyses
    if !storage.as_vec().is_empty() {
        return (engine.intern(ir), engine.intern_unsized(storage.into_vec()));
    }

    let premise = engine.get_active_premise(id).await;
    let ty_env = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Borrowed(engine),
        pernixc_type_system::normalizer::NO_OP,
    );

    // do memory checking analysis
    match pernixc_memory_checker::memory_check(&mut ir, &ty_env, id, &storage)
        .await
    {
        Ok(()) => {}
        Err(UnrecoverableError::Reported) => {
            return (
                engine.intern(pernixc_ir::FunctionIR::default()),
                engine.intern_unsized(storage.into_vec()),
            );
        }
    }

    // if there's an error, should not proceed to borrow checking
    if !storage.as_vec().is_empty() {
        return (engine.intern(ir), engine.intern_unsized(storage.into_vec()));
    }

    // do borrow checking analysis
    match pernixc_borrow_checker::borrow_check(
        ir.root_ir(),
        &value::Environment::builder()
            .current_site(id)
            .type_environment(&ty_env)
            .handling_scopes(ir.handling_scopes())
            .build(),
        &storage,
    )
    .await
    {
        Ok(()) => {}
        Err(UnrecoverableError::Reported) => {
            return (
                engine.intern(pernixc_ir::FunctionIR::default()),
                engine.intern_unsized(storage.into_vec()),
            );
        }
    }

    (engine.intern(ir), engine.intern_unsized(storage.into_vec()))
}

#[distributed_slice(PERNIX_PROGRAM)]
static BUILD_FUNCTION_IR_EXECUTOR: Registration<Config> =
    Registration::new::<BuildFunctionIRKey, IrWithDiagnosticExecutor>();

/// Query key for rendering diagnostics of a single function IR.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    qbice::Query,
)]
#[value(Interned<[Rendered<ByteIndex>]>)]
struct SingleRenderedKey(pub Global<pernixc_symbol::ID>);

#[executor(config = Config)]
async fn ir_diagnostic_executor(
    &SingleRenderedKey(id): &SingleRenderedKey,
    engine: &TrackedEngine,
) -> Interned<[Rendered<ByteIndex>]> {
    let (_ir, diagnostics) = engine.query(&BuildFunctionIRKey(id)).await;

    let mut rendered_diagnostics = Vec::new();

    for diag in diagnostics.iter() {
        rendered_diagnostics.push(diag.report(engine).await);
    }

    engine.intern_unsized(rendered_diagnostics)
}

#[distributed_slice(PERNIX_PROGRAM)]
static SINGLE_RENDERED_EXECUTOR: Registration<Config> =
    Registration::new::<SingleRenderedKey, IrDiagnosticExecutor>();

#[executor(config = Config)]
async fn extract_ir_executor(
    &pernixc_ir::Key { function_id }: &pernixc_ir::Key,
    engine: &TrackedEngine,
) -> Interned<pernixc_ir::FunctionIR> {
    let (ir, _diagnostics) =
        engine.query(&BuildFunctionIRKey(function_id)).await;

    ir
}

// Register the IR building query.
#[distributed_slice(PERNIX_PROGRAM)]
static EXTRACT_IR_EXECUTOR: Registration<Config> =
    Registration::new::<pernixc_ir::Key, ExtractIrExecutor>();

/// The main query for extracting all diagnostics related to IRs in a target
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    qbice::Query,
)]
#[value(Interned<[Interned<[pernixc_diagnostic::Rendered<ByteIndex>]>]>)]
pub struct AllRenderedKey(pub TargetID);

#[executor(config = Config)]
async fn all_ir_rendered_diagnostics_executor(
    &AllRenderedKey(target_id): &AllRenderedKey,
    engine: &TrackedEngine,
) -> Interned<[Interned<[pernixc_diagnostic::Rendered<ByteIndex>]>]> {
    let mut handles = JoinSet::new();
    let mut diagnostics = Vec::new();
    let all_function_ids =
        engine.get_all_function_with_body_ids(target_id).await;

    // PARALLEL: retrieves rendered diagnostics for each function in
    // parallel
    unsafe {
        engine.start_unordered_callee_group();
    }

    for ids in all_function_ids.chunk_for_tasks().map(|x| {
        x.iter().copied().map(|x| target_id.make_global(x)).collect::<Vec<_>>()
    }) {
        let engine = engine.clone();

        handles.spawn(async move {
            let mut chunk_diagnostics = Vec::new();
            for id in ids {
                chunk_diagnostics
                    .push(engine.query(&SingleRenderedKey(id)).await);
            }

            chunk_diagnostics
        });
    }

    while let Some(handle) = handles.next().await {
        for diag in handle {
            diagnostics.push(diag);
        }
    }

    unsafe {
        engine.end_unordered_callee_group();
    }

    engine.intern_unsized(diagnostics)
}

#[distributed_slice(PERNIX_PROGRAM)]
static ALL_RENDERED_EXECUTOR: Registration<Config> =
    Registration::new::<AllRenderedKey, AllIrRenderedDiagnosticsExecutor>();
