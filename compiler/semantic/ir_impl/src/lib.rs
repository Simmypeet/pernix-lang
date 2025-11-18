//! Crate for implementing the IR queries.

use std::{borrow::Cow, sync::Arc};

use pernixc_bind::binder::{self, Binder, UnrecoverableError};
use pernixc_diagnostic::{ByteIndex, Rendered, Report};
use pernixc_handler::Storage;
use pernixc_ir::{value, FunctionIR};
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    get_all_function_with_body_ids, syntax::get_function_body_syntax,
};
use pernixc_target::{Global, TargetID};
use pernixc_tokio::scoped;
use pernixc_type_system::environment::{get_active_premise, Environment};

/// A collection of all diagnostics related to IRs.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    StableHash,
    Serialize,
    Deserialize,
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
        engine: &pernixc_query::TrackedEngine,
    ) -> Result<
        pernixc_diagnostic::Rendered<pernixc_source_file::ByteIndex>,
        pernixc_query::runtime::executor::CyclicError,
    > {
        match self {
            Self::Bind(d) => d.report(engine).await,
            Self::MemoryChecker(d) => d.report(engine).await,
            Self::BorrowChecker(d) => d.report(engine).await,
        }
    }
}

#[pernixc_query::query(
    key(BuildFunctionIRKey),
    executor(BuildIRExecutor),
    value((Arc<FunctionIR>, Arc<[Diagnostic]>)),
    id(Global<pernixc_symbol::ID>)
)]
#[allow(clippy::too_many_lines)]
pub async fn ir_with_diagnostic_executor(
    id: Global<pernixc_symbol::ID>,
    engine: &TrackedEngine,
) -> Result<
    (Arc<FunctionIR>, Arc<[Diagnostic]>),
    pernixc_query::runtime::executor::CyclicError,
> {
    let function_body_syntax = engine.get_function_body_syntax(id).await;

    let Some(function_body_syntax) = function_body_syntax else {
        return Ok((
            Arc::new(pernixc_ir::FunctionIR::default()),
            Arc::default(),
        ));
    };

    let storage = Storage::<Diagnostic>::default();
    let environment = binder::Environment::new(engine, id).await?;

    let mut binder =
        match Binder::new_function(engine, &environment, &storage).await {
            Ok(binder) => binder,
            Err(UnrecoverableError::Reported) => {
                return Ok((
                    Arc::new(pernixc_ir::FunctionIR::default()),
                    storage.into_vec().into(),
                ));
            }
            Err(UnrecoverableError::CyclicDependency(error)) => {
                return Err(error);
            }
        };

    for statement in
        function_body_syntax.members().filter_map(|x| x.into_line().ok())
    {
        match binder.bind_statement(&statement, &storage).await {
            Ok(()) => {}
            Err(UnrecoverableError::CyclicDependency(error)) => {
                return Err(error);
            }
            Err(UnrecoverableError::Reported) => {
                return Ok((
                    Arc::new(pernixc_ir::FunctionIR::default()),
                    storage.into_vec().into(),
                ))
            }
        };
    }

    // finalize the binder to an ir
    let mut ir = match binder.finalize_function_ir(&storage).await {
        Ok(ir) => ir,
        Err(UnrecoverableError::CyclicDependency(error)) => {
            return Err(error);
        }
        Err(UnrecoverableError::Reported) => {
            return Ok((
                Arc::new(pernixc_ir::FunctionIR::default()),
                storage.into_vec().into(),
            ))
        }
    };

    // if there's an error from binding, do not proceed to analyses
    if !storage.as_vec().is_empty() {
        return Ok((Arc::new(ir), storage.into_vec().into()));
    }

    let premise = engine.get_active_premise(id).await?;
    let ty_env = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Borrowed(engine),
        pernixc_type_system::normalizer::NO_OP,
    );

    let value_environment = value::Environment::builder()
        .current_site(id)
        .type_environment(&ty_env)
        .build();

    // do memory checking analysis
    match pernixc_memory_checker::memory_check(
        &mut ir.ir,
        &value_environment,
        &storage,
    )
    .await
    {
        Ok(()) => {}
        Err(UnrecoverableError::CyclicDependency(error)) => {
            return Err(error);
        }
        Err(UnrecoverableError::Reported) => {
            return Ok((
                Arc::new(pernixc_ir::FunctionIR::default()),
                storage.into_vec().into(),
            ))
        }
    }

    // if there's an error, should not proceed to borrow checking
    if !storage.as_vec().is_empty() {
        return Ok((Arc::new(ir), storage.into_vec().into()));
    }

    // do borrow checking analysis
    match pernixc_borrow_checker::borrow_check(
        &ir.ir,
        &value_environment,
        &storage,
    )
    .await
    {
        Ok(()) => {}

        Err(UnrecoverableError::CyclicDependency(error)) => {
            return Err(error);
        }

        Err(UnrecoverableError::Reported) => {
            return Ok((
                Arc::new(pernixc_ir::FunctionIR::default()),
                storage.into_vec().into(),
            ))
        }
    }

    Ok((Arc::new(ir), storage.into_vec().into()))
}

pernixc_register::register!(BuildFunctionIRKey, BuildIRExecutor);

#[pernixc_query::query(
    key(SingleRenderedKey),
    executor(SingleRenderedExecutor),
    value(Arc<[Rendered<ByteIndex>]>),
    id(Global<pernixc_symbol::ID>)
)]
pub async fn ir_diagnostic_executor(
    id: Global<pernixc_symbol::ID>,
    engine: &TrackedEngine,
) -> Result<
    Arc<[Rendered<ByteIndex>]>,
    pernixc_query::runtime::executor::CyclicError,
> {
    let (_ir, diagnostics) = engine.query(&BuildFunctionIRKey(id)).await?;

    let mut rendered_diagnostics = Vec::new();

    for diag in diagnostics.iter() {
        rendered_diagnostics.push(diag.report(engine).await?);
    }

    Ok(Arc::from(rendered_diagnostics))
}

pernixc_register::register!(SingleRenderedKey, SingleRenderedExecutor);

#[pernixc_query::executor(key(pernixc_ir::Key), name(ExtractIRExecutor))]
pub async fn extract_ir_executor(
    key: &pernixc_ir::Key,
    engine: &TrackedEngine,
) -> Result<
    Arc<pernixc_ir::FunctionIR>,
    pernixc_query::runtime::executor::CyclicError,
> {
    let (ir, _diagnostics) = engine.query(&BuildFunctionIRKey(key.0)).await?;

    Ok(ir)
}

// Register the IR building query.
pernixc_register::register!(pernixc_ir::Key, ExtractIRExecutor);

/// The main query for extracting all diagnostics related to IRs in a target
#[pernixc_query::query(
    key(AllRenderedKey),
    executor(AllRenderedExecutor),
    value(Arc<[Arc<[pernixc_diagnostic::Rendered<ByteIndex>]>]>),
    id(TargetID)
)]
pub async fn all_ir_rendered_diagnostics_executor(
    id: TargetID,
    engine: &TrackedEngine,
) -> Result<
    Arc<[Arc<[pernixc_diagnostic::Rendered<ByteIndex>]>]>,
    pernixc_query::runtime::executor::CyclicError,
> {
    scoped!(|handles| async move {
        let mut diagnostics = Vec::new();
        let all_function_ids = engine.get_all_function_with_body_ids(id).await;

        // SAFETY: each diagnostic for each IR is independent, so it's safe to
        // parallelize
        unsafe {
            engine.start_parallel();
        }

        for id in all_function_ids.iter().map(|x| Global::new(id, *x)) {
            let engine = engine.clone();
            handles.spawn(
                async move { engine.query(&SingleRenderedKey(id)).await },
            );
        }

        unsafe {
            engine.end_parallel();
        }

        while let Some(handle) = handles.next().await {
            diagnostics.push(handle?);
        }

        Ok(diagnostics.into())
    })
}

pernixc_register::register!(AllRenderedKey, AllRenderedExecutor);
