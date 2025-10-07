use std::sync::Arc;

use pernixc_bind::binder::{self, Binder, UnrecoverableError};
use pernixc_handler::Storage;
use pernixc_ir::value::Environment as ValueEnvironment;
use pernixc_query::runtime::executor;
use pernixc_symbol::{kind::get_kind, syntax::get_function_body_syntax};
use pernixc_type_system::{
    environment::{get_active_premise, Environment},
    normalizer,
};

use crate::build::{self, Build, Output};

pub mod diagnostic;

async fn build_ir_for_function(
    engine: &pernixc_query::TrackedEngine,
    key: &pernixc_ir::Key,
    mut binder: Binder<'_>,
    storage: &Storage<diagnostic::Diagnostic>,
) -> Result<Arc<pernixc_ir::IR>, executor::CyclicError> {
    let function_body_syntax = engine.get_function_body_syntax(key.0).await;

    let Some(function_body_syntax) = function_body_syntax else {
        return Ok(Arc::new(pernixc_ir::IR::default()));
    };

    for statement in
        function_body_syntax.members().filter_map(|x| x.into_line().ok())
    {
        match binder.bind_statement(&statement, storage).await {
            Ok(()) => {}
            Err(UnrecoverableError::CyclicDependency(error)) => {
                return Err(error);
            }
            Err(UnrecoverableError::Reported) => {
                return Ok(Arc::new(pernixc_ir::IR::default()))
            }
        };
    }

    // finalize the binder to an ir
    let mut ir = match binder.finalize(storage).await {
        Ok(ir) => ir,
        Err(UnrecoverableError::CyclicDependency(error)) => {
            return Err(error);
        }
        Err(UnrecoverableError::Reported) => {
            return Ok(Arc::new(pernixc_ir::IR::default()))
        }
    };

    // do memory checking analysis
    match pernixc_memory_checker::memory_check(
        engine, &mut ir, key.0, None, storage,
    )
    .await
    {
        Ok(()) => {}
        Err(UnrecoverableError::CyclicDependency(error)) => {
            return Err(error);
        }
        Err(UnrecoverableError::Reported) => {
            return Ok(Arc::new(pernixc_ir::IR::default()))
        }
    }

    // if there's an error, should not proceed to borrow checking
    if !storage.as_vec().is_empty() {
        return Ok(Arc::new(ir));
    }

    let active_premse = engine.get_active_premise(key.0).await?;

    let env = Environment::new(
        std::borrow::Cow::Borrowed(&active_premse),
        std::borrow::Cow::Borrowed(engine),
        normalizer::NO_OP,
    );

    // do borrow checking analysis
    match pernixc_borrow_checker::borrow_check(
        &ir,
        &ValueEnvironment::builder()
            .current_site(key.0)
            .type_environment(&env)
            .build(),
        storage,
    )
    .await
    {
        Ok(()) => {}

        Err(UnrecoverableError::CyclicDependency(error)) => {
            return Err(error);
        }

        Err(UnrecoverableError::Reported) => {
            return Ok(Arc::new(pernixc_ir::IR::default()))
        }
    }

    Ok(Arc::new(ir))
}

impl Build for pernixc_ir::Key {
    type Diagnostic = diagnostic::Diagnostic;

    async fn execute(
        engine: &pernixc_query::TrackedEngine,
        key: &Self,
    ) -> Result<Output<Self>, executor::CyclicError> {
        let kind = engine.get_kind(key.0).await;
        let storage = Storage::<Self::Diagnostic>::default();

        let environment = binder::Environment::new(engine, key.0).await?;

        let binder = match kind {
            pernixc_symbol::kind::Kind::Function
            | pernixc_symbol::kind::Kind::ImplementationFunction => {
                pernixc_bind::binder::Binder::new_function(
                    engine,
                    &environment,
                    &storage,
                )
                .await
            }

            _ => panic!("unexpected kind: {kind:?}"),
        };

        let binder = match binder {
            Ok(binder) => binder,
            Err(UnrecoverableError::CyclicDependency(error)) => {
                return Err(error)
            }
            Err(UnrecoverableError::Reported) => {
                return Ok(Output {
                    item: Arc::new(pernixc_ir::IR::default()),
                    diagnostics: storage.into_vec().into(),
                    occurrences: Arc::default(),
                });
            }
        };

        let ir = match kind {
            pernixc_symbol::kind::Kind::Function
            | pernixc_symbol::kind::Kind::ImplementationFunction => {
                Box::pin(build_ir_for_function(engine, key, binder, &storage))
                    .await?
            }

            _ => unreachable!(),
        };

        Ok(Output {
            item: ir,
            diagnostics: storage.into_vec().into(),
            occurrences: Arc::default(),
        })
    }
}

build::register_build!(pernixc_ir::Key);
