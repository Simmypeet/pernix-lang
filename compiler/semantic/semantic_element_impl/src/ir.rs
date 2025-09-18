use std::sync::Arc;

use pernixc_bind::binder::{Binder, UnrecoverableError};
use pernixc_handler::Storage;
use pernixc_query::runtime::executor;
use pernixc_symbol::{kind::get_kind, syntax::get_function_body_syntax};

use crate::build::{self, Build, Output};

async fn build_ir_for_function(
    engine: &pernixc_query::TrackedEngine,
    key: &pernixc_ir::Key,
    mut binder: Binder<'_>,
    storage: &Storage<pernixc_bind::diagnostic::Diagnostic>,
) -> Result<Arc<pernixc_ir::IR>, executor::CyclicError> {
    let function_body_syntax = engine.get_function_body_syntax(key.0).await;

    let Some(function_body_syntax) = function_body_syntax else {
        return Ok(Arc::new(pernixc_ir::IR::default()));
    };

    for statement in
        function_body_syntax.members().filter_map(|x| x.into_line().ok())
    {
        binder.bind_statement(&statement, storage).await.unwrap();
    }

    Ok(Arc::new(binder.finalize(storage)))
}

impl Build for pernixc_ir::Key {
    type Diagnostic = pernixc_bind::diagnostic::Diagnostic;

    async fn execute(
        engine: &pernixc_query::TrackedEngine,
        key: &Self,
    ) -> Result<Output<Self>, executor::CyclicError> {
        let kind = engine.get_kind(key.0).await;
        let storage = Storage::<Self::Diagnostic>::default();

        let binder = match kind {
            pernixc_symbol::kind::Kind::Function
            | pernixc_symbol::kind::Kind::ImplementationFunction => {
                pernixc_bind::binder::Binder::new_function(
                    engine, key.0, &storage,
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
                    item: Arc::default(),
                    diagnostics: storage.into_vec().into(),
                    occurrences: Arc::default(),
                });
            }
        };

        let ir = match kind {
            pernixc_symbol::kind::Kind::Function
            | pernixc_symbol::kind::Kind::ImplementationFunction => {
                build_ir_for_function(engine, key, binder, &storage).await?
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
