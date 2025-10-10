//! Contains the analyzer implementation.

use std::sync::Arc;

use pernixc_query::{runtime::executor, Engine};
use pernixc_source_file::SourceFile;
use pernixc_target::{Arguments, Check, Input, TargetID};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::Url;

use crate::workspace::{self, NewWorkspaceError, Workspace};

/// The struct that handles analysis of the workspace (e.g., checking errors).
#[derive(Debug)]
pub struct Analyzer {
    engine: RwLock<Arc<Engine>>,
    workspace: Workspace,
    current_target_id: TargetID,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct LoadSourceFileExecutor;

impl executor::Executor<pernixc_source_file::Key> for LoadSourceFileExecutor {
    /// The original executor always recomputes the value, which is not what we
    /// want here in the lsp server. We'll depend on the LSP server to tell us
    /// when to recompute.
    const ALWAYS_RECOMPUTE: bool = false;

    async fn execute(
        &self,
        _: &pernixc_query::TrackedEngine,
        key: &pernixc_source_file::Key,
    ) -> Result<
        Result<Arc<SourceFile>, pernixc_source_file::Error>,
        executor::CyclicError,
    > {
        Ok(std::fs::File::open(key.path.as_ref())
            .and_then(|file| {
                Ok(Arc::new(SourceFile::load(file, key.path.to_path_buf())?))
            })
            .map_err(|x| pernixc_source_file::Error(x.to_string().into())))
    }
}

impl Analyzer {
    /// Creates a new analyzer for the given workspace URL.
    pub async fn new(workspace_url: Url) -> Result<Self, NewWorkspaceError> {
        let workspace = workspace::Workspace::new(&workspace_url)?;

        let target_name = workspace.target_name().to_string();
        let local_target_id = TargetID::from_target_name(&target_name);

        let command = pernixc_target::Command::Check(Check {
            input: Input {
                file: workspace.root_source_file().to_path_buf(),
                target_name: Some(target_name),
                library_paths: Vec::new(),
                incremental_path: None,
                chrome_tracing: false,
                target_seed: None,
            },
        });

        // initialize the engine with corelib
        let mut engine = Engine::default();
        pernixc_register::Registration::register_executor(
            &mut engine.runtime.executor,
        );

        assert!(engine
            .runtime
            .executor
            .register(Arc::new(LoadSourceFileExecutor))
            .is_some());

        engine
            .input_session(async |x| {
                x.set_input(
                    pernixc_target::LinkKey(local_target_id),
                    Arc::new(std::iter::once(TargetID::CORE).collect()),
                )
                .await;

                x.set_input(
                    pernixc_target::AllTargetIDsKey,
                    Arc::new(
                        vec![local_target_id, TargetID::CORE]
                            .into_iter()
                            .collect(),
                    ),
                )
                .await;

                x.set_input(
                    pernixc_target::MapKey,
                    Arc::new(
                        [
                            (command.input().target_name(), local_target_id),
                            ("core".into(), TargetID::CORE),
                        ]
                        .into_iter()
                        .collect(),
                    ),
                )
                .await;

                x.set_input(
                    pernixc_target::Key(local_target_id),
                    Arc::new(Arguments { command }),
                )
                .await;
            })
            .await;

        let mut engine = Arc::new(engine);
        pernixc_corelib::initialize_corelib(&mut engine).await;

        Ok(Self {
            engine: RwLock::new(engine),
            workspace,
            current_target_id: local_target_id,
        })
    }
}
