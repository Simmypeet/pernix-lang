//! Contains the analyzer implementation.

use std::sync::Arc;

use pernixc_query::Engine;
use pernixc_target::{Arguments, Check, Input, TargetID};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::Url;

use crate::workspace::{self, NewWorkspaceError, Workspace};

/// The struct that handles analysis of the workspace (e.g., checking errors).
#[derive(Debug)]
pub struct Analyzer {
    engine: RwLock<Arc<Engine>>,
    workspace: Workspace,
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
        let mut engine = Arc::new(Engine::default());

        Arc::get_mut(&mut engine)
            .unwrap()
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

        pernixc_corelib::initialize_corelib(&mut engine).await;

        Ok(Self { engine: RwLock::new(engine), workspace })
    }
}
