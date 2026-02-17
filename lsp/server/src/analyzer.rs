//! Contains the analyzer implementation.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use pernixc_diagnostic::ByteIndex;
use pernixc_qbice::{Engine, IncrementalStorageEngine, TrackedEngine};
use pernixc_source_file::{
    EditorLocation, GlobalSourceID, SourceFile, get_source_file_by_id,
};
use pernixc_target::{Arguments, Check, Input, TargetID};
use qbice::{
    engine::{EngineOptions, YieldFrequency},
    executor,
    serialize::Plugin,
    stable_hash::SeededStableHasherBuilder,
    storage::{intern::Interned, storage_engine::StorageEngineFactory},
};
use tokio::sync::RwLock;
use tokio_util::sync::CancellationToken;
use tower_lsp::lsp_types::{DidChangeTextDocumentParams, Url};
use tracing::info;

use crate::{
    test_config::TestConfig,
    workspace::{self, NewWorkspaceError, Workspace},
};

/// The struct that handles analysis of the workspace (e.g., checking errors).
#[derive(Debug)]
pub struct Analyzer {
    engine: Arc<Engine>,
    workspace: Workspace,
    current_target_id: TargetID,
    published_diagnostics: RwLock<Vec<Url>>,
    cancellation_token: RwLock<CancellationToken>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct LoadSourceFileExecutor;

impl executor::Executor<pernixc_source_file::Key, pernixc_qbice::Config>
    for LoadSourceFileExecutor
{
    async fn execute(
        &self,
        key: &pernixc_source_file::Key,
        engine: &pernixc_qbice::TrackedEngine,
    ) -> Result<SourceFile, pernixc_source_file::Error> {
        let file = std::fs::File::open(key.path.as_ref()).map_err(|x| {
            pernixc_source_file::Error(engine.intern_unsized(x.to_string()))
        })?;

        let source_file = SourceFile::from_reader(
            std::io::BufReader::new(file),
            engine.intern_unsized(key.path.to_path_buf()),
        )
        .map_err(|x| {
            pernixc_source_file::Error(engine.intern_unsized(x.to_string()))
        })?;

        Ok(source_file)
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::ExternalInput
    }
}

impl Analyzer {
    /// Returns a read guard to the engine.
    #[must_use]
    pub const fn engine(&self) -> &Arc<Engine> { &self.engine }

    /// Returns the current target ID.
    #[must_use]
    pub const fn current_target_id(&self) -> TargetID { self.current_target_id }
}

impl Analyzer {
    /// Creates a new [`Engine`] setup for LSP services.
    ///
    /// # Arguments
    ///
    /// - `target_name`: The name of the target to create the engine for.
    /// - `root_source_file`: The root source file path of the target.
    /// - `source_file_loader_overide`: An executor to override the default
    ///   source file loader.
    /// - `target_seed`: An optional seed for the target.
    pub async fn create_engine<
        E: executor::Executor<pernixc_source_file::Key, pernixc_qbice::Config>,
        S: StorageEngineFactory<StorageEngine = pernixc_qbice::StorageEngine>,
    >(
        target_name: String,
        root_source_file: PathBuf,
        source_file_loader_override: Arc<E>,
        is_testing_lsp: bool,
        target_seed: Option<u64>,
        storage_engine_factory: S,
    ) -> Result<Arc<Engine>, S::Error> {
        let local_target_id = TargetID::from_target_name(&target_name);

        let command = pernixc_target::Command::Check(Check {
            input: Input {
                file: root_source_file,
                target_name: Some(target_name),
                library_paths: Vec::new(),
                incremental_path: None,
                chrome_tracing: false,
                target_seed,
                fancy: false,
            },
        });

        // initialize the engine with corelib
        let mut engine = Engine::new_with_options()
            .options(
                EngineOptions::builder()
                    .yield_frequency(YieldFrequency::EveryNQuery(0))
                    .build(),
            )
            .serialization_plugin(Plugin::default())
            .storage_engine_factory(storage_engine_factory)
            .stable_hasher(SeededStableHasherBuilder::new(0))
            .build()
            .await?;

        // Due to how rust compiler work, if a crate is linked without having
        // any symbols in it used, the crate will be completely ignored
        // and the static distributed registration will be optimized
        // out, causing the engine to not have the executors
        pernixc_source_file_impl::black_box();
        pernixc_lexical_impl::black_box();
        pernixc_syntax_impl::black_box();

        engine.register_program(pernixc_qbice::PERNIX_PROGRAM);

        engine.register_executor(source_file_loader_override);

        let engine = Arc::new(engine);

        {
            let mut input_session = engine.input_session().await;

            pernixc_corelib::initialize_corelib(&mut input_session).await;

            input_session
                .set_input(
                    pernixc_target::LinkKey { target_id: local_target_id },
                    input_session
                        .intern(std::iter::once(TargetID::CORE).collect()),
                )
                .await;

            input_session
                .set_input(
                    pernixc_target::AllTargetIDsKey,
                    input_session.intern(
                        vec![local_target_id, TargetID::CORE]
                            .into_iter()
                            .collect(),
                    ),
                )
                .await;

            input_session.set_input(TestConfig, is_testing_lsp).await;

            input_session
                .set_input(
                    pernixc_target::MapKey,
                    input_session.intern(
                        [
                            (
                                input_session.intern_unsized(
                                    command.input().target_name(),
                                ),
                                local_target_id,
                            ),
                            (
                                input_session.intern_unsized("core"),
                                TargetID::CORE,
                            ),
                        ]
                        .into_iter()
                        .collect(),
                    ),
                )
                .await;

            input_session
                .set_input(
                    pernixc_target::Key { target_id: local_target_id },
                    input_session.intern(Arguments { command }),
                )
                .await;

            if let Some(target_seed) = target_seed {
                input_session
                    .set_input(
                        pernixc_target::SeedKey { target_id: local_target_id },
                        target_seed,
                    )
                    .await;
            }

            pernixc_source_file_impl::refresh_source_file_executors(
                &mut input_session,
            )
            .await;

            input_session.commit().await;
        }

        Ok(engine)
    }

    /// Creates a new analyzer for the given workspace URL.
    pub async fn new(workspace_url: Url) -> Result<Self, NewWorkspaceError> {
        let workspace = workspace::Workspace::new(&workspace_url)?;
        let incremental_dir = workspace.root_path().join(".pernix");

        let target_name = workspace.target_name().to_string();
        let local_target_id = TargetID::from_target_name(&target_name);

        let engine = Self::create_engine(
            target_name,
            workspace.root_source_file().to_path_buf(),
            Arc::new(LoadSourceFileExecutor),
            false,
            None,
            IncrementalStorageEngine(incremental_dir),
        )
        .await
        .map_err(NewWorkspaceError::IncrementalDbFailure)?;

        Ok(Self {
            engine,
            workspace,
            current_target_id: local_target_id,
            published_diagnostics: RwLock::new(Vec::new()),
            cancellation_token: RwLock::new(CancellationToken::new()),
        })
    }

    /// Applies the given change to the source file in the engine.
    /// This does not perform a check, that must be done separately.
    pub async fn apply_change(&self, mut params: DidChangeTextDocumentParams) {
        let path: Interned<Path> = self
            .engine
            .intern_unsized(params.text_document.uri.to_file_path().unwrap());

        // cancel any ongoing analysis to the engine
        self.cancellation_token.write().await.cancel();

        let mut input_lock = self.engine.input_session().await;

        // create new cancellation token for the new analysis session
        *self.cancellation_token.write().await = CancellationToken::new();

        let key = pernixc_source_file::Key {
            path: path.clone(),
            target_id: self.current_target_id,
        };

        let changes = match params
            .content_changes
            .iter()
            .rposition(|x| x.range.is_none())
        {
            Some(index) => {
                let change = &mut params.content_changes[index];

                input_lock
                    .set_input(
                        key.clone(),
                        Ok(SourceFile::from_str(&change.text, path)),
                    )
                    .await;

                &params.content_changes[index + 1..]
            }
            None => &params.content_changes,
        };

        input_lock
            .update(key, |source_file| {
                let mut source_file = source_file.unwrap();
                let source_file_ref = source_file.as_mut().unwrap();

                for change in changes {
                    Self::update_source_file(
                        source_file_ref,
                        &change.text,
                        change.range.unwrap(),
                    );
                }

                source_file
            })
            .await;

        input_lock.commit().await;
    }

    fn update_source_file(
        source_file: &mut SourceFile,
        text: &str,
        range: tower_lsp::lsp_types::Range,
    ) {
        let pernix_location_start = EditorLocation::new(
            range.start.line as usize,
            range.start.character as usize,
        );
        let pernix_location_end = EditorLocation::new(
            range.end.line as usize,
            range.end.character as usize,
        );

        let start = source_file
            .get_byte_index_from_editor_location(&pernix_location_start)
            .unwrap_or_else(|| {
                panic!(
                    "the given range {pernix_location_start:?} is invalid to \
                     the source file"
                )
            });

        let end = source_file
            .get_byte_index_from_editor_location(&pernix_location_end)
            .unwrap_or_else(|| {
                panic!(
                    "the given range {pernix_location_end:?} is invalid to \
                     the source file"
                )
            });

        source_file.replace_range(start..end, text);
    }

    /// Obtains the cancellation token for the current analysis session.
    pub async fn cancellation_token(&self) -> CancellationToken {
        self.cancellation_token.read().await.clone()
    }

    /// Performs a compile check and sends diagnostics to the client.
    pub async fn check(
        &self,
        client: &tower_lsp::Client,
        version: Option<i32>,
    ) {
        let cancellation_token = self.cancellation_token().await;
        let engine = self.engine.clone().tracked().await;

        let check = cancellation_token
            .run_until_cancelled(engine.query(&pernixc_check::Key {
                target_id: self.current_target_id,
            }))
            .await;

        // collect diagnostics and group them by source file URL
        let mut diagnostics = HashMap::new();

        if let Some(check) = check {
            for diag in check.all_diagnostics() {
                let url = if let Some(primary) = &diag.primary_highlight {
                    engine.get_url_by_source_id(primary.span.source_id).await
                } else {
                    Url::from_file_path(self.workspace.root_source_file())
                        .unwrap()
                };

                diagnostics
                    .entry(url)
                    .or_insert_with(Vec::new)
                    .push(engine.to_lsp_diagnostic(diag).await);
            }
        } else {
            info!("check was cancelled, not sending diagnostics");
        }

        let mut published_diagnostics =
            self.published_diagnostics.write().await;

        // clear diagnostics for files that no longer have diagnostics
        for url in published_diagnostics.iter() {
            if !diagnostics.contains_key(url) {
                client
                    .publish_diagnostics(url.clone(), Vec::new(), version)
                    .await;
            }
        }

        published_diagnostics.clear();
        for (url, diags) in diagnostics {
            client
                .publish_diagnostics(url.clone(), diags.clone(), version)
                .await;
            published_diagnostics.push(url);
        }

        info!("check completed, sent diagnostics to client");
    }
}

#[pernixc_extend::extend]
#[allow(clippy::cast_possible_truncation)]
async fn to_lsp_range(
    self: &TrackedEngine,
    span: &pernixc_source_file::Span<ByteIndex>,
) -> tower_lsp::lsp_types::Range {
    let source_file = self.get_source_file_by_id(span.source_id).await;
    let start = source_file
        .get_editor_location_from_byte_index(span.start)
        .unwrap_or_else(|| {
            let line = source_file.line_coount() - 1;
            let column = source_file.get_line(line).unwrap().len();

            EditorLocation::new(line, column)
        });

    let end = source_file
        .get_editor_location_from_byte_index(span.end)
        .unwrap_or_else(|| {
            let line = source_file.line_coount() - 1;
            let column = source_file.get_line(line).unwrap().len();

            EditorLocation::new(line, column)
        });

    tower_lsp::lsp_types::Range {
        start: tower_lsp::lsp_types::Position {
            line: start.line as u32,
            character: start.column as u32,
        },
        end: tower_lsp::lsp_types::Position {
            line: end.line as u32,
            character: end.column as u32,
        },
    }
}

#[pernixc_extend::extend]
async fn get_url_by_source_id(
    self: &TrackedEngine,
    source_id: GlobalSourceID,
) -> Url {
    let source_file = self.get_source_file_by_id(source_id).await;

    let absolute_path = std::fs::canonicalize(source_file.path()).unwrap();

    Url::from_file_path(absolute_path).unwrap()
}

#[pernixc_extend::extend]
async fn to_lsp_diagnostic(
    self: &TrackedEngine,
    diagnostic: &pernixc_diagnostic::Rendered<ByteIndex>,
) -> tower_lsp::lsp_types::Diagnostic {
    let primary_range =
        if let Some(primary_highlight) = &diagnostic.primary_highlight {
            self.to_lsp_range(&primary_highlight.span).await
        } else {
            tower_lsp::lsp_types::Range {
                start: tower_lsp::lsp_types::Position { line: 0, character: 0 },
                end: tower_lsp::lsp_types::Position { line: 0, character: 0 },
            }
        };

    let severity = match diagnostic.severity {
        pernixc_diagnostic::Severity::Error => {
            tower_lsp::lsp_types::DiagnosticSeverity::ERROR
        }
        pernixc_diagnostic::Severity::Warning => {
            tower_lsp::lsp_types::DiagnosticSeverity::WARNING
        }
        pernixc_diagnostic::Severity::Info => {
            tower_lsp::lsp_types::DiagnosticSeverity::INFORMATION
        }
    };

    let related_information = {
        let mut result = Vec::new();
        for related in &diagnostic.related {
            result.push(tower_lsp::lsp_types::DiagnosticRelatedInformation {
                location: tower_lsp::lsp_types::Location {
                    uri: self
                        .get_url_by_source_id(related.span.source_id)
                        .await,
                    range: self.to_lsp_range(&related.span).await,
                },
                message: related.message.clone().unwrap_or_default(),
            });
        }

        if result.is_empty() { None } else { Some(result) }
    };

    let mut message = diagnostic.message.clone();

    if let Some(primary_message) =
        diagnostic.primary_highlight.as_ref().and_then(|x| x.message.as_ref())
    {
        message.push('\n');
        message.push_str(primary_message);
    }

    tower_lsp::lsp_types::Diagnostic {
        range: primary_range,
        severity: Some(severity),
        code: None,
        code_description: None,
        source: Some("pernixc".to_string()),
        message,
        related_information,
        tags: None,
        data: None,
    }
}
