//! Contains the server implementation.

use log::{debug, error, info};
use parking_lot::RwLock;
use tower_lsp::{
    jsonrpc,
    lsp_types::{
        DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, FileChangeType,
        InitializeParams, InitializeResult, InitializedParams, OneOf,
        Registration, ServerCapabilities, TextDocumentSyncCapability,
        TextDocumentSyncKind, Url, WorkspaceFoldersServerCapabilities,
        WorkspaceServerCapabilities,
    },
    Client, LanguageServer,
};

/// A diagnostic with its source file uri.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticsWithUrl {
    /// The uri where the diagnostics are reported.
    pub uri: Url,

    /// Diagnostic messages.
    pub diagnostics: Vec<tower_lsp::lsp_types::Diagnostic>,
}

pub mod extension;
pub mod semantic;
pub mod syntax;
pub mod workspace;

/// The language server protocal implementation for Pernix.
#[derive(Debug)]
pub struct Server {
    client: Client,

    syntax: RwLock<syntax::Syntax>,
    semantic: RwLock<semantic::Semantic>,

    /// Present if the server started with a workspace.
    workspace: RwLock<Option<workspace::Workspace>>,
}

impl Server {
    /// Creates a new server instance.
    #[must_use]
    pub fn new(client: Client) -> Self {
        Self {
            client,
            syntax: RwLock::new(syntax::Syntax::default()),
            semantic: RwLock::new(semantic::Semantic::default()),
            workspace: RwLock::new(None),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Server {
    async fn initialize(
        &self,
        _: InitializeParams,
    ) -> jsonrpc::Result<InitializeResult> {
        info!("Server initialized");

        Ok(InitializeResult {
            server_info: None,
            offset_encoding: None,
            capabilities: ServerCapabilities {
                inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),

                completion_provider: None,
                execute_command_provider: None,

                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(
                        WorkspaceFoldersServerCapabilities {
                            supported: Some(true),
                            change_notifications: Some(OneOf::Left(true)),
                        },
                    ),
                    file_operations: None,
                }),

                semantic_tokens_provider: None,

                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        // initialize the workspace if it exists
        if let Some(workspace) = self
            .client
            .workspace_folders()
            .await
            .into_iter()
            .flatten()
            .flatten()
            .next()
        {
            match self
                .client
                .register_capability(vec![Registration {
                    id: "pernixWatcher".to_string(),
                    method: "workspace/didChangeWatchedFiles".to_string(),
                    register_options: Some(serde_json::json!({
                        "watchers": [
                            {
                                "globPattern": "**/pernix.json",
                                "kind": 7,
                            }
                        ]
                    })),
                }])
                .await
            {
                Ok(()) => {}

                Err(response) => {
                    error!("failed to register watcher: {response}");
                }
            };

            if !self.configure_workspace(workspace.uri).await {
                info!("failed to configure workspace");
            }
        }
    }

    async fn did_change_watched_files(
        &self,
        params: DidChangeWatchedFilesParams,
    ) {
        info!("Did change watched files: {params:?}");

        let Some((workspace_uri, expected_file_uri)) =
            self.get_workspace_and_configuration_uri().await
        else {
            error!("failed to get workspace and configuration uri");
            return;
        };

        for change in params.changes {
            if change.uri != expected_file_uri {
                info!("ignoring file change: {:?}", change.uri);
                continue;
            }

            match change.typ {
                FileChangeType::CREATED | FileChangeType::CHANGED => {
                    // clear any prior diagnostics
                    self.client
                        .publish_diagnostics(
                            expected_file_uri.clone(),
                            vec![],
                            None,
                        )
                        .await;

                    let _ =
                        self.configure_workspace(workspace_uri.clone()).await;
                }

                FileChangeType::DELETED => {
                    self.workspace.write().take();
                }

                unknown => {
                    error!("unknown file change type: {unknown:?}");
                }
            }
        }
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> { Ok(()) }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        info!("Did open: {}", params.text_document.uri.path());

        let uri = params.text_document.uri.clone();
        let version = params.text_document.version;
        let diagnostics = self.analyze_did_open(params);

        self.client.publish_diagnostics(uri, diagnostics, Some(version)).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        info!("Did change: {}", params.text_document.uri.path());

        let uri = params.text_document.uri.clone();
        let version = params.text_document.version;
        let diagnostics = self.analyze_did_change(params);

        self.client.publish_diagnostics(uri, diagnostics, Some(version)).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        info!("Did save: {}", params.text_document.uri.path());

        let diagnostics = self.analyze_did_save(params);

        for (uri, diagnostics) in diagnostics {
            self.client.publish_diagnostics(uri, diagnostics, None).await;
        }
    }
}

impl Server {
    /// Reads the workspace configuration file and sets the workspace.
    #[must_use]
    async fn configure_workspace(&self, base_uri: Url) -> bool {
        let workspace = workspace::Workspace::new(&base_uri);

        // successfully create a new workspace
        match workspace {
            Ok(workspace) => {
                self.workspace.write().replace(workspace);
                true
            }

            // workspace file does exist but failed to parse the json
            Err(error) => match error {
                workspace::NewWorkspaceError::JsonConfiguration(
                    json_file_path,
                    errors,
                ) => {
                    let Ok(uri) = Url::from_file_path(&json_file_path) else {
                        error!(
                            "failed to convert file path to uri: {}",
                            json_file_path.display()
                        );
                        return false;
                    };

                    debug!(
                        "failed to parse workspace configuration: {errors:?} \
                         {uri:?}",
                    );

                    self.client
                        .publish_diagnostics(
                            uri,
                            errors
                                .into_iter()
                                .filter_map(|x| x.to_diagnostic(true))
                                .collect(),
                            None,
                        )
                        .await;

                    false
                }

                error => {
                    error!("failed to create workspace: {error}");
                    false
                }
            },
        }
    }

    async fn get_workspace_and_configuration_uri(&self) -> Option<(Url, Url)> {
        if let Some(workspace) = self
            .client
            .workspace_folders()
            .await
            .into_iter()
            .flatten()
            .flatten()
            .next()
        {
            let mut root_path = workspace.uri.to_file_path().ok()?;
            root_path.push("pernix.json");

            Some((workspace.uri, Url::from_file_path(root_path).ok()?))
        } else {
            None
        }
    }
}

impl Server {
    fn analyze_did_open(
        &self,
        params: DidOpenTextDocumentParams,
    ) -> Vec<tower_lsp::lsp_types::Diagnostic> {
        let mut syntax = self.syntax.write();

        syntax.register_source_file(
            params.text_document.uri.clone(),
            params.text_document.text,
        );

        syntax.analyze(&params.text_document.uri).unwrap_or_else(|| {
            error!(
                "failed to diagnose syntax: {}",
                params.text_document.uri.path()
            );
            Vec::new()
        })
    }

    /// Analyze the semantic when the source file is saved.
    fn analyze_did_save(
        &self,
        params: DidSaveTextDocumentParams,
    ) -> Vec<(Url, Vec<tower_lsp::lsp_types::Diagnostic>)> {
        let mut semantic = self.semantic.write();
        let workspace = self.workspace.read();

        semantic.analyze(workspace.as_ref().map_or_else(
            || semantic::OperatingMode::SingleFile(params.text_document.uri),
            semantic::OperatingMode::Workspace,
        ))
    }

    /// Analyze the changes in the source file and report syntax errors.
    ///
    /// TODO: In the [`semantic::Semantic::latest_semantic_errors_by_uri`]
    /// diagnostics' location should be updated according to the
    /// changes so that we can append those semantic diagnostics along with
    /// the syntax diagnostics.
    fn analyze_did_change(
        &self,
        mut params: DidChangeTextDocumentParams,
    ) -> Vec<tower_lsp::lsp_types::Diagnostic> {
        let mut syntax = self.syntax.write();

        // if found a change that replaces the whole content, all the changes
        // before it are ignored
        let changes = match params
            .content_changes
            .iter()
            .rposition(|x| x.range.is_none())
        {
            Some(index) => {
                syntax.register_source_file(
                    params.text_document.uri.clone(),
                    std::mem::take(&mut params.content_changes[index].text),
                );

                &params.content_changes[index + 1..]
            }
            None => &params.content_changes,
        };

        for change in changes {
            if let Err(err) = syntax.update_source_file(
                &params.text_document.uri,
                &change.text,
                change.range.unwrap(),
            ) {
                error!(
                    "failed to update source file: {}, reason: {}",
                    params.text_document.uri.path(),
                    err
                );
            }
        }

        syntax.analyze(&params.text_document.uri).unwrap_or_else(|| {
            error!(
                "failed to diagnose syntax: {}",
                params.text_document.uri.path()
            );
            Vec::new()
        })
    }
}
