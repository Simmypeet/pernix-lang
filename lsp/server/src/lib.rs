//! Contains the server implementation.

use log::{error, info};
use tokio::sync::RwLock;
use tower_lsp::{
    jsonrpc,
    lsp_types::{
        DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, FileChangeType,
        HoverProviderCapability, InitializeParams, InitializeResult,
        InitializedParams, MessageType, OneOf, Registration,
        ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
        Url, WorkspaceFoldersServerCapabilities, WorkspaceServerCapabilities,
    },
    Client, LanguageServer,
};

use crate::{analyzer::Analyzer, hover::handle_hover};

/// A diagnostic with its source file uri.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticsWithUrl {
    /// The uri where the diagnostics are reported.
    pub uri: Url,

    /// Diagnostic messages.
    pub diagnostics: Vec<tower_lsp::lsp_types::Diagnostic>,
}

pub mod analyzer;
pub mod hover;
pub mod pointing;
pub mod workspace;

/// The language server protocal implementation for Pernix.
#[derive(Debug)]
pub struct Server {
    client: Client,

    /// Present if the server started with a workspace.
    analyzer: RwLock<Option<Analyzer>>,
}

impl Server {
    /// Creates a new server instance.
    #[must_use]
    pub fn new(client: Client) -> Self {
        Self { client, analyzer: RwLock::new(None) }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Server {
    async fn initialize(
        &self,
        _: InitializeParams,
    ) -> jsonrpc::Result<InitializeResult> {
        info!("Pernix Language Server is initializing...");

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
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                semantic_tokens_provider: None,

                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        // initialize the workspace if it exists
        let Some(workspace) = self
            .client
            .workspace_folders()
            .await
            .into_iter()
            .flatten()
            .flatten()
            .next()
        else {
            self.client
                .log_message(
                    MessageType::ERROR,
                    "no workspace folder found, the server's services will be \
                     limited",
                )
                .await;
            return;
        };

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
                error!("failed to register file watcher: {response}");
                self.client
                    .log_message(
                        MessageType::ERROR,
                        "failed to initialize the file watcher, the server \
                         will not respond to changes in `pernix.json`",
                    )
                    .await;
                return;
            }
        }

        match analyzer::Analyzer::new(workspace.uri.clone()).await {
            Ok(analyzer) => {
                info!(
                    "Pernix Language Server is initialized at workspace: {}",
                    workspace.uri
                );

                // perform an initial check of the workspace
                analyzer.check(&self.client, None).await;

                self.analyzer.write().await.replace(analyzer);
            }

            Err(err) => {
                self.client.log_message(MessageType::ERROR, err).await;
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
                    match analyzer::Analyzer::new(workspace_uri.clone()).await {
                        Ok(analyzer) => {
                            self.analyzer.write().await.replace(analyzer);
                        }

                        Err(err) => {
                            self.client
                                .log_message(MessageType::ERROR, err)
                                .await;
                        }
                    }
                }

                FileChangeType::DELETED => {
                    self.analyzer.write().await.take();
                }

                unknown => {
                    error!("unknown file change type: {unknown:?}");
                }
            }
        }
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> { Ok(()) }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let analyzer = self.analyzer.read().await;
        let Some(analyzer) = analyzer.as_ref() else {
            return;
        };

        info!("Did open: {}", params.text_document.uri.path());

        analyzer.check(&self.client, None).await;
    }

    async fn hover(
        &self,
        params: tower_lsp::lsp_types::HoverParams,
    ) -> jsonrpc::Result<Option<tower_lsp::lsp_types::Hover>> {
        let analyzer = self.analyzer.read().await;
        let Some(analyzer) = analyzer.as_ref() else {
            return Ok(None);
        };

        let engine = analyzer.engine().await;
        let engine = engine.tracked();

        Ok(engine
            .handle_hover(analyzer.current_target_id(), params)
            .await
            .unwrap_or_default())
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let analyzer = self.analyzer.read().await;
        let Some(analyzer) = analyzer.as_ref() else {
            return;
        };

        info!("Did change: {}", params.text_document.uri.path());

        analyzer.apply_change(params).await;
        analyzer.check(&self.client, None).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let analyzer = self.analyzer.read().await;
        let Some(analyzer) = analyzer.as_ref() else {
            return;
        };

        info!("Did save: {}", params.text_document.uri.path());

        analyzer.check(&self.client, None).await;
    }
}

impl Server {
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
