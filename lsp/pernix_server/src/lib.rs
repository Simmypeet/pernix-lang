//! Contains the server implementation.

use log::{error, info};
use tower_lsp::{
    jsonrpc,
    lsp_types::{
        DidChangeTextDocumentParams, DidOpenTextDocumentParams,
        DidSaveTextDocumentParams, InitializeParams, InitializeResult, OneOf,
        ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
        WorkspaceFoldersServerCapabilities, WorkspaceServerCapabilities,
    },
    Client, LanguageServer,
};

pub mod syntax;

/// The language server protocal implementation for Pernix.
#[derive(Debug)]
pub struct Server {
    client: Client,

    syntax_content: syntax::Context,
}

impl Server {
    /// Creates a new server instance.
    #[must_use]
    pub fn new(client: Client) -> Self {
        Self { client, syntax_content: syntax::Context::default() }
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
                // definition: Some(GotoCapability::default()),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> { Ok(()) }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        info!("Did open: {}", params.text_document.uri.path());

        let _ = self.syntax_content.register_source_file(
            params.text_document.uri.clone(),
            params.text_document.text,
        );

        let Some(diagnostics) =
            self.syntax_content.diagnose_syntax(&params.text_document.uri)
        else {
            error!(
                "failed to diagnose syntax: {}",
                params.text_document.uri.path()
            );
            return;
        };

        self.client
            .publish_diagnostics(
                params.text_document.uri,
                diagnostics,
                Some(params.text_document.version),
            )
            .await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        info!("Did change: {}", params.text_document.uri.path(),);

        // if found a change that replaces the whole content, all the changes
        // before it are ignored
        let changes = match params
            .content_changes
            .iter()
            .rposition(|x| x.range.is_none())
        {
            Some(index) => {
                let _ = self.syntax_content.register_source_file(
                    params.text_document.uri.clone(),
                    std::mem::take(&mut params.content_changes[index].text),
                );

                &params.content_changes[index + 1..]
            }
            None => &params.content_changes,
        };

        for change in changes {
            if let Err(err) = self.syntax_content.update_source_file(
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

        let Some(diagnostics) =
            self.syntax_content.diagnose_syntax(&params.text_document.uri)
        else {
            error!(
                "failed to diagnose syntax: {}",
                params.text_document.uri.path()
            );
            return;
        };

        self.client
            .publish_diagnostics(
                params.text_document.uri,
                diagnostics,
                Some(params.text_document.version),
            )
            .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        info!("Did save: {}", params.text_document.uri.path());
    }
}
