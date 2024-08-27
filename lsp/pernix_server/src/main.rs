//! The main entry point for the LSP server.

use pernix_server::Server;
use tower_lsp::LspService;

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(Server::new).finish();

    tower_lsp::Server::new(stdin, stdout, socket).serve(service).await;
}
