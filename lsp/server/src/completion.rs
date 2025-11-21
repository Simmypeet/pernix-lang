//! Handles completion requests.

use std::{path::Path, sync::Arc};

use pernixc_extend::extend;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_source_file::{calculate_path_id, get_source_file_by_id};
use tower_lsp::lsp_types::{CompletionParams, CompletionResponse};

use crate::{
    completion::qualified_identifier::qualified_identifier_completion,
    conversion::to_pernix_editor_location,
};

pub mod keyword;
pub mod qualified_identifier;

/// Handles completion requests from the LSP client.
#[extend]
pub async fn handle_completion(
    self: &TrackedEngine,
    target_id: pernixc_target::TargetID,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>, CyclicError> {
    let source_file_path: Arc<Path> = Arc::from(
        params.text_document_position.text_document.uri.to_file_path().unwrap(),
    );
    let source_id = target_id.make_global(
        self.calculate_path_id(&source_file_path, target_id).await,
    );
    let source_file = self.get_source_file_by_id(source_id).await;
    let syntax_tree = self
        .query(&pernixc_syntax::Key {
            path: source_file_path.clone(),
            target_id,
        })
        .await?;

    let token_tree = self
        .query(&pernixc_lexical::Key { path: source_file_path, target_id })
        .await?;

    let Ok((token_tree, _error)) = token_tree else {
        return Ok(None);
    };

    let Ok((Some(content), error)) = syntax_tree else {
        return Ok(None);
    };

    let pernix_editor_location =
        params.text_document_position.position.to_pernix_editor_location();
    let byte_index = source_file
        .get_byte_index_from_editor_location(&pernix_editor_location);

    let mut completions = Vec::new();

    keyword::keyword_completion(
        &token_tree,
        &error,
        byte_index,
        &mut completions,
    );

    self.qualified_identifier_completion(
        byte_index,
        source_id,
        content,
        &token_tree,
        target_id,
        &mut completions,
    )
    .await?;

    Ok(Some(CompletionResponse::Array(completions)))
}
