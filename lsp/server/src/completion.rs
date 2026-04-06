//! Handles completion requests.

use std::path::Path;

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_source_file::{get_source_file_by_id, get_stable_path_id};
use qbice::storage::intern::Interned;
use tower_lsp::lsp_types::{CompletionParams, CompletionResponse};

use crate::{
    completion::qualified_identifier::{
        qualified_identifier_completion, retrieve_qulaified_identifier_matching,
    },
    conversion::lsp_position_to_editor_location,
    test_config::is_testing_lsp,
};

pub mod keyword;
pub mod qualified_identifier;

/// Handles completion requests from the LSP client.
#[extend]
pub async fn handle_completion(
    self: &TrackedEngine,
    target_id: pernixc_target::TargetID,
    params: CompletionParams,
) -> Option<CompletionResponse> {
    let source_file_path: Interned<Path> = self.intern_unsized(
        params.text_document_position.text_document.uri.to_file_path().unwrap(),
    );
    let source_id = target_id.make_global(
        self.get_stable_path_id(source_file_path.clone(), target_id)
            .await
            .expect("lsp URL should've been valid"),
    );
    let source_file = self.get_source_file_by_id(source_id).await;
    let syntax_tree = self
        .query(&pernixc_syntax::Key {
            path: source_file_path.clone(),
            target_id,
        })
        .await;

    let token_tree = self
        .query(&pernixc_lexical::Key { path: source_file_path, target_id })
        .await;

    let Ok((token_tree, _error)) = token_tree else {
        return None;
    };

    let Ok((Some(content), error)) = syntax_tree else {
        return None;
    };

    let pernix_editor_location = source_file.lsp_position_to_editor_location(
        &params.text_document_position.position,
    );
    let byte_index = source_file
        .get_byte_index_from_editor_location(&pernix_editor_location)
        .unwrap();

    let in_qualified_identifier_context = self
        .retrieve_qulaified_identifier_matching(
            byte_index,
            &token_tree,
            &content,
        )
        .await
        .is_some();

    let mut qualified_identifier_completions = self
        .qualified_identifier_completion(
            byte_index,
            source_id,
            content,
            &token_tree,
            target_id,
        )
        .await;

    // in testing mode, sort completions to make snapshots stable
    if self.is_testing_lsp().await {
        qualified_identifier_completions.sort();
    }

    let mut completions = Vec::new();

    if !in_qualified_identifier_context {
        keyword::keyword_completion(
            &token_tree,
            &error,
            byte_index,
            &mut completions,
        );
    }

    completions.reserve(qualified_identifier_completions.len());

    for completion in qualified_identifier_completions {
        completions.push(completion.to_lsp_completion(self).await);
    }

    Some(CompletionResponse::Array(completions))
}
