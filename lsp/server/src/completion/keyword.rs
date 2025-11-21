//! Provides keyword completions based on parser errors.

use pernixc_diagnostic::ByteIndex;
use pernixc_parser::expect::Expected;
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

/// Provides keyword completions based on the parser errors at the given byte
/// index.
pub fn keyword_completion(
    token_tree: &pernixc_lexical::tree::Tree,
    errors: &[pernixc_parser::error::Error],
    byte_index: ByteIndex,
    completions: &mut Vec<CompletionItem>,
) {
    for error in errors {
        let span = error.at.to_relative_span(token_tree);
        let abs_span = token_tree.absolute_span_of(&span);
        let range_incl = abs_span.start..=abs_span.end;

        if range_incl.contains(&byte_index) {
            // Found the token at the completion position
            for expected in &error.expecteds {
                // take keywords only for now
                let Expected::Keyword(keyword) = *expected else {
                    continue;
                };

                completions.push(CompletionItem {
                    label: keyword.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    sort_text: Some(format!("99_keyword_{keyword}")),
                    ..Default::default()
                });
            }
        }
    }
}
