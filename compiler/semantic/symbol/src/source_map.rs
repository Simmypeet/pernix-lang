//! Contains utilities for converting between relative and absolute spans.

use pernixc_diagnostic::ByteIndex;
use pernixc_extend::extend;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::TrackedEngine;
use pernixc_source_file::{Span, get_source_file_path};

/// Converts the given relative span to an absolute span.
///
/// This method uses the token tree in order to calculate an absolute position.
#[extend]
pub async fn to_absolute_span(
    self: &TrackedEngine,
    relative_span: &Span<RelativeLocation>,
) -> Span<ByteIndex> {
    let path = self.get_source_file_path(relative_span.source_id).await;
    let file = self
        .query(&pernixc_source_file::Key {
            path: path.clone(),
            target_id: relative_span.source_id.target_id,
        })
        .await
        .unwrap()
        .unwrap();

    let token_tree = self
        .query(&pernixc_lexical::Key {
            path,
            target_id: relative_span.source_id.target_id,
        })
        .await
        .unwrap()
        .unwrap()
        .0;

    relative_span.to_absolute_span(&file, &token_tree)
}
