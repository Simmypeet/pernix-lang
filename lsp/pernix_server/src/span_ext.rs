//! Contains extension traits for the [`pernixc_base::source_file::Span`] type.

use tower_lsp::lsp_types::Position;

/// Extension trait for converting the [`pernixc_base::source_file::Span`] type
/// to the [`tower_lsp::lsp_types::Range`] type.
pub trait SpanExt {
    /// Converts the span to a range.
    fn to_range(&self) -> tower_lsp::lsp_types::Range;
}

impl SpanExt for pernixc_base::source_file::Span {
    fn to_range(&self) -> tower_lsp::lsp_types::Range {
        let start = Position::new(
            self.start_location().line as u32,
            self.start_location().column as u32,
        );
        let end = self.end_location().map_or_else(
            || {
                let line = self.source_file().line_coount() - 1;
                let column = self
                    .source_file()
                    .get_line(self.source_file().line_coount() - 1)
                    .unwrap()
                    .len();

                Position::new(line as u32, column as u32)
            },
            |x| Position::new(x.line as u32, x.column as u32),
        );

        tower_lsp::lsp_types::Range { start, end }
    }
}
