//! Contains the configuration for terminal display using annotate-snippets.

use std::io::Write;

use annotate_snippets::{
    AnnotationKind, Group, Level, Renderer, Snippet, renderer::DecorStyle,
};
use pernixc_diagnostic::{ByteIndex, Rendered};
use pernixc_source_file::SourceFile;
use pernixc_symbol_impl::source_map::SourceMap;

/// The default number of context lines to show before and after annotations.
const DEFAULT_CONTEXT_LINES: usize = 3;

/// A struct that handles emitting diagnostics to the terminal.
pub struct ReportTerm<'a> {
    renderer: Renderer,
    /// The writer to emit diagnostics to.
    writer: &'a mut dyn Write,
    /// The source map containing source files for diagnostics.
    source_map: Option<&'a SourceMap>,
    fancy: bool,
}

impl<'a> ReportTerm<'a> {
    /// Creates a new [`ReportTerm`] with the given writer,
    /// using unicode character set with optional colors.
    #[must_use]
    pub fn new(writer: &'a mut dyn Write, fancy: bool) -> Self {
        let renderer = if fancy {
            Renderer::styled().decor_style(DecorStyle::Unicode)
        } else {
            Renderer::plain().decor_style(DecorStyle::Unicode)
        };

        Self { renderer, writer, source_map: None, fancy }
    }

    /// Sets the source map for the [`ReportTerm`], which is used to render
    /// source code snippets in diagnostics.
    pub const fn set_source_map(&mut self, source_map: &'a SourceMap) {
        self.source_map = Some(source_map);
    }

    /// Determines whether the [`ReportTerm`] is configured to use fancy unicode
    /// characters and colors.
    #[must_use]
    pub const fn fancy(&self) -> bool { self.fancy }
}

impl std::fmt::Debug for ReportTerm<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReportTerm")
            .field("renderer", &self.renderer)
            .field("source_map", &self.source_map)
            .field("fancy", &self.fancy)
            .finish_non_exhaustive()
    }
}

impl ReportTerm<'_> {
    /// Reports a rendered pernixc diagnostic to the terminal using the
    /// configured graphical theme.
    pub fn report_rendered(&mut self, diagnostic: &Rendered<ByteIndex>) {
        let source_map =
            self.source_map.expect("ReportTerm must have a source map");
        let output = render_diagnostic(diagnostic, source_map, &self.renderer);
        let _ = writeln!(self.writer, "{output}\n");
    }

    /// Reports a simple error message without source code.
    pub fn report_simple_error(&mut self, message: impl Into<String>) {
        let message_str = message.into();
        let groups = vec![Group::with_title(
            Level::ERROR.with_name("[error]").primary_title(&message_str),
        )];
        let output = self.renderer.render(&groups);
        let _ = writeln!(self.writer, "{output}\n");
    }

    /// Reports a simple warning message without source code.
    pub fn report_simple_warning(&mut self, message: impl Into<String>) {
        let message_str = message.into();
        let groups = vec![Group::with_title(
            Level::WARNING.with_name("[warning]").primary_title(&message_str),
        )];
        let output = self.renderer.render(&groups);
        let _ = writeln!(self.writer, "{output}\n");
    }

    /// Reports a simple note message without source code.
    pub fn report_simple_note(&mut self, message: impl Into<String>) {
        let message_str = message.into();
        let groups = vec![Group::with_title(
            Level::NOTE.with_name("[note]").primary_title(&message_str),
        )];
        let output = self.renderer.render(&groups);
        let _ = writeln!(self.writer, "{output}\n");
    }

    /// Reports a simple error message with a help message, without source code.
    pub fn report_simple_error_with_help(
        &mut self,
        message: impl Into<String>,
        help: impl Into<String>,
    ) {
        let message_str = message.into();
        let help_str = help.into();
        let groups = vec![
            Group::with_title(
                Level::ERROR.with_name("[error]").primary_title(&message_str),
            )
            .element(Level::HELP.message(&help_str)),
        ];
        let output = self.renderer.render(&groups);
        let _ = writeln!(self.writer, "{output}\n");
    }
}

/// Information about a single source context (a file's content with
/// annotations).
struct SourceContext {
    /// The source content (materialized from the rope).
    content: String,
    /// The path to display for this source.
    path: String,
    /// The line number where this context starts (1-indexed for display).
    line_start: usize,
    /// Annotations for this context, with spans adjusted relative to the
    /// context start.
    annotations: Vec<(std::ops::Range<usize>, String, bool)>, /* (span, label, is_primary) */
}

/// Computes the byte range for the context lines around the given spans.
fn compute_context_range(
    source_file: &SourceFile,
    spans: &[(ByteIndex, ByteIndex)],
    context_lines: usize,
) -> (usize, usize, usize) {
    // Find the overall span covering all highlights
    let min_start = spans.iter().map(|(s, _)| *s).min().unwrap_or(0);
    let max_end = spans.iter().map(|(_, e)| *e).max().unwrap_or(0);

    let total_lines = source_file.line_coount();

    // Find lines containing the span boundaries
    let start_line = source_file.byte_to_line(min_start);
    let end_line = if min_start == max_end {
        start_line
    } else {
        source_file.byte_to_line(max_end.saturating_sub(1))
    };

    // Add context lines
    let context_start_line = start_line.saturating_sub(context_lines);
    let context_end_line = (end_line + context_lines + 1).min(total_lines);

    // Get byte range for context
    let context_start = source_file.line_to_byte(context_start_line);
    let context_end = if context_end_line >= total_lines {
        source_file.len_bytes()
    } else {
        source_file.line_to_byte(context_end_line)
    };

    // Line start is 1-indexed for display
    (context_start, context_end, context_start_line + 1)
}

/// A highlight entry with (start, end, label, is_primary).
#[allow(clippy::doc_markdown)]
type HighlightEntry<'a> = (ByteIndex, ByteIndex, Option<&'a str>, bool);

/// Renders a pernixc diagnostic to a string using annotate-snippets.
fn render_diagnostic(
    diagnostic: &Rendered<ByteIndex>,
    source_map: &SourceMap,
    renderer: &Renderer,
) -> String {
    let level = match diagnostic.severity {
        pernixc_diagnostic::Severity::Error => {
            Level::ERROR.with_name("[error]")
        }
        pernixc_diagnostic::Severity::Warning => {
            Level::WARNING.with_name("[warning]")
        }
        pernixc_diagnostic::Severity::Info => Level::NOTE.with_name("[note]"),
    };

    // Collect all highlights and group them by source file.
    let mut highlights_by_file: pernixc_hash::HashMap<
        pernixc_source_file::GlobalSourceID,
        Vec<HighlightEntry<'_>>,
    > = pernixc_hash::HashMap::default();

    // Add primary highlight
    if let Some(primary) = &diagnostic.primary_highlight {
        highlights_by_file.entry(primary.span.source_id).or_default().push((
            primary.span.start,
            primary.span.end,
            primary.message.as_deref(),
            true,
        ));
    }

    // Add related highlights
    for related in &diagnostic.related {
        highlights_by_file.entry(related.span.source_id).or_default().push((
            related.span.start,
            related.span.end,
            related.message.as_deref(),
            false,
        ));
    }

    // If there's no primary highlight and no related highlights, create a
    // simple message
    if highlights_by_file.is_empty() {
        let mut group =
            Group::with_title(level.primary_title(&diagnostic.message));
        if let Some(help) = &diagnostic.help_message {
            group = group.element(Level::HELP.message(help.as_str()));
        }
        return renderer.render(&[group]);
    }

    // Build source contexts for each file
    let mut source_contexts: Vec<SourceContext> = Vec::new();

    for (source_id, highlights) in &highlights_by_file {
        let Some(source_file) = source_map.0.get(source_id) else {
            continue;
        };

        let spans: Vec<(ByteIndex, ByteIndex)> =
            highlights.iter().map(|(s, e, _, _)| (*s, *e)).collect();

        let (context_start, context_end, line_start) =
            compute_context_range(source_file, &spans, DEFAULT_CONTEXT_LINES);

        // Materialize only the context portion
        let content = source_file.slice(context_start..context_end);
        let path = source_file.path().display().to_string();

        // Adjust spans to be relative to context start
        let annotations: Vec<(std::ops::Range<usize>, String, bool)> =
            highlights
                .iter()
                .filter_map(|(start, end, label, is_primary)| {
                    // Ensure spans are within context bounds
                    if *start < context_start || *end > context_end {
                        return None;
                    }
                    let relative_start = start - context_start;
                    let relative_end = end - context_start;
                    Some((
                        relative_start..relative_end,
                        label.unwrap_or("").to_string(),
                        *is_primary,
                    ))
                })
                .collect();

        source_contexts.push(SourceContext {
            content,
            path,
            line_start,
            annotations,
        });
    }

    // If all source contexts failed to load, render without source
    if source_contexts.is_empty() {
        let mut group =
            Group::with_title(level.primary_title(&diagnostic.message));
        if let Some(help) = &diagnostic.help_message {
            group = group.element(Level::HELP.message(help.as_str()));
        }
        return renderer.render(&[group]);
    }

    // Build a single group with all snippets
    let mut group =
        Group::with_title(level.primary_title(diagnostic.message.clone()));

    for ctx in &source_contexts {
        let mut snippet = Snippet::source(ctx.content.clone())
            .path(ctx.path.clone())
            .line_start(ctx.line_start)
            .fold(true);

        for (span, label, is_primary) in &ctx.annotations {
            let kind = if *is_primary {
                AnnotationKind::Primary
            } else {
                AnnotationKind::Context
            };
            snippet = snippet.annotation(kind.span(span.clone()).label(label));
        }

        group = group.element(snippet);
    }

    // Add help message if present
    if let Some(help) = &diagnostic.help_message {
        group = group.element(Level::HELP.message(help.as_str()));
    }

    renderer.render(&[group])
}
