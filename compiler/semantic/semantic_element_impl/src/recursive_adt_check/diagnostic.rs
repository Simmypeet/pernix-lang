use pernixc_diagnostic::{Highlight, Note, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_source_file::ByteIndex;
use pernixc_symbol::{
    kind::get_kind, name::get_qualified_name, source_map::to_absolute_span,
    span::get_span,
};
use pernixc_target::Global;
use pernixc_term::{display::Display, generic_arguments::Symbol};
use qbice::{Decode, Encode, Identifiable, StableHash};

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Identifiable,
)]
#[allow(clippy::large_enum_variant)]
pub enum Diagnostic {
    RecursiveAdt(RecursiveAdt),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        match self {
            Self::RecursiveAdt(diagnostic) => diagnostic.report(engine).await,
        }
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Identifiable,
)]
pub struct RecursiveAdt {
    root_adt_id: Global<pernixc_symbol::SymbolID>,
    expansion_links: Vec<PathLink>,
    expansion_path: Vec<Symbol>,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct PathLink {
    from: Symbol,
    span: Option<RelativeSpan>,
    to: Symbol,
}

impl PathLink {
    pub const fn new(
        from: Symbol,
        span: Option<RelativeSpan>,
        to: Symbol,
    ) -> Self {
        Self { from, span, to }
    }
}

impl RecursiveAdt {
    pub const fn new(
        root_adt_id: Global<pernixc_symbol::SymbolID>,
        expansion_links: Vec<PathLink>,
        expansion_path: Vec<Symbol>,
    ) -> Self {
        Self { root_adt_id, expansion_links, expansion_path }
    }
}

impl Report for RecursiveAdt {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let root_name = engine.get_qualified_name(self.root_adt_id).await;
        let root_kind = engine.get_kind(self.root_adt_id).await.kind_str();
        let cycle = render_cycle_path(&self.expansion_path, engine).await;

        let primary_highlight = if let Some(span) =
            self.expansion_links.last().and_then(|x| x.span.as_ref())
        {
            Some(Highlight::new(
                engine.to_absolute_span(span).await,
                Some(format!(
                    "this member closes the recursive expansion `{cycle}`"
                )),
            ))
        } else {
            None
        };

        let mut related = Vec::new();

        if let Some(span) = engine.get_span(self.root_adt_id).await {
            related.push(Highlight::new(
                engine.to_absolute_span(&span).await,
                Some(format!("`{root_name}` is declared here")),
            ));
        }

        for link in self
            .expansion_links
            .iter()
            .take(self.expansion_links.len().saturating_sub(1))
        {
            let Some(span) = link.span.as_ref() else {
                continue;
            };

            let from = link.from.write_to_string(engine).await.unwrap();
            let to = link.to.write_to_string(engine).await.unwrap();

            related.push(Highlight::new(
                engine.to_absolute_span(span).await,
                Some(format!("`{from}` expands to `{to}` here")),
            ));
        }

        pernixc_diagnostic::Rendered::builder()
            .message(format!(
                "{root_kind} `{root_name}` is infinitely sized because it \
                 recursively contains itself"
            ))
            .maybe_primary_highlight(primary_highlight)
            .maybe_related((!related.is_empty()).then_some(related))
            .notes(vec![
                Note::builder()
                    .message(format!("recursive expansion path: {cycle}"))
                    .build(),
            ])
            .help_message(
                "consider introducing indirection in the recursive path so \
                 the ADT no longer stores itself by value",
            )
            .build()
    }
}

async fn render_cycle_path(
    cycle_path: &[Symbol],
    engine: &TrackedEngine,
) -> String {
    let mut rendered = Vec::with_capacity(cycle_path.len());

    for symbol in cycle_path {
        rendered.push(symbol.write_to_string(engine).await.unwrap());
    }

    rendered.join(" -> ")
}
