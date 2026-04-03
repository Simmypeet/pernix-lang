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
    declaration_span: Option<RelativeSpan>,
    cycle_path: Vec<Symbol>,
}

impl RecursiveAdt {
    pub const fn new(
        root_adt_id: Global<pernixc_symbol::SymbolID>,
        declaration_span: Option<RelativeSpan>,
        cycle_path: Vec<Symbol>,
    ) -> Self {
        Self { root_adt_id, declaration_span, cycle_path }
    }
}

impl Report for RecursiveAdt {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let root_name = engine.get_qualified_name(self.root_adt_id).await;
        let root_kind = engine.get_kind(self.root_adt_id).await.kind_str();
        let cycle = render_cycle_path(&self.cycle_path, engine).await;

        let primary_highlight = match &self.declaration_span {
            Some(span) => Some(Highlight::new(
                engine.to_absolute_span(span).await,
                Some(format!(
                    "this member introduces the recursive expansion `{cycle}`"
                )),
            )),
            None => None,
        };

        let related = match engine.get_span(self.root_adt_id).await {
            Some(span) => Some(vec![Highlight::new(
                engine.to_absolute_span(&span).await,
                Some(format!("`{root_name}` is declared here")),
            )]),
            None => None,
        };

        pernixc_diagnostic::Rendered::builder()
            .message(format!(
                "{root_kind} `{root_name}` is infinitely sized because it \
                 recursively contains itself"
            ))
            .maybe_primary_highlight(primary_highlight)
            .maybe_related(related)
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
