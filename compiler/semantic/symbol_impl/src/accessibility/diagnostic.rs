//! Contains the diagnostics related to accessibility of symbols.

use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_qbice::TrackedEngine;
use pernixc_source_file::ByteIndex;
use pernixc_symbol::{
    ID,
    accessibility::{accessibility_description, get_accessibility},
    name::{get_name, get_qualified_name},
    source_map::to_absolute_span,
    span::get_span,
};
use pernixc_target::TargetID;
use qbice::{Decode, Encode, Identifiable, StableHash};

/// Enumeration of all diagnostics related to accessibility.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Identifiable,
)]
#[allow(missing_docs)]
pub enum Diagnostic {
    SymbolIsMoreAccessibleThanParent(SymbolIsMoreAccessibleThanParent),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        match self {
            Self::SymbolIsMoreAccessibleThanParent(diagnostic) => {
                diagnostic.report(engine).await
            }
        }
    }
}

/// The symbol is more accessible than the parent symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct SymbolIsMoreAccessibleThanParent {
    /// The ID of the symbol that is more accessible than the parent symbol.
    pub symbol_id: ID,

    /// The ID of the parent symbol.
    pub parent_id: ID,

    /// The target ID of the symbol.
    pub target_id: TargetID,
}

impl Report for SymbolIsMoreAccessibleThanParent {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let symbol_id = self.target_id.make_global(self.symbol_id);
        let parent_id = self.target_id.make_global(self.parent_id);

        let symbol_name = engine.get_name(symbol_id).await;
        let parent_qualified_name = engine.get_qualified_name(parent_id).await;

        let symbol_accessibility = engine.get_accessibility(symbol_id).await;
        let parent_accessibility = engine.get_accessibility(parent_id).await;

        let symbol_span = if let Some(s) = engine.get_span(symbol_id).await {
            Some(engine.to_absolute_span(&s).await)
        } else {
            None
        };

        let parent_span = if let Some(s) = engine.get_span(parent_id).await {
            Some(engine.to_absolute_span(&s).await)
        } else {
            None
        };

        let symbol_accessibility_description = engine
            .accessibility_description(
                symbol_accessibility.into_global(self.target_id),
            )
            .await;

        let parent_accessibility_description = engine
            .accessibility_description(
                parent_accessibility.into_global(self.target_id),
            )
            .await;

        pernixc_diagnostic::Rendered {
            primary_highlight: symbol_span.map(|span| {
                Highlight::new(
                    span,
                    Some(format!(
                        "the symbol `{}` in `{parent_qualified_name}` is more \
                         accessible than the parent symbol",
                        symbol_name.as_ref(),
                    )),
                )
            }),

            message: "the symbol is more accessible than the parent symbol"
                .to_owned(),

            severity: Severity::Error,
            help_message: Some(format!(
                "the symbol `{}` is {symbol_accessibility_description} while \
                 the parent symbol is {parent_accessibility_description}",
                symbol_name.as_ref(),
            )),
            related: parent_span
                .map(|x| {
                    Highlight::new(
                        x,
                        Some(format!(
                            "the parent symbol is \
                             {parent_accessibility_description}",
                        )),
                    )
                })
                .into_iter()
                .collect(),
        }
    }
}
