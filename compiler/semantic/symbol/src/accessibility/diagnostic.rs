//! Contains the diagnostics related to accessibility of symbols.

use pernixc_diagnostic::{Related, Report, Severity};
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::TargetID;

use crate::{
    accessibility::{accessibility_description, get_accessibility},
    name::{get_name, get_qualified_name},
    span::get_span,
    ID,
};

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
    Serialize,
    Deserialize,
    StableHash,
)]
#[allow(missing_docs)]
pub enum Diagnostic {
    SymbolIsMoreAccessibleThanParent(SymbolIsMoreAccessibleThanParent),
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
    Serialize,
    Deserialize,
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

impl Report<&TrackedEngine> for SymbolIsMoreAccessibleThanParent {
    type Location = RelativeLocation;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let symbol_id = self.target_id.make_global(self.symbol_id);
        let parent_id = self.target_id.make_global(self.parent_id);

        let symbol_name = engine.get_name(symbol_id).await;
        let parent_qualified_name = engine.get_qualified_name(parent_id).await;

        let symbol_accessibility = engine.get_accessibility(symbol_id).await;
        let parent_accessibility = engine.get_accessibility(parent_id).await;

        let symbol_span = engine.get_span(symbol_id).await;
        let parent_span = engine.get_span(parent_id).await;

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

        pernixc_diagnostic::Diagnostic {
            span: symbol_span.map(|span| {
                (
                    span,
                    Some(format!(
                        "the symbol `{}` in `{parent_qualified_name}` is more \
                         accessible than the parent symbol",
                        symbol_name.as_str()
                    )),
                )
            }),

            message: "the symbol is more accessible than the parent symbol"
                .to_owned(),

            severity: Severity::Error,
            help_message: Some(format!(
                "the symbol `{}` is {symbol_accessibility_description} while \
                 the parent symbol is {parent_accessibility_description}",
                symbol_name.as_str()
            )),
            related: parent_span
                .map(|x| Related {
                    span: x,
                    message: format!(
                        "the parent symbol is \
                         {parent_accessibility_description}",
                    ),
                })
                .into_iter()
                .collect(),
        }
    }
}
