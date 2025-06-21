//! Contains all the diagnostics related creating a new compilation target.

use pernixc_diagnostic::Report;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::Engine;
use pernixc_target::Global;

use crate::{name::Ext as _, span::Ext as _, symbol};

/// The item symbol with the same name already exists in the given scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemRedifinition {
    /// The ID of the existing symbol.
    pub existing_id: Global<symbol::ID>,

    /// The ID of the new symbol.
    pub new_id: Global<symbol::ID>,

    /// The scope in which the duplication occurred.
    pub in_id: Global<symbol::ID>,
}

impl Report<&Engine> for ItemRedifinition {
    type Location = RelativeLocation;

    fn report(
        &self,
        engine: &Engine,
    ) -> pernixc_diagnostic::Diagnostic<RelativeLocation> {
        let existing_symbol_span = engine.get_span(self.existing_id);
        let new_symbol_span = engine.get_span(self.new_id);
        let existing_symbol_name = engine.get_name(self.existing_id);
        let in_name = engine.get_qualified_name(self.in_id);

        pernixc_diagnostic::Diagnostic {
            span: Some((
                *new_symbol_span,
                Some("redefinition here".to_string()),
            )),
            message: format!(
                "symbol `{}` is already defined in the scope `{}`",
                *existing_symbol_name, in_name
            ),
            severity: pernixc_diagnostic::Severity::Error,
            help_message: None,
            related: vec![pernixc_diagnostic::Related {
                span: *existing_symbol_span,
                message: format!(
                    "symbol `{}` is already defined here",
                    *existing_symbol_name
                ),
            }],
        }
    }
}

/// List of all diagnostics that can be reported during addition of new
/// compilation target.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Diagnostic {
    ItemRedifinition(ItemRedifinition),
}

impl Report<&Engine> for Diagnostic {
    type Location = RelativeLocation;

    fn report(
        &self,
        engine: &Engine,
    ) -> pernixc_diagnostic::Diagnostic<RelativeLocation> {
        match self {
            Self::ItemRedifinition(item) => item.report(engine),
        }
    }
}
