//! Contains the diagnostic related to the symbol resolution process.

use pernixc_diagnostic::{Diagnostic, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;

use crate::table::{GlobalID, Table};

/// The symbol was not found in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolNotFound {
    /// The [`GlobalID`] where the symbol was searched in. If `None`, the root
    /// module was searched.
    pub searched_item_id: Option<GlobalID>,

    /// The span where the symbol was searched from.
    pub resolution_span: Span,
}

impl Report<&Table> for SymbolNotFound {
    fn report(&self, table: &Table) -> Diagnostic {
        self.searched_item_id.map_or_else(
            || Diagnostic {
                span: self.resolution_span.clone(),
                message: format!(
                    "the symbol named `{}` does not exist",
                    self.resolution_span.str()
                ),
                severity: Severity::Error,
                help_message: None,
                related: Vec::new(),
            },
            |searched_in_module_id| {
                let qualified_name =
                    table.get_qualified_name(searched_in_module_id);

                Diagnostic {
                    span: self.resolution_span.clone(),
                    message: format!(
                        "the symbol named `{}` does not exist in `{}`",
                        self.resolution_span.str(),
                        qualified_name
                    ),
                    severity: Severity::Error,
                    help_message: None,
                    related: Vec::new(),
                }
            },
        )
    }
}

/// The symbol is not accessible from the referring site.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolIsNotAccessible {
    /// [`GlobalID`] where the [`Self::referred`] is referred.
    pub referring_site: GlobalID,

    /// The symbol that was referred and is not accessible.
    pub referred: GlobalID,

    /// The span where the [`Self::referred`] is referred from.
    pub referred_span: Span,
}

impl Report<&Table> for SymbolIsNotAccessible {
    fn report(&self, table: &Table) -> Diagnostic {
        let referring_site_qualified_name =
            table.get_qualified_name(self.referring_site);
        let referred_qualified_name = table.get_qualified_name(self.referred);

        Diagnostic {
            span: self.referred_span.clone(),
            message: format!(
                "the symbol `{referred_qualified_name}` is not accessible \
                 from `{referring_site_qualified_name}`",
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// `this` keyword is used outside the allowed scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ThisNotFound {
    /// The span where the `this` keyword was found.
    pub span: Span,
}

impl Report<&Table> for ThisNotFound {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: "`this` keyword cannot be used here".to_string(),
            severity: Severity::Error,
            help_message: Some(
                "`this` keyword can only be used in `trait` and `implements` \
                 to refer to that particular symbol"
                    .to_string(),
            ),
            related: Vec::new(),
        }
    }
}

/// The symbol doesn't require any generic arguments but some were supplied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NoGenericArgumentsRequired {
    /// The symbol that  was supplied with generic arguments.
    pub global_id: GlobalID,

    /// The span where the generic arguments were supplied.
    pub generic_argument_span: Span,
}

impl Report<&Table> for NoGenericArgumentsRequired {
    fn report(&self, table: &Table) -> Diagnostic {
        let qualified_name = table.get_qualified_name(self.global_id);

        Diagnostic {
            span: self.generic_argument_span.clone(),
            message: format!(
                "the symbol `{qualified_name}` doesn't require any generic \
                 arguments"
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}
