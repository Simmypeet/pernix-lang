//! Contains the diagnostic related to the symbol resolution process.

use pernixc_base::{
    diagnostic::{Diagnostic, Report},
    log::Severity,
    source_file::Span,
};

use crate::{
    diagnostic::ReportError,
    table::{GlobalID, Table},
};

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
    type Error = ReportError;

    fn report(&self, table: &Table) -> Result<Diagnostic, Self::Error> {
        if let Some(searched_in_module_id) = self.searched_item_id {
            let qualified_name = table
                .get_qualified_name(searched_in_module_id)
                .ok_or(ReportError)?;

            Ok(Diagnostic {
                span: self.resolution_span.clone(),
                message: format!(
                    "the symbol named `{}` does not exist in `{}`",
                    self.resolution_span.str(),
                    qualified_name
                ),
                severity: Severity::Error,
                help_message: None,
                related: Vec::new(),
            })
        } else {
            Ok(Diagnostic {
                span: self.resolution_span.clone(),
                message: format!(
                    "the symbol named `{}` does not exist",
                    self.resolution_span.str()
                ),
                severity: Severity::Error,
                help_message: None,
                related: Vec::new(),
            })
        }
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
    type Error = ReportError;

    fn report(&self, table: &Table) -> Result<Diagnostic, Self::Error> {
        let referring_site_qualified_name =
            table.get_qualified_name(self.referring_site).ok_or(ReportError)?;

        let referred_qualified_name =
            table.get_qualified_name(self.referred).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.referred_span.clone(),
            message: format!(
                "the symbol `{referred_qualified_name}` is not accessible \
                 from `{referring_site_qualified_name}`",
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}
