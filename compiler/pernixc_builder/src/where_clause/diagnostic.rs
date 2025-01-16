//! Contains the diagnostic information related to building the where clause.

use pernixc_diagnostic::{Diagnostic, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;
use pernixc_table::{
    component::SymbolKind, diagnostic::ReportError, GlobalID, Table,
};

/// The higher-ranked lifetime with the same name already exists in the given
/// scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HigherRankedLifetimeRedefinition {
    /// The span of the redefinition.
    pub redefinition_span: Span,
}

impl Report<&Table> for HigherRankedLifetimeRedefinition {
    type Error = ReportError;

    fn report(&self, _: &Table) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.redefinition_span.clone(),
            message: "the higher-ranked lifetime with the same name already \
                      exists in the given scope"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The kinds of the predicate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum PredicateKind {
    Trait,
    Marker,
    TraitTypeEquality,
}

/// The unexpected symbol was found in the predicate.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnexpectedSymbolInPredicate {
    /// The kind of the predicate that found the unexpected symbol.
    pub predicate_kind: PredicateKind,

    /// The ID of the found symbol.
    pub found_id: GlobalID,

    /// The span of the qualified identifier of the found symbol.
    pub qualified_identifier_span: Span,
}

impl Report<&Table> for UnexpectedSymbolInPredicate {
    type Error = ReportError;

    fn report(&self, table: &Table) -> Result<Diagnostic, Self::Error> {
        let found_symbol_qualified_name =
            table.get_qualified_name(self.found_id).ok_or(ReportError)?;
        let symbol_kind =
            *table.get::<SymbolKind>(self.found_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.qualified_identifier_span.clone(),
            message: format!(
                "unexpected symbol in the {} predicate: `{} {}`",
                match self.predicate_kind {
                    PredicateKind::Trait => "trait",
                    PredicateKind::Marker => "marker",
                    PredicateKind::TraitTypeEquality => "trait-type equality",
                },
                symbol_kind.kind_str(),
                found_symbol_qualified_name
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}
