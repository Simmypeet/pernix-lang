//! Contains the diagnostic information related to building the where clause.

use pernixc_diagnostic::{Diagnostic, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;
use pernixc_table::{component::SymbolKind, GlobalID, Table};
use pernixc_term::{
    forall_lifetime::{self, ForallLifetimeID},
    r#type::Type,
    Default,
};

/// The higher-ranked lifetime with the same name already exists in the given
/// scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HigherRankedLifetimeRedefinition {
    /// The span of the redefinition.
    pub redefinition_span: Span,
}

impl Report<&Table> for HigherRankedLifetimeRedefinition {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.redefinition_span.clone(),
            message: "the higher-ranked lifetime with the same name already \
                      exists in the given scope"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The kinds of the predicate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum PredicateKind {
    Trait,
    Marker,
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
    fn report(&self, table: &Table) -> Diagnostic {
        let found_symbol_qualified_name =
            table.get_qualified_name(self.found_id);
        let symbol_kind = *table.get::<SymbolKind>(self.found_id);

        Diagnostic {
            span: self.qualified_identifier_span.clone(),
            message: format!(
                "unexpected symbol in the {} predicate: `{} {}`",
                match self.predicate_kind {
                    PredicateKind::Trait => "trait",
                    PredicateKind::Marker => "marker",
                },
                symbol_kind.kind_str(),
                found_symbol_qualified_name
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The left-hand side of the type equality predicate must be a trait type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnexpectedTypeEqualityPredicate {
    /// The span of the invalid left-hand side type.
    pub invalid_lhs_type_span: Span,

    /// The span of the type equality predicate.
    pub found_type: Type<Default>,
}

impl Report<&Table> for UnexpectedTypeEqualityPredicate {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.invalid_lhs_type_span.clone(),
            message: "the left-hand side of the type equality predicate must \
                      be a trait associated type"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Forall lifetimes cannot appear in the outlives predicate.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ForallLifetimeIsNotAllowedInOutlivesPredicate {
    /// The span of the term that contains the forall lifetime.
    pub forall_lifetime_span: Span,

    /// The list of forall lifetimes found in the term.
    pub forall_lifetimes: Vec<ForallLifetimeID>,
}

impl Report<&Table> for ForallLifetimeIsNotAllowedInOutlivesPredicate {
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.forall_lifetime_span.clone(),
            message: format!(
                "forall lifetime(s) {} cannot appear in the outlives predicate",
                self.forall_lifetimes
                    .iter()
                    .filter_map(|x| {
                        let map = table
                            .query::<forall_lifetime::Map>(x.parent)
                            .unwrap();

                        let string = map
                            .get(x.id)
                            .unwrap()
                            .as_named()
                            .map(|x| format!("`{}`", x.name));

                        string
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}
