//! Contains the diagnostic related to the where clause check.

use pernixc_diagnostic::{Diagnostic, Related, Report};
use pernixc_log::Severity;
use pernixc_semantic::{
    table::{DisplayObject, Table},
    term::{
        self,
        predicate::{self, Predicate},
        r#type,
    },
};
use pernixc_source_file::Span;

/// The predicates are ambiguous to each other.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AmbiguousPredicates {
    /// The list of predicates that are ambiguous to each other.
    pub predicates: Vec<Predicate<term::Default>>,

    /// The span where the ambiguous predicates are declared.
    pub predicate_declaration_spans: Vec<Span>,
}

impl Report<&Table> for AmbiguousPredicates {
    fn report(&self, table: &Table) -> Diagnostic {
        let mut span_iter = self.predicate_declaration_spans.iter();
        let first_span = span_iter.next().unwrap();

        Diagnostic {
            span: first_span.clone(),
            message: format!(
                "the predicates are ambiguous to each other: {}",
                self.predicates
                    .iter()
                    .map(|predicate| {
                        DisplayObject { table, display: predicate }.to_string()
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            severity: Severity::Error,
            help_message: None,
            related: span_iter
                .map(|span| Related {
                    span: span.clone(),
                    message: "the ambiguous predicate is declared here"
                        .to_string(),
                })
                .collect(),
        }
    }
}

/// The predicate declared is definite (its satisfiability is already known).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitePremisePredicate {
    /// The span of the definite premise.
    pub span: Span,

    /// The definite premise.
    pub predicate: Predicate<term::Default>,
}

impl Report<&Table> for DefinitePremisePredicate {
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the predicate `{}` is definite, the satisfiability of the \
                 predicate is already known when it's declared",
                DisplayObject { display: &self.predicate, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The trait type equality is declared in such a way that it can be expanded
/// infinitely.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecursiveTraitTypeEquality {
    /// The trait type equality that is recursive.
    pub trait_type_equality: predicate::Compatible<
        r#type::TraitMember<term::Default>,
        r#type::Type<term::Default>,
    >,

    /// The span where the recursive trait type equalities are declared.
    pub predicate_declaration_spans: Vec<Span>,
}

impl Report<&Table> for RecursiveTraitTypeEquality {
    fn report(&self, table: &Table) -> Diagnostic {
        let mut span_iter = self.predicate_declaration_spans.iter();
        let first_span = span_iter.next().unwrap();

        Diagnostic {
            span: first_span.clone(),
            message: format!(
                "the predicate `{}` is recursive, it can be expanded \
                 infinitely",
                DisplayObject { table, display: &self.trait_type_equality }
            ),
            severity: Severity::Error,
            help_message: None,
            related: span_iter
                .map(|span| Related {
                    span: span.clone(),
                    message: "the recursive trait type equality is declared \
                              here"
                        .to_string(),
                })
                .collect(),
        }
    }
}
