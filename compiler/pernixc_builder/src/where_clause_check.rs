//! Contains the code for checking the well-formedness of the where clause.

use std::borrow::Cow;

use diagnostic::{
    AmbiguousPredicates, DefinitePremisePredicate, RecursiveTraitTypeEquality,
};
use pernixc_handler::Handler;
use pernixc_table::{
    component::SymbolKind, diagnostic::Diagnostic, GlobalID, Table,
};
use pernixc_term::predicate::Predicate;
use pernixc_type_system::{
    diagnostic::TypeCalculatingOverflow,
    environment::{Environment, Error, GetActivePremiseExt},
    normalizer,
};

pub mod diagnostic;

/// Checks the well-formedness of the where clause of the symbol.
pub fn check(
    table: &Table,
    id: GlobalID,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    let symbol_kind = *table.get::<SymbolKind>(id);

    if !symbol_kind.has_where_clause() && !symbol_kind.has_implied_predicates()
    {
        return;
    }

    let premise = table.get_active_premise::<pernixc_term::Default>(id);

    let (_, errors) = Environment::diagnose(
        Cow::Borrowed(&premise),
        table,
        normalizer::NO_OP,
    );

    let active_premise_with_span = table
        .get_active_premise_predicates_with_span::<pernixc_term::Default>(id);

    for error in errors {
        match error {
            Error::AmbiguousPredicates(predicates) => {
                let spans = predicates
                    .iter()
                    .flat_map(|x| {
                        active_premise_with_span.get(x).unwrap().iter()
                    })
                    .cloned()
                    .collect::<Vec<_>>();

                handler.receive(Box::new(AmbiguousPredicates {
                    predicates,
                    predicate_declaration_spans: spans,
                }));
            }

            Error::DefinintePremise(predicate) => {
                let span = active_premise_with_span.get(&predicate).unwrap();

                for span in span {
                    handler.receive(Box::new(DefinitePremisePredicate {
                        predicate: predicate.clone(),
                        span: span.clone(),
                    }));
                }
            }

            Error::RecursiveTraitTypeEqualityPredicate(pred) => {
                let pred = Predicate::TraitTypeCompatible(pred);
                let spans =
                    active_premise_with_span.get(&pred).unwrap().clone();

                handler.receive(Box::new(RecursiveTraitTypeEquality {
                    trait_type_equality: pred
                        .into_trait_type_compatible()
                        .unwrap(),
                    predicate_declaration_spans: spans,
                }));
            }

            Error::Abrupt(predicate, error) => match error {
                pernixc_type_system::Error::Overflow(overflow_error) => {
                    let spans =
                        active_premise_with_span.get(&predicate).unwrap();

                    for span in spans {
                        handler.receive(Box::new(TypeCalculatingOverflow {
                            overflow_span: span.clone(),
                            overflow_error,
                        }));
                    }
                }
                pernixc_type_system::Error::Abort(_) => {}
            },
        }
    }
}

#[cfg(test)]
mod test;
