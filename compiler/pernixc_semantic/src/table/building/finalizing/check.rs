use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree;

use super::{finalize::Occurrences, Finalizer};
use crate::{
    error::{self, UndecidablePredicate},
    semantic::{
        self, equality,
        instantiation::Instantiation,
        predicate::{self, ConstantType, Outlives, Trait, Tuple},
        session::{self, Limit},
        term::{self, GenericArguments},
        Premise,
    },
    symbol::{GenericID, GlobalID},
    table::Table,
};

impl Table<Finalizer> {
    #[allow(clippy::too_many_arguments)]
    fn check_where_clause_predicates(
        &self,
        caller: GlobalID,
        caller_premise: &Premise,
        instantiated: GenericID,
        generic_arguments: GenericArguments,
        instantiation_span: &Span,
        semantic: &mut semantic::Default,
        session: &mut session::Default,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        // make sure that the given instantiated has built the where clause
        // predicates
        let _ = self.build_where_clause(instantiated, caller, handler);

        // get all the predicates and instantiate them with the given generic
        // arguments
        let instantiated_sym = self.get_generic(instantiated).unwrap();
        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments,
            instantiated,
            &instantiated_sym.generic_declaration().parameters,
        )
        .unwrap();

        #[allow(clippy::significant_drop_in_scrutinee)]
        for predicate_info in &instantiated_sym.generic_declaration().predicates
        {
            let mut predicate = predicate_info.predicate.clone();
            predicate.instantiate(&instantiation);

            // check if the predicate is satisfied
            let undecidiable = match predicate {
                predicate::Predicate::TypeEquality(eq) => equality::equals(
                    &eq.lhs,
                    &eq.rhs,
                    caller_premise,
                    self,
                    semantic,
                    &mut Limit::new(session),
                )
                .map_or(true, |result| false),
                predicate::Predicate::ConstantEquality(eq) => equality::equals(
                    &eq.lhs,
                    &eq.rhs,
                    caller_premise,
                    self,
                    semantic,
                    &mut Limit::new(session),
                )
                .map_or(true, |result| false),
                predicate::Predicate::ConstantType(pred) => {
                    ConstantType::satisfies(
                        &pred.0,
                        caller_premise,
                        self,
                        semantic,
                        &mut Limit::new(session),
                    )
                    .map_or(true, |result| false)
                }
                predicate::Predicate::LifetimeOutlives(pred) => {
                    Outlives::satisfies(
                        &pred.operand,
                        &pred.bound,
                        caller_premise,
                        self,
                        semantic,
                        &mut Limit::new(session),
                    )
                    .map_or(true, |result| false)
                }
                predicate::Predicate::TypeOutlives(pred) => {
                    Outlives::satisfies(
                        &pred.operand,
                        &pred.bound,
                        caller_premise,
                        self,
                        semantic,
                        &mut Limit::new(session),
                    )
                    .map_or(true, |result| false)
                }
                predicate::Predicate::TupleType(pred) => Tuple::satisfies(
                    &pred.0,
                    caller_premise,
                    self,
                    semantic,
                    &mut Limit::new(session),
                )
                .map_or(true, |result| false),
                predicate::Predicate::TupleConstant(pred) => Tuple::satisfies(
                    &pred.0,
                    caller_premise,
                    self,
                    semantic,
                    &mut Limit::new(session),
                )
                .map_or(true, |result| false),
                predicate::Predicate::Trait(pred) => Trait::satisfies(
                    pred.id,
                    pred.is_const,
                    &pred.generic_arguments,
                    caller_premise,
                    self,
                    semantic,
                    &mut Limit::new(session),
                )
                .map_or(true, |result| false),
            };

            if undecidiable {
                handler.receive(Box::new(UndecidablePredicate {
                    instantiation_span: instantiation_span.clone(),
                    predicate: predicate_info.clone(),
                }));
            }
        }
    }

    fn check_type_ocurrences<'a>(
        &self,
        caller: GlobalID,
        caller_active_premise: &Premise,
        occurrences: impl IntoIterator<
            Item = (&'a term::r#type::Type, &'a syntax_tree::r#type::Type),
        >,
        semantic: &mut semantic::Default,
        session: &mut session::Default,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        for (ty, syn) in occurrences {
            match ty {
                term::r#type::Type::Primitive(_)
                | term::r#type::Type::Parameter(_)
                | term::r#type::Type::Inference(_) => {
                    /* no additional check */
                }

                term::r#type::Type::Symbol(term::Symbol {
                    id,
                    generic_arguments,
                }) => {
                    self.check_where_clause_predicates(
                        caller,
                        caller_active_premise,
                        (*id).into(),
                        generic_arguments.clone(),
                        &syn.span(),
                        semantic,
                        session,
                        handler,
                    );
                }

                term::r#type::Type::Pointer(_) => todo!(),
                term::r#type::Type::Reference(_) => todo!(),
                term::r#type::Type::Array(_) => todo!(),
                term::r#type::Type::Tuple(_) => todo!(),
                term::r#type::Type::Local(_) => todo!(),
                term::r#type::Type::MemberSymbol(_) => todo!(),
                term::r#type::Type::TraitMember(_) => todo!(),
            }
        }
    }

    /// Checks if the occurrences of symbols are valid (i.e. they satisfy the
    /// where clause predicates).
    pub fn check_occurrences(
        &self,
        id: GlobalID,
        occurrences: &Occurrences,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let active_premise = self.get_active_premise(id).unwrap();
        let mut semantic = semantic::Default;
        let mut session = session::Default::default();

        #[allow(clippy::map_identity)]
        self.check_type_ocurrences(
            id,
            &active_premise,
            occurrences.types.iter().map(|(ty, syn)| (ty, syn)),
            &mut semantic,
            &mut session,
            handler,
        );
    }
}
