use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::{self, item, QualifiedIdentifier};
use pernixc_system::{arena, diagnostic::Handler};
use proptest::sample::Subsequence;
use table::Error;

use super::{
    drafting::{States, SymbolState},
    Table,
};
use crate::{
    error::{self, CyclicDependency, LifetimeArgumentMustBeSuppliedPriorToTypeArgument},
    table, GenericableID, GlobalID, Substitution, Trait, WhereClause, ID,
};

impl States {
    fn mark_as_constructing(&mut self, id: ID) -> Result<(), CyclicDependency> {
        let mut symbol_state = self.symbol_states_by_id.get_mut(&id).expect("invalid ID");

        match symbol_state {
            // mark the symbol as constructing
            SymbolState::Drafted => {
                *symbol_state = SymbolState::Constructing {
                    constructing_order: self.current_constructing_order,
                };

                self.current_constructing_order += 1;
                Ok(())
            }
            SymbolState::Constructing { constructing_order } => {
                let constructing_order = *constructing_order;
                // found cyclic dependency

                // any symbols that are marked as constructing after the `constructing_order` are
                // considered as participating in the cyclic dependency

                let cyclic_symbols = self
                    .symbol_states_by_id
                    .iter()
                    .filter_map(|(k, v)| match v {
                        SymbolState::Drafted => None,
                        SymbolState::Constructing {
                            constructing_order: comparing_constructing_order,
                        } => {
                            if *comparing_constructing_order >= constructing_order {
                                Some(*k)
                            } else {
                                None
                            }
                        }
                    })
                    .collect::<Vec<_>>();

                // remove all symbols that are participating in the cyclic dependency
                for symbol in &cyclic_symbols {
                    assert!(self.symbol_states_by_id.remove(symbol).is_some());
                }

                if cyclic_symbols.is_empty() {
                    Ok(())
                } else {
                    Err(CyclicDependency {
                        participants: cyclic_symbols,
                    })
                }
            }
        }
    }
}

impl Table {
    pub(super) fn finalize_symbols(
        &mut self,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        while !states.symbol_states_by_id.is_empty() {
            let id = states
                .symbol_states_by_id
                .iter()
                .find_map(|(id, symbol_state)| {
                    if *symbol_state == SymbolState::Drafted {
                        Some(*id)
                    } else {
                        None
                    }
                })
                .unwrap();
        }
    }

    fn finalize_symbol(
        &mut self,
        id: ID,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        match id {
            ID::Struct(i) => todo!(),
            ID::Function(i) => todo!(),
            ID::Type(i) => todo!(),
            ID::Trait(i) => self.finalize_trait(i, states, handler),
            ID::TraitFunction(i) => todo!(),
            ID::TraitType(i) => todo!(),
            ID::Field(i) => todo!(),
            ID::FunctionParameter(i) => todo!(),
            ID::Implements(i) => todo!(),
            ID::ImplementsType(i) => todo!(),
            ID::ImplementsFunction(i) => todo!(),
            ID::ImplementsFunctionParameter(i) => todo!(),
            ID::TraitFunctionParameter(i) => todo!(),

            ID::Module(..)
            | ID::Enum(..)
            | ID::EnumVariant(..)
            | ID::TypeParameter(..)
            | ID::LifetimeParameter(..) => unreachable!(),
        }
    }

    fn finalize_where_clause(
        &mut self,
        where_clause_syntax_tree: &item::WhereClause,
        current_id: ID,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> WhereClause {
        let mut where_clause = WhereClause::default();

        // populate lifetime bounds
        for element in where_clause_syntax_tree.constraint_list.elements() {
            let item::Constraint::LifetimeBound(lifetime_bound) = element else {
                continue;
            };

            let Ok(lifetime_parameter) = self.resolve_lifetime_parameter(
                current_id,
                &lifetime_bound.operand.identifier,
                handler,
            ) else {
                continue;
            };

            let lifetime_argument_sets = where_clause
                .lifetime_argument_sets_by_lifetime_parameter
                .entry(lifetime_parameter)
                .or_default();

            for argument in lifetime_bound.arguments.elements() {
                let Ok(lifetime_argument) =
                    self.resolve_lifetime_argument(current_id, argument, handler)
                else {
                    continue;
                };

                lifetime_argument_sets.insert(lifetime_argument);
            }
        }

        where_clause
    }

    fn check_generic_parameter(
        &mut self,
        current_id: ID,
        genericable_id: GenericableID,
        generic_arguments: &syntax_tree::GenericArguments,
        explicit_lifetime_required: bool,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<Substitution, Error> {
        let mut lifetime_arguments = Vec::new();
        let mut type_arguments = Vec::new();
        let substitution = Substitution::default();

        // extract lifetime argument and type argument
        for generic_argument in generic_arguments.argument_list.elements() {
            match generic_argument {
                syntax_tree::GenericArgument::TypeSpecifier(type_specifier) => {
                    type_arguments.push(type_specifier)
                }
                syntax_tree::GenericArgument::Lifetime(lifetime_argument) => {
                    if !type_arguments.is_empty() {
                        handler.recieve(
                            error::Error::LifetimeArgumentMustBeSuppliedPriorToTypeArgument(
                                LifetimeArgumentMustBeSuppliedPriorToTypeArgument {
                                    lifetime_argument_span: lifetime_argument.span()?,
                                },
                            ),
                        );
                        return Err(Error::FatalSemantic);
                    }

                    lifetime_arguments.push(lifetime_argument)
                }
            }
        }

        match (explicit_lifetime_required, lifetime_arguments.len()) {
            (false, 0) => {}

            // lifetime argument lenth mismatches
            (_, len)
                if len
                    != self
                        .get_genericable(genericable_id)?
                        .generic_parameters()
                        .lifetime_parameter_order
                        .len() => {}
        }

        Ok(substitution)
    }

    fn resolve_symbol_with_finalization_internal(
        &mut self,
        current_id: ID,
        qualified_identifier: &QualifiedIdentifier,
        explicit_lifetime_required: bool,
        states: &mut States,
        handler: &impl Handler<Error>,
    ) -> Result<(GlobalID, Substitution), Error> {
        let root_global_id = if qualified_identifier.leading_scope_separator.is_some() {
        } else {
        };
        todo!()
    }

    fn finalize_trait(
        &mut self,
        trait_id: arena::ID<Trait>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        if let Err(err) = states.mark_as_constructing(trait_id.into()) {
            handler.recieve(error::Error::CyclicDependency(err));
            return;
        }

        todo!()
    }
}
