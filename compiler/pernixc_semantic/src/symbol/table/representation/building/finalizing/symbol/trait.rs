use parking_lot::RwLockReadGuard;
use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree;

use crate::{
    arena::ID,
    error::{
        self, AmbiguousImplementation, FinalImplementationCannotBeOverriden,
    },
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    state::Finalize,
                    utility::{builder, occurrences::Occurrences},
                    Finalizer,
                },
                Index, RwLockContainer,
            },
            Building, Table,
        },
        Trait, TraitImplementationID,
    },
    type_system::{environment::Environment, normalizer, order, Premise},
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The where clause of the trait is built.
pub const WHERE_CLAUSE_STATE: usize = 1;

/// Bounds check are performed
pub const CHECK_STATE: usize = 2;

impl Finalize for Trait {
    type SyntaxTree = syntax_tree::item::TraitSignature;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = (Occurrences, Occurrences);

    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    fn finalize(
        table: &Table<Building<RwLockContainer, Finalizer>>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        (
            generic_parameter_occurrences,
            where_clause_occurrences,
        ): &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            GENERIC_PARAMETER_STATE => {
                table.create_generic_parameters(
                    symbol_id,
                    syntax_tree.generic_parameters().as_ref(),
                    generic_parameter_occurrences,
                    handler,
                );
            }

            WHERE_CLAUSE_STATE => {
                table.create_where_clause(
                    symbol_id,
                    syntax_tree.where_clause().as_ref(),
                    where_clause_occurrences,
                    handler,
                );
            }

            CHECK_STATE => {
                table.check_occurrences(
                    symbol_id.into(),
                    &generic_parameter_occurrences,
                    handler,
                );
                table.check_occurrences(
                    symbol_id.into(),
                    &where_clause_occurrences,
                    handler,
                );

                table.check_where_clause(symbol_id.into(), handler);

                // check for the ambiguous implementations
                let implementation_ids = {
                    let table_sym = table.get(symbol_id).unwrap();

                    table_sym
                        .implementations
                        .iter()
                        .copied()
                        .collect::<Vec<_>>()
                };

                let trait_sym = table.get(symbol_id).unwrap();
                let definition =
                    builder::TypeSystem::new(symbol_id.into(), handler);

                let default_environment = Environment::new_with(
                    Premise::default(),
                    table,
                    normalizer::NO_OP,
                    &definition,
                )
                .0;

                for (i, lhs) in implementation_ids.iter().copied().enumerate() {
                    let (lhs_arguments, lhs_is_final) = match lhs {
                        TraitImplementationID::Positive(id) => {
                            let is_final = table.get(id).unwrap().is_final;
                            (
                                RwLockReadGuard::map(
                                    table.get(id).unwrap(),
                                    |x| &x.arguments,
                                ),
                                is_final,
                            )
                        }
                        TraitImplementationID::Negative(id) => {
                            let is_final = table.get(id).unwrap().is_final;
                            (
                                RwLockReadGuard::map(
                                    table.get(id).unwrap(),
                                    |x| &x.arguments,
                                ),
                                is_final,
                            )
                        }
                    };

                    // arity check
                    if lhs_arguments.lifetimes.len()
                        != trait_sym
                            .generic_declaration
                            .parameters
                            .lifetimes()
                            .len()
                        || lhs_arguments.types.len()
                            != trait_sym
                                .generic_declaration
                                .parameters
                                .types()
                                .len()
                        || lhs_arguments.constants.len()
                            != trait_sym
                                .generic_declaration
                                .parameters
                                .constants()
                                .len()
                    {
                        continue;
                    }

                    for rhs in implementation_ids.iter().copied().skip(i + 1) {
                        let (rhs_arguments, rhs_is_final) = match rhs {
                            TraitImplementationID::Positive(id) => {
                                let is_final = table.get(id).unwrap().is_final;
                                (
                                    RwLockReadGuard::map(
                                        table.get(id).unwrap(),
                                        |x| &x.arguments,
                                    ),
                                    is_final,
                                )
                            }
                            TraitImplementationID::Negative(id) => {
                                let is_final = table.get(id).unwrap().is_final;
                                (
                                    RwLockReadGuard::map(
                                        table.get(id).unwrap(),
                                        |x| &x.arguments,
                                    ),
                                    is_final,
                                )
                            }
                        };

                        if rhs_arguments.lifetimes.len()
                            != trait_sym
                                .generic_declaration
                                .parameters
                                .lifetimes()
                                .len()
                            || rhs_arguments.types.len()
                                != trait_sym
                                    .generic_declaration
                                    .parameters
                                    .types()
                                    .len()
                            || rhs_arguments.constants.len()
                                != trait_sym
                                    .generic_declaration
                                    .parameters
                                    .constants()
                                    .len()
                        {
                            continue;
                        }

                        let Ok(result) = lhs_arguments
                            .order(&rhs_arguments, &default_environment)
                        else {
                            panic!("shouldn't be overflowing");
                        };

                        drop(rhs_arguments);

                        // ambiguous trait implementation error
                        if result == order::Order::Ambiguous {
                            handler.receive(Box::new(
                                AmbiguousImplementation {
                                    first_implementation_id: lhs,
                                    second_implementation_id: rhs,
                                },
                            ));
                        }

                        // final cannot be more general
                        if lhs_is_final && result == order::Order::MoreGeneral {
                            handler.receive(Box::new(
                                FinalImplementationCannotBeOverriden {
                                    final_implementation_id: lhs,
                                    overriden_implementation_id: rhs,
                                },
                            ))
                        }

                        if rhs_is_final && result == order::Order::MoreSpecific
                        {
                            handler.receive(Box::new(
                                FinalImplementationCannotBeOverriden {
                                    final_implementation_id: rhs,
                                    overriden_implementation_id: lhs,
                                },
                            ))
                        }
                    }
                }
            }

            _ => panic!("invalid state flag"),
        }
    }
}
