use parking_lot::RwLockReadGuard;
use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::Finalize;
use crate::{
    arena::ID,
    error::{
        self, AmbiguousImplementation, UndecidableImplementationSpecialization,
    },
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    finalizer::build_preset, occurrences::Occurrences,
                    Finalizer,
                },
                Index, RwLockContainer,
            },
            Building, Table,
        },
        GlobalID, Trait, TraitImplementationID,
    },
    type_system::{environment::Environment, normalizer::NO_OP, order},
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// Where cluase predicates are built
pub const WHERE_CLAUSE_STATE: usize = 1;

/// All the trait members are built to completion.
pub const MEMBERS_STATE: usize = 2;

/// All the trait implementations are built to completion.
pub const IMPLEMENTATIONS_STATE: usize = 3;

/// Bounds check are performed
pub const CHECK_STATE: usize = 4;

impl Finalize for Trait {
    type SyntaxTree = syntax_tree::item::TraitSignature;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = Occurrences;

    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    fn finalize(
        table: &Table<Building<RwLockContainer, Finalizer>>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        data: &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            GENERIC_PARAMETER_STATE => {
                table.create_generic_parameters(
                    symbol_id,
                    syntax_tree.generic_parameters().as_ref(),
                    data,
                    handler,
                );
            }
            WHERE_CLAUSE_STATE => {
                table.create_where_clause_predicates(
                    symbol_id,
                    syntax_tree.where_clause().as_ref(),
                    data,
                    handler,
                );
            }
            MEMBERS_STATE => {
                let trait_members = table
                    .get(symbol_id)
                    .unwrap()
                    .member_ids_by_name
                    .values()
                    .copied()
                    .collect::<Vec<_>>();

                for member_id in trait_members {
                    let _ = table.build_preset::<build_preset::Complete>(
                        member_id.into(),
                        Some(symbol_id.into()),
                        false,
                        handler,
                    );
                }
            }
            IMPLEMENTATIONS_STATE => {
                let implementations = table
                    .get(symbol_id)
                    .unwrap()
                    .implementations
                    .iter()
                    .copied()
                    .map(GlobalID::from)
                    .collect::<Vec<_>>();

                for implementation_id in implementations {
                    let _ = table.build_preset::<build_preset::Complete>(
                        implementation_id,
                        Some(symbol_id.into()),
                        false,
                        handler,
                    );
                }

                data.build_all_occurrences_to::<build_preset::Complete>(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );
            }
            CHECK_STATE => {
                table.check_occurrences(symbol_id.into(), data, handler);

                // check for the ambiguous implementations
                let implementation_ids = {
                    let table_sym = table.get(symbol_id).unwrap();

                    table_sym
                        .implementations
                        .iter()
                        .copied()
                        .collect::<Vec<_>>()
                };

                let (environment, _) = Environment::new(
                    table.get_active_premise(symbol_id.into()).unwrap(),
                    table,
                    &NO_OP,
                );

                let trait_sym = table.get(symbol_id).unwrap();

                for (i, lhs) in implementation_ids.iter().copied().enumerate() {
                    let lhs_arguments = match lhs {
                        TraitImplementationID::Positive(id) => {
                            RwLockReadGuard::map(table.get(id).unwrap(), |x| {
                                &x.arguments
                            })
                        }
                        TraitImplementationID::Negative(id) => {
                            RwLockReadGuard::map(table.get(id).unwrap(), |x| {
                                &x.arguments
                            })
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
                        let rhs_arguments = match rhs {
                            TraitImplementationID::Positive(id) => {
                                RwLockReadGuard::map(
                                    table.get(id).unwrap(),
                                    |x| &x.arguments,
                                )
                            }
                            TraitImplementationID::Negative(id) => {
                                RwLockReadGuard::map(
                                    table.get(id).unwrap(),
                                    |x| &x.arguments,
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

                        let Ok(result) =
                            lhs_arguments.order(&rhs_arguments, &environment)
                        else {
                            handler.receive(Box::new(
                                UndecidableImplementationSpecialization {
                                    first_implementation_id: lhs,
                                    second_implementation_id: rhs,
                                },
                            ));
                            continue;
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
                    }
                }
            }
            _ => panic!("invalid state flag"),
        }
    }
}
