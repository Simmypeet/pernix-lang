use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{positive_trait_implementation, Finalize};
use crate::{
    arena::ID,
    error::{self},
    symbol::{
        table::{
            representation::{
                building::finalizing::{occurrences::Occurrences, Finalizer},
                Index, RwLockContainer,
            },
            Building, Table,
        },
        Trait, TraitImplementationID,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The definition of the trait is built
pub const DEFINITION_STATE: usize = 3;

/// All the non-final trait implementation are built to completion
pub const NON_FINAL_IMPLEMENTATION_STATE: usize = 1;

/// All the final trait implementation are built to completion
pub const FINAL_IMPLEMENTATION_STATE: usize = 2;

/// The information required to check the bounds is built. (the definition of
/// where caluses are built)
pub const WELL_FORMED_STATE: usize = 4;

/// Bounds check are performed
pub const CHECK_STATE: usize = 5;

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

            DEFINITION_STATE => {
                table.create_where_clause_predicates_for_well_formed(
                    symbol_id,
                    syntax_tree.where_clause().as_ref(),
                    where_clause_occurrences,
                    handler,
                );
            }

            FINAL_IMPLEMENTATION_STATE => {
                let final_implementations = table
                    .get(symbol_id)
                    .unwrap()
                    .implementations
                    .iter()
                    .copied()
                    .filter(|id| match id {
                        TraitImplementationID::Positive(id) => {
                            table.get(*id).unwrap().is_final
                        }
                        TraitImplementationID::Negative(id) => {
                            table.get(*id).unwrap().is_final
                        }
                    })
                    .collect::<Vec<_>>();

                for implementation_id in final_implementations {
                    let _ = match implementation_id {
                        TraitImplementationID::Positive(id) => table.build_to(
                            id,
                            Some(symbol_id.into()),
                            positive_trait_implementation::ARGUMENTS_STATE,
                            handler,
                        ),
                        TraitImplementationID::Negative(id) => table.build_to(
                            id,
                            Some(symbol_id.into()),
                            positive_trait_implementation::ARGUMENTS_STATE,
                            handler,
                        ),
                    };
                }
            }

            NON_FINAL_IMPLEMENTATION_STATE => {
                let non_final_implementations = table
                    .get(symbol_id)
                    .unwrap()
                    .implementations
                    .iter()
                    .copied()
                    .filter(|id| match id {
                        TraitImplementationID::Positive(id) => {
                            !table.get(*id).unwrap().is_final
                        }
                        TraitImplementationID::Negative(id) => {
                            !table.get(*id).unwrap().is_final
                        }
                    })
                    .collect::<Vec<_>>();

                for implementation_id in non_final_implementations {
                    let _ = match implementation_id {
                        TraitImplementationID::Positive(id) => table.build_to(
                            id,
                            Some(symbol_id.into()),
                            positive_trait_implementation::WELL_FORMED_STATE,
                            handler,
                        ),
                        TraitImplementationID::Negative(id) => table.build_to(
                            id,
                            Some(symbol_id.into()),
                            positive_trait_implementation::WELL_FORMED_STATE,
                            handler,
                        ),
                    };
                }
            }

            WELL_FORMED_STATE => table
                .create_where_clause_predicates_for_well_formed(
                    symbol_id,
                    syntax_tree.where_clause().as_ref(),
                    where_clause_occurrences,
                    handler,
                ),

            CHECK_STATE => {}

            _ => panic!("invalid state flag"),
        }
        // match state_flag {
        //     GENERIC_PARAMETER_STATE => {
        //         table.create_generic_parameters(
        //             symbol_id,
        //             syntax_tree.generic_parameters().as_ref(),
        //             data,
        //             handler,
        //         );
        //     }

        //     WHERE_CLAUSE_STATE => {
        //         table.create_where_clause_predicates(
        //             symbol_id,
        //             syntax_tree.where_clause().as_ref(),
        //             data,
        //             handler,
        //         );
        //     }

        //     FINAL_IMPLEMENTATION_STATE => {}

        //     NON_FINAL_IMPLEMENTATION_STATE => {
        //         let implementations = table
        //             .get(symbol_id)
        //             .unwrap()
        //             .implementations
        //             .iter()
        //             .copied()
        //             .map(GlobalID::from)
        //             .collect::<Vec<_>>();

        //         for implementation_id in implementations {
        //             let _ = table.build_preset::<builder::Complete>(
        //                 implementation_id,
        //                 Some(symbol_id.into()),
        //                 handler,
        //             );
        //         }

        //         data.build_all_occurrences_to::<builder::Complete>(
        //             table,
        //             symbol_id.into(),
        //             handler,
        //         );
        //     }

        //     CHECK_STATE => {
        //         table.check_occurrences(symbol_id.into(), data, handler);
        //         table.check_where_clause(symbol_id.into(), handler);

        //         // check for the ambiguous implementations
        //         let implementation_ids = {
        //             let table_sym = table.get(symbol_id).unwrap();

        //             table_sym
        //                 .implementations
        //                 .iter()
        //                 .copied()
        //                 .collect::<Vec<_>>()
        //         };

        //         let trait_sym = table.get(symbol_id).unwrap();
        //         let default_environment =
        //             Environment::new(Premise::default(), table, &NO_OP).0;

        //         for (i, lhs) in
        // implementation_ids.iter().copied().enumerate() {
        // let (lhs_arguments, lhs_is_final) = match lhs {
        // TraitImplementationID::Positive(id) => {
        // let is_final = table.get(id).unwrap().is_final;
        // (                         RwLockReadGuard::map(
        //                             table.get(id).unwrap(),
        //                             |x| &x.arguments,
        //                         ),
        //                         is_final,
        //                     )
        //                 }
        //                 TraitImplementationID::Negative(id) => {
        //                     let is_final = table.get(id).unwrap().is_final;
        //                     (
        //                         RwLockReadGuard::map(
        //                             table.get(id).unwrap(),
        //                             |x| &x.arguments,
        //                         ),
        //                         is_final,
        //                     )
        //                 }
        //             };

        //             // arity check
        //             if lhs_arguments.lifetimes.len()
        //                 != trait_sym
        //                     .generic_declaration
        //                     .parameters
        //                     .lifetimes()
        //                     .len()
        //                 || lhs_arguments.types.len()
        //                     != trait_sym
        //                         .generic_declaration
        //                         .parameters
        //                         .types()
        //                         .len()
        //                 || lhs_arguments.constants.len()
        //                     != trait_sym
        //                         .generic_declaration
        //                         .parameters
        //                         .constants()
        //                         .len()
        //             {
        //                 continue;
        //             }

        //             for rhs in implementation_ids.iter().copied().skip(i + 1)
        // {                 let (rhs_arguments, rhs_is_final) = match
        // rhs {                     TraitImplementationID::Positive(id)
        // => {                         let is_final =
        // table.get(id).unwrap().is_final;                         (
        //                             RwLockReadGuard::map(
        //                                 table.get(id).unwrap(),
        //                                 |x| &x.arguments,
        //                             ),
        //                             is_final,
        //                         )
        //                     }
        //                     TraitImplementationID::Negative(id) => {
        //                         let is_final =
        // table.get(id).unwrap().is_final;                         (
        //                             RwLockReadGuard::map(
        //                                 table.get(id).unwrap(),
        //                                 |x| &x.arguments,
        //                             ),
        //                             is_final,
        //                         )
        //                     }
        //                 };

        //                 if rhs_arguments.lifetimes.len()
        //                     != trait_sym
        //                         .generic_declaration
        //                         .parameters
        //                         .lifetimes()
        //                         .len()
        //                     || rhs_arguments.types.len()
        //                         != trait_sym
        //                             .generic_declaration
        //                             .parameters
        //                             .types()
        //                             .len()
        //                     || rhs_arguments.constants.len()
        //                         != trait_sym
        //                             .generic_declaration
        //                             .parameters
        //                             .constants()
        //                             .len()
        //                 {
        //                     continue;
        //                 }

        //                 let Ok(result) = lhs_arguments
        //                     .order(&rhs_arguments, &default_environment)
        //                 else {
        //                     panic!("shouldn't be overflowing");
        //                 };

        //                 drop(rhs_arguments);

        //                 // ambiguous trait implementation error
        //                 if result == order::Order::Ambiguous {
        //                     handler.receive(Box::new(
        //                         AmbiguousImplementation {
        //                             first_implementation_id: lhs,
        //                             second_implementation_id: rhs,
        //                         },
        //                     ));
        //                 }

        //                 // final cannot be more general
        //                 if lhs_is_final && result ==
        // order::Order::MoreGeneral {
        // handler.receive(Box::new(
        // FinalImplementationCannotBeOverriden {
        // final_implementation_id: lhs,
        // overriden_implementation_id: rhs,                         },
        //                     ))
        //                 }

        //                 if rhs_is_final && result ==
        // order::Order::MoreSpecific                 {
        //                     handler.receive(Box::new(
        //                         FinalImplementationCannotBeOverriden {
        //                             final_implementation_id: rhs,
        //                             overriden_implementation_id: lhs,
        //                         },
        //                     ))
        //                 }
        //             }
        //         }
        //     }
        //     _ => panic!("invalid state flag"),
        // }
    }
}
