use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{r#trait, Finalize};
use crate::{
    arena::ID,
    error,
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
        GlobalID, PositiveTraitImplementation,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// Where cluase predicates are built
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The generic arguments of the implementation are built.
pub const GENERIC_ARGUMENTS_STATE: usize = 2;

/// The complete information of the constant is built.
pub const COMPLETE_STATE: usize = 3;

/// Bounds check are performed
pub const CHECK_STATE: usize = 4;

impl Finalize for PositiveTraitImplementation {
    type SyntaxTree = syntax_tree::item::ImplementationSignature;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = Occurrences;

    #[allow(clippy::too_many_lines)]
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

            WHERE_CLAUSE_STATE => table.create_where_clause_predicates(
                symbol_id,
                syntax_tree.where_clause().as_ref(),
                data,
                handler,
            ),

            GENERIC_ARGUMENTS_STATE => {
                let parent_trait_id =
                    table.get(symbol_id).unwrap().implemented_id;

                let generic_identifier = syntax_tree
                    .qualified_identifier()
                    .rest()
                    .last()
                    .map_or_else(
                        || syntax_tree.qualified_identifier().first(),
                        |(_, ident)| ident,
                    );

                table
                    .positive_trait_implementations
                    .get(symbol_id)
                    .unwrap()
                    .write()
                    .arguments = table.create_implementation_arguments(
                    symbol_id.into(),
                    parent_trait_id,
                    r#trait::GENERIC_PARAMETER_STATE,
                    generic_identifier,
                    data,
                    handler,
                );
            }

            COMPLETE_STATE => {
                // build all the trait member
                let member_ids = table
                    .get(symbol_id)
                    .unwrap()
                    .member_ids_by_name
                    .values()
                    .copied()
                    .map(GlobalID::from)
                    .collect::<Vec<_>>();

                for member_id in member_ids {
                    let _ = table.build_preset::<build_preset::Complete>(
                        member_id,
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
                // make sure the implemented trait has the where clause
                let parent_trait_id =
                    table.get(symbol_id).unwrap().implemented_id;
                let _ = table.build_to(
                    parent_trait_id,
                    Some(symbol_id.into()),
                    r#trait::WHERE_CLAUSE_STATE,
                    true,
                    handler,
                );

                table.check_occurrences(symbol_id.into(), data, handler);
                table.implementation_signature_check(symbol_id, handler);
            }

            _ => panic!("invalid state flag"),
        }
    }
}
