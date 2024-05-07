use core::panic;

use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{r#trait, Finalize};
use crate::{
    arena::ID,
    error::Error,
    symbol::TraitType,
    table::{
        building::finalizing::{
            finalizer::build_preset, occurrences::Occurrences, Finalizer,
        },
        Index, Table,
    },
};
/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// Where cluase predicates are built
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The trait type alias signature is built.
pub const SIGNATURE_STATE: usize = 2;

/// Bounds check are performed
pub const CHECK_STATE: usize = 3;

impl Finalize for TraitType {
    type SyntaxTree = syntax_tree::item::TraitType;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = Occurrences;

    fn finalize(
        table: &Table<Finalizer>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        data: &mut Self::Data,
        handler: &dyn Handler<Box<dyn Error>>,
    ) {
        match state_flag {
            GENERIC_PARAMETER_STATE => {
                // make sure the trait's geneirc parameters are built
                let parent_trait_id = table.get(symbol_id).unwrap().parent_id;
                let _ = table.build_to(
                    parent_trait_id,
                    Some(symbol_id.into()),
                    r#trait::GENERIC_PARAMETER_STATE,
                    true,
                    handler,
                );

                table.create_generic_parameters(
                    symbol_id,
                    syntax_tree.signature().generic_parameters().as_ref(),
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

            SIGNATURE_STATE => {
                data.build_all_occurrences_to::<build_preset::Complete>(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );
            }

            CHECK_STATE => {
                // make sure the trait where clause predicates are built
                let parent_trait_id = table.get(symbol_id).unwrap().parent_id;
                let _ = table.build_to(
                    parent_trait_id,
                    Some(symbol_id.into()),
                    r#trait::WHERE_CLAUSE_STATE,
                    true,
                    handler,
                );

                table.check_occurrences(symbol_id.into(), data, handler);
            }

            _ => panic!("invalid state flag"),
        }
    }
}
