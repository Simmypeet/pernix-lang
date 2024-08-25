use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::Finalize;
use crate::{
    arena::ID,
    error,
    symbol::{
        table::{
            representation::{
                building::finalizing::{occurrences::Occurrences, Finalizer},
                RwLockContainer,
            },
            Building, Table,
        },
        NegativeTraitImplementation,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The generic arguments of the implementation are built.
pub const DEFINITION_STATE: usize = 1;

/// The information required to check the bounds is built. (the definition of
/// where caluses are built)
#[allow(unused)]
pub const WELL_FORMED_STATE: usize = 2;

/// Bounds check are performed
pub const CHECK_STATE: usize = 3;

impl Finalize for NegativeTraitImplementation {
    type SyntaxTree = syntax_tree::item::ImplementationSignature;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = Occurrences;

    fn finalize(
        _table: &Table<Building<RwLockContainer, Finalizer>>,
        _symbol_id: ID<Self>,
        _state_flag: usize,
        _syntax_tree: &Self::SyntaxTree,
        _data: &mut Self::Data,
        _handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
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

        //     DEFINITION_STATE => {
        //         let parent_trait_id =
        //             table.get(symbol_id).unwrap().implemented_id;

        //         let generic_identifier = syntax_tree
        //             .qualified_identifier()
        //             .rest()
        //             .last()
        //             .map_or_else(
        //                 || syntax_tree.qualified_identifier().first(),
        //                 |(_, ident)| ident,
        //             );

        //         table
        //             .negative_trait_implementations
        //             .get(symbol_id)
        //             .unwrap()
        //             .write()
        //             .arguments = table.create_implementation_arguments(
        //             symbol_id.into(),
        //             parent_trait_id,
        //             r#trait::GENERIC_PARAMETER_STATE,
        //             generic_identifier,
        //             data,
        //             handler,
        //         );
        //     }

        //     NON_FINAL_IMPLEMENTATION_STATE => {}

        //     CHECK_STATE => {
        //         data.build_all_occurrences_to::<builder::Complete>(
        //             table,
        //             symbol_id.into(),
        //             handler,
        //         );

        //         table.check_occurrences(symbol_id.into(), data, handler);
        //         table.check_where_clause(symbol_id.into(), handler);
        //         table.implementation_signature_check(symbol_id, handler);
        //     }

        //     _ => panic!("invalid state flag"),
        // }
    }
}
