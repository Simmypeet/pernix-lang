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
                RwLockContainer, Table,
            },
            Building,
        },
        AdtImplementation,
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

impl Finalize for AdtImplementation {
    type SyntaxTree = syntax_tree::item::ImplementationSignature;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = Occurrences;

    fn finalize(
        _ctable: &Table<Building<RwLockContainer, Finalizer>>,
        _csymbol_id: ID<Self>,
        _cstate_flag: usize,
        _csyntax_tree: &Self::SyntaxTree,
        _cdata: &mut Self::Data,
        _chandler: &dyn Handler<Box<dyn error::Error>>,
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
        //         let parent_adt_id =
        //             table.get(symbol_id).unwrap().implemented_id;

        //         let generic_identifier = syntax_tree
        //             .qualified_identifier()
        //             .rest()
        //             .last()
        //             .map_or_else(
        //                 || syntax_tree.qualified_identifier().first(),
        //                 |(_, ident)| ident,
        //             );

        //         match parent_adt_id {
        //             AdtID::Struct(struct_id) => {
        //                 table
        //                     .adt_implementations
        //                     .get(symbol_id)
        //                     .unwrap()
        //                     .write()
        //                     .arguments =
        // table.create_implementation_arguments(
        // symbol_id.into(),                     struct_id,
        //                     r#struct::GENERIC_PARAMETER_STATE,
        //                     generic_identifier,
        //                     data,
        //                     handler,
        //                 );
        //             }
        //             AdtID::Enum(enum_id) => {
        //                 table
        //                     .adt_implementations
        //                     .get(symbol_id)
        //                     .unwrap()
        //                     .write()
        //                     .arguments =
        // table.create_implementation_arguments(
        // symbol_id.into(),                     enum_id,
        //                     r#enum::GENERIC_PARAMETER_STATE,
        //                     generic_identifier,
        //                     data,
        //                     handler,
        //                 );
        //             }
        //         }
        //     }

        //     COMPLETE_STATE => {
        //         // build all the implementation members
        //         let member_ids = table
        //             .get(symbol_id)
        //             .unwrap()
        //             .member_ids_by_name
        //             .values()
        //             .copied()
        //             .map(GlobalID::from)
        //             .collect::<Vec<_>>();

        //         for member_id in member_ids {
        //             let _ = table.build_preset::<builder::Complete>(
        //                 member_id,
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
        //     }

        //     _ => unimplemented!(),
        // }
    }
}
