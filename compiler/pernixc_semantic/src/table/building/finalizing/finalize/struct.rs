use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{build_flag, Finalize};
use crate::{
    arena::ID,
    error,
    symbol::{Struct, Variance},
    table::{
        building::finalizing::{occurrences::Occurrences, Finalizer},
        Table,
    },
};

build_flag! {
    pub enum Flag {
        /// Generic parameters are built
        GenericParameter,
        /// Where clause predicates are built
        WhereClause,
        /// All structs are field are built
        Complete,
        /// Bounds check are performed
        Check,
    }
}

impl Finalize for Struct {
    type SyntaxTree = syntax_tree::item::Struct;
    type Flag = Flag;
    type Data = Occurrences;

    fn finalize(
        table: &Table<Finalizer>,
        symbol_id: ID<Self>,
        state_flag: Self::Flag,
        syntax_tree: &Self::SyntaxTree,
        data: &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            Flag::GenericParameter => table.create_generic_parameters(
                symbol_id,
                syntax_tree.signature().generic_parameters().as_ref(),
                Variance::Covariant,
                data,
                handler,
            ),
            Flag::WhereClause => {
                table.create_where_clause_predicates(
                    symbol_id,
                    syntax_tree.signature().where_clause().as_ref(),
                    data,
                    handler,
                );
            }
            Flag::Complete => {
                /* TODO: build all struct fields */
                data.build_all_occurrences_to_completion(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );
            }
            Flag::Check => {
                table.check_occurrences(symbol_id.into(), data, handler);
            }
        }
    }
}
