use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{build_flag, Finalize};
use crate::{
    arena::ID,
    error::Error,
    symbol::TraitType,
    table::{
        building::finalizing::{occurrences::Occurrences, Finalizer},
        Index, Table,
    },
};

build_flag! {
    pub enum Flag {
        /// Generic parameters are built
        GenericParameter,
        /// Where clause predicates are built
        WhereClause,
        /// Bounds check are performed
        Check,
    }
}

impl Finalize for TraitType {
    type SyntaxTree = syntax_tree::item::TraitType;
    type Flag = Flag;
    type Data = Occurrences;

    fn finalize(
        table: &Table<Finalizer>,
        symbol_id: ID<Self>,
        state_flag: Self::Flag,
        syntax_tree: &Self::SyntaxTree,
        data: &mut Self::Data,
        handler: &dyn Handler<Box<dyn Error>>,
    ) {
        match state_flag {
            Flag::GenericParameter => {
                // make sure the trait's geneirc parameters are built
                let parent_trait_id = table.get(symbol_id).unwrap().parent_id;
                let _ = table.build_to(
                    parent_trait_id,
                    Some(symbol_id.into()),
                    super::r#trait::Flag::GenericParameter,
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
            Flag::WhereClause => {
                table.create_where_clause_predicates(
                    symbol_id,
                    syntax_tree.where_clause().as_ref(),
                    data,
                    handler,
                );

                data.build_all_occurrences_to_completion(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );
            }
            Flag::Check => {
                // make sure the trait where clause predicates are built
                let parent_trait_id = table.get(symbol_id).unwrap().parent_id;
                let _ = table.build_to(
                    parent_trait_id,
                    Some(symbol_id.into()),
                    super::r#trait::Flag::WhereClause,
                    true,
                    handler,
                );

                table.check_occurrences(symbol_id.into(), data, handler);
            }
        }
    }
}
