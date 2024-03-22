use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{build_flag, Finalize, Occurrences};
use crate::{
    arena::ID,
    error,
    symbol::Struct,
    table::{building::finalizing::Finalizer, resolution::Config, Table},
};

build_flag! {
    pub enum Flag {
        GenericParameter,
        WhereClause,
        Body,
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
        let resolution_config = Config {
            ellided_lifetime_provider: None,
            ellided_type_provider: None,
            ellided_constant_provider: None,
            observer: Some(data),
            higher_ranked_liftimes: None,
        };
        match state_flag {
            Flag::GenericParameter => table.create_generic_parameters(
                symbol_id,
                syntax_tree.signature().generic_parameters().as_ref(),
                resolution_config,
                handler,
            ),
            Flag::WhereClause => {
                table.create_where_clause_predicates(
                    symbol_id,
                    syntax_tree.signature().where_clause().as_ref(),
                    resolution_config,
                    handler,
                );
            }
            Flag::Body => { /*create struct fields*/ }
            Flag::Check => {
                table.check_occurrences(symbol_id.into(), data, handler);
            }
        }
    }
}
