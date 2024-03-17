use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{build_flag, Symbol};
use crate::{
    arena::ID,
    error,
    symbol::TraitImplementationFunction,
    table::{state::building::Building, Table},
};

build_flag! {
    pub enum Flag {
        GenericParameter,
        Body,
        WhereClause,
        Check,
    }
}

impl Symbol for TraitImplementationFunction {
    type SyntaxTree = syntax_tree::item::Function;
    type Flag = Flag;
    type Data = ();

    fn build(
        _table: &Table<Building>,
        _symbol_id: ID<Self>,
        _state_flag: Self::Flag,
        _syntax_tree: &Self::SyntaxTree,
        _data: &mut Self::Data,
        _handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
    }
}
