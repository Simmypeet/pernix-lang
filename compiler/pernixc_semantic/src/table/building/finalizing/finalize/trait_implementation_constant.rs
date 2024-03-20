use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{build_flag, Finalize};
use crate::{
    arena::ID,
    error,
    symbol::TraitImplementationConstant,
    table::{building::finalizing::Finalizer, Table},
};

build_flag! {
    pub enum Flag {
        GenericParameter,
        WhereClause,
        Body,
        Check,
    }
}

impl Finalize for TraitImplementationConstant {
    type SyntaxTree = syntax_tree::item::Constant;
    type Flag = Flag;
    type Data = ();

    fn finalize(
        _table: &Table<Finalizer>,
        _symbol_id: ID<Self>,
        _state_flag: Self::Flag,
        _syntax_tree: &Self::SyntaxTree,
        _data: &mut Self::Data,
        _handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
    }
}
