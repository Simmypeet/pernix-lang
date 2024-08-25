use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::Finalize;
use crate::{
    arena::ID,
    error,
    symbol::{
        table::{
            representation::{
                building::finalizing::Finalizer, RwLockContainer, Table,
            },
            Building,
        },
        TraitFunction,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The function signature information is built, including parameters and return
/// type.
pub const DEFINITION_STATE: usize = 1;

/// The information required to check the bounds is built. (the definition of
/// where caluses are built)
pub const WELL_FORMED_STATE: usize = 2;

/// Bounds check are performed
pub const CHECK_STATE: usize = 3;

impl Finalize for TraitFunction {
    type SyntaxTree = syntax_tree::item::TraitFunction;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = ();

    fn finalize(
        _table: &Table<Building<RwLockContainer, Finalizer>>,
        _symbol_id: ID<Self>,
        _state_flag: usize,
        _syntax_tree: &Self::SyntaxTree,
        _data: &mut Self::Data,
        _handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
    }
}
