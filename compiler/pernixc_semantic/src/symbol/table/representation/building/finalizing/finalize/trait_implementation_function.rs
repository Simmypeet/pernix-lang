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
        TraitImplementationFunction,
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

/// The intermediate representation of the function is built.
pub const INTERMEDIATE_REPRESENTATION_AND_CEHCK_STATE: usize = 3;

impl Finalize for TraitImplementationFunction {
    type SyntaxTree = syntax_tree::item::Function;
    const FINAL_STATE: usize = INTERMEDIATE_REPRESENTATION_AND_CEHCK_STATE;
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
