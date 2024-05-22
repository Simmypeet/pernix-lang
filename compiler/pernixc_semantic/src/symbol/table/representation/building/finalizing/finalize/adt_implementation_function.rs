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
        AdtImplementationFunction,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// Where cluase predicates are built
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The function signature information is built, including parameters and return
/// type.
pub const SIGNATURE_STATE: usize = 2;

/// The intermediate representation of the function is built.
pub const DEFINITION_AND_CHECK_STATE: usize = 3;

impl Finalize for AdtImplementationFunction {
    type SyntaxTree = syntax_tree::item::Function;
    const FINAL_STATE: usize = DEFINITION_AND_CHECK_STATE;
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
