use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree;

use super::r#trait;
use crate::{
    arena::ID,
    error,
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    state::Finalize, utility::occurrences::Occurrences,
                    Finalizer,
                },
                RwLockContainer,
            },
            Building, Table,
        },
        PositiveTraitImplementation,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The where clause of the trait implementation is built.
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The generic arguments of the implementation are built.
pub const ARGUMENT_STATE: usize = 2;

/// Bounds check are performed
pub const CHECK_STATE: usize = 3;

impl Finalize for PositiveTraitImplementation {
    type SyntaxTree = syntax_tree::item::ImplementationSignature;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = (Occurrences, Occurrences, Occurrences);

    #[allow(clippy::too_many_lines)]
    fn finalize(
        table: &Table<Building<RwLockContainer, Finalizer>>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        (
            generic_parameter_occurrences,
            where_clause_occurrences,
            argument_occurrences,
        ): &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        table.build_implementation(
            symbol_id,
            state_flag,
            GENERIC_PARAMETER_STATE,
            WHERE_CLAUSE_STATE,
            ARGUMENT_STATE,
            CHECK_STATE,
            syntax_tree.generic_parameters().as_ref(),
            syntax_tree.where_clause().as_ref(),
            syntax_tree.qualified_identifier(),
            generic_parameter_occurrences,
            where_clause_occurrences,
            argument_occurrences,
            r#trait::GENERIC_PARAMETER_STATE,
            r#trait::WHERE_CLAUSE_STATE,
            handler,
        );
    }
}
