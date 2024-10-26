use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree;

use super::{negative_trait_implementation, positive_trait_implementation};
use crate::{
    arena::ID,
    error::{self},
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
        Trait, TraitImplementationID,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The where clause of the trait is built.
pub const WHERE_CLAUSE_STATE: usize = 1;

/// Bounds check are performed
pub const CHECK_STATE: usize = 2;

impl Finalize for Trait {
    type SyntaxTree = syntax_tree::item::TraitSignature;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = (Occurrences, Occurrences);

    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    fn finalize(
        table: &Table<Building<RwLockContainer, Finalizer>>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        (generic_parameter_occurrences, where_clause_occurrences): &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        table.build_trait_and_marker(
            symbol_id,
            state_flag,
            GENERIC_PARAMETER_STATE,
            WHERE_CLAUSE_STATE,
            CHECK_STATE,
            syntax_tree.generic_parameters().as_ref(),
            syntax_tree.where_clause().as_ref(),
            generic_parameter_occurrences,
            where_clause_occurrences,
            |id| match id {
                TraitImplementationID::Positive(id) => {
                    let _ = table.build_to(
                        id,
                        Some(symbol_id.into()),
                        positive_trait_implementation::ARGUMENT_STATE,
                        handler,
                    );
                }
                TraitImplementationID::Negative(id) => {
                    let _ = table.build_to(
                        id,
                        Some(symbol_id.into()),
                        negative_trait_implementation::ARGUMENT_STATE,
                        handler,
                    );
                }
            },
            handler,
        );
    }
}
