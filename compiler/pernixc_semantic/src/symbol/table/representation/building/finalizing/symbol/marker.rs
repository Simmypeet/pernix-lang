use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree;

use super::{negative_marker_implementation, positive_marker_implementation};
use crate::{
    arena::ID,
    error,
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    utility::occurrences::Occurrences, Finalize, Finalizer,
                },
                RwLockContainer,
            },
            Building, Table,
        },
        Marker, MarkerImplementationID,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The where clause of the trait is built.
pub const WHERE_CLAUSE_STATE: usize = 1;

/// Bounds check are performed
pub const CHECK_STATE: usize = 2;

impl Finalize for Marker {
    type SyntaxTree = syntax_tree::item::Marker;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = (Occurrences, Occurrences);

    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    fn finalize(
        table: &Table<Building<RwLockContainer, Finalizer>>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        (generic_parameter_occurrences,where_clause_occurrences): &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        table.build_trait_and_marker(
            symbol_id,
            state_flag,
            GENERIC_PARAMETER_STATE,
            WHERE_CLAUSE_STATE,
            CHECK_STATE,
            syntax_tree.signature().generic_parameters().as_ref(),
            syntax_tree.signature().where_clause().as_ref(),
            generic_parameter_occurrences,
            where_clause_occurrences,
            |id| match id {
                MarkerImplementationID::Positive(id) => {
                    let _ = table.build_to(
                        id,
                        Some(symbol_id.into()),
                        positive_marker_implementation::ARGUMENT_STATE,
                        handler,
                    );
                }
                MarkerImplementationID::Negative(id) => {
                    let _ = table.build_to(
                        id,
                        Some(symbol_id.into()),
                        negative_marker_implementation::ARGUMENT_STATE,
                        handler,
                    );
                }
            },
            handler,
        );
    }
}
