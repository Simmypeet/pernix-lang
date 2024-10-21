use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree;

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
        NegativeMarkerImplementation,
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

impl Finalize for NegativeMarkerImplementation {
    type SyntaxTree = syntax_tree::item::ImplementationSignature;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = (Occurrences, Occurrences, Occurrences);

    fn finalize(
        _table: &Table<Building<RwLockContainer, Finalizer>>,
        _symbol_id: ID<Self>,
        state_flag: usize,
        _syntax_tree: &Self::SyntaxTree,
        (
            _generic_parameter_occurrences,
            _where_clause_occurrences,
            _argument_occurrences,
        ): &mut Self::Data,
        _handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            GENERIC_PARAMETER_STATE
            | WHERE_CLAUSE_STATE
            | ARGUMENT_STATE
            | CHECK_STATE => {}
            _ => panic!("invalid state flag"),
        }
        /*
        match state_flag {
            GENERIC_PARAMETER_STATE => {
                table.create_generic_parameters(
                    symbol_id,
                    syntax_tree.generic_parameters().as_ref(),
                    generic_parameter_occurrences,
                    handler,
                );
            }

            WHERE_CLAUSE_STATE => {
                table.create_where_clause(
                    symbol_id,
                    syntax_tree.where_clause().as_ref(),
                    where_clause_occurrences,
                    handler,
                );
            }

            ARGUMENT_STATE => {
                let parent_trait_id =
                    table.get(symbol_id).unwrap().implemented_id;

                table
                    .negative_trait_implementations
                    .get(symbol_id)
                    .unwrap()
                    .write()
                    .arguments = table.create_implementation_arguments(
                    symbol_id.into(),
                    parent_trait_id,
                    r#trait::GENERIC_PARAMETER_STATE,
                    syntax_tree
                        .qualified_identifier()
                        .rest()
                        .last()
                        .map_or_else(
                            || {
                                syntax_tree
                                    .qualified_identifier()
                                    .root()
                                    .as_generic_identifier()
                                    .unwrap()
                            },
                            |x| &x.1,
                        ),
                    argument_occurrences,
                    handler,
                );
            }

            CHECK_STATE => {
                // make sure the implemented trait has the where clause
                let parent_trait_id =
                    table.get(symbol_id).unwrap().implemented_id;
                let _ = table.build_to(
                    parent_trait_id,
                    Some(symbol_id.into()),
                    r#trait::WHERE_CLAUSE_STATE,
                    handler,
                );

                table.check_occurrences(
                    symbol_id.into(),
                    generic_parameter_occurrences,
                    handler,
                );
                table.check_occurrences(
                    symbol_id.into(),
                    where_clause_occurrences,
                    handler,
                );
                table.check_occurrences(
                    symbol_id.into(),
                    argument_occurrences,
                    handler,
                );
                table.check_where_clause(symbol_id.into(), handler);
                table.implementation_signature_check(symbol_id, handler);
            }

            _ => panic!("invalid state flag"),
        }
        */
    }
}
