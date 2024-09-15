use pernixc_base::{handler::Handler, source_file::SourceElement};
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
                Index, RwLockContainer,
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
                    .positive_trait_implementations
                    .get(symbol_id)
                    .unwrap()
                    .write()
                    .arguments = table.create_implementation_arguments(
                    symbol_id.into(),
                    parent_trait_id,
                    r#trait::GENERIC_PARAMETER_STATE,
                    syntax_tree.generic_arguments().as_ref(),
                    &syntax_tree.simple_path().rest().last().map_or_else(
                        || syntax_tree.simple_path().root().span(),
                        |(_, ident)| ident.span.clone(),
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
                    &generic_parameter_occurrences,
                    handler,
                );
                table.check_occurrences(
                    symbol_id.into(),
                    &where_clause_occurrences,
                    handler,
                );
                table.check_occurrences(
                    symbol_id.into(),
                    &argument_occurrences,
                    handler,
                );
                table.check_where_clause(symbol_id.into(), handler);
                table.implementation_signature_check(symbol_id, handler);
            }

            _ => panic!("invalid state flag"),
        }
    }
}
