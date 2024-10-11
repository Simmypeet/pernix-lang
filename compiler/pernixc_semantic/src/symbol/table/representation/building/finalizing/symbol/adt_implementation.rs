use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree;

use super::{r#enum, r#struct};
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
                Index, RwLockContainer, Table,
            },
            Building,
        },
        AdtID, AdtImplementation,
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

impl Finalize for AdtImplementation {
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
                let parent_adt_id =
                    table.get(symbol_id).unwrap().implemented_id;

                match parent_adt_id {
                    AdtID::Struct(struct_id) => {
                        table
                            .adt_implementations
                            .get(symbol_id)
                            .unwrap()
                            .write()
                            .arguments = table.create_implementation_arguments(
                            symbol_id.into(),
                            struct_id,
                            r#struct::GENERIC_PARAMETER_STATE,
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
                    AdtID::Enum(enum_id) => {
                        table
                            .adt_implementations
                            .get(symbol_id)
                            .unwrap()
                            .write()
                            .arguments = table.create_implementation_arguments(
                            symbol_id.into(),
                            enum_id,
                            r#enum::GENERIC_PARAMETER_STATE,
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
                }
            }

            CHECK_STATE => {
                // make sure the implemented adt has the where clause and
                // generic arguments
                let parent_adt_id =
                    table.get(symbol_id).unwrap().implemented_id;

                match parent_adt_id {
                    AdtID::Struct(struct_id) => {
                        let _ = table.build_to(
                            struct_id,
                            Some(symbol_id.into()),
                            r#struct::WHERE_CLAUSE_STATE,
                            handler,
                        );
                    }
                    AdtID::Enum(enum_id) => {
                        let _ = table.build_to(
                            enum_id,
                            Some(symbol_id.into()),
                            r#enum::WHERE_CLAUSE_STATE,
                            handler,
                        );
                    }
                }

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

            _ => unimplemented!(),
        }
    }
}
