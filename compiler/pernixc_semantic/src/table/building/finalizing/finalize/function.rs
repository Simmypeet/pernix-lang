use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::Finalize;
use crate::{
    arena::ID,
    error,
    semantic::term::r#type::Type,
    symbol::Function,
    table::{
        building::finalizing::{occurrences::Occurrences, Finalizer},
        resolution, Table,
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

impl Finalize for Function {
    type SyntaxTree = syntax_tree::item::Function;
    const FINAL_STATE: usize = DEFINITION_AND_CHECK_STATE;
    type Data = Occurrences;

    fn finalize(
        table: &Table<Finalizer>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        data: &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            GENERIC_PARAMETER_STATE => {
                // Create the generic parameters
                table.create_generic_parameters(
                    symbol_id,
                    syntax_tree.signature().generic_parameters().as_ref(),
                    data,
                    handler,
                );
            }

            WHERE_CLAUSE_STATE => table.create_where_clause_predicates(
                symbol_id,
                syntax_tree.signature().where_clause().as_ref(),
                data,
                handler,
            ),

            DEFINITION_AND_CHECK_STATE => {
                // build the parameters and return type
                let parameters = syntax_tree
                    .signature()
                    .parameters()
                    .parameter_list()
                    .iter()
                    .flat_map(ConnectedList::elements)
                    .map(|parameter| {
                        (
                            parameter.irrefutable_pattern(),
                            table
                                .resolve_type(
                                    parameter.r#type(),
                                    symbol_id.into(),
                                    resolution::Config {
                                        ellided_lifetime_provider: None,
                                        ellided_type_provider: None,
                                        ellided_constant_provider: None,
                                        observer: Some(data),
                                        higher_ranked_liftimes: None,
                                    },
                                    handler,
                                )
                                .unwrap_or_default(),
                        )
                    })
                    .collect::<Vec<_>>();

                let return_type = syntax_tree
                    .signature()
                    .return_type()
                    .as_ref()
                    .map_or_else(Type::default, |ty| {
                        table
                            .resolve_type(
                                ty.r#type(),
                                symbol_id.into(),
                                resolution::Config {
                                    ellided_lifetime_provider: None,
                                    ellided_type_provider: None,
                                    ellided_constant_provider: None,
                                    observer: Some(data),
                                    higher_ranked_liftimes: None,
                                },
                                handler,
                            )
                            .unwrap_or_default()
                    });
            }

            _ => panic!("invalid state flag"),
        }
    }
}
