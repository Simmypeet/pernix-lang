use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{build_flag, Finalize};
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

build_flag! {
    pub enum Flag {
        /// Generic parameters are built
        GenericParameter,
        /// Where clause predicates are built
        WhereClause,
        /// The parameters and return type are built
        Complete,
        /// Bounds check are performed and the full function body is built
        Check,
    }
}

impl Finalize for Function {
    type SyntaxTree = syntax_tree::item::Function;
    type Flag = Flag;
    type Data = Occurrences;

    fn finalize(
        table: &Table<Finalizer>,
        symbol_id: ID<Self>,
        state_flag: Self::Flag,
        syntax_tree: &Self::SyntaxTree,
        data: &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            Flag::GenericParameter => {
                // Create the generic parameters
                table.create_generic_parameters(
                    symbol_id,
                    syntax_tree.signature().generic_parameters().as_ref(),
                    data,
                    handler,
                );
            }

            Flag::WhereClause => table.create_where_clause_predicates(
                symbol_id,
                syntax_tree.signature().where_clause().as_ref(),
                data,
                handler,
            ),

            Flag::Complete => {
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
            Flag::Check => {}
        }
    }
}
