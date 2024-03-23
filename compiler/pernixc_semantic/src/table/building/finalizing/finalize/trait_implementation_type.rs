use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{build_flag, Finalize, Occurrences};
use crate::{
    arena::ID,
    error,
    symbol::TraitImplementationType,
    table::{building::finalizing::Finalizer, resolution, Table},
};

build_flag! {
    pub enum Flag {
        /// Generic parameters are built
        GenericParameter,
        /// Where clause predicates are built
        WhereClause,
        /// The type alias is built
        Complete,
        /// Bounds check are performed
        Check,
    }
}

impl Finalize for TraitImplementationType {
    type SyntaxTree = syntax_tree::item::Type;
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
                let parent_implementation_id = table
                    .trait_implementation_types
                    .get(symbol_id)
                    .unwrap()
                    .read()
                    .parent_id;

                let _ = table.build_to(
                    parent_implementation_id,
                    Some(symbol_id.into()),
                    super::trait_implementation::Flag::GenericParameter,
                    true,
                    handler,
                );

                table.create_generic_parameters(
                    symbol_id,
                    syntax_tree.signature().generic_parameters().as_ref(),
                    data,
                    handler,
                );
            }
            Flag::WhereClause => {
                table.create_where_clause_predicates(
                    symbol_id,
                    syntax_tree.definition().where_clause().as_ref(),
                    data,
                    handler,
                );
            }
            Flag::Complete => {
                table
                    .trait_implementation_types
                    .get(symbol_id)
                    .unwrap()
                    .write()
                    .r#type = table
                    .resolve_type(
                        syntax_tree.definition().ty(),
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
                    .unwrap_or_default();

                data.build_all_occurrences_to_completion(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );
            }
            Flag::Check => {
                table.check_occurrences(symbol_id.into(), data, handler);

                // TODO: make sure the signature is the same as the trait
                // definition
            }
        }
    }
}
