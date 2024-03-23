use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{build_flag, Finalize};
use crate::{
    arena::ID,
    error,
    symbol::NegativeTraitImplementation,
    table::{
        building::finalizing::{occurrences::Occurrences, Finalizer},
        resolution, Index, Table,
    },
};

build_flag! {
    pub enum Flag {
        /// Generic parameters are built
        GenericParameter,
        /// Generic arguments are built
        GenericArguments,
        /// Where clause predicates are built
        WhereClause,
        Complete,
        /// Bounds check are performed
        Check,
    }
}

impl Finalize for NegativeTraitImplementation {
    type SyntaxTree = syntax_tree::item::ImplementationSignature;
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
                table.create_generic_parameters(
                    symbol_id,
                    syntax_tree.generic_parameters().as_ref(),
                    data,
                    handler,
                );
            }
            Flag::GenericArguments => {
                let parent_trait_id =
                    table.get(symbol_id).unwrap().signature.implemented_id;

                // make sure the trait has the generic parameters
                let _ = table.build_to(
                    parent_trait_id,
                    Some(symbol_id.into()),
                    super::r#trait::Flag::GenericParameter,
                    true,
                    handler,
                );

                let generic_identifier = syntax_tree
                    .qualified_identifier()
                    .generic_identifiers()
                    .last()
                    .unwrap();

                let generic_arguments = table.resolve_generic_arguments(
                    generic_identifier,
                    symbol_id.into(),
                    parent_trait_id.into(),
                    resolution::Config {
                        ellided_lifetime_provider: None,
                        ellided_type_provider: None,
                        ellided_constant_provider: None,
                        observer: Some(data),
                        higher_ranked_liftimes: None,
                    },
                    handler,
                );

                table
                    .negative_trait_implementations
                    .get(symbol_id)
                    .unwrap()
                    .write()
                    .signature
                    .arguments =
                    generic_arguments.ok().and_then(|x| x).unwrap_or_default();
            }
            Flag::WhereClause => {
                table.create_where_clause_predicates(
                    symbol_id,
                    syntax_tree.where_clause().as_ref(),
                    data,
                    handler,
                );
            }
            Flag::Complete => {
                data.build_all_occurrences_to_completion(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );
            }
            Flag::Check => {
                table.check_occurrences(symbol_id.into(), data, handler);
                table.implementation_signature_check(symbol_id, handler);
            }
        }
    }
}
