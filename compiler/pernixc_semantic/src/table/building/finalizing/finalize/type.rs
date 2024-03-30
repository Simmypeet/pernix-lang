use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{build_flag, Finalize};
use crate::{
    arena::ID,
    error::{self, PrivateEntityLeakedToPublicInterface},
    symbol::Type,
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
        /// The type alias is built
        Complete,
        /// Bounds check are performed
        Check,
    }
}

impl Finalize for Type {
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
            Flag::GenericParameter => table.create_generic_parameters(
                symbol_id,
                syntax_tree.signature().generic_parameters().as_ref(),
                data,
                handler,
            ),
            Flag::WhereClause => {
                table.create_where_clause_predicates(
                    symbol_id,
                    syntax_tree.definition().where_clause().as_ref(),
                    data,
                    handler,
                );
            }
            Flag::Complete => {
                let ty = table
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

                let ty_accessible =
                    table.get_type_overall_accessibility(&ty).unwrap();

                // private entity leaked to public interface
                if ty_accessible
                    < table.get_accessibility(symbol_id.into()).unwrap()
                {
                    handler.receive(Box::new(
                        PrivateEntityLeakedToPublicInterface {
                            entity: ty.clone(),
                            entity_overall_accessibility: ty_accessible,
                            leaked_span: syntax_tree.definition().ty().span(),
                            public_interface_id: symbol_id.into(),
                        },
                    ));
                }

                table.types.get(symbol_id).unwrap().write().r#type = ty;
                data.build_all_occurrences_to_completion(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );
            }
            Flag::Check => {
                table.check_occurrences(symbol_id.into(), data, handler);
            }
        }
    }
}
