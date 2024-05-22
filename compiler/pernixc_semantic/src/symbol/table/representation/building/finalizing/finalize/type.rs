use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::Finalize;
use crate::{
    arena::ID,
    error::{self, PrivateEntityLeakedToPublicInterface},
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    finalizer::build_preset, occurrences::Occurrences,
                    Finalizer,
                },
                RwLockContainer,
            },
            resolution, Building, Table,
        },
        Type,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// Where cluase predicates are built
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The complete information of the type alias is built.
pub const COMPLETE_STATE: usize = 2;

/// All the bounds are checked.
pub const CHECK_STATE: usize = 3;

impl Finalize for Type {
    type SyntaxTree = syntax_tree::item::Type;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = Occurrences;

    fn finalize(
        table: &Table<Building<RwLockContainer, Finalizer>>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        data: &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            GENERIC_PARAMETER_STATE => table.create_generic_parameters(
                symbol_id,
                syntax_tree.signature().generic_parameters().as_ref(),
                data,
                handler,
            ),
            WHERE_CLAUSE_STATE => {
                table.create_where_clause_predicates(
                    symbol_id,
                    syntax_tree.definition().where_clause().as_ref(),
                    data,
                    handler,
                );
            }
            COMPLETE_STATE => {
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
                data.build_all_occurrences_to::<build_preset::Complete>(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );
            }
            CHECK_STATE => {
                table.check_occurrences(symbol_id.into(), data, handler);
            }
            _ => panic!("invalid state flag"),
        }
    }
}
