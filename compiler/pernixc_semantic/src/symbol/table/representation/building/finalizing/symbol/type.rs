use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use crate::{
    arena::ID,
    error::{self, PrivateEntityLeakedToPublicInterface},
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    state::Finalize,
                    utility::{builder, occurrences::Occurrences},
                    Finalizer,
                },
                RwLockContainer,
            },
            resolution::{self, Observer},
            Building, Table,
        },
        Type,
    },
    type_system::{environment::Environment, normalizer},
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The where clause of the type alias is built.
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The complete information of the type alias is built.
pub const DEFINITION_STATE: usize = 2;

/// All the bounds are checked.
pub const CHECK_STATE: usize = 3;

impl Finalize for Type {
    type SyntaxTree = syntax_tree::item::Type;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = (Occurrences, Occurrences, Occurrences);

    fn finalize(
        table: &Table<Building<RwLockContainer, Finalizer>>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        (
            generic_parameter_occurrences,
            where_clause_occurrences,
            definition_occurrences,
        ): &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            GENERIC_PARAMETER_STATE => table.create_generic_parameters(
                symbol_id,
                syntax_tree.signature().generic_parameters().as_ref(),
                generic_parameter_occurrences,
                handler,
            ),

            WHERE_CLAUSE_STATE => {
                table.create_where_clause(
                    symbol_id,
                    syntax_tree.definition().where_clause().as_ref(),
                    where_clause_occurrences,
                    handler,
                );
            }

            DEFINITION_STATE => {
                // resolve the type
                let mut ty = table
                    .resolve_type(
                        syntax_tree.definition().ty(),
                        symbol_id.into(),
                        resolution::Config {
                            elided_lifetime_provider: None,
                            elided_type_provider: None,
                            elided_constant_provider: None,
                            observer: Some(
                                &mut (&mut builder::Resolution::basic())
                                    .chain(definition_occurrences),
                            ),
                            higher_ranked_lifetimes: None,
                        },
                        handler,
                    )
                    .unwrap_or_default();

                let observer =
                    builder::TypeSystem::new(symbol_id.into(), handler);

                let (environment, _) = Environment::new_with(
                    table.get_active_premise(symbol_id.into()).unwrap(),
                    table,
                    normalizer::NO_OP,
                    &observer,
                );

                ty = environment.simplify_and_check_lifetime_constraints(
                    &ty,
                    &syntax_tree.definition().ty().span(),
                    handler,
                );

                let ty_accessible = table.get_type_accessibility(&ty).unwrap();

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

                table.types().get(symbol_id).unwrap().write().r#type = ty;
            }

            CHECK_STATE => {
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
                    &definition_occurrences,
                    handler,
                );

                table.check_where_clause(symbol_id.into(), handler);
            }

            _ => panic!("invalid state flag"),
        }
    }
}
