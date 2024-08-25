use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{positive_trait_implementation, Finalize};
use crate::{
    arena::ID,
    error::{self, PrivateEntityLeakedToPublicInterface},
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    finalizer::builder::{Definition, DefinitionA},
                    occurrences::Occurrences,
                    Finalizer,
                },
                RwLockContainer,
            },
            resolution::{self, Observer},
            Building, Table,
        },
        TraitImplementationType,
    },
    type_system::{environment::Environment, normalizer},
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The complete information of the trait implementation type is built.
pub const DEFINITION_STATE: usize = 1;

/// The information required to check the bounds is built. (the definition of
/// where caluses are built)
pub const WELL_FORMED_STATE: usize = 2;

/// Bounds check are performed
pub const CHECK_STATE: usize = 3;

impl Finalize for TraitImplementationType {
    type SyntaxTree = syntax_tree::item::Type;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = (Occurrences, Occurrences, Occurrences);

    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    fn finalize(
        table: &Table<Building<RwLockContainer, Finalizer>>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        (
            generic_parameters_occurrences,
            where_clause_occurrences,
            definition_occurrences,
        ): &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            GENERIC_PARAMETER_STATE => {
                // build the parent trait implementation's generic parameters
                let parent_implementation_id = table
                    .trait_implementation_types
                    .get(symbol_id)
                    .unwrap()
                    .read()
                    .parent_id;
                let _ = table.build_to(
                    parent_implementation_id,
                    Some(symbol_id.into()),
                    positive_trait_implementation::GENERIC_PARAMETER_STATE,
                    handler,
                );

                table.create_generic_parameters(
                    symbol_id,
                    syntax_tree.signature().generic_parameters().as_ref(),
                    generic_parameters_occurrences,
                    handler,
                );
            }

            DEFINITION_STATE => {
                let parent_implementation_id = table
                    .trait_implementation_types
                    .get(symbol_id)
                    .unwrap()
                    .read()
                    .parent_id;
                let _ = table.build_to(
                    parent_implementation_id,
                    Some(symbol_id.into()),
                    positive_trait_implementation::DEFINITION_STATE,
                    handler,
                );

                table.create_where_clause_predicates_for_definition(
                    symbol_id,
                    syntax_tree.definition().where_clause().as_ref(),
                    where_clause_occurrences,
                    handler,
                );

                // resolve the type
                let mut definition_builder = Definition::new();
                let mut observer =
                    definition_builder.chain(definition_occurrences);

                let mut ty = table
                    .resolve_type(
                        syntax_tree.definition().ty(),
                        symbol_id.into(),
                        resolution::Config {
                            ellided_lifetime_provider: None,
                            ellided_type_provider: None,
                            ellided_constant_provider: None,
                            observer: Some(&mut observer),
                            higher_ranked_lifetimes: None,
                        },
                        handler,
                    )
                    .unwrap_or_default();

                let observer = DefinitionA::new(symbol_id.into(), handler);

                let (environment, _) = Environment::new_with(
                    table.get_active_premise(symbol_id.into()).unwrap(),
                    table,
                    &normalizer::NO_OP,
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

                table
                    .trait_implementation_types()
                    .get(symbol_id)
                    .unwrap()
                    .write()
                    .r#type = ty;
            }

            WELL_FORMED_STATE => table
                .create_where_clause_predicates_for_well_formed(
                    symbol_id,
                    syntax_tree.definition().where_clause().as_ref(),
                    where_clause_occurrences,
                    handler,
                ),

            CHECK_STATE => {
                // let (trait_implementation_id, trait_id) = {
                //     let trait_implementation_type_sym =
                //         table.get(symbol_id).unwrap();
                //     let trait_implementation_sym = table
                //         .get(trait_implementation_type_sym.parent_id)
                //         .unwrap();

                //     (
                //         trait_implementation_type_sym.parent_id,
                //         trait_implementation_sym.implemented_id,
                //     )
                // };

                // let _ = table.build_to(
                //     trait_implementation_id,
                //     Some(symbol_id.into()),
                //     positive_trait_implementation::WHERE_CLAUSE_STATE,
                //     handler,
                // );

                // let trait_implementation_type_sym = table
                //     .trait_implementation_types
                //     .get(symbol_id)
                //     .unwrap()
                //     .read();
                // let trait_implementation_sym = table
                //     .positive_trait_implementations
                //     .get(trait_implementation_type_sym.parent_id)
                //     .unwrap()
                //     .read();
                // let trait_sym = table.traits.get(trait_id).unwrap().read();

                // // get the trait type id equivalent
                // let Some(trait_type_id) = trait_sym
                //     .member_ids_by_name
                //     .get(trait_implementation_type_sym.name())
                //     .copied()
                //     .and_then(|x| x.into_type().ok())
                // else {
                //     return;
                // };

                // table.check_occurrences(symbol_id.into(), data, handler);
                // table.check_where_clause(symbol_id.into(), handler);

                // let _ = table.build_to(
                //     trait_type_id,
                //     Some(symbol_id.into()),
                //     trait_type::WHERE_CLAUSE_STATE,
                //     handler,
                // );

                // table.implementation_member_check(
                //     symbol_id.into(),
                //     trait_type_id.into(),
                //     Instantiation::from_generic_arguments(
                //         trait_implementation_sym.arguments.clone(),
                //         trait_implementation_sym.implemented_id.into(),
                //         &trait_sym.generic_declaration.parameters,
                //     )
                //     .unwrap_or_default(),
                //     handler,
                // );
            }

            _ => panic!("invalid state flag"),
        }
    }
}
