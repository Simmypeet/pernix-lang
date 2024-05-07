use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{trait_implementation, trait_type, Finalize};
use crate::{
    arena::ID,
    error,
    semantic::instantiation::Instantiation,
    symbol::TraitImplementationType,
    table::{
        building::finalizing::{
            finalizer::build_preset, occurrences::Occurrences, Finalizer,
        },
        resolution, Index, Table,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// Where cluase predicates are built
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The complete information of the trait implementation type is built.
pub const COMPLETE_STATE: usize = 2;

/// Bounds check are performed
pub const CHECK_STATE: usize = 3;

impl Finalize for TraitImplementationType {
    type SyntaxTree = syntax_tree::item::Type;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = Occurrences;

    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
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
                let parent_implementation_id = table
                    .trait_implementation_types
                    .get(symbol_id)
                    .unwrap()
                    .read()
                    .parent_id;

                let _ = table.build_to(
                    parent_implementation_id,
                    Some(symbol_id.into()),
                    trait_implementation::GENERIC_PARAMETER_STATE,
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
            WHERE_CLAUSE_STATE => {
                table.create_where_clause_predicates(
                    symbol_id,
                    syntax_tree.definition().where_clause().as_ref(),
                    data,
                    handler,
                );
            }
            COMPLETE_STATE => {
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

                data.build_all_occurrences_to::<build_preset::Complete>(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );
            }
            CHECK_STATE => {
                let (trait_implementation_id, trait_id) = {
                    let trait_implementation_type_sym =
                        table.get(symbol_id).unwrap();
                    let trait_implementation_sym = table
                        .get(trait_implementation_type_sym.parent_id)
                        .unwrap();

                    (
                        trait_implementation_type_sym.parent_id,
                        trait_implementation_sym.signature.implemented_id,
                    )
                };

                // make sure that the trait implementation's generic arguments
                // are built
                let _ = table.build_to(
                    trait_implementation_id,
                    Some(symbol_id.into()),
                    trait_implementation::WHERE_CLAUSE_STATE,
                    true,
                    handler,
                );

                let trait_implementation_type_sym = table
                    .trait_implementation_types
                    .get(symbol_id)
                    .unwrap()
                    .read();
                let trait_implementation_sym = table
                    .trait_implementations
                    .get(trait_implementation_type_sym.parent_id)
                    .unwrap()
                    .read();
                let trait_sym = table.traits.get(trait_id).unwrap().read();

                // get the trait type id equivalent
                let Some(trait_type_id) = trait_implementation_sym
                    .implementation_type_ids_by_trait_type_id
                    .iter()
                    .find_map(|(key, val)| {
                        if *val == symbol_id {
                            Some(*key)
                        } else {
                            None
                        }
                    })
                else {
                    return;
                };

                table.check_occurrences(symbol_id.into(), data, handler);
                let _ = table.build_to(
                    trait_type_id,
                    Some(symbol_id.into()),
                    trait_type::WHERE_CLAUSE_STATE,
                    true,
                    handler,
                );

                table.implementation_member_check(
                    symbol_id.into(),
                    trait_type_id.into(),
                    Instantiation::from_generic_arguments(
                        trait_implementation_sym.signature.arguments.clone(),
                        trait_implementation_sym
                            .signature
                            .implemented_id
                            .into(),
                        &trait_sym.generic_declaration.parameters,
                    )
                    .unwrap_or_default(),
                    handler,
                );
            }

            _ => panic!("invalid state flag"),
        }
    }
}
