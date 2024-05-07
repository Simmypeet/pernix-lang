use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{r#trait, Finalize};
use crate::{
    arena::ID,
    error,
    symbol::{GlobalID, TraitImplementation},
    table::{
        building::finalizing::{
            finalizer::build_preset, occurrences::Occurrences, Finalizer,
        },
        resolution, Index, Table,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The generic arguments of the implementation are built.
pub const GENERIC_ARGUMENTS_STATE: usize = 1;

/// Where cluase predicates are built
pub const WHERE_CLAUSE_STATE: usize = 2;

/// The complete information of the constant is built.
pub const COMPLETE_STATE: usize = 3;

/// Bounds check are performed
pub const CHECK_STATE: usize = 4;

impl Finalize for TraitImplementation {
    type SyntaxTree = syntax_tree::item::ImplementationSignature;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = Occurrences;

    #[allow(clippy::too_many_lines)]
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
                table.create_generic_parameters(
                    symbol_id,
                    syntax_tree.generic_parameters().as_ref(),
                    data,
                    handler,
                );
            }
            GENERIC_ARGUMENTS_STATE => {
                let parent_trait_id =
                    table.get(symbol_id).unwrap().signature.implemented_id;

                // make sure the trait has the generic parameters
                let _ = table.build_to(
                    parent_trait_id,
                    Some(symbol_id.into()),
                    r#trait::GENERIC_PARAMETER_STATE,
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
                    .trait_implementations
                    .get(symbol_id)
                    .unwrap()
                    .write()
                    .signature
                    .arguments =
                    generic_arguments.ok().and_then(|x| x).unwrap_or_default();
            }
            WHERE_CLAUSE_STATE => table.create_where_clause_predicates(
                symbol_id,
                syntax_tree.where_clause().as_ref(),
                data,
                handler,
            ),
            COMPLETE_STATE => {
                // build all the trait member
                let member_ids = table
                    .get(symbol_id)
                    .unwrap()
                    .implementation_constant_ids_by_trait_constant_id
                    .values()
                    .copied()
                    .map(GlobalID::from)
                    .chain(
                        table
                            .get(symbol_id)
                            .unwrap()
                            .implementation_function_ids_by_trait_function_id
                            .values()
                            .copied()
                            .map(GlobalID::from),
                    )
                    .chain(
                        table
                            .get(symbol_id)
                            .unwrap()
                            .implementation_type_ids_by_trait_type_id
                            .values()
                            .copied()
                            .map(GlobalID::from),
                    )
                    .collect::<Vec<_>>();

                for member_id in member_ids {
                    let _ = table.build_preset::<build_preset::Complete>(
                        member_id,
                        Some(symbol_id.into()),
                        false,
                        handler,
                    );
                }

                data.build_all_occurrences_to::<build_preset::Complete>(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );
            }
            CHECK_STATE => {
                // make sure the implemented trait has the where clause
                let parent_trait_id =
                    table.get(symbol_id).unwrap().signature.implemented_id;
                let _ = table.build_to(
                    parent_trait_id,
                    Some(symbol_id.into()),
                    r#trait::WHERE_CLAUSE_STATE,
                    true,
                    handler,
                );

                table.check_occurrences(symbol_id.into(), data, handler);
                table.implementation_signature_check(symbol_id, handler);
            }
            _ => panic!("invalid state flag"),
        }
    }
}
