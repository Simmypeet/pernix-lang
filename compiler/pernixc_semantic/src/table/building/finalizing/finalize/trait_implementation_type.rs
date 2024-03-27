use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{build_flag, Finalize};
use crate::{
    arena::ID,
    error,
    semantic::instantiation::Instantiation,
    symbol::{TraitImplementationType, Variance},
    table::{
        building::finalizing::{occurrences::Occurrences, Finalizer},
        resolution, Index, Table,
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

impl Finalize for TraitImplementationType {
    type SyntaxTree = syntax_tree::item::Type;
    type Flag = Flag;
    type Data = Occurrences;

    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
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
                    Variance::Invariant,
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
                let (trait_implementation_id, trait_id) = {
                    let trait_implementation_type_sym =
                        table.get(symbol_id).unwrap();
                    let trait_implementation_sym = table
                        .get(trait_implementation_type_sym.parent_id)
                        .unwrap();
                    let trait_sym = table
                        .get(trait_implementation_sym.signature.implemented_id)
                        .unwrap();

                    (trait_implementation_sym.id, trait_sym.id)
                };

                // make sure that the trait implementation's generic arguments
                // are built
                let _ = table.build_to(
                    trait_implementation_id,
                    Some(symbol_id.into()),
                    super::trait_implementation::Flag::WhereClause,
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
                        if *val == trait_implementation_type_sym.id {
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
                    super::trait_type::Flag::WhereClause,
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
        }
    }
}
