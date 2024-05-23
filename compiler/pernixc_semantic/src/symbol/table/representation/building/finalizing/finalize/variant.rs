use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{r#enum, Finalize};
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
                Index, RwLockContainer,
            },
            resolution, Building, Table,
        },
        Variant,
    },
};

/// The generic parameters of the parent enum are built.
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The complete information of the variant is built.
pub const COMPLETE_STATE: usize = 1;

/// Bounds check are performed
pub const CHECK_STATE: usize = 2;

impl Finalize for Variant {
    type SyntaxTree = syntax_tree::item::Variant;
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
            GENERIC_PARAMETER_STATE => {
                // make sure the enum generic parameters are built
                let parent_enum_id =
                    table.get(symbol_id).unwrap().parent_enum_id;
                let _ = table.build_to(
                    parent_enum_id,
                    Some(symbol_id.into()),
                    r#enum::GENERIC_PARAMETER_STATE,
                    true,
                    handler,
                );
            }

            COMPLETE_STATE => {
                if let Some(association) = syntax_tree.association() {
                    let ty = table
                        .resolve_type(
                            association.ty(),
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
                        table.get_type_accessibility(&ty).unwrap();

                    // private entity leaked to public interface
                    if ty_accessible
                        < table.get_accessibility(symbol_id.into()).unwrap()
                    {
                        handler.receive(Box::new(
                            PrivateEntityLeakedToPublicInterface {
                                entity: ty.clone(),
                                entity_overall_accessibility: ty_accessible,
                                leaked_span: association.span(),
                                public_interface_id: symbol_id.into(),
                            },
                        ));
                    }

                    table
                        .representation
                        .variants
                        .get(symbol_id)
                        .unwrap()
                        .write()
                        .associated_type = Some(ty);
                }

                data.build_all_occurrences_to::<build_preset::PartialComplete>(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );
            }

            CHECK_STATE => {
                // make sure the enum where clause is built
                let parent_enum_id =
                    table.get(symbol_id).unwrap().parent_enum_id;
                let _ = table.build_to(
                    parent_enum_id,
                    Some(symbol_id.into()),
                    r#enum::WHERE_CLAUSE_STATE,
                    true,
                    handler,
                );

                table.check_occurrences(symbol_id.into(), data, handler);
            }

            _ => panic!("invalid state flag"),
        }
    }
}
