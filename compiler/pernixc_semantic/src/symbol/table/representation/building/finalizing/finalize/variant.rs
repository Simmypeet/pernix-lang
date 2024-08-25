use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::Finalize;
use crate::{
    arena::ID,
    error,
    symbol::{
        table::{
            representation::{
                building::finalizing::{occurrences::Occurrences, Finalizer},
                RwLockContainer,
            },
            Building, Table,
        },
        Variant,
    },
};

/// The generic parameters of the parent enum are built.
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The complete information of the variant is built.
pub const DEFINITION_STATE: usize = 1;

/// Bounds check are performed
pub const CHECK_STATE: usize = 2;

impl Finalize for Variant {
    type SyntaxTree = syntax_tree::item::Variant;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = Occurrences;

    fn finalize(
        _table: &Table<Building<RwLockContainer, Finalizer>>,
        _symbol_id: ID<Self>,
        _state_flag: usize,
        _syntax_tree: &Self::SyntaxTree,
        _data: &mut Self::Data,
        _handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        /*
        match state_flag {
            GENERIC_PARAMETER_STATE => {
                // make sure the enum generic parameters are built
                let parent_enum_id =
                    table.get(symbol_id).unwrap().parent_enum_id;
                let _ = table.build_to(
                    parent_enum_id,
                    Some(symbol_id.into()),
                    r#enum::GENERIC_PARAMETER_STATE,
                    handler,
                );
            }

            COMPLETE_STATE => {
                if let Some(association) = syntax_tree.association() {
                    let ty = table
                        .resolve_type(
                            association.r#type(),
                            symbol_id.into(),
                            resolution::Config {
                                ellided_lifetime_provider: None,
                                ellided_type_provider: None,
                                ellided_constant_provider: None,
                                observer: Some(data),
                                higher_ranked_lifetimes: None,
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

                    // assign the simplfiied type to the variant
                    table
                        .representation
                        .variants
                        .get(symbol_id)
                        .unwrap()
                        .write()
                        .associated_type = Some(ty);
                }
            }

            CHECK_STATE => {
                // make sure the enum where clause is built
                let parent_enum_id =
                    table.get(symbol_id).unwrap().parent_enum_id;
                let _ = table.build_to(
                    parent_enum_id,
                    Some(symbol_id.into()),
                    r#enum::WHERE_CLAUSE_STATE,
                    handler,
                );

                table.check_occurrences(symbol_id.into(), data, handler);
            }

            _ => panic!("invalid state flag"),
        }
        */
    }
}
