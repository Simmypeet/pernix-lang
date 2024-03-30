use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{build_flag, Finalize};
use crate::{
    arena::ID,
    error::{self, PrivateEntityLeakedToPublicInterface},
    symbol::Variant,
    table::{
        building::finalizing::{occurrences::Occurrences, Finalizer},
        Index, Table,
    },
};

build_flag! {
    pub enum Flag {
        Complete,
        /// Bounds check are performed
        Check,
    }
}

impl Finalize for Variant {
    type SyntaxTree = syntax_tree::item::Variant;
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
            Flag::Complete => {
                // make sure the enum generic parameters are built
                let parent_enum_id =
                    table.get(symbol_id).unwrap().parent_enum_id;
                let _ = table.build_to(
                    parent_enum_id,
                    Some(symbol_id.into()),
                    super::r#enum::Flag::GenericParameter,
                    true,
                    handler,
                );

                if let Some(association) = syntax_tree.association() {
                    let ty = table
                        .resolve_type(
                            association.ty(),
                            symbol_id.into(),
                            crate::table::resolution::Config {
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

                data.build_all_occurrences_to_completion(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );
            }
            Flag::Check => {
                // make sure the enum where clause is built
                let parent_enum_id =
                    table.get(symbol_id).unwrap().parent_enum_id;
                let _ = table.build_to(
                    parent_enum_id,
                    Some(symbol_id.into()),
                    super::r#enum::Flag::WhereClause,
                    true,
                    handler,
                );

                table.check_occurrences(symbol_id.into(), data, handler);
            }
        }
    }
}
