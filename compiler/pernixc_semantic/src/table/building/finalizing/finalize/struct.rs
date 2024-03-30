use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{build_flag, Finalize};
use crate::{
    arena::ID,
    error::{self, DuplicatedField, PrivateEntityLeakedToPublicInterface},
    symbol::{Accessibility, Field, Struct},
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
        /// All structs are field are built
        Complete,
        /// Bounds check are performed
        Check,
    }
}

impl Finalize for Struct {
    type SyntaxTree = syntax_tree::item::Struct;
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
                    syntax_tree.signature().where_clause().as_ref(),
                    data,
                    handler,
                );
            }
            Flag::Complete => {
                for field_syn in syntax_tree
                    .body()
                    .field_list()
                    .iter()
                    .flat_map(ConnectedList::elements)
                {
                    let ty = table
                        .resolve_type(
                            field_syn.r#type(),
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

                    let field_accessibility = Accessibility::from_syntax_tree(
                        field_syn.access_modifier(),
                    );

                    let ty_accessibility =
                        table.get_type_overall_accessibility(&ty).unwrap();

                    // private entity leaked to public interface
                    if ty_accessibility < field_accessibility {
                        handler.receive(Box::new(
                            PrivateEntityLeakedToPublicInterface {
                                entity: ty.clone(),
                                entity_overall_accessibility: ty_accessibility,
                                leaked_span: field_syn.r#type().span(),
                                public_interface_id: symbol_id.into(),
                            },
                        ));
                    }

                    #[allow(clippy::significant_drop_in_scrutinee)]
                    match table
                        .representation
                        .structs
                        .get(symbol_id)
                        .unwrap()
                        .write()
                        .fields
                        .insert(
                            field_syn.identifier().span.str().to_owned(),
                            Field {
                                accessibility: field_accessibility,
                                name: field_syn
                                    .identifier()
                                    .span
                                    .str()
                                    .to_owned(),
                                r#type: ty,
                                span: Some(field_syn.identifier().span.clone()),
                            },
                        ) {
                        Ok(_) => {}
                        Err((existing, _)) => {
                            handler.receive(Box::new(DuplicatedField {
                                struct_id: symbol_id,
                                field_id: existing,
                                redeclaration_span: field_syn
                                    .identifier()
                                    .span
                                    .clone(),
                            }));
                        }
                    }
                }

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
