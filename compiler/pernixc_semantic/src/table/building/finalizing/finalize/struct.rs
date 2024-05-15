use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::Finalize;
use crate::{
    arena::ID,
    error::{self, DuplicatedField, PrivateEntityLeakedToPublicInterface},
    semantic::{
        normalizer::NoOp,
        session::{self, Limit},
        simplify::simplify,
        Environment,
    },
    symbol::{Accessibility, Field, GenericParameterVariances, Struct},
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

/// The fields of the struct are built and some of the variance information is
/// built
pub const STRUCTURAL_AND_PARTIAL_VARIANCE_STATE: usize = 2;

/// The complete information of the struct is built.
pub const COMPLETE_STATE: usize = 3;

/// Bounds check are performed
pub const CHECK_STATE: usize = 4;

impl Finalize for Struct {
    type SyntaxTree = syntax_tree::item::Struct;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = Occurrences;

    #[allow(
        clippy::too_many_lines,
        clippy::significant_drop_tightening,
        clippy::significant_drop_in_scrutinee
    )]
    fn finalize(
        table: &Table<Finalizer>,
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
                    syntax_tree.signature().where_clause().as_ref(),
                    data,
                    handler,
                );
            }

            STRUCTURAL_AND_PARTIAL_VARIANCE_STATE => {
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

                // build all the occurrences to partial
                data.build_all_occurrences_to::<build_preset::PartialComplete>(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );

                let premise =
                    table.get_active_premise(symbol_id.into()).unwrap();
                let mut session = session::Default::default();

                // simplify all the field types
                {
                    let fields_to_simplify = {
                        let struct_sym = table.get(symbol_id).unwrap();

                        struct_sym
                            .fields
                            .iter_primary()
                            .map(|(id, field)| (id, field.r#type.clone()))
                            .collect::<Vec<_>>()
                    };

                    for (id, field_type) in fields_to_simplify {
                        if let Ok(simplified) = simplify(
                            &field_type,
                            &Environment {
                                table,
                                premise: &premise,
                                normalizer: &NoOp,
                            },
                            &mut Limit::new(&mut session),
                        ) {
                            let mut struct_sym = table
                                .representation
                                .structs
                                .get(symbol_id)
                                .unwrap()
                                .write();

                            struct_sym.fields.get_mut(id).unwrap().r#type =
                                simplified;
                        }
                    }
                }

                // partial variance information
                {
                    let mut generic_parameter_variances =
                        GenericParameterVariances::default();
                    let struct_sym = table.get(symbol_id).unwrap();

                    #[allow(clippy::needless_collect)]
                    let type_usages = struct_sym
                        .fields
                        .values()
                        .map(|f| f.r#type.clone())
                        .collect::<Vec<_>>();

                    table.build_variance(
                        &struct_sym.generic_declaration.parameters,
                        &mut generic_parameter_variances,
                        &premise,
                        symbol_id.into(),
                        type_usages.iter(),
                        true,
                        handler,
                    );

                    // drop the read
                    drop(struct_sym);

                    // write the variances
                    let mut struct_sym = table
                        .representation
                        .structs
                        .get(symbol_id)
                        .unwrap()
                        .write();

                    struct_sym.generic_parameter_variances =
                        generic_parameter_variances;
                }
            }

            COMPLETE_STATE => {
                // build all the occurrences to complete
                data.build_all_occurrences_to::<build_preset::Complete>(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );

                // simplify all the types in the struct fields and build partial
                // variance
                let premise =
                    table.get_active_premise(symbol_id.into()).unwrap();

                let struct_sym = table.get(symbol_id).unwrap();
                let mut generic_parameter_variances =
                    struct_sym.generic_parameter_variances.clone();

                #[allow(clippy::needless_collect)]
                let type_usages = struct_sym
                    .fields
                    .values()
                    .map(|f| f.r#type.clone())
                    .collect::<Vec<_>>();

                table.build_variance(
                    &struct_sym.generic_declaration.parameters,
                    &mut generic_parameter_variances,
                    &premise,
                    symbol_id.into(),
                    type_usages.iter(),
                    false,
                    handler,
                );

                // drop the read
                drop(struct_sym);

                // write the variances
                let mut struct_sym = table
                    .representation
                    .structs
                    .get(symbol_id)
                    .unwrap()
                    .write();

                struct_sym.generic_parameter_variances =
                    generic_parameter_variances;
            }

            CHECK_STATE => {
                table.check_occurrences(symbol_id.into(), data, handler);
            }

            _ => panic!("invalid state flag"),
        }
    }
}
