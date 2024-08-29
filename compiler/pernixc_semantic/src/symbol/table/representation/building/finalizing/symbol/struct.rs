use std::collections::HashMap;

use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::r#enum;
use crate::{
    arena::ID,
    error::{self, FieldDuplication, PrivateEntityLeakedToPublicInterface},
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    state::Finalize,
                    utility::{builder, occurrences::Occurrences},
                    Finalizer,
                },
                Index, RwLockContainer, Table,
            },
            resolution::{self, Observer},
            Building,
        },
        Field, GenericParameterVariances, GlobalID, HierarchyRelationship,
        Struct,
    },
    type_system::{environment::Environment, normalizer},
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The where clause predicates are built.
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The fields of the struct are built and some of the variance information is
/// built
pub const PRE_DEFINITION_STATE: usize = 2;

/// The complete information of the struct is built.
pub const DEFINITION_STATE: usize = 3;

/// Bounds check are performed
pub const CHECK_STATE: usize = 4;

impl Finalize for Struct {
    type SyntaxTree = syntax_tree::item::Struct;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = (Occurrences, Occurrences, Occurrences);

    #[allow(
        clippy::too_many_lines,
        clippy::significant_drop_tightening,
        clippy::significant_drop_in_scrutinee
    )]
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
                    syntax_tree.signature().where_clause().as_ref(),
                    where_clause_occurrences,
                    handler,
                );
            }

            PRE_DEFINITION_STATE => {
                let mut field_syntax_trees_by_id = HashMap::new();

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
                                observer: Some(
                                    &mut (&mut builder::Resolution::basic())
                                        .chain(definition_occurrences),
                                ),
                                higher_ranked_lifetimes: None,
                            },
                            handler,
                        )
                        .unwrap_or_default();

                    let field_accessibility = table
                        .create_accessibility(
                            symbol_id.into(),
                            field_syn.access_modifier(),
                        )
                        .unwrap();

                    if let Ok(ty_accessibility) =
                        table.get_type_accessibility(&ty)
                    {
                        if table.accessibility_hierarchy_relationship(
                            ty_accessibility,
                            field_accessibility,
                        ) == Some(HierarchyRelationship::Child)
                        {
                            handler.receive(Box::new(
                                PrivateEntityLeakedToPublicInterface {
                                    entity: ty.clone(),
                                    entity_overall_accessibility:
                                        ty_accessibility,
                                    leaked_span: field_syn.r#type().span(),
                                    public_interface_id: symbol_id.into(),
                                },
                            ));
                        }
                    }

                    let field = Field {
                        accessibility: field_accessibility,
                        name: field_syn.identifier().span.str().to_owned(),
                        r#type: ty,
                        span: Some(field_syn.identifier().span.clone()),
                    };

                    #[allow(clippy::significant_drop_in_scrutinee)]
                    match table
                        .representation
                        .structs
                        .get(symbol_id)
                        .unwrap()
                        .write()
                        .insert_field(field)
                    {
                        Ok(id) => {
                            assert!(field_syntax_trees_by_id
                                .insert(id, field_syn)
                                .is_none());
                        }

                        Err((existing, _)) => {
                            handler.receive(Box::new(FieldDuplication {
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

                // simplify all the types
                let definition_builder =
                    builder::TypeSystem::new(symbol_id.into(), handler);
                let environment = Environment::new_with(
                    table.get_active_premise(symbol_id.into()).unwrap(),
                    table,
                    normalizer::NO_OP,
                    &definition_builder,
                )
                .0;

                for (field_id, field_syn) in &field_syntax_trees_by_id {
                    let mut field_ty = table.get(symbol_id).unwrap().fields
                        [*field_id]
                        .r#type
                        .clone();

                    field_ty = environment
                        .simplify_and_check_lifetime_constraints(
                            &field_ty,
                            &field_syn.r#type().span(),
                            handler,
                        );

                    table
                        .representation
                        .structs
                        .get(symbol_id)
                        .unwrap()
                        .write()
                        .fields[*field_id]
                        .r#type = field_ty;
                }

                for id in definition_occurrences
                    .resolutions()
                    .iter()
                    .map(|x| x.0.global_id())
                {
                    match id {
                        GlobalID::Struct(id) => {
                            let _ = table.build_to(
                                id,
                                Some(symbol_id.into()),
                                super::r#struct::PRE_DEFINITION_STATE,
                                handler,
                            );
                        }

                        GlobalID::Enum(id) => {
                            let _ = table.build_to(
                                id,
                                Some(symbol_id.into()),
                                r#enum::PRE_DEFINITION_STATE,
                                handler,
                            );
                        }

                        _ => {}
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
                        table.get_active_premise(symbol_id.into()).unwrap(),
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

            DEFINITION_STATE => {
                // build all the occurrences to complete
                for id in definition_occurrences
                    .resolutions()
                    .iter()
                    .map(|x| x.0.global_id())
                {
                    match id {
                        GlobalID::Struct(id) => {
                            let _ = table.build_to(
                                id,
                                Some(symbol_id.into()),
                                super::r#struct::DEFINITION_STATE,
                                handler,
                            );
                        }

                        GlobalID::Enum(id) => {
                            let _ = table.build_to(
                                id,
                                Some(symbol_id.into()),
                                r#enum::DEFINITION_STATE,
                                handler,
                            );
                        }

                        _ => {}
                    }
                }

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
                    premise,
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
