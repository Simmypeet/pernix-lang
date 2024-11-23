use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::r#struct;
use crate::{
    arena::ID,
    error::{self, PrivateEntityLeakedToPublicInterface},
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    state::Finalize,
                    utility::{builder, occurrences::Occurrences},
                    Finalizer,
                },
                Index, RwLockContainer,
            },
            resolution::{self, Observer},
            Building, Table,
        },
        Enum, GenericParameterVariances, GlobalID,
    },
    type_system::{environment::Environment, normalizer},
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The where clause predicates are built.
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The variants of the enum are built and some of the variance information is
/// built
pub const PRE_DEFINITION_STATE: usize = 2;

/// The complete information of the struct is built.
pub const DEFINITION_STATE: usize = 3;

/// Bounds check are performed
pub const CHECK_STATE: usize = 4;

impl Finalize for Enum {
    type SyntaxTree = syntax_tree::item::Enum;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = (Occurrences, Occurrences, Occurrences);

    #[allow(
        clippy::significant_drop_in_scrutinee,
        clippy::significant_drop_tightening,
        clippy::too_many_lines
    )]
    fn finalize(
        table: &Table<Building<RwLockContainer, Finalizer>>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        (
            generic_parameter_occurrences,
            where_caluse_occurrences,
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
                    where_caluse_occurrences,
                    handler,
                );
            }

            PRE_DEFINITION_STATE => {
                for variant_id in table
                    .get(symbol_id)
                    .unwrap()
                    .variant_ids_by_name
                    .values()
                    .copied()
                {
                    let variant = table.get(variant_id).unwrap();
                    let syntax_tree = syntax_tree
                        .body()
                        .connected_list()
                        .as_ref()
                        .into_iter()
                        .flat_map(ConnectedList::elements)
                        .find(|x| {
                            &x.identifier().span
                                == variant.span.as_ref().unwrap()
                        })
                        .unwrap();
                    drop(variant);

                    if let Some(association) = syntax_tree.association() {
                        let ty = table
                            .resolve_type(
                                association.tree(),
                                symbol_id.into(),
                                resolution::Config {
                                    elided_lifetime_provider: None,
                                    elided_type_provider: None,
                                    elided_constant_provider: None,
                                    observer: Some(
                                        &mut builder::Resolution::basic()
                                            .chain(definition_occurrences),
                                    ),
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
                            .get(variant_id)
                            .unwrap()
                            .write()
                            .associated_type = Some(ty);
                    }
                }

                let definition_builder =
                    builder::TypeSystem::new(symbol_id.into(), handler);
                let variant_ids = table
                    .get(symbol_id)
                    .unwrap()
                    .variant_ids_by_name
                    .values()
                    .copied()
                    .collect::<Vec<_>>();

                let environment = Environment::new_with(
                    table.get_active_premise(symbol_id.into()).unwrap(),
                    table,
                    normalizer::NO_OP,
                    &definition_builder,
                )
                .0;

                for variant_id in variant_ids {
                    let Some(mut variant_ty) =
                        table.get(variant_id).unwrap().associated_type.clone()
                    else {
                        continue;
                    };

                    let variant = table.get(variant_id).unwrap();
                    let syntax_tree = syntax_tree
                        .body()
                        .connected_list()
                        .as_ref()
                        .into_iter()
                        .flat_map(ConnectedList::elements)
                        .find_map(|x| {
                            (&x.identifier().span
                                == variant.span.as_ref().unwrap())
                            .then(|| x.association().as_ref().unwrap())
                        })
                        .unwrap();
                    drop(variant);

                    variant_ty = environment
                        .simplify_and_check_lifetime_constraints(
                            &variant_ty,
                            &syntax_tree.span(),
                            handler,
                        );

                    table
                        .representation
                        .variants
                        .get(variant_id)
                        .unwrap()
                        .write()
                        .associated_type = Some(variant_ty);
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
                                r#struct::PRE_DEFINITION_STATE,
                                handler,
                            );
                        }

                        GlobalID::Enum(id) => {
                            let _ = table.build_to(
                                id,
                                Some(symbol_id.into()),
                                super::r#enum::PRE_DEFINITION_STATE,
                                handler,
                            );
                        }

                        _ => {}
                    }
                }

                // build the variance
                let premise =
                    table.get_active_premise(symbol_id.into()).unwrap();
                let enum_sym = table.get(symbol_id).unwrap();

                #[allow(clippy::needless_collect)]
                let type_usages = enum_sym
                    .variant_ids_by_name
                    .values()
                    .filter_map(|f| {
                        table.get(*f).unwrap().associated_type.clone()
                    })
                    .collect::<Vec<_>>();

                let mut generic_parameter_variances =
                    GenericParameterVariances::default();

                table.build_variance(
                    &enum_sym.generic_declaration.parameters,
                    &mut generic_parameter_variances,
                    premise,
                    symbol_id.into(),
                    type_usages.iter(),
                    true,
                    handler,
                );

                // drop the current read
                drop(enum_sym);

                // write the variance
                table
                    .representation
                    .enums
                    .get(symbol_id)
                    .unwrap()
                    .write()
                    .generic_parameter_variances = generic_parameter_variances;
            }

            DEFINITION_STATE => {
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
                                r#struct::DEFINITION_STATE,
                                handler,
                            );
                        }

                        GlobalID::Enum(id) => {
                            let _ = table.build_to(
                                id,
                                Some(symbol_id.into()),
                                super::r#enum::DEFINITION_STATE,
                                handler,
                            );
                        }

                        _ => {}
                    }
                }

                // build the variance
                let premise =
                    table.get_active_premise(symbol_id.into()).unwrap();
                let enum_sym = table.get(symbol_id).unwrap();

                #[allow(clippy::needless_collect)]
                let type_usages = enum_sym
                    .variant_ids_by_name
                    .values()
                    .filter_map(|f| {
                        table.get(*f).unwrap().associated_type.clone()
                    })
                    .collect::<Vec<_>>();

                let mut generic_parameter_variances =
                    enum_sym.generic_parameter_variances.clone();

                table.build_variance(
                    &enum_sym.generic_declaration.parameters,
                    &mut generic_parameter_variances,
                    premise,
                    symbol_id.into(),
                    type_usages.iter(),
                    false,
                    handler,
                );

                // drop the read
                drop(enum_sym);

                // write the variance
                table
                    .representation
                    .enums
                    .get(symbol_id)
                    .unwrap()
                    .write()
                    .generic_parameter_variances = generic_parameter_variances;
            }

            CHECK_STATE => {
                table.check_occurrences(
                    symbol_id.into(),
                    generic_parameter_occurrences,
                    handler,
                );
                table.check_occurrences(
                    symbol_id.into(),
                    where_caluse_occurrences,
                    handler,
                );
                table.check_occurrences(
                    symbol_id.into(),
                    definition_occurrences,
                    handler,
                );
                table.check_where_clause(symbol_id.into(), handler);
            }

            _ => panic!("invalid state flag"),
        }
    }
}
