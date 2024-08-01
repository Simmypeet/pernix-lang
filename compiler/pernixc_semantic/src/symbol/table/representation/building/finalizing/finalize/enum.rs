use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{variant, Finalize};
use crate::{
    arena::ID,
    error,
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    finalizer::build_preset, occurrences::Occurrences,
                    Finalizer,
                },
                Index, RwLockContainer,
            },
            Building, Table,
        },
        Enum, GenericParameterVariances,
    },
    type_system::{environment::Environment, normalizer::NO_OP},
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// Where cluase predicates are built
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The variants of the enum are built and some of the variance information is
/// built
pub const STRUCTURAL_AND_PARTIAL_VARIANCE_STATE: usize = 2;

/// The complete information of the struct is built.
pub const COMPLETE_STATE: usize = 3;

/// Bounds check are performed
pub const CHECK_STATE: usize = 4;

impl Finalize for Enum {
    type SyntaxTree = syntax_tree::item::EnumSignature;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = Occurrences;

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
        data: &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            GENERIC_PARAMETER_STATE => table.create_generic_parameters(
                symbol_id,
                syntax_tree.generic_parameters().as_ref(),
                data,
                handler,
            ),

            WHERE_CLAUSE_STATE => {
                table.create_where_clause_predicates(
                    symbol_id,
                    syntax_tree.where_clause().as_ref(),
                    data,
                    handler,
                );
            }

            STRUCTURAL_AND_PARTIAL_VARIANCE_STATE => {
                for variant in table
                    .get(symbol_id)
                    .unwrap()
                    .variant_ids_by_name
                    .values()
                    .copied()
                {
                    let _ = table.build_to(
                        variant,
                        Some(symbol_id.into()),
                        variant::COMPLETE_STATE,
                        false,
                        handler,
                    );
                }

                // build all the occurrences to partial complete
                data.build_all_occurrences_to::<build_preset::PartialComplete>(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );

                let variant_ids = table
                    .get(symbol_id)
                    .unwrap()
                    .variant_ids_by_name
                    .values()
                    .copied()
                    .collect::<Vec<_>>();

                let (environment, _) = Environment::new(
                    table.get_active_premise(symbol_id.into()).unwrap(),
                    table,
                    &NO_OP,
                );
                for variant_id in variant_ids {
                    let Some(mut variant_ty) =
                        table.get(variant_id).unwrap().associated_type.clone()
                    else {
                        continue;
                    };

                    let variant_ty_syn = table
                        .get(variant_id)
                        .unwrap()
                        .syntax_tree
                        .as_ref()
                        .map(|x| {
                            x.association().as_ref().map(|x| x.r#type().span())
                        })
                        .unwrap()
                        .unwrap();

                    variant_ty = environment
                        .simplify_and_check_lifetime_constraints(
                            &variant_ty,
                            &variant_ty_syn,
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

            COMPLETE_STATE => {
                // build all the occurrences to partial complete
                data.build_all_occurrences_to::<build_preset::Complete>(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );

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
                table.check_occurrences(symbol_id.into(), data, handler);
                table.check_where_clause(symbol_id.into(), handler);
            }

            _ => panic!("invalid state flag"),
        }
    }
}
