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
        Enum,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The variants of the enum are built and some of the variance information is
/// built
pub const PRE_DEFINITION_STATE: usize = 1;

/// The complete information of the struct is built.
pub const DEFINITION_STATE: usize = 2;

/// The information required to check the bounds is built. (the definition of
/// where caluses are built)
pub const WELL_FORMED_STATE: usize = 3;

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
        _table: &Table<Building<RwLockContainer, Finalizer>>,
        _symbol_id: ID<Self>,
        _state_flag: usize,
        _syntax_tree: &Self::SyntaxTree,
        _data: &mut Self::Data,
        _handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        // match state_flag {
        //     GENERIC_PARAMETER_STATE => table.create_generic_parameters(
        //         symbol_id,
        //         syntax_tree.generic_parameters().as_ref(),
        //         data,
        //         handler,
        //     ),

        //     WHERE_CLAUSE_STATE => {
        //         table.create_where_clause_predicates(
        //             symbol_id,
        //             syntax_tree.where_clause().as_ref(),
        //             data,
        //             handler,
        //         );
        //     }

        //     PRE_DEFINITION_STATE => {
        //         for variant in table
        //             .get(symbol_id)
        //             .unwrap()
        //             .variant_ids_by_name
        //             .values()
        //             .copied()
        //         {
        //             let _ = table.build_to(
        //                 variant,
        //                 Some(symbol_id.into()),
        //                 variant::COMPLETE_STATE,
        //                 handler,
        //             );
        //         }

        //         // build all the occurrences to partial complete
        //         data.build_all_occurrences_to::<builder::PartialComplete>(
        //             table,
        //             symbol_id.into(),
        //             handler,
        //         );

        //         let variant_ids = table
        //             .get(symbol_id)
        //             .unwrap()
        //             .variant_ids_by_name
        //             .values()
        //             .copied()
        //             .collect::<Vec<_>>();

        //         let (environment, _) = Environment::new(
        //             table.get_active_premise(symbol_id.into()).unwrap(),
        //             table,
        //             &NO_OP,
        //         );
        //         for variant_id in variant_ids {
        //             let Some(mut variant_ty) =
        //
        // table.get(variant_id).unwrap().associated_type.clone()
        //             else {
        //                 continue;
        //             };

        //             let variant_ty_syn = table
        //                 .get(variant_id)
        //                 .unwrap()
        //                 .syntax_tree
        //                 .as_ref()
        //                 .map(|x| {
        //                     x.association().as_ref().map(|x|
        // x.r#type().span())                 })
        //                 .unwrap()
        //                 .unwrap();

        //             variant_ty = environment
        //                 .simplify_and_check_lifetime_constraints(
        //                     &variant_ty,
        //                     &variant_ty_syn,
        //                     handler,
        //                 );

        //             table
        //                 .representation
        //                 .variants
        //                 .get(variant_id)
        //                 .unwrap()
        //                 .write()
        //                 .associated_type = Some(variant_ty);
        //         }

        //         // build the variance
        //         let premise =
        //             table.get_active_premise(symbol_id.into()).unwrap();
        //         let enum_sym = table.get(symbol_id).unwrap();

        //         #[allow(clippy::needless_collect)]
        //         let type_usages = enum_sym
        //             .variant_ids_by_name
        //             .values()
        //             .filter_map(|f| {
        //                 table.get(*f).unwrap().associated_type.clone()
        //             })
        //             .collect::<Vec<_>>();

        //         let mut generic_parameter_variances =
        //             GenericParameterVariances::default();

        //         table.build_variance(
        //             &enum_sym.generic_declaration.parameters,
        //             &mut generic_parameter_variances,
        //             premise,
        //             symbol_id.into(),
        //             type_usages.iter(),
        //             true,
        //             handler,
        //         );

        //         // drop the current read
        //         drop(enum_sym);

        //         // write the variance
        //         table
        //             .representation
        //             .enums
        //             .get(symbol_id)
        //             .unwrap()
        //             .write()
        //             .generic_parameter_variances =
        // generic_parameter_variances;     }

        //     DEFINITION_STATE => {
        //         // build all the occurrences to partial complete
        //         data.build_all_occurrences_to::<builder::Complete>(
        //             table,
        //             symbol_id.into(),
        //             handler,
        //         );

        //         // build the variance
        //         let premise =
        //             table.get_active_premise(symbol_id.into()).unwrap();
        //         let enum_sym = table.get(symbol_id).unwrap();

        //         #[allow(clippy::needless_collect)]
        //         let type_usages = enum_sym
        //             .variant_ids_by_name
        //             .values()
        //             .filter_map(|f| {
        //                 table.get(*f).unwrap().associated_type.clone()
        //             })
        //             .collect::<Vec<_>>();

        //         let mut generic_parameter_variances =
        //             enum_sym.generic_parameter_variances.clone();

        //         table.build_variance(
        //             &enum_sym.generic_declaration.parameters,
        //             &mut generic_parameter_variances,
        //             premise,
        //             symbol_id.into(),
        //             type_usages.iter(),
        //             false,
        //             handler,
        //         );

        //         // drop the read
        //         drop(enum_sym);

        //         // write the variance
        //         table
        //             .representation
        //             .enums
        //             .get(symbol_id)
        //             .unwrap()
        //             .write()
        //             .generic_parameter_variances =
        // generic_parameter_variances;     }

        //     CHECK_STATE => {
        //         table.check_occurrences(symbol_id.into(), data, handler);
        //         table.check_where_clause(symbol_id.into(), handler);
        //     }

        //     _ => panic!("invalid state flag"),
        // }
    }
}
