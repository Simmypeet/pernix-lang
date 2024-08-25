use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::Finalize;
use crate::{
    arena::ID,
    error::{self},
    symbol::{
        table::{
            representation::{
                building::finalizing::{occurrences::Occurrences, Finalizer},
                RwLockContainer, Table,
            },
            Building,
        },
        Struct,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The fields of the struct are built and some of the variance information is
/// built
pub const PRE_DEFINITION_STATE: usize = 1;

/// The complete information of the struct is built.
pub const DEFINITION_STATE: usize = 2;

/// The information required to check the bounds is built. (the definition of
/// where caluses are built)
#[allow(unused)]
pub const WELL_FORMED_STATE: usize = 3;

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
        //         syntax_tree.signature().generic_parameters().as_ref(),
        //         data,
        //         handler,
        //     ),

        //     WHERE_CLAUSE_STATE => {
        //         table.create_where_clause_predicates(
        //             symbol_id,
        //             syntax_tree.signature().where_clause().as_ref(),
        //             data,
        //             handler,
        //         );
        //     }

        //     PRE_DEFINITION_STATE => {
        //         let mut field_syntax_trees_by_id = HashMap::new();

        //         for field_syn in syntax_tree
        //             .body()
        //             .field_list()
        //             .iter()
        //             .flat_map(ConnectedList::elements)
        //         {
        //             let ty = table
        //                 .resolve_type(
        //                     field_syn.r#type(),
        //                     symbol_id.into(),
        //                     resolution::Config {
        //                         ellided_lifetime_provider: None,
        //                         ellided_type_provider: None,
        //                         ellided_constant_provider: None,
        //                         observer: Some(data),
        //                         higher_ranked_lifetimes: None,
        //                     },
        //                     handler,
        //                 )
        //                 .unwrap_or_default();

        //             let field_accessibility = table
        //                 .create_accessibility(
        //                     symbol_id.into(),
        //                     field_syn.access_modifier(),
        //                 )
        //                 .unwrap();

        //             if let Ok(ty_accessibility) =
        //                 table.get_type_accessibility(&ty)
        //             {
        //                 if table.accessibility_hierarchy_relationship(
        //                     ty_accessibility,
        //                     field_accessibility,
        //                 ) == Some(HierarchyRelationship::Child)
        //                 {
        //                     handler.receive(Box::new(
        //                         PrivateEntityLeakedToPublicInterface {
        //                             entity: ty.clone(),
        //                             entity_overall_accessibility:
        //                                 ty_accessibility,
        //                             leaked_span: field_syn.r#type().span(),
        //                             public_interface_id: symbol_id.into(),
        //                         },
        //                     ));
        //                 }
        //             }

        //             let field = Field {
        //                 accessibility: field_accessibility,
        //                 name: field_syn.identifier().span.str().to_owned(),
        //                 r#type: ty,
        //                 span: Some(field_syn.identifier().span.clone()),
        //             };

        //             #[allow(clippy::significant_drop_in_scrutinee)]
        //             match table
        //                 .representation
        //                 .structs
        //                 .get(symbol_id)
        //                 .unwrap()
        //                 .write()
        //                 .insert_field(field)
        //             {
        //                 Ok(id) => {
        //                     assert!(field_syntax_trees_by_id
        //                         .insert(id, field_syn)
        //                         .is_none());
        //                 }

        //                 Err((existing, _)) => {
        //                     handler.receive(Box::new(DuplicatedField {
        //                         struct_id: symbol_id,
        //                         field_id: existing,
        //                         redeclaration_span: field_syn
        //                             .identifier()
        //                             .span
        //                             .clone(),
        //                     }));
        //                 }
        //             }
        //         }

        //         // build all the occurrences to partial
        //         data.build_all_occurrences_to::<builder::PartialComplete>(
        //             table,
        //             symbol_id.into(),
        //             handler,
        //         );

        //         // simplify all the types
        //         let (environment, _) = Environment::new(
        //             table.get_active_premise(symbol_id.into()).unwrap(),
        //             table,
        //             &NO_OP,
        //         );
        //         for (field_id, field_syn) in &field_syntax_trees_by_id {
        //             let mut field_ty = table.get(symbol_id).unwrap().fields
        //                 [*field_id]
        //                 .r#type
        //                 .clone();

        //             field_ty = environment
        //                 .simplify_and_check_lifetime_constraints(
        //                     &field_ty,
        //                     &field_syn.r#type().span(),
        //                     handler,
        //                 );

        //             table
        //                 .representation
        //                 .structs
        //                 .get(symbol_id)
        //                 .unwrap()
        //                 .write()
        //                 .fields[*field_id]
        //                 .r#type = field_ty;
        //         }

        //         // partial variance information
        //         {
        //             let mut generic_parameter_variances =
        //                 GenericParameterVariances::default();
        //             let struct_sym = table.get(symbol_id).unwrap();

        //             #[allow(clippy::needless_collect)]
        //             let type_usages = struct_sym
        //                 .fields
        //                 .values()
        //                 .map(|f| f.r#type.clone())
        //                 .collect::<Vec<_>>();

        //             table.build_variance(
        //                 &struct_sym.generic_declaration.parameters,
        //                 &mut generic_parameter_variances,
        //                 table.get_active_premise(symbol_id.into()).unwrap(),
        //                 symbol_id.into(),
        //                 type_usages.iter(),
        //                 true,
        //                 handler,
        //             );

        //             // drop the read
        //             drop(struct_sym);

        //             // write the variances
        //             let mut struct_sym = table
        //                 .representation
        //                 .structs
        //                 .get(symbol_id)
        //                 .unwrap()
        //                 .write();

        //             struct_sym.generic_parameter_variances =
        //                 generic_parameter_variances;
        //         }
        //     }

        //     DEFINITION_STATE => {
        //         // build all the occurrences to complete
        //         data.build_all_occurrences_to::<builder::Complete>(
        //             table,
        //             symbol_id.into(),
        //             handler,
        //         );

        //         // simplify all the types in the struct fields and build
        // partial         // variance
        //         let premise =
        //             table.get_active_premise(symbol_id.into()).unwrap();

        //         let struct_sym = table.get(symbol_id).unwrap();
        //         let mut generic_parameter_variances =
        //             struct_sym.generic_parameter_variances.clone();

        //         #[allow(clippy::needless_collect)]
        //         let type_usages = struct_sym
        //             .fields
        //             .values()
        //             .map(|f| f.r#type.clone())
        //             .collect::<Vec<_>>();

        //         table.build_variance(
        //             &struct_sym.generic_declaration.parameters,
        //             &mut generic_parameter_variances,
        //             premise,
        //             symbol_id.into(),
        //             type_usages.iter(),
        //             false,
        //             handler,
        //         );

        //         // drop the read
        //         drop(struct_sym);

        //         // write the variances
        //         let mut struct_sym = table
        //             .representation
        //             .structs
        //             .get(symbol_id)
        //             .unwrap()
        //             .write();

        //         struct_sym.generic_parameter_variances =
        //             generic_parameter_variances;
        //     }

        //     CHECK_STATE => {
        //         table.check_occurrences(symbol_id.into(), data, handler);
        //         table.check_where_clause(symbol_id.into(), handler);
        //     }

        //     _ => panic!("invalid state flag"),
        // }
    }
}
