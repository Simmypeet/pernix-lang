use pernixc_base::diagnostic::Handler;

use super::Finalize;
use crate::{
    arena::ID,
    error,
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    occurrences::Occurrences, Finalizer, FunctionKind,
                },
                RwLockContainer,
            },
            Building, Table,
        },
        Function,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// Where cluase predicates are built
#[allow(unused)]
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The function signature information is built, including parameters and return
/// type.
pub const DEFINITION_STATE: usize = 2;

/// The information required to check the bounds is built. (the definition of
/// where caluses are built)
#[allow(unused)]
pub const WELL_FORMED_STATE: usize = 3;

/// The intermediate representation of the function is built.
pub const INTERMEDIATE_REPRESENTATION_AND_CHECK_STATE: usize = 4;

impl Finalize for Function {
    type SyntaxTree = FunctionKind;
    const FINAL_STATE: usize = INTERMEDIATE_REPRESENTATION_AND_CHECK_STATE;
    type Data = Occurrences;

    #[allow(clippy::too_many_lines, clippy::significant_drop_in_scrutinee)]
    fn finalize(
        _table: &Table<Building<RwLockContainer, Finalizer>>,
        _symbol_id: ID<Self>,
        _state_flag: usize,
        _syntax_tree: &Self::SyntaxTree,
        _data: &mut Self::Data,
        _handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        // match state_flag {
        //     GENERIC_PARAMETER_STATE => {
        //         // Create the generic parameters
        //         table.create_generic_parameters(
        //             symbol_id,
        //             syntax_tree.signature().generic_parameters().as_ref(),
        //             data,
        //             handler,
        //         );
        //     }

        //     WHERE_CLAUSE_STATE => table.create_where_clause_predicates(
        //         symbol_id,
        //         syntax_tree.signature().where_clause().as_ref(),
        //         data,
        //         handler,
        //     ),

        //     DEFINITION_STATE => {
        //         // determine if the function is const
        //         if let FunctionKind::Normal(function) = syntax_tree {
        //             *table
        //                 .representation
        //                 .functions
        //                 .get(symbol_id)
        //                 .unwrap()
        //                 .write()
        //                 .definition
        //                 .as_regular_mut()
        //                 .unwrap()
        //                 .0 = function.const_keyword().is_some();
        //         }

        //         let active_premise =
        //             table.get_active_premise(symbol_id.into()).unwrap();
        //         let (environment, _) =
        //             Environment::new(active_premise, table, &NO_OP);

        //         // build the parameters
        //         for parameter in syntax_tree
        //             .signature()
        //             .parameters()
        //             .parameter_list()
        //             .iter()
        //             .flat_map(ConnectedList::elements)
        //         {
        //             let parameter_ty = table
        //                 .resolve_type(
        //                     parameter.r#type(),
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
        //                 .unwrap_or(Type::Error(term::Error));

        //             // build the occurrences
        //             data.build_all_occurrences_to::<builder::Complete>(
        //                 table,
        //                 symbol_id.into(),
        //                 handler,
        //             );

        //             let mut function_write = table
        //                 .representation
        //                 .functions
        //                 .get(symbol_id)
        //                 .unwrap()
        //                 .write();

        //             let parameter_id =
        //                 function_write.parameters.insert(Parameter {
        //                     r#type: environment
        //                         .simplify_and_check_lifetime_constraints(
        //                             &parameter_ty,
        //                             &parameter.r#type().span(),
        //                             handler,
        //                         ),
        //                     span: Some(parameter.span()),
        //                 });
        //             function_write.parameter_order.push(parameter_id);
        //         }

        //         // build the return type
        //         {
        //             if let Some(return_type_syn) =
        //                 syntax_tree.signature().return_type()
        //             {
        //                 let return_type = table
        //                     .resolve_type(
        //                         return_type_syn.r#type(),
        //                         symbol_id.into(),
        //                         resolution::Config {
        //                             ellided_lifetime_provider: None,
        //                             ellided_type_provider: None,
        //                             ellided_constant_provider: None,
        //                             observer: Some(data),
        //                             higher_ranked_lifetimes: None,
        //                         },
        //                         handler,
        //                     )
        //                     .unwrap();

        //                 data.build_all_occurrences_to::<builder::Complete>(
        //                     table,
        //                     symbol_id.into(),
        //                     handler,
        //                 );

        //                 table
        //                     .representation
        //                     .functions
        //                     .get(symbol_id)
        //                     .unwrap()
        //                     .write()
        //                     .return_type = environment
        //                     .simplify_and_check_lifetime_constraints(
        //                         &return_type,
        //                         &return_type_syn.r#type().span(),
        //                         handler,
        //                     );
        //             } else {
        //                 table
        //                     .representation
        //                     .functions
        //                     .get(symbol_id)
        //                     .unwrap()
        //                     .write()
        //                     .return_type =
        // Type::Tuple(term::Tuple::default());             }
        //         }

        //         // build the occurrences
        //         data.build_all_occurrences_to::<builder::Complete>(
        //             table,
        //             symbol_id.into(),
        //             handler,
        //         );
        //     }

        //     INTERMEDIATE_REPRESENTATION_AND_CHECK_STATE => {
        //         // check all the occurrences
        //         table.check_occurrences(symbol_id.into(), data, handler);
        //         table.check_where_clause(symbol_id.into(), handler);

        //         // build the complete definition of the function

        //         if let FunctionKind::Normal(syntax_tree) = syntax_tree {
        //             let irrefutable_patterns = syntax_tree
        //                 .signature()
        //                 .parameters()
        //                 .parameter_list()
        //                 .as_ref()
        //                 .into_iter()
        //                 .flat_map(ConnectedList::elements)
        //                 .map(|x| x.irrefutable_pattern())
        //                 .collect::<Vec<_>>();

        //             let mut binder = Binder::new_function(
        //                 table,
        //                 finalizer::Observer::<Complete>::default(),
        //                 symbol_id,
        //                 irrefutable_patterns.into_iter(),
        //                 handler,
        //             )
        //             .unwrap();

        //             for statement in syntax_tree.body().statements() {
        //                 let _ = binder.bind_statement(statement, handler);
        //             }

        //             dbg!(binder.intermediate_representation());
        //         }
        //     }

        //     _ => panic!("invalid state flag"),
        // }
    }
}
