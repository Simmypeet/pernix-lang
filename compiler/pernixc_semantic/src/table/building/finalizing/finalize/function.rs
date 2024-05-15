use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::Finalize;
use crate::{
    arena::ID,
    error,
    ir::{Building, IR},
    semantic::{
        normalizer::NoOp,
        session::{self, Limit},
        simplify::simplify,
        term::r#type::Type,
        Environment,
    },
    symbol::Function,
    table::{
        building::finalizing::{
            finalizer::build_preset, occurrences::Occurrences, Finalizer,
        },
        resolution, Table,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// Where cluase predicates are built
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The intermediate representation of the function is built.
pub const DEFINITION_AND_CHECK_STATE: usize = 2;

impl Finalize for Function {
    type SyntaxTree = syntax_tree::item::Function;
    const FINAL_STATE: usize = DEFINITION_AND_CHECK_STATE;
    type Data = Occurrences;

    #[allow(clippy::too_many_lines, clippy::significant_drop_in_scrutinee)]
    fn finalize(
        table: &Table<Finalizer>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        data: &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            GENERIC_PARAMETER_STATE => {
                // Create the generic parameters
                table.create_generic_parameters(
                    symbol_id,
                    syntax_tree.signature().generic_parameters().as_ref(),
                    data,
                    handler,
                );
            }

            WHERE_CLAUSE_STATE => table.create_where_clause_predicates(
                symbol_id,
                syntax_tree.signature().where_clause().as_ref(),
                data,
                handler,
            ),

            DEFINITION_AND_CHECK_STATE => {
                // build the parameters and return type on the local first to
                // obtain the occurences then build them
                let parameters = syntax_tree
                    .signature()
                    .parameters()
                    .parameter_list()
                    .iter()
                    .flat_map(ConnectedList::elements)
                    .map(|parameter| {
                        (
                            parameter,
                            table
                                .resolve_type(
                                    parameter.r#type(),
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
                                .unwrap_or_default(),
                        )
                    })
                    .collect::<Vec<_>>();

                let mut return_type = syntax_tree
                    .signature()
                    .return_type()
                    .as_ref()
                    .map_or_else(Type::default, |ty| {
                        table
                            .resolve_type(
                                ty.r#type(),
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
                            .unwrap_or_default()
                    });

                data.build_all_occurrences_to::<build_preset::Complete>(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );

                let mut session = session::Default::default();
                let premise =
                    table.get_active_premise(symbol_id.into()).unwrap();

                // simplify the return type
                {
                    if let Ok(simplified) = simplify(
                        &return_type,
                        &Environment {
                            premise: &premise,
                            table,
                            normalizer: &NoOp,
                        },
                        &mut Limit::new(&mut session),
                    ) {
                        return_type = simplified;
                    };

                    let mut function_write = table
                        .representation
                        .functions
                        .get(symbol_id)
                        .unwrap()
                        .write();

                    function_write.return_type = return_type;
                }

                let mut ir = IR::<Building>::default();

                // simplify the parameter type and create pattern
                {
                    /*
                    for (parameter_syn, mut parameter_ty) in parameters {
                        if let Ok(simplified) = simplify(
                            &parameter_ty,
                            &Environment { premise: &premise, table },
                            &mut Limit::new(&mut session),
                        ) {
                            parameter_ty = simplified;
                        }

                        // assign the parameter type
                        let parameter_id = {
                            let mut function_write = table
                                .representation
                                .functions
                                .get(symbol_id)
                                .unwrap()
                                .write();

                            function_write.parameters.insert(Parameter {
                                r#type: parameter_ty.clone(),
                                span: Some(parameter_syn.r#type().span()),
                            })
                        };

                        let pattern = ir
                            .representation
                            .create_irrefutable(
                                table,
                                parameter_syn.irrefutable_pattern(),
                                &parameter_ty,
                                &Address::Parameter(parameter_id),
                                ir.control_flow_graph().entry_block_id(),
                                symbol_id.into(),
                                handler,
                            )
                            .unwrap();

                        // assign the pattern of the parameter
                        {
                            let mut function_write = table
                                .representation
                                .functions
                                .get(symbol_id)
                                .unwrap()
                                .write();

                            assert!(function_write
                                .patterns_by_parameter_id
                                .insert(parameter_id, pattern)
                                .is_none());

                            drop(function_write);
                        }
                    }

                    */
                }

                // check all the occurrences
                table.check_occurrences(symbol_id.into(), data, handler);
            }

            _ => panic!("invalid state flag"),
        }
    }
}
