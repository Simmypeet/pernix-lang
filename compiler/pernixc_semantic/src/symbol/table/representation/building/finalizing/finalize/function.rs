use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::Finalize;
use crate::{
    arena::ID,
    error,
    ir::representation::building::binder::Binder,
    semantic::{
        normalizer::NoOp,
        session::{self, Limit},
        simplify::simplify,
        term::r#type::{self, Type},
        Environment,
    },
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    finalizer::{
                        self,
                        build_preset::{self, Complete},
                    },
                    occurrences::Occurrences,
                    Finalizer,
                },
                Index, RwLockContainer,
            },
            resolution, Building, Table,
        },
        Function, Parameter,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// Where cluase predicates are built
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The function signature information is built, including parameters and return
/// type.
pub const SIGNATURE_STATE: usize = 2;

/// The intermediate representation of the function is built.
pub const DEFINITION_AND_CHECK_STATE: usize = 3;

impl Finalize for Function {
    type SyntaxTree = syntax_tree::item::Function;
    const FINAL_STATE: usize = DEFINITION_AND_CHECK_STATE;
    type Data = Occurrences;

    #[allow(clippy::too_many_lines, clippy::significant_drop_in_scrutinee)]
    fn finalize(
        table: &Table<Building<RwLockContainer, Finalizer>>,
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

            SIGNATURE_STATE => {
                // determine if the function is const
                table
                    .representation
                    .functions
                    .get(symbol_id)
                    .unwrap()
                    .write()
                    .const_function = syntax_tree.const_keyword().is_some();

                // build the parameters
                for parameter in syntax_tree
                    .signature()
                    .parameters()
                    .parameter_list()
                    .iter()
                    .flat_map(ConnectedList::elements)
                {
                    let parameter_ty = table
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
                        .unwrap_or(r#type::Type::default());

                    let mut function_write = table
                        .representation
                        .functions
                        .get(symbol_id)
                        .unwrap()
                        .write();

                    let parameter_id =
                        function_write.parameters.insert(Parameter {
                            r#type: parameter_ty,
                            span: Some(parameter.span()),
                        });
                    function_write.parameter_order.push(parameter_id);
                }

                // build the return type
                {
                    let return_ty = syntax_tree
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

                    table
                        .representation
                        .functions
                        .get(symbol_id)
                        .unwrap()
                        .write()
                        .return_type = return_ty
                }

                // build the occurrences
                data.build_all_occurrences_to::<build_preset::Complete>(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );

                // simplify the parameter types
                let mut session = session::Default::default();
                {
                    let parameter_ids = {
                        let function_sym = table.get(symbol_id.into()).unwrap();

                        function_sym
                            .parameters
                            .iter()
                            .map(|(id, parameter)| {
                                (id, parameter.r#type.clone())
                            })
                            .collect::<Vec<_>>()
                    };

                    for (parameter_id, parameter_ty) in parameter_ids {
                        if let Ok(simplified) = simplify(
                            &parameter_ty,
                            &Environment {
                                premise: &table
                                    .get_active_premise(symbol_id.into())
                                    .unwrap(),
                                table,
                                normalizer: &NoOp,
                            },
                            &mut Limit::new(&mut session),
                        ) {
                            let mut function_write = table
                                .representation
                                .functions
                                .get(symbol_id)
                                .unwrap()
                                .write();

                            function_write
                                .parameters
                                .get_mut(parameter_id)
                                .unwrap()
                                .r#type = simplified;
                        }
                    }
                }

                // simplify the return type
                {
                    let return_ty = {
                        let function_sym = table.get(symbol_id.into()).unwrap();

                        function_sym.return_type.clone()
                    };

                    if let Ok(simplified) = simplify(
                        &return_ty,
                        &Environment {
                            premise: &table
                                .get_active_premise(symbol_id.into())
                                .unwrap(),
                            table,
                            normalizer: &NoOp,
                        },
                        &mut Limit::new(&mut session),
                    ) {
                        let mut function_write = table
                            .representation
                            .functions
                            .get(symbol_id)
                            .unwrap()
                            .write();

                        function_write.return_type = simplified;
                    }
                }
            }

            DEFINITION_AND_CHECK_STATE => {
                // check all the occurrences
                table.check_occurrences(symbol_id.into(), data, handler);

                // build the complete definition of the function
                {
                    let irrefutable_patterns = syntax_tree
                        .signature()
                        .parameters()
                        .parameter_list()
                        .as_ref()
                        .into_iter()
                        .flat_map(ConnectedList::elements)
                        .map(|x| x.irrefutable_pattern())
                        .collect::<Vec<_>>();

                    let mut binder = Binder::new_function(
                        table,
                        finalizer::Observer::<Complete>::default(),
                        symbol_id,
                        irrefutable_patterns.into_iter(),
                        syntax_tree.const_keyword().is_some(),
                        handler,
                    )
                    .unwrap();

                    for statement in syntax_tree.body().statements() {
                        binder.bind_statement(statement);
                    }
                }
            }

            _ => panic!("invalid state flag"),
        }
    }
}
