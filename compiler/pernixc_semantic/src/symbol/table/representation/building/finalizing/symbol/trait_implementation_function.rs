use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{positive_trait_implementation, trait_function};
use crate::{
    arena::ID,
    error::{self, MismatchedFunctionParameterCountInImplementation},
    ir::representation::binding::Binder,
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
            Building,
        },
        FunctionIR, TraitImplementationFunction,
    },
    type_system::instantiation::Instantiation,
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The where clause of the trait implementation function is built.
#[allow(unused)]
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The function signature information is built, including parameters and return
/// type.
pub const DEFINITION_STATE: usize = 2;

/// The intermediate representation of the function is built.
pub const INTERMEDIATE_REPRESENTATION_AND_CEHCK_STATE: usize = 3;

impl Finalize for TraitImplementationFunction {
    type SyntaxTree = syntax_tree::item::Function;
    const FINAL_STATE: usize = INTERMEDIATE_REPRESENTATION_AND_CEHCK_STATE;
    type Data = (Occurrences, Occurrences, Occurrences);

    #[allow(clippy::too_many_lines)]
    fn finalize(
        table: &Table<Building<RwLockContainer, Finalizer>>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        (
            generic_parameter_occurrences,
            where_clause_occurrences,
            signature_occurrences,
        ): &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            GENERIC_PARAMETER_STATE => {
                // build the parent trait implementation's generic parameters
                let parent_implementation_id =
                    table.get(symbol_id).unwrap().parent_id;
                let _ = table.build_to(
                    parent_implementation_id,
                    Some(symbol_id.into()),
                    positive_trait_implementation::GENERIC_PARAMETER_STATE,
                    handler,
                );

                table.create_generic_parameters(
                    symbol_id,
                    syntax_tree.signature().generic_parameters().as_ref(),
                    generic_parameter_occurrences,
                    handler,
                );
            }

            WHERE_CLAUSE_STATE => table.create_where_clause(
                symbol_id,
                syntax_tree.signature().where_clause().as_ref(),
                where_clause_occurrences,
                handler,
            ),

            DEFINITION_STATE => {
                table.build_function_signature(
                    symbol_id,
                    syntax_tree.signature(),
                    signature_occurrences,
                    handler,
                );

                for global_id in generic_parameter_occurrences
                    .resolutions()
                    .iter()
                    .chain(where_clause_occurrences.resolutions().iter())
                    .chain(signature_occurrences.resolutions().iter())
                    .map(|x| x.0.global_id())
                {
                    let _ = builder::build_for_definition(
                        table,
                        global_id,
                        Some(symbol_id.into()),
                        handler,
                    );
                }
            }

            INTERMEDIATE_REPRESENTATION_AND_CEHCK_STATE => {
                let parent_implementation_id =
                    table.get(symbol_id).unwrap().parent_id;
                let _ = table.build_to(
                    parent_implementation_id,
                    Some(symbol_id.into()),
                    positive_trait_implementation::ARGUMENT_STATE,
                    handler,
                );

                table.check_occurrences(
                    symbol_id.into(),
                    generic_parameter_occurrences,
                    handler,
                );
                table.check_occurrences(
                    symbol_id.into(),
                    where_clause_occurrences,
                    handler,
                );
                table.check_occurrences(
                    symbol_id.into(),
                    signature_occurrences,
                    handler,
                );
                table.check_where_clause(symbol_id.into(), handler);

                let trait_id = {
                    let trait_implementation_function_sym =
                        table.get(symbol_id).unwrap();
                    let trait_implementation_sym = table
                        .get(trait_implementation_function_sym.parent_id)
                        .unwrap();

                    trait_implementation_sym.implemented_id
                };

                let trait_implementation_id =
                    table.get(symbol_id).unwrap().parent_id;

                // get the trait function id equivalent
                let Some(trait_function_id) = table
                    .get(trait_id)
                    .unwrap()
                    .member_ids_by_name
                    .get(table.get(symbol_id).unwrap().name())
                    .copied()
                    .and_then(|x| x.into_function().ok())
                else {
                    return;
                };

                let _ = table.build_to(
                    trait_function_id,
                    Some(symbol_id.into()),
                    trait_function::DEFINITION_STATE,
                    handler,
                );

                // do basic check, generic parameters and where caluse.
                let implementation_generic_arguments = table
                    .get(trait_implementation_id)
                    .unwrap()
                    .arguments
                    .clone();
                let implementation_instantiation =
                    Instantiation::from_generic_arguments(
                        implementation_generic_arguments,
                        trait_id.into(),
                        &table
                            .get(trait_id)
                            .unwrap()
                            .generic_declaration
                            .parameters,
                    )
                    .unwrap_or_default();
                table.implementation_member_check(
                    symbol_id.into(),
                    trait_function_id.into(),
                    implementation_instantiation,
                    handler,
                );

                // do function signature match check
                {
                    let trait_function_sym =
                        table.get(trait_function_id).unwrap();

                    let trait_implementation_function_sym =
                        table.get(symbol_id).unwrap();

                    // mismatched parameter count
                    if trait_function_sym.parameter_order.len()
                        != trait_implementation_function_sym
                            .parameter_order
                            .len()
                    {
                        handler.receive(Box::new(
                            MismatchedFunctionParameterCountInImplementation {
                                trait_function_id,
                                expected_count: trait_function_sym
                                    .parameter_order
                                    .len(),
                                found_count: trait_implementation_function_sym
                                    .parameter_order
                                    .len(),
                                span: syntax_tree.signature().span(),
                            },
                        ));
                    }
                }

                #[allow(clippy::needless_collect)]
                let irrefutable_patterns = syntax_tree
                    .signature()
                    .parameters()
                    .connected_list()
                    .as_ref()
                    .into_iter()
                    .flat_map(ConnectedList::elements)
                    .map(syntax_tree::item::Parameter::irrefutable_pattern)
                    .collect::<Vec<_>>();

                let mut binder = Binder::new_function(
                    table,
                    builder::Resolution::definition(),
                    builder::TypeSystem::new(symbol_id.into(), handler),
                    symbol_id,
                    irrefutable_patterns.into_iter(),
                    handler,
                )
                .unwrap();

                for statement in syntax_tree.statements().tree() {
                    let _ = binder.bind_statement(statement, handler);
                }

                table
                    .representation
                    .trait_implementation_functions
                    .get(symbol_id)
                    .unwrap()
                    .write()
                    .ir = match binder.finalize(handler) {
                    Ok(ir) => FunctionIR::Success(ir),
                    Err(_) => return,
                };
            }

            _ => panic!("invalid state flag"),
        }
    }
}
