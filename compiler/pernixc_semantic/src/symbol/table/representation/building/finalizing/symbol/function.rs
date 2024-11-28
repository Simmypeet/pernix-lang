use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use crate::{
    arena::ID,
    error,
    ir::representation::binding::Binder,
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    state::Finalize,
                    utility::{builder, occurrences::Occurrences},
                    Finalizer, FunctionKind,
                },
                RwLockContainer,
            },
            Building, Table,
        },
        Function, FunctionDefinition, FunctionIR,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// Where cluase predicates are built
#[allow(unused)]
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The function signature information is built, including parameters and return
/// type.
pub const SIGNATURE_STATE: usize = 2;

/// The intermediate representation of the function is built.
pub const INTERMEDIATE_REPRESENTATION_AND_CHECK_STATE: usize = 3;

impl Finalize for Function {
    type SyntaxTree = FunctionKind;
    const FINAL_STATE: usize = INTERMEDIATE_REPRESENTATION_AND_CHECK_STATE;
    type Data = (Occurrences, Occurrences, Occurrences);

    #[allow(clippy::too_many_lines, clippy::significant_drop_in_scrutinee)]
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
                // Create the generic parameters
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

            SIGNATURE_STATE => {
                // determine if the function is const
                if let FunctionKind::Normal(function) = syntax_tree {
                    *table
                        .representation
                        .functions
                        .get(symbol_id)
                        .unwrap()
                        .write()
                        .definition
                        .as_regular_mut()
                        .unwrap()
                        .0 = function.const_keyword().is_some();
                }

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

            INTERMEDIATE_REPRESENTATION_AND_CHECK_STATE => {
                // check all the occurrences
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

                // build the complete definition of the function
                if let FunctionKind::Normal(syntax_tree) = syntax_tree {
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

                    dbg!(binder.intermediate_representation());

                    *table
                        .representation
                        .functions
                        .get(symbol_id)
                        .unwrap()
                        .write()
                        .definition = FunctionDefinition::Regular {
                        const_function: syntax_tree.const_keyword().is_some(),
                        ir: match binder.finalize(handler) {
                            Ok(ir) => FunctionIR::Success(ir),
                            Err(ir) => FunctionIR::Suboptimal(ir),
                        },
                    };
                }
            }

            _ => panic!("invalid state flag"),
        }
    }
}
