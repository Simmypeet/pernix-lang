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
                    Finalizer,
                },
                RwLockContainer, Table,
            },
            Building,
        },
        AdtImplementationFunction,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The where clause of the trait implementation function is built.
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The function signature information is built, including parameters and return
/// type.
pub const SIGNATURE_STATE: usize = 2;

/// The intermediate representation of the function is built.
pub const INTERMEDIATE_REPRESENTATION_AND_CHECK_STATE: usize = 3;

impl Finalize for AdtImplementationFunction {
    type SyntaxTree = syntax_tree::item::Function;
    const FINAL_STATE: usize = INTERMEDIATE_REPRESENTATION_AND_CHECK_STATE;
    type Data = (Occurrences, Occurrences, Occurrences);

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

            SIGNATURE_STATE => table.build_function_parameters(
                symbol_id,
                syntax_tree.signature(),
                signature_occurrences,
                handler,
            ),

            INTERMEDIATE_REPRESENTATION_AND_CHECK_STATE => {
                // check all the occurrences
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
                    &signature_occurrences,
                    handler,
                );
                table.check_where_clause(symbol_id.into(), handler);

                let irrefutable_patterns = syntax_tree
                    .signature()
                    .parameters()
                    .parameter_list()
                    .as_ref()
                    .into_iter()
                    .flat_map(ConnectedList::elements)
                    .map(|x| x.irrefutable_pattern())
                    .collect::<Vec<_>>();

                let type_system_observer =
                    builder::TypeSystem::new(symbol_id.into(), handler);

                let mut binder = Binder::new_function(
                    table,
                    builder::Resolution::definition(),
                    type_system_observer,
                    symbol_id,
                    irrefutable_patterns.into_iter(),
                    handler,
                )
                .unwrap();

                for statement in syntax_tree.body().statements() {
                    let _ = binder.bind_statement(statement, handler);
                }

                dbg!(binder.intermediate_representation());
            }

            _ => panic!("invalid state flag"),
        }
    }
}
