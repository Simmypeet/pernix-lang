use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{positive_trait_implementation, trait_function};
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
                Index, RwLockContainer, Table,
            },
            Building,
        },
        TraitImplementationFunction,
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
            generic_parameters_occurrences,
            where_clause_occurrences,
            signature_occurrences,
        ): &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            GENERIC_PARAMETER_STATE => {
                // build the parent trait implementation's generic parameters
                let parent_implementation_id = table
                    .trait_implementation_functions
                    .get(symbol_id)
                    .unwrap()
                    .read()
                    .parent_id;
                let _ = table.build_to(
                    parent_implementation_id,
                    Some(symbol_id.into()),
                    positive_trait_implementation::GENERIC_PARAMETER_STATE,
                    handler,
                );

                table.create_generic_parameters(
                    symbol_id,
                    syntax_tree.signature().generic_parameters().as_ref(),
                    generic_parameters_occurrences,
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
            }

            INTERMEDIATE_REPRESENTATION_AND_CEHCK_STATE => {
                let parent_implementation_id = table
                    .trait_implementation_functions
                    .get(symbol_id)
                    .unwrap()
                    .read()
                    .parent_id;
                let _ = table.build_to(
                    parent_implementation_id,
                    Some(symbol_id.into()),
                    positive_trait_implementation::ARGUMENT_STATE,
                    handler,
                );

                table.check_occurrences(
                    symbol_id.into(),
                    generic_parameters_occurrences,
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

                let trait_implementation_function_sym = table
                    .trait_implementation_functions
                    .get(symbol_id)
                    .unwrap()
                    .read();
                let trait_implementation_sym = table
                    .positive_trait_implementations
                    .get(trait_implementation_function_sym.parent_id)
                    .unwrap()
                    .read();
                let trait_sym = table.traits.get(trait_id).unwrap().read();

                // get the trait type id equivalent
                let Some(trait_function_id) = trait_sym
                    .member_ids_by_name
                    .get(trait_implementation_function_sym.name())
                    .copied()
                    .and_then(|x| x.into_function().ok())
                else {
                    return;
                };

                let _ = table.build_to(
                    trait_function_id,
                    Some(symbol_id.into()),
                    trait_function::WHERE_CLAUSE_STATE,
                    handler,
                );

                table.implementation_member_check(
                    symbol_id.into(),
                    trait_function_id.into(),
                    Instantiation::from_generic_arguments(
                        trait_implementation_sym.arguments.clone(),
                        trait_implementation_sym.implemented_id.into(),
                        &trait_sym.generic_declaration.parameters,
                    )
                    .unwrap_or_default(),
                    handler,
                );

                drop(trait_sym);
                drop(trait_implementation_function_sym);
                drop(trait_implementation_sym);

                #[allow(clippy::needless_collect)]
                let irrefutable_patterns = syntax_tree
                    .signature()
                    .parameters()
                    .parameter_list()
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

                for statement in syntax_tree.body().statements() {
                    let _ = binder.bind_statement(statement, handler);
                }

                dbg!(binder.intermediate_representation());
            }

            _ => panic!("invalid state flag"),
        }
    }
}
