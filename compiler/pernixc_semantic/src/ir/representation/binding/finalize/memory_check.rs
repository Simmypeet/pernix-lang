use std::collections::HashMap;

use getset::Getters;
use pernixc_base::handler::Handler;

use super::state::{
    LatestLoad, Scope, SetStateError, SetStateSucceeded, State, Uninitialized,
};
use crate::{
    arena::ID,
    error::{UseAfterMove, UseBeforeInitialization},
    ir::{
        self,
        address::{Address, Memory},
        control_flow_graph::Block,
        instruction::Instruction,
        representation::binding::HandlerWrapper,
        scope,
        value::register::{Assignment, Register},
    },
    symbol::{
        table::{self, Table},
        CallableID, GlobalID,
    },
    type_system::{
        environment::Environment,
        normalizer::Normalizer,
        observer::Observer,
        predicate::{PositiveMarker, Predicate},
        term::{
            r#type::{Qualifier, Type},
            GenericArguments, Term,
        },
        well_formedness,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Stack {
    scopes: Vec<Scope>,
}

impl Stack {
    pub const fn new() -> Self { Self { scopes: Vec::new() } }

    pub fn new_scope(&mut self, scope_id: ID<scope::Scope>) {
        self.scopes.push(Scope::new(scope_id));
    }

    pub fn pop_scope(&mut self) -> Option<Scope> { self.scopes.pop() }

    pub fn set_initialized(
        &mut self,
        address: &Address<ir::Model>,
        table: &Table<impl table::State>,
    ) -> Result<SetStateSucceeded, SetStateError> {
        let root = address.get_root_memory();
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains(root) {
                return scope.set_initialized(address, table);
            }
        }

        // not found
        Err(SetStateError::InvalidAddress)
    }

    pub fn set_uninitialized(
        &mut self,
        address: &Address<ir::Model>,
        load_register_id: ID<Register<ir::Model>>,
        table: &Table<impl table::State>,
    ) -> Result<SetStateSucceeded, SetStateError> {
        let root = address.get_root_memory();
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains(root) {
                return scope.set_uninitialized(
                    address,
                    load_register_id,
                    table,
                );
            }
        }

        // not found
        Err(SetStateError::InvalidAddress)
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn current(&self) -> &Scope { self.scopes.last().unwrap() }
}

impl ir::Representation<ir::Model> {
    fn handle<S: table::State>(
        &mut self,
        block: ID<Block<ir::Model>>,
        inst_index: usize,
        stack: &mut Stack,
        visited: &mut Vec<ID<Block<ir::Model>>>,
        stack_states_by_scope_id: &mut HashMap<ID<scope::Scope>, Stack>,
        current_site: GlobalID,
        environment: &Environment<
            ir::Model,
            S,
            impl Normalizer<ir::Model, S>,
            impl Observer<ir::Model, S>,
        >,
        handler: &HandlerWrapper,
    ) {
        let block = self.control_flow_graph.get_block(block).unwrap();

        match &block.instructions()[inst_index] {
            Instruction::Store(store) => {
                stack
                    .current_mut()
                    .set_initialized(&store.address, environment.table())
                    .unwrap();
            }
            Instruction::RegisterAssignment(register_assignment) => {
                let register =
                    self.registers.get(register_assignment.id).unwrap();

                match &register.assignment {
                    Assignment::Load(load) => {
                        let ty = self
                            .type_of_address(
                                &load.address,
                                current_site,
                                environment,
                            )
                            .unwrap();

                        // has been checked previously
                        if load.address.get_reference_qualifier()
                            == Some(Qualifier::Immutable)
                            || load.address.is_behind_index()
                        {
                            return;
                        }

                        let copy_marker = environment
                            .table()
                            .get_by_qualified_name(["core", "Copy"].into_iter())
                            .unwrap()
                            .into_marker()
                            .unwrap();

                        // no need to move

                        if well_formedness::predicate_satisfied(
                            Predicate::PositiveMarker(PositiveMarker::new(
                                copy_marker,
                                GenericArguments {
                                    lifetimes: Vec::new(),
                                    types: vec![ty.result],
                                    constants: Vec::new(),
                                },
                            )),
                            None,
                            false,
                            environment,
                        )
                        .iter()
                        .all(well_formedness::Error::is_lifetime_constraints)
                        {
                            return;
                        }

                        let state = dbg!(stack
                            .set_uninitialized(
                                dbg!(&load.address),
                                register_assignment.id,
                                environment.table(),
                            )
                            .unwrap());

                        let latest_load = match state {
                            SetStateSucceeded::Unchanged(initialized) => {
                                initialized
                                    .as_false()
                                    .and_then(Uninitialized::latest_accessor)
                                    .map(LatestLoad::Moved)
                            }
                            SetStateSucceeded::Updated(state) => {
                                state.get_latest_load_register_id()
                            }
                        };

                        if let Some(latest_load) = latest_load {
                            match latest_load {
                                LatestLoad::Uninitialized => {
                                    handler.receive(Box::new(
                                        UseBeforeInitialization {
                                            use_span: register
                                                .span
                                                .clone()
                                                .unwrap(),
                                        },
                                    ));
                                }
                                LatestLoad::Moved(id) => {
                                    let prior_load_register =
                                        self.registers.get(id).unwrap();

                                    handler.receive(Box::new(UseAfterMove {
                                        use_span: register
                                            .span
                                            .clone()
                                            .unwrap(),
                                        move_span: prior_load_register
                                            .span
                                            .clone()
                                            .unwrap(),
                                    }));
                                }
                            }
                        }
                    }

                    Assignment::Tuple(_)
                    | Assignment::ReferenceOf(_)
                    | Assignment::Prefix(_)
                    | Assignment::Struct(_)
                    | Assignment::Variant(_)
                    | Assignment::FunctionCall(_)
                    | Assignment::Binary(_)
                    | Assignment::Array(_)
                    | Assignment::Phi(_)
                    | Assignment::Cast(_)
                    | Assignment::VariantNumber(_) => {}
                }
            }
            Instruction::AllocaDeclaration(alloca_declaration) => {
                assert!(stack.current_mut().new_state(
                    Memory::Alloca(alloca_declaration.id),
                    false,
                    self.allocas
                        .get(alloca_declaration.id)
                        .unwrap()
                        .r#type
                        .clone(),
                ));
            }

            Instruction::RegisterDiscard(_) | Instruction::TuplePack(_) => {}

            Instruction::ScopePush(scope_push) => {
                stack.new_scope(scope_push.0);

                'out: {
                    // if we're in the function and at the root scope,
                    // we need to initialize the parameters
                    if scope_push.0 == self.scope_tree.root_scope_id() {
                        let Ok(callable_id) =
                            CallableID::try_from(current_site)
                        else {
                            break 'out;
                        };

                        let callable_symbol = environment
                            .table()
                            .get_callable(callable_id)
                            .unwrap();

                        for (parameter_id, parameter) in callable_symbol
                            .parameter_order()
                            .iter()
                            .copied()
                            .map(|x| {
                                (
                                    x,
                                    callable_symbol
                                        .parameters()
                                        .get(x)
                                        .unwrap(),
                                )
                            })
                        {
                            assert!(stack.current_mut().new_state(
                                Memory::Parameter(parameter_id),
                                true,
                                Type::from_default_model(
                                    parameter.r#type.clone(),
                                ),
                            ));
                        }

                        dbg!(stack.current());
                    }
                }
            }

            Instruction::ScopePop(scope_pop) => {
                // record the stack state
                assert_eq!(stack.pop_scope().unwrap().scope_id(), scope_pop.0);

                let stack_snapshot = stack.clone();
                stack_states_by_scope_id.insert(scope_pop.0, stack_snapshot);
            }

            Instruction::Drop(_) => {}
        }
    }

    #[allow(clippy::too_many_lines, clippy::too_many_arguments)]
    fn walk<S: table::State>(
        &mut self,
        block_id: ID<Block<ir::Model>>,
        stack: &mut Stack,
        visited: &mut Vec<ID<Block<ir::Model>>>,
        stack_states_by_scope_id: &mut HashMap<ID<scope::Scope>, Stack>,
        current_site: GlobalID,
        environment: &Environment<
            ir::Model,
            S,
            impl Normalizer<ir::Model, S>,
            impl Observer<ir::Model, S>,
        >,
        handler: &HandlerWrapper,
    ) {
        if visited.contains(&block_id) {
            // TODO: handle loops
            return;
        }
        visited.push(block_id);

        let mut current_index = 0;
        while current_index
            < self
                .control_flow_graph
                .get_block(block_id)
                .unwrap()
                .instructions()
                .len()
        {
            self.handle(
                block_id,
                current_index,
                stack,
                visited,
                stack_states_by_scope_id,
                current_site,
                environment,
                handler,
            );

            current_index += 1;
        }
    }

    pub(super) fn memory_check<S: table::State>(
        &mut self,
        current_site: GlobalID,
        environment: &Environment<
            ir::Model,
            S,
            impl Normalizer<ir::Model, S>,
            impl Observer<ir::Model, S>,
        >,
        handler: &HandlerWrapper,
    ) {
        let mut stack = Stack::new();
        let mut visited = Vec::new();
        let mut stack_states_by_scope_id = HashMap::new();

        self.walk(
            self.control_flow_graph.entry_block_id(),
            &mut stack,
            &mut visited,
            &mut stack_states_by_scope_id,
            current_site,
            environment,
            handler,
        );
    }
}
