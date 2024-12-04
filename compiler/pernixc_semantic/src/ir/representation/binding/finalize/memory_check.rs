use std::collections::{hash_map::Entry, HashMap};

use getset::Getters;
use pernixc_base::{handler::Handler, source_file::Span};

use super::state::{self, Scope, SetStateError, SetStateSucceeded, Summary};
use crate::{
    arena::ID,
    error::{
        MovedOutValueFromMutableReference, UseAfterMove,
        UseBeforeInitialization,
    },
    ir::{
        self,
        address::{Address, Memory},
        control_flow_graph::Block,
        instruction::Instruction,
        representation::{binding::HandlerWrapper, Values},
        scope,
        value::{
            register::{Assignment, Load, ReferenceOf},
            Value,
        },
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

    pub fn get_state(
        &self,
        address: &Address<ir::Model>,
    ) -> Option<&state::State> {
        let root = address.get_root_memory();

        for scope in self.scopes.iter().rev() {
            if scope.contains(root) {
                return scope.get_state(address);
            }
        }

        // not found
        None
    }

    pub fn set_uninitialized(
        &mut self,
        address: &Address<ir::Model>,
        move_span: Span,
        table: &Table<impl table::State>,
    ) -> Result<SetStateSucceeded, SetStateError> {
        let root = address.get_root_memory();
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains(root) {
                return scope.set_uninitialized(address, move_span, table);
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct PopSnapshot {
    stack_snapshot: Stack,
    poped_scope: Scope,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ScopeMemory {
    before_push_stack_snapshot: Stack,
    after_pop_stack_snapshot: Option<PopSnapshot>,
}

impl Values<ir::Model> {
    fn handle_reference_of(
        reference_of: &ReferenceOf<ir::Model>,
        register_span: Span,
        stack: &mut Stack,
        handler: &HandlerWrapper,
    ) {
        let state =
            stack.get_state(&reference_of.address).unwrap().get_state_summary();

        match state {
            Summary::Uninitialized => {
                handler.receive(Box::new(UseBeforeInitialization {
                    use_span: register_span,
                }));
            }

            Summary::Moved(span) => {
                handler.receive(Box::new(UseAfterMove {
                    use_span: register_span,
                    move_span: span,
                }));
            }

            Summary::Initialized => {}
        }
    }

    fn handle_store<S: table::State>(
        &self,
        store_address: &Address<ir::Model>,
        store_span: Span,
        stack: &mut Stack,
        table: &Table<S>,
        handler: &HandlerWrapper,
    ) -> Vec<Instruction<ir::Model>> {
        let state = stack.set_initialized(&store_address, table).unwrap();

        let (state, target_address) = match state {
            SetStateSucceeded::Unchanged(initialized, address) => {
                (state::State::Total(initialized), address)
            }
            SetStateSucceeded::Updated(state) => (state, store_address.clone()),
        };

        // check if there's left-over mutable reference
        if let Some(span) = state.get_moved_out_mutable_reference() {
            handler.receive(Box::new(MovedOutValueFromMutableReference {
                moved_out_value_span: span.clone(),
                reassignment_span: Some(store_span),
            }));
        }

        state.get_drop_instructions(&target_address, table).unwrap()
    }

    fn handle_load<S: table::State>(
        &self,
        load: &Load<ir::Model>,
        register_span: Span,
        stack: &mut Stack,
        current_site: GlobalID,
        environment: &Environment<
            ir::Model,
            S,
            impl Normalizer<ir::Model, S>,
            impl Observer<ir::Model, S>,
        >,
        handler: &HandlerWrapper,
    ) {
        let ty = self
            .type_of_address(&load.address, current_site, environment)
            .unwrap();

        // has been checked previously
        let memory_state = if load.address.get_reference_qualifier()
            == Some(Qualifier::Immutable)
            || load.address.is_behind_index()
        {
            stack.get_state(&load.address).unwrap().get_state_summary()
        } else {
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

            let state = stack
                .set_uninitialized(
                    &load.address,
                    register_span.clone(),
                    environment.table(),
                )
                .unwrap();

            match state {
                SetStateSucceeded::Unchanged(initialized, _) => initialized
                    .as_false()
                    .and_then(|x| x.latest_accessor().as_ref())
                    .map_or(Summary::Initialized, |x| {
                        Summary::Moved(x.clone())
                    }),

                SetStateSucceeded::Updated(state) => state.get_state_summary(),
            }
        };

        match memory_state {
            Summary::Uninitialized => {
                handler.receive(Box::new(UseBeforeInitialization {
                    use_span: register_span,
                }));
            }

            Summary::Moved(span) => {
                handler.receive(Box::new(UseAfterMove {
                    use_span: register_span,
                    move_span: span,
                }));
            }

            Summary::Initialized => {}
        }
    }
}

impl ir::Representation<ir::Model> {
    #[allow(clippy::too_many_lines, clippy::too_many_arguments)]
    fn walk<S: table::State>(
        &mut self,
        block_id: ID<Block<ir::Model>>,
        stack: &mut Stack,
        scope_memories_by_scope_id: &mut HashMap<ID<scope::Scope>, ScopeMemory>,
        current_site: GlobalID,
        environment: &Environment<
            ir::Model,
            S,
            impl Normalizer<ir::Model, S>,
            impl Observer<ir::Model, S>,
        >,
        handler: &HandlerWrapper,
    ) {
        let mut current_index = 0;
        let block = self.control_flow_graph.get_block_mut(block_id).unwrap();

        while current_index < block.instructions().len() {
            let step = match &block.instructions()[current_index] {
                Instruction::Store(store) => {
                    let instructions = self.values.handle_store(
                        &store.address,
                        match &store.value {
                            Value::Register(id) => self
                                .values
                                .registers
                                .get(*id)
                                .unwrap()
                                .span
                                .clone()
                                .unwrap(),
                            Value::Literal(literal) => {
                                literal.span().cloned().unwrap()
                            }
                        },
                        stack,
                        environment.table(),
                        handler,
                    );

                    let instructions_len = instructions.len();

                    block.insert_instructions(current_index, instructions);

                    instructions_len + 1
                }

                Instruction::RegisterAssignment(register_assignment) => {
                    let register = self
                        .values
                        .registers
                        .get(register_assignment.id)
                        .unwrap();

                    match &register.assignment {
                        Assignment::Load(load) => {
                            self.values.handle_load(
                                load,
                                register.span.clone().unwrap(),
                                stack,
                                current_site,
                                environment,
                                handler,
                            );

                            1
                        }

                        Assignment::ReferenceOf(reference_of) => {
                            Values::handle_reference_of(
                                reference_of,
                                register.span.clone().unwrap(),
                                stack,
                                handler,
                            );

                            1
                        }

                        Assignment::Prefix(_)
                        | Assignment::Tuple(_)
                        | Assignment::Struct(_)
                        | Assignment::Variant(_)
                        | Assignment::FunctionCall(_)
                        | Assignment::Binary(_)
                        | Assignment::Array(_)
                        | Assignment::Phi(_)
                        | Assignment::Cast(_)
                        | Assignment::VariantNumber(_) => 1,
                    }
                }

                Instruction::AllocaDeclaration(alloca_declaration) => {
                    assert!(stack.current_mut().new_state(
                        Memory::Alloca(alloca_declaration.id),
                        false,
                        self.values
                            .allocas
                            .get(alloca_declaration.id)
                            .unwrap()
                            .r#type
                            .clone(),
                    ));

                    1
                }

                Instruction::TuplePack(tuple_pack) => {
                    let moved_tuple_ty = self
                        .values
                        .type_of_address(
                            &tuple_pack.tuple_address,
                            current_site,
                            environment,
                        )
                        .unwrap()
                        .result
                        .into_tuple()
                        .unwrap();

                    let moved_range = tuple_pack.before_packed_element_count
                        ..(moved_tuple_ty.elements.len()
                            - tuple_pack.after_packed_element_count);

                    for i in moved_range {
                        let address = Address::Tuple(ir::address::Tuple {
                            tuple_address: Box::new(
                                tuple_pack.tuple_address.clone(),
                            ),
                            offset: ir::address::Offset::FromStart(i),
                        });

                        // mark as moved out
                        let state = stack
                            .set_uninitialized(
                                &address,
                                tuple_pack.packed_tuple_span.clone().unwrap(),
                                environment.table(),
                            )
                            .unwrap();

                        let sumamry = match state {
                            SetStateSucceeded::Unchanged(initialized, _) => {
                                initialized
                                    .as_false()
                                    .and_then(|x| x.latest_accessor().as_ref())
                                    .map_or(Summary::Initialized, |x| {
                                        Summary::Moved(x.clone())
                                    })
                            }

                            SetStateSucceeded::Updated(state) => {
                                state.get_state_summary()
                            }
                        };

                        match sumamry {
                            Summary::Uninitialized => {
                                handler.receive(Box::new(
                                    UseBeforeInitialization {
                                        use_span: tuple_pack
                                            .packed_tuple_span
                                            .clone()
                                            .unwrap(),
                                    },
                                ));
                            }

                            Summary::Moved(span) => {
                                handler.receive(Box::new(UseAfterMove {
                                    use_span: tuple_pack
                                        .packed_tuple_span
                                        .clone()
                                        .unwrap(),
                                    move_span: span,
                                }));
                            }

                            Summary::Initialized => {}
                        }
                    }

                    let instructions = self.values.handle_store(
                        &tuple_pack.store_address,
                        tuple_pack.packed_tuple_span.clone().unwrap(),
                        stack,
                        environment.table(),
                        handler,
                    );

                    let instructions_len = instructions.len();

                    block.insert_instructions(current_index, instructions);

                    instructions_len + 1
                }

                Instruction::RegisterDiscard(_)
                | Instruction::DropUnpackTuple(_)
                | Instruction::Drop(_) => 1,

                Instruction::ScopePush(scope_push) => {
                    match scope_memories_by_scope_id.entry(scope_push.0) {
                        // skip if already processed
                        Entry::Occupied(_) => return,
                        Entry::Vacant(entry) => {
                            entry.insert(ScopeMemory {
                                before_push_stack_snapshot: stack.clone(),
                                after_pop_stack_snapshot: None,
                            });
                        }
                    }

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
                        }
                    }

                    1
                }

                Instruction::ScopePop(scope_pop) => {
                    // record the stack state
                    let poped_scope = stack.pop_scope().unwrap();
                    assert_eq!(poped_scope.scope_id(), scope_pop.0);

                    let stack_snapshot = stack.clone();
                    scope_memories_by_scope_id
                        .get_mut(&scope_pop.0)
                        .unwrap()
                        .after_pop_stack_snapshot =
                        Some(PopSnapshot { stack_snapshot, poped_scope });

                    1
                }
            };

            current_index += step;
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
        let mut pop_snapshots_by_scope_id = HashMap::new();

        self.walk(
            self.control_flow_graph.entry_block_id(),
            &mut stack,
            &mut pop_snapshots_by_scope_id,
            current_site,
            environment,
            handler,
        );
    }
}
