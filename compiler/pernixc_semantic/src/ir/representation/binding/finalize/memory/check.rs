use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    env,
};

use getset::Getters;
use pernixc_base::{handler::Handler, source_file::Span};
use pernixc_syntax::syntax_tree::target;

use super::state::{self, Scope, SetStateSucceeded, Summary};
use crate::{
    arena::{Arena, ID},
    error::{
        MoveInLoop, MovedOutValueFromMutableReference, UseAfterMove,
        UseBeforeInitialization,
    },
    ir::{
        self,
        address::{Address, Memory},
        alloca::Alloca,
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
        table::{self, representation::MergeAccessibilityError, Table},
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
    ) -> SetStateSucceeded {
        let root = address.get_root_memory();
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains(root) {
                return scope.set_initialized(address, table).unwrap();
            }
        }

        // not found
        panic!("Invalid address");
    }

    pub fn get_state(&self, address: &Address<ir::Model>) -> &state::State {
        let root = address.get_root_memory();

        for scope in self.scopes.iter().rev() {
            if scope.contains(root) {
                return scope.get_state(address).unwrap();
            }
        }

        // not found
        panic!("Invalid address");
    }

    pub fn set_uninitialized(
        &mut self,
        address: &Address<ir::Model>,
        move_span: Span,
        table: &Table<impl table::State>,
    ) -> SetStateSucceeded {
        let root = address.get_root_memory();
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains(root) {
                return scope
                    .set_uninitialized(address, move_span, table)
                    .unwrap();
            }
        }

        // not found
        panic!("Invalid address");
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn min_merge(&mut self, other: &Self) {
        assert_eq!(self.scopes.len(), other.scopes.len());

        for (lhs, rhs) in self.scopes.iter_mut().zip(other.scopes.iter()) {
            take_mut::take(lhs, |lhs| lhs.min_merge(rhs).unwrap());
        }
    }
}

impl Values<ir::Model> {
    fn handle_reference_of(
        reference_of: &ReferenceOf<ir::Model>,
        register_span: Span,
        stack: &mut Stack,
        handler: &HandlerWrapper,
    ) {
        let state = stack.get_state(&reference_of.address).get_state_summary();

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
        store_address: &Address<ir::Model>,
        store_span: Span,
        stack: &mut Stack,
        table: &Table<S>,
        handler: &HandlerWrapper,
    ) -> Vec<Instruction<ir::Model>> {
        let state = stack.set_initialized(store_address, table);

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
            stack.get_state(&load.address).get_state_summary()
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

            let state = stack.set_uninitialized(
                &load.address,
                register_span.clone(),
                environment.table(),
            );

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

fn sort_drop_addresses(
    addresses: &mut Vec<Memory<ir::Model>>,
    allocas: &Arena<Alloca<ir::Model>>,
    current_site: GlobalID,
    table: &Table<impl table::State>,
) {
    let callable = CallableID::try_from(current_site)
        .ok()
        .map(|x| table.get_callable(x).unwrap());

    addresses.sort_by(|x, y| match (x, y) {
        (Memory::Parameter(x_id), Memory::Parameter(y_id)) => {
            let x = callable
                .as_ref()
                .unwrap()
                .parameter_order()
                .iter()
                .position(|y| y == x_id)
                .unwrap();
            let y = callable
                .as_ref()
                .unwrap()
                .parameter_order()
                .iter()
                .position(|y| y == y_id)
                .unwrap();

            x.cmp(&y).reverse()
        }

        (Memory::Alloca(x_id), Memory::Alloca(y_id)) => {
            let x = allocas.get(*x_id).unwrap().declaration_order;
            let y = allocas.get(*y_id).unwrap().declaration_order;

            x.cmp(&y).reverse()
        }

        (Memory::Parameter(_), Memory::Alloca(_)) => Ordering::Greater,
        (Memory::Alloca(_), Memory::Parameter(_)) => Ordering::Less,
    });
}

impl ir::Representation<ir::Model> {
    #[allow(clippy::too_many_lines, clippy::too_many_arguments)]
    fn walk_instructions<S: table::State>(
        &mut self,
        block_id: ID<Block<ir::Model>>,
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
        let mut current_index = 0;
        let block = self.control_flow_graph.get_block_mut(block_id).unwrap();

        while current_index < block.instructions().len() {
            let step = match &block.instructions()[current_index] {
                Instruction::Store(store) => {
                    let instructions = Values::handle_store(
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
                        let state = stack.set_uninitialized(
                            &address,
                            tuple_pack.packed_tuple_span.clone().unwrap(),
                            environment.table(),
                        );

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

                    let instructions = Values::handle_store(
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

                    let mut memories = poped_scope
                        .memories_by_address()
                        .keys()
                        .copied()
                        .collect::<Vec<_>>();

                    sort_drop_addresses(
                        &mut memories,
                        &self.values.allocas,
                        current_site,
                        environment.table(),
                    );

                    let mut drop_instructions = Vec::new();

                    for memory in memories {
                        let state = poped_scope
                            .get_state(&Address::Memory(memory))
                            .unwrap();

                        if let Some(moved_out) =
                            state.get_moved_out_mutable_reference()
                        {
                            handler.receive(Box::new(
                                MovedOutValueFromMutableReference {
                                    moved_out_value_span: moved_out.clone(),
                                    reassignment_span: None,
                                },
                            ));
                        }

                        drop_instructions.extend(
                            state
                                .get_drop_instructions(
                                    &Address::Memory(memory),
                                    environment.table(),
                                )
                                .unwrap(),
                        );
                    }

                    let len = drop_instructions.len();
                    block.insert_instructions(current_index, drop_instructions);

                    1 + len
                }
            };

            current_index += step;
        }
    }

    #[allow(clippy::too_many_lines)]
    pub(in super::super) fn walk<S: table::State>(
        &mut self,
        block_id: ID<Block<ir::Model>>,
        stacks_by_block_id: &mut HashMap<ID<Block<ir::Model>>, Option<Stack>>,
        target_stacks_by_block_id: &mut HashMap<ID<Block<ir::Model>>, Stack>,
        current_site: GlobalID,
        environment: &Environment<
            ir::Model,
            S,
            impl Normalizer<ir::Model, S>,
            impl Observer<ir::Model, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Option<Stack> {
        // skip if already processed
        if let Some(stack) = stacks_by_block_id.get(&block_id) {
            return stack.clone();
        }

        // mark as processing
        stacks_by_block_id.insert(block_id, None);

        let block = self.control_flow_graph.get_block(block_id).unwrap();

        let mut starting_stack = if block.is_entry() {
            Stack::new()
        } else {
            let predecessors =
                block.predecessors().iter().copied().collect::<Vec<_>>();

            let mut merging_stakcs = Vec::new();
            let mut looped_block_ids = Vec::new();

            for predecessor_id in predecessors.iter().copied() {
                if let Some(stack) = self.walk(
                    predecessor_id,
                    stacks_by_block_id,
                    target_stacks_by_block_id,
                    current_site,
                    environment,
                    handler,
                ) {
                    merging_stakcs.push((predecessor_id, stack));
                } else {
                    looped_block_ids.push(predecessor_id);
                }
            }

            if merging_stakcs.is_empty() {
                // try again later
                stacks_by_block_id.remove(&block_id);

                return None;
            }

            // Sanity check
            for i in merging_stakcs.iter().map(|x| x.0) {
                for j in merging_stakcs.iter().map(|x| x.0) {
                    if i != j {
                        assert!(
                            !self
                                .control_flow_graph
                                .is_reachable_from(i, j)
                                .unwrap(),
                            "ICE: two merging blocks shouldn't be reachable \
                             to each other {:#?}",
                            self.control_flow_graph
                        );
                    }
                }
            }

            // merge the stacks
            let mut starting_stack = merging_stakcs.pop().unwrap().1;
            starting_stack = merging_stakcs.into_iter().fold(
                starting_stack,
                |mut acc, (_, stack)| {
                    acc.min_merge(&stack);
                    acc
                },
            );

            for looped in looped_block_ids.iter().copied() {
                target_stacks_by_block_id
                    .insert(looped, starting_stack.clone());
            }

            for predecessor in predecessors
                .iter()
                .copied()
                .filter(|x| !looped_block_ids.contains(x))
            {
                let alternate_stack = stacks_by_block_id
                    .get(&predecessor)
                    .unwrap()
                    .as_ref()
                    .unwrap();

                // drop instructions to insert at the end of the block
                let mut drop_instructions = Vec::new();

                for (this, alternate) in starting_stack
                    .scopes
                    .iter()
                    .rev()
                    .zip(alternate_stack.scopes.iter().rev())
                {
                    assert_eq!(this.scope_id(), alternate.scope_id());

                    let mut memories = this
                        .memories_by_address()
                        .keys()
                        .copied()
                        .collect::<Vec<_>>();

                    sort_drop_addresses(
                        &mut memories,
                        &self.values.allocas,
                        current_site,
                        environment.table(),
                    );

                    for memory_to_drop in memories {
                        let this_state = this
                            .get_state(&Address::Memory(memory_to_drop))
                            .unwrap();
                        let alternate_state = alternate
                            .get_state(&Address::Memory(memory_to_drop))
                            .unwrap();

                        drop_instructions.extend(
                            this_state
                                .get_alternate_drop_instructions(
                                    alternate_state,
                                    &Address::Memory(memory_to_drop),
                                    environment.table(),
                                )
                                .unwrap(),
                        );
                    }
                }

                let block =
                    self.control_flow_graph.get_block_mut(predecessor).unwrap();

                block.insert_instructions(
                    block.instructions().len(),
                    drop_instructions,
                );
            }

            starting_stack
        };

        self.walk_instructions(
            block_id,
            &mut starting_stack,
            current_site,
            environment,
            handler,
        );

        // handle loop
        if let Some(target_stack) = target_stacks_by_block_id.get(&block_id) {
            assert_eq!(target_stack.scopes.len(), starting_stack.scopes.len());

            for (this_scope, target_scope) in
                starting_stack.scopes.iter().zip(target_stack.scopes.iter())
            {
                assert_eq!(this_scope.scope_id(), target_scope.scope_id());

                let mut memories = this_scope
                    .memories_by_address()
                    .keys()
                    .copied()
                    .collect::<Vec<_>>();

                sort_drop_addresses(
                    &mut memories,
                    &self.values.allocas,
                    current_site,
                    environment.table(),
                );

                for memory in memories {
                    let this_state =
                        this_scope.get_state(&Address::Memory(memory)).unwrap();
                    let target_state = target_scope
                        .get_state(&Address::Memory(memory))
                        .unwrap();

                    // get the drop instructions that will make this scope the
                    // same as the target scope
                    let drop_instructions = target_state
                        .get_alternate_drop_instructions(
                            this_state,
                            &Address::Memory(memory),
                            environment.table(),
                        )
                        .unwrap();

                    let block = self
                        .control_flow_graph
                        .get_block_mut(block_id)
                        .unwrap();
                    block.insert_instructions(
                        block.instructions().len(),
                        drop_instructions,
                    );

                    dbg!(&target_state, &this_state);
                    for move_span in target_state
                        .get_uninitialized_diff(this_state)
                        .unwrap()
                        .into_iter()
                        .map(|x| x.get_state_summary().into_moved().unwrap())
                    {
                        handler.receive(Box::new(MoveInLoop {
                            moved_value_span: move_span,
                        }));
                    }
                }
            }

            starting_stack = target_stack.clone();
        }

        // mark as done
        assert!(stacks_by_block_id
            .insert(block_id, Some(starting_stack.clone()))
            .unwrap()
            .is_none());

        Some(starting_stack)
    }

    pub(in super::super) fn memory_check<S: table::State>(
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
        let mut stacks_by_block_id = HashMap::new();
        let mut target_stack_by_block_ids = HashMap::new();

        let all_block_ids =
            self.control_flow_graph.blocks().ids().collect::<Vec<_>>();

        for block_id in all_block_ids {
            self.walk(
                block_id,
                &mut stacks_by_block_id,
                &mut target_stack_by_block_ids,
                current_site,
                environment,
                handler,
            );
        }

        assert!(stacks_by_block_id.values().all(Option::is_some));
    }
}
