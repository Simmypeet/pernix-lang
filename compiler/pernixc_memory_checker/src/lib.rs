//! Contains the logic related to memory checking pass on the IR.

use std::{cmp::Ordering, collections::HashMap};

use diagnostic::{
    MoveInLoop, MovedOutValueFromMutableReference, UseAfterMove,
    UseBeforeInitialization,
};
use pernixc_abort::Abort;
use pernixc_arena::{Arena, ID};
use pernixc_component::function_signature::FunctionSignature;
use pernixc_handler::Handler;
use pernixc_ir::{
    address::{self, Address, Memory},
    alloca::Alloca,
    control_flow_graph::Block,
    instruction::{Instruction, Jump, Terminator, UnconditionalJump},
    model,
    value::register::{Assignment, Borrow, Load},
    Representation, Values,
};
use pernixc_source_file::Span;
use pernixc_semantic::{
    component::SymbolKind, diagnostic::Diagnostic, GlobalID, Table,
};
use pernixc_term::{
    generic_arguments::GenericArguments,
    predicate::{PositiveMarker, Predicate},
    r#type::Qualifier,
    Model,
};
use pernixc_type_system::{
    environment::{Environment as TyEnvironment, GetActivePremiseExt},
    normalizer::{self, Normalizer},
    well_formedness,
};
use state::{SetStateSucceeded, Stack, Summary};

pub mod diagnostic;
pub(crate) mod simplify_drop;
pub(crate) mod state;

fn handle_store(
    store_address: &Address<model::Model>,
    store_span: Span,
    stack: &mut Stack,
    ty_environment: &TyEnvironment<model::Model, impl Normalizer<model::Model>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Vec<Instruction<model::Model>>, Abort> {
    let state = stack.set_initialized(
        store_address,
        store_span.clone(),
        ty_environment,
        handler,
    )?;

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

    Ok(state
        .get_drop_instructions(&target_address, ty_environment.table())
        .unwrap())
}

fn handle_borrow(
    borrow: &Borrow<model::Model>,
    register_span: Span,
    stack: &mut Stack,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    let state = stack.get_state(&borrow.address).unwrap();

    match state.get_state_summary() {
        Summary::Initialized => {}

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
    }
}

fn handle_load(
    values: &Values<model::Model>,
    load: &Load<model::Model>,
    register_span: Span,
    stack: &mut Stack,
    current_site: GlobalID,
    ty_environment: &TyEnvironment<model::Model, impl Normalizer<model::Model>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<(), Abort> {
    let ty = values
        .type_of_address(&load.address, current_site, ty_environment)
        .unwrap();

    // has been checked previously
    let memory_state = 'memory_state: {
        if load.address.get_reference_qualifier() == Some(Qualifier::Immutable)
            || load.address.is_behind_index()
        {
            stack
                .get_state(&load.address)
                .expect("should found")
                .get_state_summary()
        } else {
            let copy_marker = ty_environment
                .table()
                .get_by_qualified_name(["core", "Copy"])
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
                ty_environment,
            )?
            .1
            .is_empty()
            {
                break 'memory_state stack
                    .get_state(&load.address)
                    .expect("should found")
                    .get_state_summary();
            }

            let state = stack.set_uninitialized(
                &load.address,
                register_span.clone(),
                ty_environment,
                handler,
            )?;

            match state {
                SetStateSucceeded::Unchanged(initialized, _) => initialized
                    .as_false()
                    .and_then(|x| x.latest_accessor())
                    .map_or(Summary::Initialized, |x| {
                        Summary::Moved(x.clone())
                    }),

                SetStateSucceeded::Updated(state) => state.get_state_summary(),
            }
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
    };

    Ok(())
}

fn sort_drop_addresses(
    addresses: &mut [Memory<model::Model>],
    allocas: &Arena<Alloca<model::Model>>,
    current_site: GlobalID,
    table: &Table,
) -> Result<(), Abort> {
    let function_signature = table.query::<FunctionSignature>(current_site)?;

    addresses.sort_by(|x, y| match (x, y) {
        (Memory::Parameter(x_id), Memory::Parameter(y_id)) => {
            let x = function_signature
                .parameter_order
                .iter()
                .position(|y| y == x_id)
                .unwrap();
            let y = function_signature
                .parameter_order
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

    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct WalkResult {
    /// The stack state after the walking
    stack: Stack,
    looped_blocks: Vec<ID<Block<model::Model>>>,
}

#[derive(Debug)]
struct Checker<'r, 'a, N: Normalizer<model::Model>> {
    representation: &'r mut Representation<model::Model>,

    /// The key represents the block ID that needs to be checked/explored.
    ///
    /// - `None` value means the block is being processed.
    /// - `Some` value means the block has been processed
    /// - No value means the block has not been explored
    walk_results_by_block_id:
        HashMap<ID<Block<model::Model>>, Option<WalkResult>>,

    /// If the block id appears in this map, it means the block is a looped
    /// block and the value is the starting environment of the looped block.
    target_stakcs_by_block_id: HashMap<ID<Block<model::Model>>, Stack>,

    current_site: GlobalID,
    ty_environment: &'a TyEnvironment<'a, model::Model, N>,
}

impl<N: Normalizer<model::Model>> Checker<'_, '_, N> {
    #[allow(clippy::too_many_lines)]
    fn walk_instructions(
        &mut self,
        block_id: ID<Block<model::Model>>,
        stack: &mut Stack,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<(), Abort> {
        let mut current_index = 0;
        let block = self
            .representation
            .control_flow_graph
            .get_block_mut(block_id)
            .unwrap();

        while current_index < block.instructions().len() {
            let step = match &block.instructions()[current_index] {
                Instruction::Store(store) => {
                    let instructions = handle_store(
                        &store.address,
                        store.span.clone().unwrap(),
                        stack,
                        self.ty_environment,
                        handler,
                    )?;

                    let instructions = simplify_drop::simplify_drops(
                        instructions,
                        &self.representation.values,
                        self.current_site,
                        self.ty_environment,
                        handler,
                    )?;
                    let instructions_len = instructions.len();

                    block.insert_instructions(current_index, instructions);

                    instructions_len + 1
                }

                Instruction::RegisterAssignment(register_assignment) => {
                    let register = self
                        .representation
                        .values
                        .registers
                        .get(register_assignment.id)
                        .unwrap();

                    match &register.assignment {
                        Assignment::Load(load) => {
                            handle_load(
                                &self.representation.values,
                                load,
                                register.span.clone().unwrap(),
                                stack,
                                self.current_site,
                                self.ty_environment,
                                handler,
                            )?;

                            1
                        }

                        Assignment::Borrow(borrow) => {
                            handle_borrow(
                                borrow,
                                register.span.clone().unwrap(),
                                stack,
                                handler,
                            );

                            1
                        }

                        Assignment::FunctionCall(_)
                        | Assignment::Struct(_)
                        | Assignment::Variant(_)
                        | Assignment::Array(_)
                        | Assignment::Phi(_)
                        | Assignment::Prefix(_)
                        | Assignment::Tuple(_)
                        | Assignment::Binary(_)
                        | Assignment::Cast(_)
                        | Assignment::VariantNumber(_) => 1,
                    }
                }

                Instruction::TuplePack(tuple_pack) => {
                    let state = stack.set_uninitialized(
                        &Address::Tuple(address::Tuple {
                            tuple_address: Box::new(
                                tuple_pack.tuple_address.clone(),
                            ),
                            offset: address::Offset::Unpacked,
                        }),
                        tuple_pack.packed_tuple_span.clone().unwrap(),
                        self.ty_environment,
                        handler,
                    )?;

                    let sumamry = match state {
                        SetStateSucceeded::Unchanged(initialized, _) => {
                            initialized
                                .as_false()
                                .and_then(|x| x.latest_accessor())
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

                    let instructions = handle_store(
                        &tuple_pack.store_address,
                        tuple_pack.packed_tuple_span.clone().unwrap(),
                        stack,
                        self.ty_environment,
                        handler,
                    )?;

                    let instructions = simplify_drop::simplify_drops(
                        instructions,
                        &self.representation.values,
                        self.current_site,
                        self.ty_environment,
                        handler,
                    )?;
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
                        if scope_push.0
                            == self.representation.scope_tree.root_scope_id()
                        {
                            if !self
                                .ty_environment
                                .table()
                                .get::<SymbolKind>(self.current_site)
                                .has_function_signature()
                            {
                                break 'out;
                            }

                            let function_signature = self
                                .ty_environment
                                .table()
                                .query::<FunctionSignature>(self.current_site)
                                .unwrap();

                            for (parameter_id, parameter) in function_signature
                                .parameter_order
                                .iter()
                                .copied()
                                .map(|x| (x, &function_signature.parameters[x]))
                            {
                                assert!(stack.current_mut().new_state(
                                    Memory::Parameter(parameter_id),
                                    true,
                                    model::Model::from_default_type(
                                        parameter.r#type.clone(),
                                    ),
                                ));
                            }
                        }
                    }

                    let allocas = self
                        .representation
                        .values
                        .allocas
                        .iter()
                        .filter_map(|(id, alloca)| {
                            (alloca.declared_in_scope_id == scope_push.0)
                                .then_some(id)
                        })
                        .collect::<Vec<_>>();

                    for id in allocas {
                        assert!(stack.current_mut().new_state(
                            Memory::Alloca(id),
                            false,
                            self.representation
                                .values
                                .allocas
                                .get(id)
                                .unwrap()
                                .r#type
                                .clone(),
                        ));
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
                        &self.representation.values.allocas,
                        self.current_site,
                        self.ty_environment.table(),
                    )?;

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
                                    self.ty_environment.table(),
                                )
                                .unwrap(),
                        );
                    }

                    let drop_instructions = simplify_drop::simplify_drops(
                        drop_instructions,
                        &self.representation.values,
                        self.current_site,
                        self.ty_environment,
                        handler,
                    )?;
                    let len = drop_instructions.len();

                    block.insert_instructions(current_index, drop_instructions);

                    1 + len
                }
            };

            current_index += step;
        }

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn walk_block(
        &mut self,
        block_id: ID<Block<model::Model>>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Option<WalkResult>, Abort> {
        // skip if already processed
        if let Some(walk_result) = self.walk_results_by_block_id.get(&block_id)
        {
            return Ok(walk_result.clone());
        }

        // mark as processing
        self.walk_results_by_block_id.insert(block_id, None);

        let block =
            self.representation.control_flow_graph.get_block(block_id).unwrap();

        let (mut stack, looped_blocks) = if block.is_entry() {
            assert!(block.predecessors().is_empty());

            (Stack::new(), Vec::new())
        } else {
            let predecessors =
                block.predecessors().iter().copied().collect::<Vec<_>>();

            let mut merging_contexts = Vec::new();
            let mut looped_block_ids = Vec::new();

            for predecessor_id in predecessors.iter().copied() {
                if let Some(result) =
                    self.walk_block(predecessor_id, handler)?
                {
                    merging_contexts.push((predecessor_id, result.stack));
                } else {
                    looped_block_ids.push(predecessor_id);
                }
            }

            if merging_contexts.is_empty() {
                // try again later
                self.walk_results_by_block_id.remove(&block_id);

                return Ok(None);
            }

            // Sanity check
            if merging_contexts.len() > 1 {
                for i in merging_contexts.iter().map(|x| x.0) {
                    assert_eq!(
                        *self
                            .representation
                            .control_flow_graph
                            .blocks()
                            .get(i)
                            .unwrap()
                            .terminator(),
                        Some(Terminator::Jump(Jump::Unconditional(
                            UnconditionalJump { target: block_id }
                        ))),
                        "merging block `{i:#?}` should directly jump to the \
                         `block_id` {:#?} {:#?}",
                        self.representation.control_flow_graph,
                        self.representation.values
                    );
                }
            }

            // merge the contexts
            let mut stack = merging_contexts.pop().unwrap().1;
            stack = merging_contexts.into_iter().fold(
                stack,
                |mut acc, (_, env)| {
                    acc.min_merge(&env);
                    acc
                },
            );

            // mark the looped block
            for looped in looped_block_ids.iter().copied() {
                self.target_stakcs_by_block_id.insert(looped, stack.clone());
            }

            for predecessor in predecessors
                .iter()
                .copied()
                .filter(|x| !looped_block_ids.contains(x))
            {
                let walk_result = self
                    .walk_results_by_block_id
                    .get(&predecessor)
                    .unwrap()
                    .as_ref()
                    .unwrap();

                // drop instructions to insert at the end of the block
                let mut drop_instructions = Vec::new();

                for (this, alternate) in stack
                    .scopes()
                    .iter()
                    .rev()
                    .zip(walk_result.stack.scopes().iter().rev())
                {
                    assert_eq!(this.scope_id(), alternate.scope_id());

                    let mut memories = this
                        .memories_by_address()
                        .keys()
                        .copied()
                        .collect::<Vec<_>>();

                    sort_drop_addresses(
                        &mut memories,
                        &self.representation.values.allocas,
                        self.current_site,
                        self.ty_environment.table(),
                    )?;

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
                                    self.ty_environment.table(),
                                )
                                .unwrap(),
                        );
                    }
                }

                let block = self
                    .representation
                    .control_flow_graph
                    .get_block_mut(predecessor)
                    .unwrap();

                block.insert_instructions(
                    block.instructions().len(),
                    simplify_drop::simplify_drops(
                        drop_instructions,
                        &self.representation.values,
                        self.current_site,
                        self.ty_environment,
                        handler,
                    )?,
                );
            }

            (stack, looped_block_ids)
        };

        self.walk_instructions(block_id, &mut stack, handler)?;

        // handle loop
        if let Some(target_stack) =
            self.target_stakcs_by_block_id.get(&block_id)
        {
            assert_eq!(target_stack.scopes().len(), stack.scopes().len());

            for (this_scope, target_scope) in
                stack.scopes().iter().zip(target_stack.scopes().iter())
            {
                assert_eq!(this_scope.scope_id(), target_scope.scope_id());

                let mut memories = this_scope
                    .memories_by_address()
                    .keys()
                    .copied()
                    .collect::<Vec<_>>();

                sort_drop_addresses(
                    &mut memories,
                    &self.representation.values.allocas,
                    self.current_site,
                    self.ty_environment.table(),
                )?;

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
                            self.ty_environment.table(),
                        )
                        .unwrap();

                    let block = self
                        .representation
                        .control_flow_graph
                        .get_block_mut(block_id)
                        .unwrap();
                    block.insert_instructions(
                        block.instructions().len(),
                        simplify_drop::simplify_drops(
                            drop_instructions,
                            &self.representation.values,
                            self.current_site,
                            self.ty_environment,
                            handler,
                        )?,
                    );

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

            stack = target_stack.clone();
        }

        let result = WalkResult { stack, looped_blocks };

        // mark as done
        assert!(self
            .walk_results_by_block_id
            .insert(block_id, Some(result.clone()))
            .unwrap()
            .is_none());

        Ok(Some(result))
    }
}

/// Performs the use-after-move and use-before-initialization check. Moreover,
/// inserts the drop instructions to the IR.
#[allow(clippy::missing_errors_doc)]
pub fn memory_check(
    table: &Table,
    representation: &mut Representation<model::Model>,
    current_site: GlobalID,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<(), Abort> {
    let ty_environment = TyEnvironment::new(
        std::borrow::Cow::Owned(table.get_active_premise(current_site)),
        table,
        normalizer::NO_OP,
    );

    let all_block_ids =
        representation.control_flow_graph.blocks().ids().collect::<Vec<_>>();

    let mut checker = Checker {
        representation,
        walk_results_by_block_id: HashMap::new(),
        target_stakcs_by_block_id: HashMap::new(),
        current_site,
        ty_environment: &ty_environment,
    };

    for block_id in all_block_ids {
        checker.walk_block(block_id, handler)?;
    }

    assert!(checker.walk_results_by_block_id.values().all(Option::is_some));

    Ok(())
}
