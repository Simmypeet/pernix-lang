//! Contains the logic related to memory checking pass on the IR.

use std::{cmp::Ordering, collections::HashMap};

use diagnostic::{
    MoveInLoop, MovedOutValueFromMutableReference, UseAfterMove,
    UseBeforeInitialization,
};
use pernixc_arena::{Arena, ID};
use pernixc_handler::{Handler, Storage};
use pernixc_ir::{
    address::{self, Address, Memory},
    alloca::Alloca,
    control_flow_graph::Block,
    instruction::{Instruction, Jump, Terminator, UnconditionalJump},
    value::{
        register::{Assignment, Borrow, Load},
        TypeOf,
    },
    Values, IR,
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_semantic_element::parameter::get_parameters;
use pernixc_symbol::{kind::get_kind, name::get_by_qualified_name};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    predicate::{PositiveMarker, Predicate},
    r#type::Qualifier,
};
use pernixc_type_system::{
    environment::{get_active_premise, Environment as TyEnvironment},
    normalizer::{self, Normalizer},
    UnrecoverableError,
};
use state::{SetStateSucceeded, Stack, Summary};

use crate::diagnostic::Diagnostic;

pub mod diagnostic;
pub(crate) mod simplify_drop;
pub(crate) mod state;

async fn handle_store<N: Normalizer>(
    store_address: &Address,
    store_span: RelativeSpan,
    stack: &mut Stack,
    ty_environment: &TyEnvironment<'_, N>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<Vec<Instruction>, UnrecoverableError> {
    let state = stack
        .set_initialized(store_address, store_span, ty_environment, handler)
        .await?;

    let (state, target_address) = match state {
        SetStateSucceeded::Unchanged(initialized, address) => {
            (state::State::Total(initialized), address)
        }
        SetStateSucceeded::Updated(state) => (state, store_address.clone()),
    };

    // check if there's left-over mutable reference
    if let Some(span) = state.get_moved_out_mutable_reference() {
        handler.receive(
            MovedOutValueFromMutableReference {
                moved_out_value_span: *span,
                reassignment_span: Some(store_span),
            }
            .into(),
        );
    }

    Ok(state
        .get_drop_instructions(&target_address, ty_environment.tracked_engine())
        .await
        .unwrap())
}

fn handle_borrow(
    borrow: &Borrow,
    register_span: RelativeSpan,
    stack: &mut Stack,
    handler: &dyn Handler<Diagnostic>,
) {
    let state = stack.get_state(&borrow.address).unwrap();

    match state.get_state_summary() {
        Summary::Initialized => {}

        Summary::Uninitialized => {
            handler.receive(
                UseBeforeInitialization { use_span: register_span }.into(),
            );
        }

        Summary::Moved(span) => {
            handler.receive(
                UseAfterMove { use_span: register_span, move_span: span }
                    .into(),
            );
        }
    }
}

async fn handle_load<N: Normalizer>(
    values: &Values,
    load: &Load,
    register_span: RelativeSpan,
    stack: &mut Stack,
    current_site: Global<pernixc_symbol::ID>,
    ty_environment: &TyEnvironment<'_, N>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), UnrecoverableError> {
    let ty = values
        .type_of(&load.address, current_site, ty_environment)
        .await
        .map_err(|x| {
            x.report_as_type_calculating_overflow(register_span, &handler)
        })?;

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
                .tracked_engine()
                .get_by_qualified_name(pernixc_corelib::copy::MARKER_SEQUENCE)
                .await
                .unwrap();

            // no need to move
            let storage =
                Storage::<pernixc_type_system::diagnostic::Diagnostic>::new();
            ty_environment
                .predicate_satisfied(
                    Predicate::PositiveMarker(PositiveMarker::new(
                        copy_marker,
                        GenericArguments {
                            lifetimes: Vec::new(),
                            types: vec![ty.result],
                            constants: Vec::new(),
                        },
                    )),
                    register_span,
                    None,
                    false,
                    &storage,
                )
                .await?;

            if storage.as_vec().is_empty() {
                break 'memory_state stack
                    .get_state(&load.address)
                    .expect("should found")
                    .get_state_summary();
            }

            let state = stack
                .set_uninitialized(
                    &load.address,
                    register_span,
                    ty_environment,
                    handler,
                )
                .await?;

            match state {
                SetStateSucceeded::Unchanged(initialized, _) => initialized
                    .as_false()
                    .and_then(|x| x.latest_accessor())
                    .map_or(Summary::Initialized, |x| Summary::Moved(*x)),

                SetStateSucceeded::Updated(state) => state.get_state_summary(),
            }
        }
    };

    match memory_state {
        Summary::Uninitialized => {
            handler.receive(
                UseBeforeInitialization { use_span: register_span }.into(),
            );
        }

        Summary::Moved(span) => {
            handler.receive(
                UseAfterMove { use_span: register_span, move_span: span }
                    .into(),
            );
        }

        Summary::Initialized => {}
    }

    Ok(())
}

async fn sort_drop_addresses(
    addresses: &mut [Memory],
    allocas: &Arena<Alloca>,
    current_site: Global<pernixc_symbol::ID>,
    engine: &TrackedEngine,
) -> Result<(), UnrecoverableError> {
    let function_signature = engine.get_parameters(current_site).await?;

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
    looped_blocks: Vec<ID<Block>>,
}

#[derive(Debug)]
struct Checker<'r, 'a, N: Normalizer> {
    representation: &'r mut IR,

    /// The key represents the block ID that needs to be checked/explored.
    ///
    /// - `None` value means the block is being processed.
    /// - `Some` value means the block has been processed
    /// - No value means the block has not been explored
    walk_results_by_block_id: HashMap<ID<Block>, Option<WalkResult>>,

    /// If the block id appears in this map, it means the block is a looped
    /// block and the value is the starting environment of the looped block.
    target_stakcs_by_block_id: HashMap<ID<Block>, Stack>,

    current_site: Global<pernixc_symbol::ID>,
    ty_environment: &'a TyEnvironment<'a, N>,
}

impl<N: Normalizer> Checker<'_, '_, N> {
    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    async fn walk_instructions(
        &mut self,
        block_id: ID<Block>,
        stack: &mut Stack,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        let mut current_index = 0;
        let root_scope_id =
            self.representation.control_flow_graph.root_scope_id();

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
                        store.span.unwrap(),
                        stack,
                        self.ty_environment,
                        handler,
                    )
                    .await?;

                    let instructions = simplify_drop::simplify_drops(
                        instructions,
                        &self.representation.values,
                        self.current_site,
                        self.ty_environment,
                        handler,
                    )
                    .await?;
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
                                register.span.unwrap(),
                                stack,
                                self.current_site,
                                self.ty_environment,
                                handler,
                            )
                            .await?;

                            1
                        }

                        Assignment::Borrow(borrow) => {
                            handle_borrow(
                                borrow,
                                register.span.unwrap(),
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
                    let state = stack
                        .set_uninitialized(
                            &Address::Tuple(address::Tuple {
                                tuple_address: Box::new(
                                    tuple_pack.tuple_address.clone(),
                                ),
                                offset: address::Offset::Unpacked,
                            }),
                            tuple_pack.packed_tuple_span.unwrap(),
                            self.ty_environment,
                            handler,
                        )
                        .await?;

                    let sumamry = match state {
                        SetStateSucceeded::Unchanged(initialized, _) => {
                            initialized
                                .as_false()
                                .and_then(|x| x.latest_accessor())
                                .map_or(Summary::Initialized, |x| {
                                    Summary::Moved(*x)
                                })
                        }

                        SetStateSucceeded::Updated(state) => {
                            state.get_state_summary()
                        }
                    };

                    match sumamry {
                        Summary::Uninitialized => {
                            handler.receive(
                                UseBeforeInitialization {
                                    use_span: tuple_pack
                                        .packed_tuple_span
                                        .unwrap(),
                                }
                                .into(),
                            );
                        }

                        Summary::Moved(span) => {
                            handler.receive(
                                UseAfterMove {
                                    use_span: tuple_pack
                                        .packed_tuple_span
                                        .unwrap(),
                                    move_span: span,
                                }
                                .into(),
                            );
                        }

                        Summary::Initialized => {}
                    }

                    let instructions = handle_store(
                        &tuple_pack.store_address,
                        tuple_pack.packed_tuple_span.unwrap(),
                        stack,
                        self.ty_environment,
                        handler,
                    )
                    .await?;

                    let instructions = simplify_drop::simplify_drops(
                        instructions,
                        &self.representation.values,
                        self.current_site,
                        self.ty_environment,
                        handler,
                    )
                    .await?;

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
                        if scope_push.0 == root_scope_id {
                            if !self
                                .ty_environment
                                .tracked_engine()
                                .get_kind(self.current_site)
                                .await
                                .has_function_signature()
                            {
                                break 'out;
                            }

                            let function_signature = self
                                .ty_environment
                                .tracked_engine()
                                .get_parameters(self.current_site)
                                .await?;

                            for (parameter_id, parameter) in function_signature
                                .parameter_order
                                .iter()
                                .copied()
                                .map(|x| (x, &function_signature.parameters[x]))
                            {
                                assert!(stack.current_mut().new_state(
                                    Memory::Parameter(parameter_id),
                                    true,
                                    parameter.r#type.clone(),
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
                        self.ty_environment.tracked_engine(),
                    )
                    .await?;

                    let mut drop_instructions = Vec::new();

                    for memory in memories {
                        let state = poped_scope
                            .get_state(&Address::Memory(memory))
                            .unwrap();

                        if let Some(moved_out) =
                            state.get_moved_out_mutable_reference()
                        {
                            handler.receive(
                                MovedOutValueFromMutableReference {
                                    moved_out_value_span: *moved_out,
                                    reassignment_span: None,
                                }
                                .into(),
                            );
                        }

                        drop_instructions.extend(
                            state
                                .get_drop_instructions(
                                    &Address::Memory(memory),
                                    self.ty_environment.tracked_engine(),
                                )
                                .await?,
                        );
                    }

                    let drop_instructions = simplify_drop::simplify_drops(
                        drop_instructions,
                        &self.representation.values,
                        self.current_site,
                        self.ty_environment,
                        handler,
                    )
                    .await?;
                    let len = drop_instructions.len();

                    block.insert_instructions(current_index, drop_instructions);

                    1 + len
                }
            };

            current_index += step;
        }

        Ok(())
    }

    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    async fn walk_block(
        &mut self,
        block_id: ID<Block>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Option<WalkResult>, UnrecoverableError> {
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
                    Box::pin(self.walk_block(predecessor_id, handler)).await?
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
                        self.ty_environment.tracked_engine(),
                    )
                    .await?;

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
                                    self.ty_environment.tracked_engine(),
                                )
                                .await
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
                    )
                    .await?,
                );
            }

            (stack, looped_block_ids)
        };

        self.walk_instructions(block_id, &mut stack, handler).await?;

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
                    self.ty_environment.tracked_engine(),
                )
                .await?;

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
                            self.ty_environment.tracked_engine(),
                        )
                        .await
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
                        )
                        .await?,
                    );

                    for move_span in target_state
                        .get_uninitialized_diff(this_state)
                        .unwrap()
                        .into_iter()
                        .map(|x| x.get_state_summary().into_moved().unwrap())
                    {
                        handler.receive(
                            MoveInLoop { moved_value_span: move_span }.into(),
                        );
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
pub async fn memory_check(
    engine: &TrackedEngine,
    representation: &mut IR,
    current_site: Global<pernixc_symbol::ID>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), UnrecoverableError> {
    let premise = engine.get_active_premise(current_site).await?;

    let ty_environment = TyEnvironment::new(
        std::borrow::Cow::Borrowed(&premise),
        std::borrow::Cow::Borrowed(engine),
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
        checker.walk_block(block_id, handler).await?;
    }

    assert!(checker.walk_results_by_block_id.values().all(Option::is_some));

    Ok(())
}
