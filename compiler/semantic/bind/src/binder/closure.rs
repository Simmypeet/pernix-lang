//! Defines the logic for building nested binders, such as for closures,
//! effect handlers, do blocks, etc.

use std::{collections::hash_map::Entry, ops::Not};

use flexstr::SharedStr;
use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_ir::{
    address::{Address, Memory, Reference},
    capture::{self, Capture, CaptureMode, ReferenceCaptureMode},
    instruction::{self, Instruction, ScopePush},
    pattern::{NameBinding, NameBindingPoint},
    value::register::Assignment,
    Values, IR,
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_term::{
    lifetime::Lifetime,
    r#type::{Qualifier, Type},
};
use pernixc_type_system::UnrecoverableError;

use crate::{
    binder::{block, r#loop, stack::Stack, type_check::Expected, Binder},
    diagnostic::{
        Diagnostic, MismatchedClosureReturnType,
        NotAllFlowPathsReturnAValueInClosure,
    },
};

/// Represents a capturing structure available at a particular point in the
/// binding process.
#[derive(Debug, Default)]
pub struct Captures {
    captures: pernixc_ir::capture::Captures,
    name_binding_point: NameBindingPoint,
}

impl Captures {
    async fn new(
        parent_stack: &Stack,
        binder: &Binder<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Self, UnrecoverableError> {
        let mut captures = Self::default();

        for scope in parent_stack.scopes().iter().rev() {
            for named_binding in scope
                .named_binding_points()
                .iter()
                .rev()
                .flat_map(|x| x.named_patterns_by_name.iter())
            {
                captures
                    .try_insert_named_binding(
                        named_binding.0,
                        named_binding.1,
                        binder,
                        handler,
                    )
                    .await?;
            }
        }

        Ok(captures)
    }

    /// Gets a clone of the underlying [`pernixc_ir::capture::Captures`].
    #[must_use]
    pub fn clone_captures(&self) -> pernixc_ir::capture::Captures {
        self.captures.clone()
    }

    async fn try_insert_named_binding(
        &mut self,
        new_name: &SharedStr,
        new_name_binding: &NameBinding,
        binder: &Binder<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        // only insert if it doesn't already exist
        if self
            .name_binding_point
            .named_patterns_by_name
            .contains_key(new_name.as_str())
        {
            return Ok(());
        }

        // check if this memory address dominates any existing captures, if
        // so, we re-adjust the existing captures to use this memory address
        let mut dominated_captures = Vec::new();

        for (existing_name, existing_binding) in
            &self.name_binding_point.named_patterns_by_name
        {
            let existing_binding_root = existing_binding
                .load_address
                .get_root_memory()
                .as_capture()
                .copied()
                .unwrap();

            let existing_captured_address = &self.captures.captures
                [existing_binding_root]
                .parent_captured_address;

            // if the existing captured address is a child of the new
            // captured address, we re-adjust the existing capture to use
            if existing_captured_address == &new_name_binding.load_address
                || existing_captured_address
                    .is_child_of(&new_name_binding.load_address)
            {
                dominated_captures.push(existing_name.clone());
            }
        }

        // add a new capture for the new name binding
        let new_capture = Capture {
            parent_captured_address: new_name_binding.load_address.clone(),
            address_type: binder
                .type_of_address(&new_name_binding.load_address, handler)
                .await?,

            // NOTE: While binding, we always capture by value. the
            // memory-checker pass will later adjust this to be by reference if
            // needed.
            capture_mode: CaptureMode::ByValue,

            span: Some(new_name_binding.span),
        };

        // insert a new capture and cooresponding name binding
        let new_capture_id = self.captures.captures.insert(new_capture);
        self.name_binding_point.named_patterns_by_name.insert(
            new_name.clone(),
            NameBinding {
                mutable: new_name_binding.mutable,
                load_address: Address::Memory(Memory::Capture(new_capture_id)),
                span: new_name_binding.span,
            },
        );

        // re-adjust all the dominated captures to use the new captured memory
        let mut removing_captures = Vec::new();

        for dominated_name in dominated_captures {
            let dominated_binding = self
                .name_binding_point
                .named_patterns_by_name
                .get_mut(&dominated_name)
                .unwrap();

            let mut dominated_address = dominated_binding.load_address.clone();
            let dominated_root = dominated_address
                .get_root_memory()
                .as_capture()
                .copied()
                .unwrap();

            let dominated_root_captured_address =
                &self.captures.captures[dominated_root].parent_captured_address;

            // replace the captured root with the actual original captured
            // address
            assert!(dominated_address.replace_with(
                &Address::Memory(Memory::Capture(dominated_root)),
                dominated_root_captured_address.clone(),
            ));

            // replace the dominated binding to use the new captured memory
            assert!(dominated_address.replace_with(
                &new_name_binding.load_address,
                Address::Memory(Memory::Capture(new_capture_id)),
            ));

            // update the dominated binding
            dominated_binding.load_address = dominated_address;

            // mark the dominated capture for removal
            removing_captures.push(dominated_root);
        }

        // remove all the dominated captures
        for dominated_capture in removing_captures {
            // it is possible we remove the same capture multiple times if
            // multiple names capture the same memory
            let _ = self.captures.captures.remove(dominated_capture);
        }

        Ok(())
    }
}

impl Binder<'_> {
    /// Creates a capturing structure representing all the captures available
    /// at the current point in the binding process.
    pub async fn create_captures(
        &self,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Captures, UnrecoverableError> {
        Captures::new(&self.stack, self, handler).await
    }

    /// Creates a nested binder that can be used to produce a nested IR.
    pub async fn new_closure_binder(
        &mut self,
        f: impl AsyncFnOnce(&mut Binder<'_>) -> Result<(), UnrecoverableError>,
        expected_type: Type,
        closure_span: RelativeSpan,
        captures: &Captures,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<IR, UnrecoverableError> {
        // temporary move out the inference context for the inner binder
        let inference_context = std::mem::take(&mut self.inference_context);

        let ir = IR::default();
        let current_block_id = ir.control_flow_graph.entry_block_id();

        let mut stack = Stack::new(ir.scope_tree.root_scope_id(), false);
        stack
            .current_scope_mut()
            .add_named_binding_point(captures.name_binding_point.clone());

        let mut binder = Binder {
            engine: self.engine,
            environment: self.environment,
            captures: Some(&captures.captures),
            ir,
            current_block_id,
            stack,
            inference_context,
            unreachable_register_ids: Vec::new(),
            expected_closure_return_type: Some(expected_type.clone()),
            effect_handler_context: std::mem::take(
                &mut self.effect_handler_context,
            ),
            block_context: block::Context::default(),
            loop_context: r#loop::Context::default(),
        };

        let root_scope_id = binder.ir.scope_tree.root_scope_id();
        binder.push_instruction(instruction::Instruction::ScopePush(
            ScopePush(root_scope_id),
        ));

        let result = f(&mut binder).await;

        binder
            .check_closure_return_type(expected_type, closure_span, handler)
            .await?;

        // restore back the inference context and the handler groups
        self.inference_context = binder.inference_context;
        self.effect_handler_context = binder.effect_handler_context;

        result.map(|()| binder.ir)
    }

    async fn check_closure_return_type(
        &mut self,
        expected_type: Type,
        closure_span: RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        // check if there's any return instructions in the closure

        let has_return = self.ir.control_flow_graph.traverse().any(|x| {
            x.1.terminator()
                .is_some_and(pernixc_ir::instruction::Terminator::is_return)
        });

        // no return, means the closure returns unit
        if !has_return {
            let unit = Type::unit();

            if self
                .type_check_as_diagnostic(
                    &unit,
                    Expected::Known(expected_type.clone()),
                    closure_span,
                    handler,
                )
                .await?
                .is_some()
            {
                handler.receive(Diagnostic::MismatchedClosureReturnType(
                    MismatchedClosureReturnType {
                        expected: expected_type,
                        found: unit,
                        has_no_return: !has_return,
                        closure_span,
                        type_inference_map: self.type_inference_rendering_map(),
                        constant_inference_map: self
                            .constant_inference_rendering_map(),
                    },
                ));
            }

            return Ok(());
        }

        let is_unit = {
            let checkpoint = self.start_inference_context_checkpoint();
            let is_unit = self
                .type_check_as_diagnostic(
                    &expected_type,
                    Expected::Known(Type::unit()),
                    closure_span,
                    handler,
                )
                .await?
                .is_none();

            self.restore_inference_context_checkpoint(checkpoint);

            is_unit
        };

        // if isn't unit, we check all return instructions
        if !is_unit {
            let all_has_return =
                self.ir.control_flow_graph.traverse().all(|x| {
                    x.1.terminator().is_some_and(
                        pernixc_ir::instruction::Terminator::is_return,
                    )
                });

            if !all_has_return {
                handler.receive(
                    Diagnostic::NotAllFlowPathsReturnAValueInClosure(
                        NotAllFlowPathsReturnAValueInClosure {
                            closure_span,
                            return_type: expected_type,
                            type_inference_map: self
                                .type_inference_rendering_map(),
                            constant_inference_map: self
                                .constant_inference_rendering_map(),
                        },
                    ),
                );
            }
        }

        Ok(())
    }
}

/// A structure used for pruning the closure captures. Initially, the closure
/// captures all the variables with by value capture mode. This is the most
/// general settings. To ensure that the closure captures only the necessary
/// variables, we can use the `PruningContext` to track the usage of each
/// variable and prune the closure captures accordingly.
#[derive(Debug, Default)]
struct PruningContext {
    unsages: HashMap<ID<capture::Capture>, CaptureMode>,
}

impl PruningContext {
    fn new() -> Self { Self { unsages: HashMap::default() } }

    fn visit_usage(
        &mut self,
        capture: ID<capture::Capture>,
        mode: CaptureMode,
    ) {
        let order_capture_mode = |x: &CaptureMode| match x {
            CaptureMode::ByValue => 2,
            CaptureMode::ByReference(by_ref) => match by_ref.qualifier {
                Qualifier::Immutable => 0,
                Qualifier::Mutable => 1,
            },
        };

        match self.unsages.entry(capture) {
            Entry::Occupied(mut occupied_entry) => {
                let existing_rank = order_capture_mode(occupied_entry.get());
                let new_rank = order_capture_mode(&mode);

                // update to the more general capture mode
                if new_rank > existing_rank {
                    occupied_entry.insert(mode);
                }
            }

            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(mode);
            }
        }
    }

    fn visit_instruction(
        &mut self,
        instruction: &Instruction,
        values: &Values,
        can_fn_once: bool,
    ) {
        for (address, access) in instruction.get_access_address(values) {
            // we're interested in the capture memory only
            let Memory::Capture(root_address) = address.get_root_memory()
            else {
                continue;
            };

            self.visit_usage(*root_address, match access {
                instruction::AccessKind::Normal(access_mode) => {
                    match access_mode {
                        instruction::AccessMode::Read(read) => {
                            CaptureMode::ByReference(ReferenceCaptureMode {
                                lifetime: Lifetime::Erased,
                                qualifier: read.qualifier,
                            })
                        }
                        instruction::AccessMode::Load(_) => {
                            if can_fn_once {
                                CaptureMode::ByValue
                            } else {
                                CaptureMode::ByReference(ReferenceCaptureMode {
                                    lifetime: Lifetime::Erased, /* defaults to
                                                                 * erased */
                                    qualifier: Qualifier::Immutable,
                                })
                            }
                        }
                        instruction::AccessMode::Write(_) => {
                            CaptureMode::ByReference(ReferenceCaptureMode {
                                lifetime: Lifetime::Erased, /* defaults to
                                                             * erased */
                                qualifier: Qualifier::Mutable,
                            })
                        }
                    }
                }
                instruction::AccessKind::Drop => unreachable!(),
            });
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum AccessMode {
    Borrow(Qualifier),
    Move,
}

impl Binder<'_> {
    /// Compares the captures and their usage in the IRs. Then, the captures
    /// are pruned based on their usage information and adjust the usage in
    /// the IRs accordingly.
    pub(crate) fn prunes_capture_ir<'x, I: Iterator<Item = &'x mut IR>>(
        irs: I,
        captures: &mut pernixc_ir::capture::Captures,
        can_fn_once: bool,
    ) {
        let mut irs = irs.collect::<Vec<_>>();

        let mut pruning_context = PruningContext::new();

        // visit all instructions in all IRs to collect usage information
        for ir in &mut irs {
            ir.control_flow_graph.traverse().for_each(|(_, block)| {
                for inst in block.instructions() {
                    pruning_context.visit_instruction(
                        inst,
                        &ir.values,
                        can_fn_once,
                    );
                }
            });
        }

        let all_capture_ids = captures.captures.ids().collect::<Vec<_>>();

        // remove unused captures
        for id in all_capture_ids {
            if pruning_context.unsages.contains_key(&id).not() {
                let _ = captures.captures.remove(id);
            }
        }

        for (capture_id, new_capture) in pruning_context.unsages {
            captures.captures[capture_id].capture_mode = new_capture;
        }

        // adjust all IRs to use the new capture modes
        for ir in &mut irs {
            for inst in ir.control_flow_graph.traverse_mut_instructions() {
                match inst {
                    Instruction::Store(store) => {
                        Self::adjust_memory_usage(
                            &mut store.address,
                            AccessMode::Borrow(Qualifier::Mutable),
                            captures,
                        );
                    }

                    Instruction::RegisterAssignment(register_assignment) => {
                        let register =
                            &mut ir.values.registers[register_assignment.id];

                        match &mut register.assignment {
                            Assignment::Load(load) => {
                                Self::adjust_memory_usage(
                                    &mut load.address,
                                    AccessMode::Move,
                                    captures,
                                );
                            }

                            Assignment::Borrow(borrow) => {
                                Self::adjust_memory_usage(
                                    &mut borrow.address,
                                    AccessMode::Borrow(borrow.qualifier),
                                    captures,
                                );
                            }

                            Assignment::VariantNumber(variant_number) => {
                                Self::adjust_memory_usage(
                                    &mut variant_number.address,
                                    AccessMode::Borrow(Qualifier::Immutable),
                                    captures,
                                );
                            }

                            Assignment::Tuple(_)
                            | Assignment::Prefix(_)
                            | Assignment::Struct(_)
                            | Assignment::Variant(_)
                            | Assignment::FunctionCall(_)
                            | Assignment::Binary(_)
                            | Assignment::Array(_)
                            | Assignment::Phi(_)
                            | Assignment::Cast(_) => {}
                        }
                    }

                    Instruction::TuplePack(tuple_pack) => {
                        Self::adjust_memory_usage(
                            &mut tuple_pack.tuple_address,
                            AccessMode::Move,
                            captures,
                        );
                        Self::adjust_memory_usage(
                            &mut tuple_pack.store_address,
                            AccessMode::Borrow(Qualifier::Mutable),
                            captures,
                        );
                    }

                    Instruction::RegisterDiscard(_)
                    | Instruction::ScopePush(_)
                    | Instruction::ScopePop(_) => {}

                    Instruction::DropUnpackTuple(_) | Instruction::Drop(_) => {
                        unreachable!()
                    }
                }
            }
        }
    }

    fn adjust_memory_usage(
        address: &mut Address,
        access_mode: AccessMode,
        captures: &pernixc_ir::capture::Captures,
    ) {
        let Memory::Capture(capture_id) = address.get_root_memory() else {
            return;
        };

        let capture_memory_address =
            Address::Memory(Memory::Capture(*capture_id));

        let capture = &captures.captures[*capture_id];

        match (&capture.capture_mode, access_mode) {
            (
                CaptureMode::ByValue,
                AccessMode::Borrow(_) | AccessMode::Move,
            ) => {
                // no-adjustment needed
            }

            (
                CaptureMode::ByReference(reference_capture_mode),
                AccessMode::Borrow(qualifier),
            ) => {
                if qualifier == Qualifier::Mutable {
                    assert_eq!(
                        reference_capture_mode.qualifier,
                        Qualifier::Mutable,
                        "cannot borrow immutable reference as mutable"
                    );
                }

                address.replace_with(
                    &capture_memory_address.clone(),
                    Address::Reference(Reference {
                        qualifier: reference_capture_mode.qualifier,
                        reference_address: Box::new(capture_memory_address),
                    }),
                );
            }
            (
                CaptureMode::ByReference(reference_capture_mode),
                AccessMode::Move,
            ) => {
                address.replace_with(
                    &capture_memory_address.clone(),
                    Address::Reference(Reference {
                        qualifier: reference_capture_mode.qualifier,
                        reference_address: Box::new(capture_memory_address),
                    }),
                );
            }
        }
    }
}
