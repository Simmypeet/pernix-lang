//! Defines the logic for building nested binders, such as for closures,
//! effect handlers, do blocks, etc.

use std::{collections::hash_map::Entry, ops::Not};

use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_ir::{
    address::{Address, Memory, Reference},
    capture::{
        self, CaptureMode, CapturesWithNameBindingPoint, ReferenceCaptureMode,
    },
    instruction::{self, Instruction, ScopePush},
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

impl Binder<'_> {
    /// Creates a capturing structure representing all the captures available
    /// at the current point in the binding process.
    pub async fn create_captures(
        &self,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<CapturesWithNameBindingPoint, UnrecoverableError> {
        let mut builder = capture::Builder::default();

        for scope in self.stack.scopes().iter().rev() {
            for name_binding_point in scope.named_binding_points().iter().rev()
            {
                for (name, binding) in
                    &name_binding_point.named_patterns_by_name
                {
                    if !builder.contains_name(&name) {
                        builder.insert_named_binding(
                            name.clone(),
                            binding,
                            self.type_of_address(
                                &binding.load_address,
                                handler,
                            )
                            .await?,
                        );
                    }
                }
            }
        }

        let typer_env = self.typer_environment();
        let typer = self.typer(handler);

        Ok(builder.build(&typer_env, &typer).await?)
    }

    /// Creates a nested binder that can be used to produce a nested IR.
    pub async fn new_closure_binder(
        &mut self,
        f: impl AsyncFnOnce(&mut Binder<'_>) -> Result<(), UnrecoverableError>,
        expected_type: Type,
        closure_span: RelativeSpan,
        captures: &CapturesWithNameBindingPoint,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<IR, UnrecoverableError> {
        // temporary move out the inference context for the inner binder
        let inference_context = std::mem::take(&mut self.inference_context);

        let ir = IR::default();
        let current_block_id = ir.control_flow_graph.entry_block_id();

        let mut stack = Stack::new(ir.scope_tree.root_scope_id(), false);
        stack
            .current_scope_mut()
            .add_named_binding_point(captures.name_binding_point().clone());

        let mut binder = Binder {
            engine: self.engine,
            environment: self.environment,
            captures: Some(captures.captures()),
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

/// A structure used for pruning the closure captures.
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
        prune_mode: PruneMode,
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
                            match prune_mode {
                                PruneMode::Once => CaptureMode::ByValue,
                                PruneMode::Multiple => CaptureMode::ByReference(
                                    ReferenceCaptureMode {
                                        lifetime: Lifetime::Erased, /* defaults to
                                                                     * erased */
                                        qualifier: Qualifier::Immutable,
                                    },
                                ),
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

/// The mode used for pruning captures.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PruneMode {
    /// The capture is used under the closure that can be called only once.
    Once,

    /// The capture is used under the closure that can be called multiple
    /// times.
    ///
    /// This implies that some operation like loading/moving on the capture
    /// value needs to be `Copy`.
    Multiple,
}

impl Binder<'_> {
    /// Initially, the closure captures all the variables with by-value capture
    /// mode. This is the most general settings. After binding, we look through
    /// all the usage of the captures and figure out the most restrictive
    /// capture mode for each capture (by-value, by-mut-ref, by-imm-ref),
    /// and adjust all the instructions related to the captures accordingly.
    ///
    /// # Parameters
    ///
    /// - captures: The captures which initially contains all captures with
    ///   by-value. After pruning, it will contain only the used captures with
    ///   the most restrictive capture mode.
    pub(crate) fn prune_capture_ir<'x, I: Iterator<Item = &'x mut IR>>(
        irs: I,
        captures: &mut pernixc_ir::capture::Captures,
        mode: PruneMode,
    ) {
        let mut irs = irs.collect::<Vec<_>>();

        let mut pruning_context = PruningContext::new();

        // visit all instructions in all IRs to collect usage information
        for ir in &mut irs {
            ir.control_flow_graph.traverse().for_each(|(_, block)| {
                for inst in block.instructions() {
                    pruning_context.visit_instruction(inst, &ir.values, mode);
                }
            });
        }

        let all_capture_ids = captures.ids().collect::<Vec<_>>();

        // remove unused captures
        for id in all_capture_ids {
            if pruning_context.unsages.contains_key(&id).not() {
                let _ = captures.remove(id);
            }
        }

        for (capture_id, new_capture) in pruning_context.unsages {
            captures[capture_id].capture_mode = new_capture;
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

        let capture = &captures[*capture_id];

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
