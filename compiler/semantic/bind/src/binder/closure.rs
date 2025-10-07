//! Defines the logic for building nested binders, such as for closures,
//! effect handlers, do blocks, etc.

use flexstr::SharedStr;
use pernixc_arena::Arena;
use pernixc_handler::Handler;
use pernixc_ir::{
    address::{Address, Memory},
    capture::{Capture, CaptureMode},
    instruction::{self, ScopePush},
    pattern::{NameBinding, NameBindingPoint},
    IR,
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_term::r#type::Type;
use pernixc_type_system::UnrecoverableError;

use crate::{
    binder::{block, r#loop, stack::Stack, type_check::Expected, Binder},
    diagnostic::{
        Diagnostic, MismatchedClosureReturnType,
        NotAllFlowPathsReturnAValueInClosure,
    },
};

#[derive(Default)]
struct Captures {
    captures: Arena<Capture>,
    name_binding_point: NameBindingPoint,
}

impl Captures {
    pub async fn new(
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

    pub async fn try_insert_named_binding(
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

            let existing_captured_address =
                &self.captures[existing_binding_root].parent_captured_address;

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
        let new_capture_id = self.captures.insert(new_capture);
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
                &self.captures[dominated_root].parent_captured_address;

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
            let _ = self.captures.remove(dominated_capture);
        }

        Ok(())
    }
}

impl Binder<'_> {
    /// Creates a nested binder that can be used to produce a nested IR.
    pub async fn new_closure_binder(
        &mut self,
        f: impl AsyncFnOnce(&mut Binder<'_>) -> Result<(), UnrecoverableError>,
        expected_type: Type,
        closure_span: RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<IR, UnrecoverableError> {
        // temporary move out the inference context for the inner binder
        let inference_context = std::mem::take(&mut self.inference_context);

        let ir = IR::default();
        let current_block_id = ir.control_flow_graph.entry_block_id();

        let captures = Captures::new(&self.stack, self, handler).await?;
        let ir_captures =
            pernixc_ir::capture::Captures { captures: captures.captures };

        let mut stack = Stack::new(ir.scope_tree.root_scope_id(), false);
        stack
            .current_scope_mut()
            .add_named_binding_point(captures.name_binding_point);

        let mut binder = Binder {
            engine: self.engine,
            environment: self.environment,
            captures: Some(&ir_captures),
            ir,
            current_block_id,
            stack,
            inference_context,
            unreachable_register_ids: Vec::new(),
            expected_closure_return_type: Some(expected_type.clone()),
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

        // restore back the inference context
        self.inference_context = binder.inference_context;

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
