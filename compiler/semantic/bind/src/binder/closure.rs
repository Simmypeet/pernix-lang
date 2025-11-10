//! Defines the logic for building nested binders, such as for closures,
//! effect handlers, do blocks, etc.

use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_ir::{
    capture::{self, builder::CapturesWithNameBindingPoint, Captures},
    instruction::{self, ScopePush},
    value::{
        register::{
            load::{Load, Purpose},
            r#do::CaptureArguments,
            Assignment, Borrow,
        },
        Value,
    },
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

impl Binder<'_> {
    /// Creates a capturing structure representing all the captures available
    /// at the current point in the binding process.
    pub async fn create_captures(
        &self,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<CapturesWithNameBindingPoint, UnrecoverableError> {
        let mut builder = capture::builder::Builder::default();

        for scope in self.stack.scopes().iter().rev() {
            for name_binding_point in scope.named_binding_points().iter().rev()
            {
                for (name, binding) in
                    &name_binding_point.named_patterns_by_name
                {
                    if !builder.contains_name(name) {
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

        builder.build(&typer_env, &typer).await
    }

    /// Creates a nested binder that can be used to produce a nested IR.
    pub async fn new_closure_binder(
        &mut self,
        f: impl AsyncFnOnce(&mut Binder<'_>) -> Result<(), UnrecoverableError>,
        expected_type: Type,
        closure_span: RelativeSpan,
        captures: &CapturesWithNameBindingPoint,
        closure_parameters: Option<
            &pernixc_ir::closure_parameters::ClosureParameters,
        >,
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
            closure_parameters,
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

    /// Given the captures, bind all the load/borrow operations for each capture
    /// and create the [`CaptureArguments`] struct.
    pub fn bind_capture_arguments(
        &mut self,
        captures: Captures,
        capture_span: RelativeSpan,
    ) -> CaptureArguments {
        let mut capture_arguments = HashMap::default();

        for (capture_id, capture) in captures.captures_as_order() {
            // load or borrow the address based on the capture mode
            let value = match &capture.capture_mode {
                capture::CaptureMode::ByValue => self
                    .create_register_assignment(
                        Assignment::Load(Load::with_purpose(
                            capture.parent_captured_address.clone(),
                            Purpose::Capture,
                        )),
                        capture_span,
                    ),
                capture::CaptureMode::ByReference(reference_capture_mode) => {
                    self.create_register_assignment(
                        Assignment::Borrow(Borrow {
                            address: capture.parent_captured_address.clone(),
                            qualifier: reference_capture_mode.qualifier,
                            lifetime: pernixc_term::lifetime::Lifetime::Erased,
                        }),
                        capture_span,
                    )
                }
            };

            assert!(capture_arguments
                .insert(capture_id, Value::Register(value))
                .is_none());
        }

        CaptureArguments::new_with_arguments(captures, capture_arguments)
    }
}
