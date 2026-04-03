//! Contains logic related to effect handlers and handler groups.

use pernixc_arena::ID;
use pernixc_ir::{
    IRWithContext,
    capture::Captures,
    function_ir::IRContext,
    handling_scope::{
        self, HandlerClause, HandlerClauseID, HandlingScope, HandlingScopes,
        OperationHandlerID,
    },
    ir::IR,
    value::register::do_with::{Do, OperationHandler},
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::parameter::Parameter;
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments, instantiation::Instantiation,
    r#type::Type,
};
use pernixc_type_system::{OverflowError, environment::Premise};

use crate::{
    binder::{
        Binder,
        inference_context::{InferenceContext, Unifiable, UnifyError},
    },
    infer::constraint,
};

/// Context struct for managing effect handlers.
#[derive(Debug, Clone, Default)]
pub struct Context {
    handling_scopes: HandlingScopes,
    handler_gruop_stack: Vec<pernixc_arena::ID<HandlingScope>>,
    operation_handler_stack: Vec<OperationHandlerID>,
}

impl pernixc_ir::resolution_visitor::MutableResolutionVisitable for Context {
    async fn accept_mut<
        T: pernixc_ir::resolution_visitor::MutableResolutionVisitor,
    >(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), pernixc_ir::resolution_visitor::Abort> {
        self.handling_scopes.accept_mut(visitor).await
    }
}

struct HandlerClauseMatcher<'x, 'y> {
    infer_ctx: &'x mut InferenceContext,
    premise: &'y Premise,
    engine: &'y TrackedEngine,
}

impl handling_scope::HandlerClauseMatcher for HandlerClauseMatcher<'_, '_> {
    async fn matches_generic_arguments(
        &mut self,
        target_generic_arguments: &GenericArguments,
        handler_generic_arguments: &GenericArguments,
    ) -> Result<bool, OverflowError> {
        if !handler_generic_arguments.arity_matches(target_generic_arguments) {
            return Ok(false);
        }

        let cp = self.infer_ctx.start_checkpoint();

        for (a, b) in target_generic_arguments
            .types()
            .iter()
            .zip(handler_generic_arguments.types())
        {
            if !self.unify_term(a, b).await? {
                self.infer_ctx.restore(cp);
                return Ok(false);
            }
        }

        for (a, b) in target_generic_arguments
            .constants()
            .iter()
            .zip(handler_generic_arguments.constants())
        {
            if !self.unify_term(a, b).await? {
                self.infer_ctx.restore(cp);
                return Ok(false);
            }
        }

        for (_, _) in target_generic_arguments
            .instances()
            .iter()
            .zip(handler_generic_arguments.instances())
        {
            todo!()
        }

        // commit the checkpoint if all unifications succeeded
        self.infer_ctx.commit_checkpoint(cp);

        Ok(true)
    }
}

impl HandlerClauseMatcher<'_, '_> {
    async fn unify_term<T: Unifiable>(
        &mut self,
        a: &T,
        b: &T,
    ) -> Result<bool, OverflowError> {
        match self.infer_ctx.unify(a, b, self.premise, self.engine).await {
            Ok(()) => Ok(true),
            Err(
                UnifyError::CyclicTypeInference(_)
                | UnifyError::CyclicConstantInference(_)
                | UnifyError::CyclicInstanceInference(_)
                | UnifyError::IncompatibleTypes { .. }
                | UnifyError::IncompatibleConstants { .. }
                | UnifyError::IncompatibleInstances { .. }
                | UnifyError::UnsatisfiedConstraint(_)
                | UnifyError::CombineConstraint(_),
            ) => Ok(false),

            Err(UnifyError::OverflowError(e)) => Err(e),
        }
    }
}

impl Binder<'_> {
    /// Creates a new `do` expression with the given IR and captures.
    #[must_use]
    pub fn new_do(
        &mut self,
        ir: IR,
        captures: Captures,
        handling_scope_id: ID<HandlingScope>,
        do_span: RelativeSpan,
    ) -> Do {
        let (capture_id, captures_args) =
            self.bind_capture_arguments(captures, do_span);

        let ir_id = self.ir_map.new_ir(IRWithContext::new(
            ir,
            IRContext::new_do_context(capture_id, handling_scope_id),
        ));

        Do::new(captures_args, ir_id)
    }

    /// Returns the handling scopes managed by the binder.
    #[must_use]
    pub const fn handling_scopes(&self) -> &HandlingScopes {
        &self.effect_handler_context.handling_scopes
    }

    /// Returns the current operation handler ID, if any.
    #[must_use]
    pub fn current_operation_handler_id(&self) -> Option<OperationHandlerID> {
        self.effect_handler_context.operation_handler_stack.last().copied()
    }

    /// Pushes a new operation handler ID onto the operation handler stack.
    pub fn push_operation_handler(
        &mut self,
        operation_handler_id: OperationHandlerID,
    ) {
        self.effect_handler_context
            .operation_handler_stack
            .push(operation_handler_id);
    }

    /// Pops the topmost operation handler ID from the operation handler stack.
    pub fn pop_operation_handler(
        &mut self,
        operation_handler_id: OperationHandlerID,
    ) {
        assert_eq!(
            self.effect_handler_context
                .operation_handler_stack
                .pop()
                .expect("Operation handler stack underflow"),
            operation_handler_id
        );
    }

    /// Creates a new operation handler with the given IR, captures, and
    /// closure parameters.
    #[must_use]
    pub fn new_operation_handler_ir(
        &mut self,
        ir: IR,
        captures_id: ID<Captures>,
        operation_handler_id: OperationHandlerID,
    ) -> OperationHandler {
        let ir_id = self.ir_map.new_ir(IRWithContext::new(
            ir,
            IRContext::new_operation_handler_context(
                operation_handler_id,
                captures_id,
            ),
        ));

        OperationHandler::new(ir_id)
    }

    /// Insert a new effect handler group
    ///
    /// Returns the ID of the newly inserted handling scope and its (inferring)
    /// return type.
    pub fn insert_handling_scope(
        &mut self,
        do_with_span: RelativeSpan,
    ) -> (pernixc_arena::ID<HandlingScope>, Type) {
        // create a fresh type variable for the return type
        let return_type =
            self.create_type_inference(constraint::Type::All(true));

        let handler_group = self
            .effect_handler_context
            .handling_scopes
            .insert_handler_scope(do_with_span, Type::Inference(return_type));

        self.effect_handler_context.handler_gruop_stack.push(handler_group);

        (handler_group, Type::Inference(return_type))
    }

    pub fn insert_operation_handler_to_handler_clause(
        &mut self,
        handler_clause_id: HandlerClauseID,
        effect_operation_symbol_id: pernixc_symbol::SymbolID,
    ) -> OperationHandlerID {
        let id = self
            .effect_handler_context
            .handling_scopes
            .insert_operation_handler_to_handler_clause(
                handler_clause_id,
                effect_operation_symbol_id,
            );

        OperationHandlerID::new(handler_clause_id, id)
    }

    /// Insert a new effect handler into an existing handler group
    pub fn insert_handler_clause_to_handling_scope(
        &mut self,
        handler_group_id: pernixc_arena::ID<HandlingScope>,
        handler: HandlerClause,
    ) -> pernixc_arena::ID<HandlerClause> {
        self.effect_handler_context
            .handling_scopes
            .insert_handler_clause_to_handling_scope(handler_group_id, handler)
    }

    /// Pops the topmost handler group from the stack
    pub fn pop_handling_scope(
        &mut self,
        handler_group: pernixc_arena::ID<HandlingScope>,
    ) {
        assert_eq!(
            self.effect_handler_context.handler_gruop_stack.pop().unwrap(),
            handler_group
        );
    }

    /// Search for an effect handler from the handler stack
    pub async fn search_handler_clause(
        &mut self,
        effect_id: Global<pernixc_symbol::SymbolID>,
        generic_arguments: &GenericArguments,
    ) -> Result<Option<HandlerClauseID>, OverflowError> {
        for (handler_id, handler_group) in self
            .effect_handler_context
            .handler_gruop_stack
            .iter()
            .rev()
            .copied()
            .map(|x| (x, &self.effect_handler_context.handling_scopes[x]))
        {
            if let Some(effect_handler_id) = handler_group
                .search_handler_clause(
                    effect_id,
                    generic_arguments,
                    &mut HandlerClauseMatcher {
                        premise: self.premise(),
                        infer_ctx: &mut self.inference_context,
                        engine: self.engine,
                    },
                )
                .await?
            {
                return Ok(Some(HandlerClauseID::new(
                    handler_id,
                    effect_handler_id,
                )));
            }
        }

        Ok(None)
    }

    #[must_use]
    pub fn get_global_operation_symbol_id(
        &self,
        operation_handler_id: OperationHandlerID,
    ) -> Global<pernixc_symbol::SymbolID> {
        self.effect_handler_context
            .handling_scopes
            .get_global_operation_symbol_id(operation_handler_id)
    }

    pub fn add_operation_parameter_span(
        &mut self,
        operation_handler_id: OperationHandlerID,
        parameter_id: ID<Parameter>,
        span: RelativeSpan,
    ) {
        self.effect_handler_context
            .handling_scopes
            .add_operation_parameter_span(
                operation_handler_id,
                parameter_id,
                span,
            );
    }

    pub async fn get_handler_instantiation(
        &self,
        handler_clause_id: HandlerClauseID,
    ) -> Instantiation {
        self.effect_handler_context
            .handling_scopes
            .get_handler_instantiation(handler_clause_id, self.engine)
            .await
    }
}

impl Context {
    /// Deconstruct the context into its handler groups
    #[must_use]
    pub fn into_handler_groups(self) -> HandlingScopes { self.handling_scopes }

    /// Asserts that the handler group stack is empty
    ///
    /// This is called once after the binder is finalized.
    pub fn assert_empty(&self) {
        assert!(self.handler_gruop_stack.is_empty());
    }
}
