//! Contains logic related to effect handlers and handler groups.

use pernixc_arena::ID;
use pernixc_ir::{
    IRWithContext,
    capture::Captures,
    closure_parameters::ClosureParameters,
    function_ir::IRContext,
    handling_scope::{
        HandlerClause, HandlerClauseID, HandlingScope, HandlingScopes,
    },
    ir::IR,
    value::register::do_with::{Do, OperationHandler},
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_target::Global;
use pernixc_term::generic_arguments::GenericArguments;
use pernixc_type_system::{environment::Environment, normalizer::Normalizer};

use crate::binder::Binder;

/// Context struct for managing effect handlers.
#[derive(Debug, Clone, Default)]
pub struct Context {
    handling_scopes: HandlingScopes,
    handler_gruop_stack: Vec<pernixc_arena::ID<HandlingScope>>,
}

impl Binder<'_> {
    /// Creates a new `do` expression with the given IR and captures.
    #[must_use]
    pub fn new_do(
        &mut self,
        ir: IR,
        captures: Captures,
        do_span: RelativeSpan,
    ) -> Do {
        let (capture_id, captures_args) =
            self.bind_capture_arguments(captures, do_span);

        let ir_id = self.ir_map.new_ir(IRWithContext::new(
            ir,
            IRContext::new_do_context(capture_id),
        ));

        Do::new(captures_args, ir_id)
    }

    /// Creates a new operation handler with the given IR, captures, and
    /// closure parameters.
    #[must_use]
    pub fn new_operation_handler(
        &mut self,
        ir: IR,
        captures_id: ID<Captures>,
        closure_parameters: ClosureParameters,
    ) -> OperationHandler {
        let closure_parameters_id =
            self.insert_closure_parameters(closure_parameters);

        let ir_id = self.ir_map.new_ir(IRWithContext::new(
            ir,
            IRContext::new_operation_handler_context(
                closure_parameters_id,
                captures_id,
            ),
        ));

        OperationHandler::new(ir_id)
    }

    /// Insert a new effect handler group
    pub fn insert_effect_handler_group(
        &mut self,
    ) -> pernixc_arena::ID<HandlingScope> {
        let handler_group =
            self.effect_handler_context.handling_scopes.insert_handler_scope();

        self.effect_handler_context.handler_gruop_stack.push(handler_group);

        handler_group
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
        &self,
        effect_id: Global<pernixc_symbol::ID>,
        generic_arguments: &GenericArguments,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<HandlerClauseID>, pernixc_type_system::Error> {
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
                    environment,
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
