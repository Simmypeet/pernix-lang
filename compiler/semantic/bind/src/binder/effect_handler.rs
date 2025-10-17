//! Contains logic related to effect handlers and handler groups.

use pernixc_ir::effect_handler::{
    EffectHandler, EffectHandlerID, HandlerGroup, HandlerGroups,
};
use pernixc_target::Global;
use pernixc_term::generic_arguments::GenericArguments;
use pernixc_type_system::{environment::Environment, normalizer::Normalizer};

use crate::binder::Binder;

/// Context struct for managing effect handlers.
#[derive(Debug, Clone, Default)]
pub struct Context {
    handler_groups: HandlerGroups,
    handler_gruop_stack: Vec<pernixc_arena::ID<HandlerGroup>>,
}

impl Binder<'_> {
    /// Insert a new effect handler group
    pub fn insert_effect_handler_group(
        &mut self,
    ) -> pernixc_arena::ID<HandlerGroup> {
        let handler_group =
            self.effect_handler_context.handler_groups.insert_handler_group();

        self.effect_handler_context.handler_gruop_stack.push(handler_group);

        handler_group
    }

    /// Insert a new effect handler into an existing handler group
    pub fn insert_effect_handler_to_group(
        &mut self,
        handler_group_id: pernixc_arena::ID<HandlerGroup>,
        handler: EffectHandler,
    ) -> pernixc_arena::ID<EffectHandler> {
        self.effect_handler_context
            .handler_groups
            .insert_effect_handler_to_group(handler_group_id, handler)
    }

    /// Pops the topmost handler group from the stack
    pub fn pop_handler_group(
        &mut self,
        handler_group: pernixc_arena::ID<HandlerGroup>,
    ) {
        assert_eq!(
            self.effect_handler_context.handler_gruop_stack.pop().unwrap(),
            handler_group
        );
    }

    /// Search for an effect handler from the handler stack
    pub async fn search_effect_handler(
        &self,
        effect_id: Global<pernixc_symbol::ID>,
        generic_arguments: &GenericArguments,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Option<EffectHandlerID>, pernixc_type_system::Error> {
        for (handler_id, handler_group) in self
            .effect_handler_context
            .handler_gruop_stack
            .iter()
            .rev()
            .copied()
            .map(|x| (x, &self.effect_handler_context.handler_groups[x]))
        {
            if let Some(effect_handler_id) = handler_group
                .search_effect_handler(
                    effect_id,
                    generic_arguments,
                    environment,
                )
                .await?
            {
                return Ok(Some(EffectHandlerID::new(
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
    pub fn into_handler_groups(self) -> HandlerGroups { self.handler_groups }
}
