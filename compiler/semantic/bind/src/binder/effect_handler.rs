use pernixc_ir::effect_handler::{EffectHandler, HandlerGroup};

use crate::binder::Binder;

impl Binder<'_> {
    /// Insert a new effect handler group
    pub fn insert_effect_handler_group(
        &mut self,
    ) -> pernixc_arena::ID<HandlerGroup> {
        self.handler_groups.insert_handler_group()
    }

    /// Insert a new effect handler into an existing handler group
    pub fn insert_effect_handler_to_group(
        &mut self,
        handler_group_id: pernixc_arena::ID<HandlerGroup>,
        handler: EffectHandler,
    ) -> pernixc_arena::ID<EffectHandler> {
        self.handler_groups
            .insert_effect_handler_to_group(handler_group_id, handler)
    }
}
