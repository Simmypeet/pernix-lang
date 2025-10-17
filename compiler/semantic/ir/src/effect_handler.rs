//! Defines effect handlers used in `do` expressions.

use derive_more::Index;
use pernixc_arena::Arena;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::generic_arguments::GenericArguments;

/// A collection of all the effect handler groups in a function body.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    StableHash,
    Serialize,
    Deserialize,
    Default,
    Index,
)]
pub struct HandlerGroups(Arena<HandlerGroup>);

impl HandlerGroups {
    /// Gets the [`EffectHandler`] with the [`EffectHandlerID`].
    #[must_use]
    pub fn get_effect_handler(&self, id: EffectHandlerID) -> &EffectHandler {
        self.0
            .get(id.handler_group_id)
            .unwrap()
            .effect_handlers
            .get(id.effect_handler_id)
            .unwrap()
    }

    /// Inserts a new [`HandlerGroup`] into the collection.
    pub fn insert_handler_group(&mut self) -> pernixc_arena::ID<HandlerGroup> {
        self.0.insert(HandlerGroup::default())
    }

    /// Inserts a new [`EffectHandler`] into the [`HandlerGroup`] with the
    /// [`HandlerGroupID`].
    pub fn insert_effect_handler_to_group(
        &mut self,
        handler_group_id: pernixc_arena::ID<HandlerGroup>,
        effect_handler: EffectHandler,
    ) -> pernixc_arena::ID<EffectHandler> {
        let handler_group = self.0.get_mut(handler_group_id).unwrap();
        handler_group.effect_handlers.insert(effect_handler)
    }
}

/// Represents a group of with effect handlers in a `do` expression.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    StableHash,
    Serialize,
    Deserialize,
    Default,
    Index,
)]
pub struct HandlerGroup {
    #[index]
    effect_handlers: Arena<EffectHandler>,
}

impl HandlerGroup {
    /// Adds an effect handler to this group.
    #[must_use]
    pub fn add_effect_handler(
        &mut self,
        effect_handler: EffectHandler,
    ) -> pernixc_arena::ID<EffectHandler> {
        self.effect_handlers.insert(effect_handler)
    }
}

/// An effect handler for a specific effect.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_new::new,
)]
pub struct EffectHandler {
    effect_id: Global<pernixc_symbol::ID>,
    generic_arguments: GenericArguments,
}

/// An ID that uniquely identifies an [`EffectHandler`] within an [`IR`].
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_new::new,
)]
pub struct EffectHandlerID {
    handler_group_id: pernixc_arena::ID<HandlerGroup>,
    effect_handler_id: pernixc_arena::ID<EffectHandler>,
}
