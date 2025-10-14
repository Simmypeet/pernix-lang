//! Defines effect handlers used in `do` expressions.

use pernixc_arena::Arena;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::generic_arguments::GenericArguments;

use crate::Values;

/// Represents a group of with effect handlers in a `do` expression.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct HandlerGroup {
    effect_handlers: Arena<EffectHandler>,
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
)]
pub struct EffectHandlerID {
    handler_group_id: pernixc_arena::ID<HandlerGroup>,
    effect_handler_id: pernixc_arena::ID<EffectHandler>,
}

impl Values {
    /// Gets the [`EffectHandler`] with the [`EffectHandlerID`].
    #[must_use]
    pub fn get_effect_handler(&self, id: EffectHandlerID) -> &EffectHandler {
        self.handler_groups
            .get(id.handler_group_id)
            .unwrap()
            .effect_handlers
            .get(id.effect_handler_id)
            .unwrap()
    }
}
