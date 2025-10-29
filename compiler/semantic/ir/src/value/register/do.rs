//! Defines the IR structures for representing `do-with` expressions.

use getset::CopyGetters;
use pernixc_hash::HashMap;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{capture::Captures, effect_handler::HandlerGroup, IR};

/// Represents a `do` part of an `do-with` expression.
///
/// This is where the main computation takes place, potentially involving
/// effectful operations.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct DoClosure {
    /// The capture strcture for the closure.
    capture: Captures,

    /// The IR containing the code body of the `do` closure.
    ir: IR,
}

impl DoClosure {
    /// Creates a new `DoClosure` with the given capture structure and IR.
    #[must_use]
    pub const fn new(capture: Captures, ir: IR) -> Self { Self { capture, ir } }
}

/// Represents a single `with` handler for a specific `effect` that could
/// have multiple operation handlers.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    StableHash,
    Serialize,
    Deserialize,
    CopyGetters,
)]
pub struct EffectHandler {
    /// The closure for handling each effect operation defined in the `effect`.
    effect_operation_handler_closures:
        HashMap<pernixc_symbol::ID, EffectOperationHandlerClosure>,
}

impl EffectHandler {
    /// Inserts a new handler closure for a specific effect operation.
    pub fn insert_effect_operation_handler_closure(
        &mut self,
        effect_operation_id: pernixc_symbol::ID,
        closure: EffectOperationHandlerClosure,
    ) {
        assert!(self
            .effect_operation_handler_closures
            .insert(effect_operation_id, closure)
            .is_none());
    }
}

/// The closure for handling a specific effect operation within an
/// [`EffectHandler`].
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct EffectOperationHandlerClosure {
    /// The IR containing the code body of the effect operation handler.
    ir: IR,
}

impl EffectOperationHandlerClosure {
    /// Creates a new effect operation handler closure with the given IR.
    #[must_use]
    pub const fn new(ir: IR) -> Self { Self { ir } }
}

/// Represents a group of `with` handlers following a `do` expression.
#[derive(
    Debug, Clone, PartialEq, Eq, Default, StableHash, Serialize, Deserialize,
)]
pub struct With {
    /// The capture structure used for all of the effect handlers in this
    /// `do-with` expression.
    capture: Captures,

    /// The effect handlers mapped by their unique IDs within the top-level
    /// IR (function-level IR).
    effect_handlers: HashMap<
        pernixc_arena::ID<crate::effect_handler::EffectHandler>,
        EffectHandler,
    >,
}

impl With {
    /// Creates a new `With` structure with the given capture structure.
    #[must_use]
    pub fn new(capture: Captures) -> Self {
        Self { capture, effect_handlers: HashMap::default() }
    }

    /// Inserts a new effect handler for a specific effect ID.
    pub fn insert_effect_handler(
        &mut self,
        effect_id: pernixc_arena::ID<crate::effect_handler::EffectHandler>,
        handler: EffectHandler,
    ) {
        assert!(self.effect_handlers.insert(effect_id, handler).is_none());
    }
}

/// Represents a `do-with` expression in the IR.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct Do {
    /// The unique ID of this `do-with` expression within the function-level
    /// IR.
    handler_group: pernixc_arena::ID<HandlerGroup>,

    /// The closure for the `do` part of the expression.
    closure: DoClosure,

    /// The `with` handlers associated with this `do` expression.
    with: With,

    /// The return type of the `do` expression.
    return_type: pernixc_term::r#type::Type,
}

impl Do {
    /// Creates a new `Do` expression with the given components.
    #[must_use]
    pub const fn new(
        handler_group: pernixc_arena::ID<HandlerGroup>,
        closure: DoClosure,
        with: With,
        return_type: pernixc_term::r#type::Type,
    ) -> Self {
        Self { handler_group, closure, with, return_type }
    }
}
