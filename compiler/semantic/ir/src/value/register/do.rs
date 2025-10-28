//! Defines the IR structures for representing `do-with` expressions.

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

/// Represents a single `with` handler for a specific `effect` that could
/// have multiple operation handlers.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct EffectHandler {
    /// The unique ID of the effect handler seen within the top-level IR
    /// (function-level IR).
    effect_handler_id: pernixc_arena::ID<crate::effect_handler::EffectHandler>,

    /// The closure for handling each effect operation defined in the `effect`.
    effect_operation_handler_closures:
        HashMap<pernixc_symbol::ID, EffectOperationHandlerClosure>,
}

/// The closure for handling a specific effect operation within an
/// [`EffectHandler`].
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct EffectOperationHandlerClosure {
    /// The IR containing the code body of the effect operation handler.
    ir: IR,
}

/// Represents a group of `with` handlers following a `do` expression.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
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
