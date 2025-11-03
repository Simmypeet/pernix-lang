//! Defines the IR structures for representing `do-with` expressions.

use getset::{CopyGetters, Getters};
use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    capture::{Capture, Captures},
    effect_handler::HandlerGroup,
    value::{register::Register, Value},
    IR,
};

/// Representing the capture initialization. This contains all the values
/// used to initialize the captures.
#[derive(
    Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize, Getters,
)]
pub struct CaptureArguments {
    /// The captures structure
    captures: Captures,
    arguments: HashMap<pernixc_arena::ID<Capture>, Value>,
}

impl CaptureArguments {
    /// Creates a new [`CaptureArguments`] with the given captures and has
    /// empty arguments.
    #[must_use]
    pub fn new(captures: Captures) -> Self {
        Self { captures, arguments: HashMap::default() }
    }

    /// Creates a new [`CaptureArguments`] with the given captures and
    /// arguments.
    #[must_use]
    pub const fn new_with_arguments(
        captures: Captures,
        arguments: HashMap<pernixc_arena::ID<Capture>, Value>,
    ) -> Self {
        Self { captures, arguments }
    }

    /// Inserts a new capture argument mapping from the given capture ID to
    /// the provided value.
    pub fn insert(
        &mut self,
        capture_id: pernixc_arena::ID<Capture>,
        value: Value,
    ) {
        assert!(self.arguments.insert(capture_id, value).is_none());
    }

    /// Returns an iterator over the used registers in the capture arguments.
    pub fn get_used_registers(
        &self,
    ) -> impl Iterator<Item = ID<Register>> + '_ {
        self.arguments.values().filter_map(|x| x.as_register().copied())
    }
}

/// Represents a `do` part of an `do-with` expression.
///
/// This is where the main computation takes place, potentially involving
/// effectful operations.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct DoClosure {
    /// The capture strcture for the closure.
    capture_arguments: CaptureArguments,

    /// The IR containing the code body of the `do` closure.
    ir: IR,
}

impl DoClosure {
    /// Creates a new `DoClosure` with the given capture structure and IR.
    #[must_use]
    pub const fn new(capture_arguments: CaptureArguments, ir: IR) -> Self {
        Self { capture_arguments, ir }
    }
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
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct With {
    /// The capture structure used for all of the effect handlers in this
    /// `do-with` expression.
    capture_arguments: CaptureArguments,

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
    pub fn new(capture_arguments: CaptureArguments) -> Self {
        Self { capture_arguments, effect_handlers: HashMap::default() }
    }

    /// Inserts a new effect handler for a specific effect ID.
    pub fn insert_effect_handler(
        &mut self,
        effect_id: pernixc_arena::ID<crate::effect_handler::EffectHandler>,
    ) -> &mut EffectHandler {
        self.effect_handlers.entry(effect_id).or_default()
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

impl Do {
    /// Retrieves all the registers used by this `do` expression.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        let mut registers = Vec::new();
        registers.extend(self.with.capture_arguments.get_used_registers());
        registers.extend(self.closure.capture_arguments.get_used_registers());
        registers
    }
}
