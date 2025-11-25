//! Defines the IR structures for representing `do-with` expressions.

use std::ops::Deref;

use getset::{CopyGetters, Getters};
use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::{constant::Constant, lifetime::Lifetime, r#type::Type};
use pernixc_type_system::{Error, Succeeded, normalizer::Normalizer};

use crate::{
    IRWithContext, Values,
    capture::Capture,
    handling_scope::HandlingScope,
    transform::{self, Element, Transformer},
    value::{Environment, TypeOf, Value, register::Register},
};

/// Representing the capture initialization. This contains all the values
/// used to initialize the captures.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    StableHash,
    Serialize,
    Deserialize,
    Getters,
)]
pub struct CaptureArguments {
    arguments: HashMap<pernixc_arena::ID<Capture>, Value>,
}

impl CaptureArguments {
    /// Creates a new [`CaptureArguments`] with the given captures and has
    /// empty arguments.
    #[must_use]
    pub fn new() -> Self { Self { arguments: HashMap::default() } }

    /// Creates a new [`CaptureArguments`] with the given captures and
    /// arguments.
    #[must_use]
    pub const fn new_with_arguments(
        arguments: HashMap<pernixc_arena::ID<Capture>, Value>,
    ) -> Self {
        Self { arguments }
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

impl transform::Element for CaptureArguments {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        _engine: &TrackedEngine,
    ) -> Result<(), CyclicError> {
        for value in
            self.arguments.values_mut().filter_map(|x| x.as_literal_mut())
        {
            value.transform(transformer).await?;
        }

        Ok(())
    }
}

/// Represents a `do` part of an `do-with` expression.
///
/// This is where the main computation takes place, potentially involving
/// effectful operations.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct Do {
    /// The capture strcture for the closure.
    capture_arguments: CaptureArguments,

    /// The IR containing the code body of the `do` closure.
    ir_id: ID<IRWithContext>,
}

impl Do {
    /// Creates a new `DoClosure` with the given capture structure and IR.
    #[must_use]
    pub const fn new(
        capture_arguments: CaptureArguments,
        ir_id: ID<IRWithContext>,
    ) -> Self {
        Self { capture_arguments, ir_id }
    }
}

impl transform::Element for Do {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &TrackedEngine,
    ) -> Result<(), CyclicError> {
        self.capture_arguments.transform(transformer, engine).await?;

        Ok(())
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
pub struct HandlerClause {
    /// The closure for handling each effect operation defined in the `effect`.
    effect_operation_handler_closures:
        HashMap<pernixc_symbol::ID, OperationHandler>,
}

impl HandlerClause {
    /// Inserts a new handler closure for a specific effect operation.
    pub fn insert_effect_operation_handler_closure(
        &mut self,
        effect_operation_id: pernixc_symbol::ID,
        closure: OperationHandler,
    ) {
        assert!(
            self.effect_operation_handler_closures
                .insert(effect_operation_id, closure)
                .is_none()
        );
    }
}

/// The closure for handling a specific effect operation within an
/// [`HandlerClause`].
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, StableHash, Serialize, Deserialize,
)]
pub struct OperationHandler {
    /// The IR containing the code body of the effect operation handler.
    ir_id: ID<IRWithContext>,
}

impl OperationHandler {
    /// Creates a new effect operation handler closure with the given IR.
    #[must_use]
    pub const fn new(ir_id: ID<IRWithContext>) -> Self { Self { ir_id } }
}

/// Represents a group of `with` handlers following a `do` expression.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct HandlerChain {
    /// The capture structure used for all of the handler clauses in this
    /// chain.
    capture_arguments: CaptureArguments,

    /// The effect handlers mapped by their unique IDs within the top-level
    /// IR (function-level IR).
    handler_clauses: HashMap<
        pernixc_arena::ID<crate::handling_scope::HandlerClause>,
        HandlerClause,
    >,
}

impl HandlerChain {
    /// Creates a new `With` structure with the given capture structure.
    #[must_use]
    pub fn new(capture_arguments: CaptureArguments) -> Self {
        Self { capture_arguments, handler_clauses: HashMap::default() }
    }

    /// Inserts a new effect handler for a specific effect ID.
    pub fn insert_handler_clause(
        &mut self,
        handler_clause_id: pernixc_arena::ID<
            crate::handling_scope::HandlerClause,
        >,
    ) -> &mut HandlerClause {
        self.handler_clauses.entry(handler_clause_id).or_default()
    }
}

impl transform::Element for HandlerChain {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &TrackedEngine,
    ) -> Result<(), CyclicError> {
        self.capture_arguments.transform(transformer, engine).await?;

        Ok(())
    }
}

/// Represents a `do-with` expression in the IR.
#[derive(
    Debug, Clone, PartialEq, Eq, StableHash, Getters, Serialize, Deserialize,
)]
pub struct DoWith {
    /// The unique ID of this `do-with` expression within the function-level
    /// IR.
    handling_scope_id: pernixc_arena::ID<HandlingScope>,

    /// The closure for the `do` part of the expression.
    do_block: Do,

    /// The `with` handlers associated with this `do` expression.
    handleer_chain: HandlerChain,
}

impl DoWith {
    /// Creates a new `Do` expression with the given components.
    #[must_use]
    pub const fn new(
        handling_scope_id: pernixc_arena::ID<HandlingScope>,
        closure: Do,
        with: HandlerChain,
    ) -> Self {
        Self { handling_scope_id, do_block: closure, handleer_chain: with }
    }
}

impl DoWith {
    /// Retrieves all the registers used by this `do` expression.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        let mut registers = Vec::new();
        registers
            .extend(self.handleer_chain.capture_arguments.get_used_registers());
        registers.extend(self.do_block.capture_arguments.get_used_registers());
        registers
    }
}

impl crate::visitor::Element for DoWith {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        for value in self.handleer_chain.capture_arguments.arguments.values() {
            visitor.visit_value(std::borrow::Cow::Borrowed(value));
        }
        for value in self.do_block.capture_arguments.arguments.values() {
            visitor.visit_value(std::borrow::Cow::Borrowed(value));
        }
    }
}

pub(super) async fn transform_do_with<
    T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
>(
    do_with: &mut DoWith,
    transformer: &mut T,
    engine: &TrackedEngine,
) -> Result<(), CyclicError> {
    do_with.do_block.transform(transformer, engine).await?;
    do_with.handleer_chain.transform(transformer, engine).await?;

    Ok(())
}

impl TypeOf<&DoWith> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        do_with: &DoWith,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        let handling_scope =
            &environment.handling_scopes[do_with.handling_scope_id];

        Ok(environment
            .type_environment
            .simplify(handling_scope.return_type().clone())
            .await?
            .deref()
            .clone())
    }
}
