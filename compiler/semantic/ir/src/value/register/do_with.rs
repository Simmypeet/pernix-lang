//! Defines the IR structures for representing `do-with` expressions.

use getset::{CopyGetters, Getters};
use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::{constant::Constant, lifetime::Lifetime, r#type::Type};
use pernixc_type_system::{normalizer::Normalizer, Error, Succeeded};

use crate::{
    capture::{Capture, Captures},
    closure_parameters::ClosureParameters,
    handling_scope::HandlingScope,
    transform::{self, Transformer},
    value::{register::Register, Environment, TypeOf, Value},
    Values, IR,
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

impl transform::Element for CaptureArguments {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &TrackedEngine,
    ) -> Result<(), CyclicError> {
        for value in
            self.arguments.values_mut().filter_map(|x| x.as_literal_mut())
        {
            value.transform(transformer).await?;
        }

        self.captures.transform(transformer, engine).await?;

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
    ir: IR,
}

impl Do {
    /// Creates a new `DoClosure` with the given capture structure and IR.
    #[must_use]
    pub const fn new(capture_arguments: CaptureArguments, ir: IR) -> Self {
        Self { capture_arguments, ir }
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
        Box::pin(self.ir.transform(transformer, engine)).await?;

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
        assert!(self
            .effect_operation_handler_closures
            .insert(effect_operation_id, closure)
            .is_none());
    }
}

impl transform::Element for HandlerClause {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &TrackedEngine,
    ) -> Result<(), CyclicError> {
        for closure in self.effect_operation_handler_closures.values_mut() {
            closure.transform(transformer, engine).await?;
        }

        Ok(())
    }
}

/// The closure for handling a specific effect operation within an
/// [`HandlerClause`].
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct OperationHandler {
    /// The IR containing the code body of the effect operation handler.
    ir: IR,

    /// The closure parameters for the effect operation handler.
    closure_parameters: ClosureParameters,
}

impl OperationHandler {
    /// Creates a new effect operation handler closure with the given IR.
    #[must_use]
    pub const fn new(ir: IR, closure_parameters: ClosureParameters) -> Self {
        Self { ir, closure_parameters }
    }
}

impl transform::Element for OperationHandler {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &TrackedEngine,
    ) -> Result<(), CyclicError> {
        self.closure_parameters.transform(transformer, engine).await?;
        Box::pin(self.ir.transform(transformer, engine)).await
    }
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

        for handler_clause in self.handler_clauses.values_mut() {
            handler_clause.transform(transformer, engine).await?;
        }

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
    handler_group: pernixc_arena::ID<HandlingScope>,

    /// The closure for the `do` part of the expression.
    do_block: Do,

    /// The `with` handlers associated with this `do` expression.
    handleer_chain: HandlerChain,

    /// The return type of the `do` expression.
    #[get = "pub"]
    return_type: pernixc_term::r#type::Type,
}

impl DoWith {
    /// Creates a new `Do` expression with the given components.
    #[must_use]
    pub const fn new(
        handler_group: pernixc_arena::ID<HandlingScope>,
        closure: Do,
        with: HandlerChain,
        return_type: pernixc_term::r#type::Type,
    ) -> Self {
        Self {
            handler_group,
            do_block: closure,
            handleer_chain: with,
            return_type,
        }
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

impl transform::Element for DoWith {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &TrackedEngine,
    ) -> Result<(), CyclicError> {
        self.do_block.transform(transformer, engine).await?;
        self.handleer_chain.transform(transformer, engine).await?;

        transformer
            .transform(
                &mut self.return_type,
                transform::TypeTermSource::DoReturnType,
                None,
            )
            .await?;

        Ok(())
    }
}

impl DoWith {
    /// Retrieves the mutable reference to the IR of the `do` closure and its
    /// captures.
    #[must_use]
    pub const fn do_closure_mut(&mut self) -> (&mut IR, &Captures) {
        (&mut self.do_block.ir, &self.do_block.capture_arguments.captures)
    }

    /// Retrieves the reference to the IR of the `do` closure and its captures.
    #[must_use]
    pub const fn do_closure(&self) -> (&IR, &Captures) {
        (&self.do_block.ir, &self.do_block.capture_arguments.captures)
    }

    /// Retrieves the reference to each of the closure of effect handler.
    pub fn with_closures(
        &self,
    ) -> (&Captures, impl Iterator<Item = (&IR, &ClosureParameters)>) {
        (
            &self.handleer_chain.capture_arguments.captures,
            self.handleer_chain
                .handler_clauses
                .values()
                .flat_map(|x| x.effect_operation_handler_closures.values())
                .map(|x| (&x.ir, &x.closure_parameters)),
        )
    }

    /// Retrieves the mutable reference to each of the closure of effect
    /// handler.
    pub fn with_closures_mut(
        &mut self,
    ) -> (&Captures, impl Iterator<Item = (&mut IR, &ClosureParameters)>) {
        (
            &self.handleer_chain.capture_arguments.captures,
            self.handleer_chain
                .handler_clauses
                .values_mut()
                .flat_map(|x| x.effect_operation_handler_closures.values_mut())
                .map(|x| (&mut x.ir, &x.closure_parameters)),
        )
    }
}

impl TypeOf<&DoWith> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        do_with: &DoWith,
        _environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        Ok(Succeeded::new(do_with.return_type().clone()))
    }
}
