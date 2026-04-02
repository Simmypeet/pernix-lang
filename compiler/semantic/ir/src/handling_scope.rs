//! Defines effect handlers used in `do` expressions.

use derive_more::Index;
use getset::{CopyGetters, Getters};
use pernixc_arena::{Arena, ID};
use pernixc_hash::FxHashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::parameter::{
    Parameter, Parameters, get_parameters,
};
use pernixc_symbol::SymbolID;
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::{GenericArguments, Symbol},
    instantiation::Instantiation,
    r#type::Type,
};
use pernixc_type_system::OverflowError;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::resolution_visitor::{
    self, Abort, IntoResolutionWithSpan, MutableResolutionVisitor, Resolution,
    ResolutionMut, ResolutionVisitable, ResolutionVisitor,
};

macro_rules! visit_handling_scopes {
    ($iterable:expr, $visitor:expr, $accept_method:ident) => {{
        for handling_scope in $iterable {
            handling_scope.$accept_method($visitor).await?;
        }
        Ok(())
    }};
}

/// A collection of all the effect handler groups in a function body.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Encode, Decode, Default)]
pub struct HandlingScopes(Arena<HandlingScope>);

impl std::ops::Index<HandlerClauseID> for HandlingScopes {
    type Output = HandlerClause;

    fn index(&self, index: HandlerClauseID) -> &Self::Output {
        self.0
            .get(index.handler_group_id)
            .unwrap()
            .handler_clauses
            .get(index.effect_handler_id)
            .unwrap()
    }
}

impl std::ops::Index<ID<HandlingScope>> for HandlingScopes {
    type Output = HandlingScope;

    fn index(&self, index: ID<HandlingScope>) -> &Self::Output {
        self.0.get(index).unwrap()
    }
}

impl resolution_visitor::MutableResolutionVisitable for HandlingScopes {
    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        visit_handling_scopes!(self.0.items_mut(), visitor, accept_mut)
    }
}

impl resolution_visitor::ResolutionVisitable for HandlingScopes {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        visit_handling_scopes!(self.0.items(), visitor, accept)
    }
}

impl HandlingScopes {
    #[must_use]
    fn get_handler_clause(&self, id: HandlerClauseID) -> &HandlerClause {
        self.0
            .get(id.handler_group_id)
            .unwrap()
            .handler_clauses
            .get(id.effect_handler_id)
            .unwrap()
    }

    #[must_use]
    pub fn get_visitable_handler_clause(
        &self,
        id: HandlerClauseID,
    ) -> impl ResolutionVisitable {
        let handler_clause = self.get_handler_clause(id);

        IntoResolutionWithSpan::new(
            handler_clause,
            *handler_clause.qualified_identifier_span(),
        )
    }

    pub async fn get_handler_instantiation(
        &self,
        handler_clause_id: HandlerClauseID,
        tracked_engine: &TrackedEngine,
    ) -> Instantiation {
        let handler_clause = self.get_handler_clause(handler_clause_id);

        handler_clause.create_instantiation(tracked_engine).await
    }

    #[must_use]
    pub async fn get_operation_handler_parameters(
        &self,
        operation_handler_id: OperationHandlerID,
        tracked_engine: &TrackedEngine,
    ) -> Interned<Parameters> {
        let operation_symbol_id =
            self.get_global_operation_symbol_id(operation_handler_id);

        tracked_engine.get_parameters(operation_symbol_id).await
    }

    /// Gets the [`HandlingScope`] with the given ID.
    #[must_use]
    pub fn get_handling_scope(
        &self,
        id: pernixc_arena::ID<HandlingScope>,
    ) -> &HandlingScope {
        self.0.get(id).unwrap()
    }

    /// Inserts a new [`HandlingScope`] into the collection.
    pub fn insert_handler_scope(
        &mut self,
        do_with_span: RelativeSpan,
        return_type: Type,
    ) -> pernixc_arena::ID<HandlingScope> {
        self.0.insert(HandlingScope::new(do_with_span, return_type))
    }

    /// Adds a new operation handler to the handler clause with the given ID.
    pub fn insert_operation_handler_to_handler_clause(
        &mut self,
        handler_clause_id: HandlerClauseID,
        operation_symbol: SymbolID,
    ) -> ID<OperationHandler> {
        let handler_clause = self
            .0
            .get_mut(handler_clause_id.handler_group_id)
            .unwrap()
            .handler_clauses
            .get_mut(handler_clause_id.effect_handler_id)
            .unwrap();

        handler_clause.operation_handlers.insert(OperationHandler {
            operation_symbol_id: operation_symbol,
            span_by_operation_parameter_id: FxHashMap::default(),
        })
    }

    /// Inserts a new [`HandlerClause`] into the [`HandlingScope`] with the
    /// [`pernixc_arena::ID<HandlingScope>`].
    pub fn insert_handler_clause_to_handling_scope(
        &mut self,
        handling_scope_id: pernixc_arena::ID<HandlingScope>,
        effect_handler: HandlerClause,
    ) -> pernixc_arena::ID<HandlerClause> {
        let handling_scope = self.0.get_mut(handling_scope_id).unwrap();
        handling_scope.handler_clauses.insert(effect_handler)
    }

    #[must_use]
    pub fn get_operation_handler(
        &self,
        operation_handler_id: OperationHandlerID,
    ) -> &OperationHandler {
        let handler_clause =
            self.get_handler_clause(operation_handler_id.handler_clause_id());

        handler_clause
            .get_operation_handler(operation_handler_id.operation_handler_id())
    }

    #[must_use]
    pub fn get_global_operation_symbol_id(
        &self,
        operation_handler_id: OperationHandlerID,
    ) -> Global<pernixc_symbol::SymbolID> {
        let handler_clause =
            self.get_handler_clause(operation_handler_id.handler_clause_id());

        let target_id = handler_clause.effect_id().target_id;

        Global::new(
            target_id,
            handler_clause
                .get_operation_handler(
                    operation_handler_id.operation_handler_id(),
                )
                .operation_symbol_id(),
        )
    }

    pub async fn get_instantiated_operation_handler_parameter_type(
        &self,
        operation_handler_id: OperationHandlerID,
        parameter_id: ID<Parameter>,
        tracked_engine: &TrackedEngine,
    ) -> Type {
        let handler_clause =
            self.get_handler_clause(operation_handler_id.handler_clause_id());

        let inst = handler_clause.create_instantiation(tracked_engine).await;

        let operation_symbol_id =
            handler_clause.effect_id().target_id.make_global(
                handler_clause
                    .get_operation_handler(
                        operation_handler_id.operation_handler_id(),
                    )
                    .operation_symbol_id(),
            );

        let parameters =
            tracked_engine.get_parameters(operation_symbol_id).await;

        inst.clone_and_instantiate(&parameters.parameters[parameter_id].r#type)
    }

    pub fn add_operation_parameter_span(
        &mut self,
        operation_handler_id: OperationHandlerID,
        parameter_id: ID<Parameter>,
        span: RelativeSpan,
    ) {
        let handler_clause = self
            .0
            .get_mut(operation_handler_id.handler_clause_id().handler_group_id)
            .unwrap()
            .handler_clauses
            .get_mut(operation_handler_id.handler_clause_id().effect_handler_id)
            .unwrap();

        let operation_handler = handler_clause
            .operation_handlers
            .get_mut(operation_handler_id.operation_handler_id)
            .unwrap();

        operation_handler
            .span_by_operation_parameter_id
            .insert(parameter_id, span);
    }
}

/// Represents a group of with effect handlers in a `do` expression.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    StableHash,
    Encode,
    Decode,
    Index,
    Getters,
    CopyGetters,
)]
pub struct HandlingScope {
    #[index]
    handler_clauses: Arena<HandlerClause>,

    /// The span of the whole `do ... with ...` expression that creates this
    /// handling scope.
    #[get_copy = "pub"]
    do_with_span: RelativeSpan,

    /// The return type of the whole handling scope.
    #[get = "pub"]
    return_type: Type,
}

impl resolution_visitor::MutableResolutionVisitable for HandlingScope {
    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        visitor
            .visit_mut(
                ResolutionMut::Type(&mut self.return_type),
                self.do_with_span,
            )
            .await?;

        for handler_clause in self.handler_clauses.items_mut() {
            handler_clause.accept_mut(visitor).await?;
        }

        Ok(())
    }
}

impl resolution_visitor::ResolutionVisitable for HandlingScope {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        visitor
            .visit(Resolution::Type(&self.return_type), self.do_with_span)
            .await?;

        for handler_clause in self.handler_clauses.items() {
            handler_clause.accept(visitor).await?;
        }

        Ok(())
    }
}

/// An interface for matching handler clause's generic arguments in the stack
/// with the queried generic arguments.
pub trait HandlerClauseMatcher {
    /// Checks whether the given generic arguments match the handler clause's
    #[allow(async_fn_in_trait)]
    async fn matches_generic_arguments(
        &mut self,
        searching_generic_arguments: &GenericArguments,
        stack_generic_arguments: &GenericArguments,
    ) -> Result<bool, OverflowError>;
}

impl HandlingScope {
    /// Creates a new [`HandlingScope`] with the given return type.
    #[must_use]
    pub fn new(do_with_span: RelativeSpan, return_type: Type) -> Self {
        Self { handler_clauses: Arena::new(), do_with_span, return_type }
    }

    /// Adds a handler clause to this scope.
    #[must_use]
    pub fn add_handler_clause(
        &mut self,
        handler_clause: HandlerClause,
    ) -> pernixc_arena::ID<HandlerClause> {
        self.handler_clauses.insert(handler_clause)
    }

    /// Searches for an handler that matches the given effect ID and generic
    /// arguments.
    pub async fn search_handler_clause(
        &self,
        effect_id: Global<pernixc_symbol::SymbolID>,
        generic_arguments: &GenericArguments,
        matcher: &mut impl HandlerClauseMatcher,
    ) -> Result<Option<pernixc_arena::ID<HandlerClause>>, OverflowError> {
        for (effect_handler_id, effect_handler) in self.handler_clauses.iter() {
            // not the same effect
            if effect_id != effect_handler.effect_id() {
                continue;
            }

            // if the generic argument can be matched, return it
            if matcher
                .matches_generic_arguments(
                    generic_arguments,
                    effect_handler.generic_arguments(),
                )
                .await?
            {
                return Ok(Some(effect_handler_id));
            }
        }

        Ok(None)
    }
}

/// An effect handler for a specific effect.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    StableHash,
    Encode,
    Decode,
    CopyGetters,
    Getters,
)]
pub struct HandlerClause {
    symbol: Symbol,
    qualified_identifier_span: RelativeSpan,

    operation_handlers: Arena<OperationHandler>,
}

impl resolution_visitor::MutableResolutionVisitable for HandlerClause {
    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        visitor
            .visit_mut(
                ResolutionMut::Symbol(&mut self.symbol),
                self.qualified_identifier_span,
            )
            .await
    }
}

impl resolution_visitor::ResolutionVisitable for HandlerClause {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        visitor
            .visit(
                Resolution::Symbol(&self.symbol),
                self.qualified_identifier_span,
            )
            .await
    }
}

impl<'a> From<&'a HandlerClause> for Resolution<'a> {
    fn from(val: &'a HandlerClause) -> Self { Resolution::Symbol(&val.symbol) }
}

impl HandlerClause {
    #[must_use]
    pub fn new(
        effect_id: Global<pernixc_symbol::SymbolID>,
        generic_arguments: GenericArguments,
        span: RelativeSpan,
    ) -> Self {
        Self {
            symbol: Symbol::new(effect_id, generic_arguments),
            qualified_identifier_span: span,
            operation_handlers: Arena::new(),
        }
    }

    #[must_use]
    pub const fn effect_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.symbol.id()
    }

    #[must_use]
    pub const fn generic_arguments(&self) -> &GenericArguments {
        self.symbol.generic_arguments()
    }

    #[must_use]
    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Instantiation {
        self.symbol.create_instantiation(engine).await
    }

    #[must_use]
    pub const fn qualified_identifier_span(&self) -> &RelativeSpan {
        &self.qualified_identifier_span
    }

    #[must_use]
    pub fn get_operation_handler(
        &self,
        id: ID<OperationHandler>,
    ) -> &OperationHandler {
        self.operation_handlers.get(id).unwrap()
    }
}

/// An ID that uniquely identifies an [`HandlerClause`] within an [`IR`].
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
    Encode,
    Decode,
    derive_new::new,
)]
pub struct HandlerClauseID {
    handler_group_id: pernixc_arena::ID<HandlingScope>,
    effect_handler_id: pernixc_arena::ID<HandlerClause>,
}

/// An ID that uniquely identifies an operation handler within an
/// [`HandlerClause`].
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
    Encode,
    Decode,
    CopyGetters,
)]
pub struct OperationHandlerID {
    /// The handler clause ID where this operation handler is defined.
    #[get_copy = "pub"]
    handler_clause_id: HandlerClauseID,

    /// The operation ID that this operation handler handles.
    #[get_copy = "pub"]
    operation_handler_id: ID<OperationHandler>,
}

impl OperationHandlerID {
    /// Creates a new [`OperationHandlerID`].
    #[must_use]
    pub const fn new(
        handler_clause_id: HandlerClauseID,
        operation_handler_id: ID<OperationHandler>,
    ) -> Self {
        Self { handler_clause_id, operation_handler_id }
    }

    /// Returns the handling scope ID where this operation handler is located.
    #[must_use]
    pub const fn handling_scope_id(
        &self,
    ) -> pernixc_arena::ID<crate::handling_scope::HandlingScope> {
        self.handler_clause_id.handler_group_id
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, StableHash, Encode, Decode, CopyGetters,
)]
pub struct OperationHandler {
    operation_symbol_id: SymbolID,
    span_by_operation_parameter_id: FxHashMap<ID<Parameter>, RelativeSpan>,
}

impl OperationHandler {
    #[must_use]
    pub const fn operation_symbol_id(&self) -> SymbolID {
        self.operation_symbol_id
    }

    pub fn add_operation_parameter_span(
        &mut self,
        parameter_id: ID<Parameter>,
        span: RelativeSpan,
    ) {
        assert!(
            self.span_by_operation_parameter_id
                .insert(parameter_id, span)
                .is_none()
        );
    }

    #[must_use]
    pub fn get_span_of_parameter(
        &self,
        parameter_id: ID<Parameter>,
    ) -> &RelativeSpan {
        self.span_by_operation_parameter_id.get(&parameter_id).unwrap()
    }
}
