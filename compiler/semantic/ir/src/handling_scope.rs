//! Defines effect handlers used in `do` expressions.

use derive_more::Index;
use getset::{CopyGetters, Getters};
use pernixc_arena::{Arena, ID};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::{GenericArguments, Symbol},
    instantiation::Instantiation,
    r#type::Type,
};
use pernixc_type_system::OverflowError;
use qbice::{Decode, Encode, StableHash};

use crate::resolution_visitor::{
    self, Abort, MutableResolutionVisitor, Resolution, ResolutionMut,
    ResolutionVisitor,
};

macro_rules! visit_handling_scopes {
    ($iterable:expr, $visitor:expr, $accept_method:ident) => {{
        for handling_scope in $iterable {
            handling_scope.$accept_method($visitor).await?;
        }
        Ok(())
    }};
}

macro_rules! visit_scope_return_type {
    (
        $this:expr,
        $visitor:expr,
        $visit_method:ident,
        $resolution_ctor:ident,
        $type_ref:ident
    ) => {{
        $visitor
            .$visit_method(
                $resolution_ctor::Type($type_ref!($this.return_type)),
                $this.do_with_span,
            )
            .await
    }};
}

macro_rules! immut_ref {
    ($field:expr) => {
        &$field
    };
}

macro_rules! mut_ref {
    ($field:expr) => {
        &mut $field
    };
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
    /// Gets the [`HandlerClause`] with the [`HandlerClauseID`].
    #[must_use]
    pub fn get_handler_clause(&self, id: HandlerClauseID) -> &HandlerClause {
        self.0
            .get(id.handler_group_id)
            .unwrap()
            .handler_clauses
            .get(id.effect_handler_id)
            .unwrap()
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
        visit_scope_return_type!(
            self,
            visitor,
            visit_mut,
            ResolutionMut,
            mut_ref
        )
    }
}

impl resolution_visitor::ResolutionVisitable for HandlingScope {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        visit_scope_return_type!(self, visitor, visit, Resolution, immut_ref)
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
        effect_id: Global<pernixc_symbol::ID>,
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
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    CopyGetters,
    Getters,
)]
pub struct HandlerClause {
    symbol: Symbol,
    qualified_identifier_span: RelativeSpan,
}

impl HandlerClause {
    #[must_use]
    pub const fn new(
        effect_id: Global<pernixc_symbol::ID>,
        generic_arguments: GenericArguments,
        span: RelativeSpan,
    ) -> Self {
        Self {
            symbol: Symbol::new(effect_id, generic_arguments),
            qualified_identifier_span: span,
        }
    }

    #[must_use]
    pub const fn effect_id(&self) -> Global<pernixc_symbol::ID> {
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
    operation_id: pernixc_symbol::ID,
}

impl OperationHandlerID {
    /// Creates a new [`OperationHandlerID`].
    #[must_use]
    pub const fn new(
        handler_clause_id: HandlerClauseID,
        operation_id: pernixc_symbol::ID,
    ) -> Self {
        Self { handler_clause_id, operation_id }
    }

    /// Returns the handling scope ID where this operation handler is located.
    #[must_use]
    pub const fn handling_scope_id(
        &self,
    ) -> pernixc_arena::ID<crate::handling_scope::HandlingScope> {
        self.handler_clause_id.handler_group_id
    }
}
