//! Defines effect handlers used in `do` expressions.

use derive_more::Index;
use getset::{CopyGetters, Getters};
use pernixc_arena::{Arena, ID};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::{generic_arguments::GenericArguments, r#type::Type};
use pernixc_type_system::{environment::Environment, normalizer::Normalizer};

use crate::transform::{self, TypeTermSource};

/// A collection of all the effect handler groups in a function body.
#[derive(
    Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize, Default,
)]
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

impl transform::Element for HandlingScopes {
    async fn transform<
        T: transform::Transformer<pernixc_term::lifetime::Lifetime>
            + transform::Transformer<Type>
            + transform::Transformer<pernixc_term::constant::Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &pernixc_query::TrackedEngine,
    ) -> Result<(), pernixc_query::runtime::executor::CyclicError> {
        for handling_scope in self.0.items_mut() {
            handling_scope.transform(transformer, engine).await?;
        }

        Ok(())
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
    Serialize,
    Deserialize,
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

impl transform::Element for HandlingScope {
    async fn transform<
        T: transform::Transformer<pernixc_term::lifetime::Lifetime>
            + transform::Transformer<Type>
            + transform::Transformer<pernixc_term::constant::Constant>,
    >(
        &mut self,
        transformer: &mut T,
        _engine: &pernixc_query::TrackedEngine,
    ) -> Result<(), pernixc_query::runtime::executor::CyclicError> {
        transformer
            .transform(
                &mut self.return_type,
                TypeTermSource::DoReturnType,
                self.do_with_span,
            )
            .await
    }
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
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<
        Option<pernixc_arena::ID<HandlerClause>>,
        pernixc_type_system::Error,
    > {
        for (effect_handler_id, effect_handler) in self.handler_clauses.iter() {
            // not the same effect
            if effect_id != effect_handler.effect_id {
                continue;
            }

            // if the generic argument subtypable, then match
            if environment
                .subtypes_generic_arguments(
                    &effect_handler.generic_arguments,
                    generic_arguments,
                )
                .await?
                .is_some_and(|x| x.result.forall_lifetime_errors.is_empty())
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
    Serialize,
    Deserialize,
    derive_new::new,
    CopyGetters,
    Getters,
)]
pub struct HandlerClause {
    /// Gets the effect ID that this handler clause handles.
    #[get_copy = "pub"]
    effect_id: Global<pernixc_symbol::ID>,

    /// The generic arguments of the effect handler.
    #[get = "pub"]
    generic_arguments: GenericArguments,
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
    Serialize,
    Deserialize,
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
    Serialize,
    Deserialize,
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
