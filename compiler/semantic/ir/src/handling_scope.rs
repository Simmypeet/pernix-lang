//! Defines effect handlers used in `do` expressions.

use derive_more::Index;
use pernixc_arena::Arena;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::generic_arguments::GenericArguments;
use pernixc_type_system::{environment::Environment, normalizer::Normalizer};

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
pub struct HandlingScopes(Arena<HandlingScope>);

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

    /// Inserts a new [`HandlingScope`] into the collection.
    pub fn insert_handler_scope(&mut self) -> pernixc_arena::ID<HandlingScope> {
        self.0.insert(HandlingScope::default())
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
    Default,
    Index,
)]
pub struct HandlingScope {
    #[index]
    handler_clauses: Arena<HandlerClause>,
}

impl HandlingScope {
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
)]
pub struct HandlerClause {
    effect_id: Global<pernixc_symbol::ID>,
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
