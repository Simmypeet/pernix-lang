//! Contains the definition of [`Scope`].

use getset::{CopyGetters, Getters};

use crate::arena::{Arena, ID};

/// Enumeration of either a scope or a branch of scopes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Child {
    /// The child is a single scope.
    Scope(ID<Scope>),

    /// The child is a branch of scopes. This is typically used for `if` and
    /// `match` expressions.
    Branch(Vec<ID<Scope>>),
}

/// Represents a scope introduced by `{ ... }` expressions.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scope {
    /// The parent scope of the current scope.
    ///
    /// If `None`, then the scope is a top-level scope.
    pub parent_scope: Option<ID<Scope>>,

    /// The depth of the scope in the scope tree.
    ///
    /// Starting from 0, the depth increases as the scope is nested.
    pub depth: usize,

    /// The children of the scope in sequential order.
    pub children: Vec<Child>,
}

/// Represents a tree of scopes.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct Tree {
    /// Contains all the scopes in the program.
    #[get = "pub"]
    scopes: Arena<Scope>,

    /// The ID of the root scope.
    #[get_copy = "pub"]
    root_scope_id: ID<Scope>,
}

impl Tree {
    /// Creates a new scope tree -- initially containing only the root scope.
    #[must_use]
    pub fn new() -> Self {
        let mut scopes = Arena::new();
        let root_scope_id = scopes.insert(Scope {
            parent_scope: None,
            depth: 0,
            children: Vec::new(),
        });

        Self { scopes, root_scope_id }
    }
}

impl Default for Tree {
    fn default() -> Self { Self::new() }
}
