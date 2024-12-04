//! Contains the definition of [`Scope`].

use std::{collections::HashSet, num::NonZeroUsize};

use getset::{CopyGetters, Getters};

use crate::arena::{Arena, ID};

/// Represents a branch of scopes that stems out from a single scope.
///
/// This can represents a branch of scopes in `if` and `match` expressions.
/// Or it can represent a normal block of scope by having only one scope ID in
/// the list.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Branch(pub Vec<ID<Scope>>);

/// Represents a scope introduced by `{ ... }` expressions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    /// The parent scope of the current scope.
    ///
    /// If `None`, then the scope is a top-level scope.
    pub parent_scope: Option<ID<Scope>>,

    /// The depth of the scope in the scope tree.
    ///
    /// Starting from 0, the depth increases as the scope is nested.
    pub depth: usize,

    /// If the scope is in a branch, these are the IDs of the neighboring
    /// scopes. Else, this is an empty set.
    ///
    /// The set doesn't include this scope's ID.
    pub neighbors: HashSet<ID<Scope>>,

    /// The children of the scope in sequential order.
    pub children: Vec<Branch>,
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
            neighbors: HashSet::new(),
            children: Vec::new(),
        });

        Self { scopes, root_scope_id }
    }

    /// Creates a new child scope under the given parent scope.
    ///
    /// Returns `None` if the parent scope does not exist.
    #[must_use]
    pub fn new_child_branch(
        &mut self,
        parent_scope_id: ID<Scope>,
        branch_count: NonZeroUsize,
    ) -> Option<Vec<ID<Scope>>> {
        let new_depth = self.scopes.get(parent_scope_id)?.depth + 1;

        let mut branch = Vec::with_capacity(branch_count.get());

        for _ in 0..branch_count.get() {
            let new_scope_id = self.scopes.insert_with(|new_scope_id| Scope {
                parent_scope: Some(parent_scope_id),
                neighbors: branch
                    .iter()
                    .copied()
                    .filter(|x| *x != new_scope_id)
                    .collect(),
                depth: new_depth,
                children: Vec::new(),
            });

            branch.push(new_scope_id);
        }

        self.scopes
            .get_mut(parent_scope_id)?
            .children
            .push(Branch(branch.clone()));

        Some(branch)
    }
}

impl Default for Tree {
    fn default() -> Self { Self::new() }
}
