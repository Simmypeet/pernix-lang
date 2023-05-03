//! Contains the definition of [`Scope`] and [`Stack`]

use std::collections::HashMap;

use derive_more::{Deref, DerefMut};

use crate::hir::AllocaID;

/// Is a data struct representing a single scope in the program. The scopec contains a map of all
/// the local variables in the scope.
#[derive(Debug, Clone, PartialEq, Eq, Default, Deref, DerefMut)]
pub struct Scope {
    local_variables: HashMap<String, AllocaID>,
}

/// Is a data struct representing the stack of scopes in the program.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stack {
    scopes: Vec<Scope>,
}

impl Stack {
    /// Creates a new [`Stack`] with a single empty scope.
    #[must_use]
    pub fn new() -> Self {
        let mut stack = Self { scopes: Vec::new() };
        stack.push_scope();
        stack
    }

    /// Pushes a new scope onto the stack.
    pub fn push_scope(&mut self) { self.scopes.push(Scope::default()); }

    /// Removes the topmost scope from the stack.
    ///
    /// If the stack only contains one scope, this function does nothing.
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Gets a reference to the topmost scope in the stack.
    #[must_use]
    pub fn top(&self) -> &Scope { self.scopes.last().unwrap() }

    /// Gets a mutable reference to the topmost scope in the stack.
    #[must_use]
    pub fn top_mut(&mut self) -> &mut Scope { self.scopes.last_mut().unwrap() }

    /// Performs a name lookup in the stack.
    ///
    /// This function searches the stack from top to bottom for a scope that contains a variable
    /// with the given name. If such a scope is found, the function returns the ID of the variable
    /// in that scope. Otherwise, the function returns `None`.
    #[must_use]
    pub fn serach<Q>(&self, key: &Q) -> Option<AllocaID>
    where
        Q: ?Sized + Eq + std::hash::Hash,
        String: std::borrow::Borrow<Q>,
    {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.local_variables.get(key) {
                return Some(*id);
            }
        }
        None
    }
}

impl Default for Stack {
    fn default() -> Self { Self::new() }
}
