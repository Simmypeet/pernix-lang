//! Contains the definition of [`Stack`], which is used to emulate the scope stack
//! behavior/semantics.

use std::collections::HashMap;

use getset::{CopyGetters, Getters};

use crate::hir::{value::VariableID, ScopeID};

/// Represents a scope that is generally used in variable resolution.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub(super) struct Local {
    /// The scope ID that this [`Local`] represents.
    #[get_copy = "pub"]
    scope_id: ScopeID,

    variable_ids_by_name: HashMap<String, VariableID>,

    /// List of all the alloca declarations in this scope (in order)
    #[get = "pub"]
    variable_declarations: Vec<VariableID>,
}

impl Local {
    fn new(scope_id: ScopeID) -> Self {
        Self {
            scope_id,
            variable_ids_by_name: HashMap::new(),
            variable_declarations: Vec::new(),
        }
    }

    /// Adds a new variable to the current local scope.
    ///
    /// The existing variable with the given name with be shadoowed in lookup.
    pub(super) fn new_variable(&mut self, name: String, variable_id: VariableID) {
        self.variable_ids_by_name.insert(name, variable_id);
        self.variable_declarations.push(variable_id);
    }

    /// Looks up a variable by name in the current local scope.
    pub(super) fn lookup_variable<Q: ?Sized + std::hash::Hash + Eq>(
        &self,
        name: &Q,
    ) -> Option<VariableID>
    where
        String: std::borrow::Borrow<Q>,
    {
        self.variable_ids_by_name.get(name).copied()
    }
}

/// Is a data structure used to emulate the stack frame and scope rules.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub(super) struct Stack {
    locals: Vec<Local>,
}

impl Stack {
    /// Creates a new stack with the given root scope ID.
    #[must_use]
    pub(super) fn new(scope_id: ScopeID) -> Self {
        Self {
            locals: vec![Local::new(scope_id)],
        }
    }

    /// Gets a reference to the topmost [`Local`] scope on the stack.
    #[must_use]
    pub(super) fn current_local(&self) -> &Local { self.locals.last().unwrap() }

    /// Gets a mutable reference to the topmost [`Local`] scope on the stack.
    #[must_use]
    pub(super) fn current_local_mut(&mut self) -> &mut Local { self.locals.last_mut().unwrap() }

    /// Gets a slice of all the [`Local`]s contanied in the stack.
    #[must_use]
    pub(super) fn locals(&self) -> &[Local] { &self.locals }

    pub(super) fn push(&mut self, scope_id: ScopeID) { self.locals.push(Local::new(scope_id)); }

    pub(super) fn pop(&mut self) {
        assert!(self.locals.len() > 1);
        self.locals.pop();
    }
}
