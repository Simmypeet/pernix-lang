//! Contains the definition of [`Stack`]

use super::infer;
use crate::{
    arena::ID,
    ir::{
        alloca::Alloca,
        pattern::{NameBindingPoint, Named},
        scope,
    },
};

/// The extra information associated with a [`scope::Scope`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    /// The scope ID in which this [`Stack`] represents.
    scope_id: ID<scope::Scope>,

    /// List of named binding points created so far in this scope.
    ///
    /// The named binding points are stored in the order they are created.
    /// When name resolution is performed, the named binding points are
    /// resolved from the last to the first.
    named_binding_points: Vec<NameBindingPoint<infer::Model>>,

    /// List of alloca variable declarations created so far in this scope.
    variable_declarations: Vec<ID<Alloca<infer::Model>>>,
}

impl Scope {
    /// Adds a new named binding point to the scope.
    pub fn add_named_binding_point(
        &mut self,
        name_binding_point: NameBindingPoint<infer::Model>,
    ) {
        if name_binding_point.named_patterns_by_name.is_empty() {
            return;
        }

        self.named_binding_points.push(name_binding_point);
    }
}

/// Represents the stack of scopes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stack {
    /// The stack of scopes.
    scopes: Vec<Scope>,
}

impl Stack {
    /// Creates a new stack of scopes starting with the root scope.
    pub fn new(root_scope_id: ID<scope::Scope>) -> Self {
        Self {
            scopes: vec![Scope {
                scope_id: root_scope_id,
                named_binding_points: Vec::new(),
                variable_declarations: Vec::new(),
            }],
        }
    }

    /// Gets the current latest scope in the stack.
    #[must_use]
    pub fn current_scope(&self) -> &Scope { self.scopes.last().unwrap() }

    /// Gets the current latest scope in the stack mutably.
    #[must_use]
    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    /// Searches for a named binding point in the stack.
    pub fn search(&self, name: &str) -> Option<&Named<infer::Model>> {
        for scope in self.scopes.iter().rev() {
            for name_binding_point in scope.named_binding_points.iter().rev() {
                if let Some(named) =
                    name_binding_point.named_patterns_by_name.get(name)
                {
                    return Some(named);
                }
            }
        }

        None
    }
}
