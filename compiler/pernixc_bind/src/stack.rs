//! Contains the definition of [`Stack`]

use getset::{CopyGetters, Getters};
use pernixc_arena::ID;
use pernixc_semantic::component::derived::ir::{
    alloca::Alloca,
    pattern::{NameBinding, NameBindingPoint},
    scope,
};

use super::infer;

/// The extra information associated with a [`scope::Scope`].
#[derive(Debug, Clone, PartialEq, Eq, CopyGetters, Getters)]
pub struct Scope {
    /// The scope ID in which this [`Stack`] represents.
    #[get_copy = "pub"]
    scope_id: ID<scope::Scope>,

    /// List of named binding points created so far in this scope.
    ///
    /// The named binding points are stored in the order they are created.
    /// When name resolution is performed, the named binding points are
    /// resolved from the last to the first.
    #[get = "pub"]
    named_binding_points: Vec<NameBindingPoint<infer::Model>>,

    /// List of alloca variable declarations created so far in this scope.
    #[get = "pub"]
    variable_declarations: Vec<ID<Alloca<infer::Model>>>,

    /// Checks if the scope was declared with `unsafe` keyword.
    #[get_copy = "pub"]
    is_unsafe: bool,
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

    /// Adds a new variable declaration to the scope.
    pub fn add_variable_declaration(
        &mut self,
        alloca_id: ID<Alloca<infer::Model>>,
    ) {
        self.variable_declarations.push(alloca_id);
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
    #[must_use]
    pub fn new(
        root_scope_id: ID<scope::Scope>,
        root_scope_is_unsafe: bool,
    ) -> Self {
        Self {
            scopes: vec![Scope {
                scope_id: root_scope_id,
                named_binding_points: Vec::new(),
                variable_declarations: Vec::new(),
                is_unsafe: root_scope_is_unsafe,
            }],
        }
    }

    /// Pushes a new scope to the stack.
    pub fn push_scope(&mut self, scope_id: ID<scope::Scope>, is_unsafe: bool) {
        self.scopes.push(Scope {
            scope_id,
            named_binding_points: Vec::new(),
            variable_declarations: Vec::new(),
            is_unsafe,
        });
    }

    /// Pops the latest scope from the stack.
    #[must_use]
    pub fn pop_scope(&mut self) -> Option<Scope> {
        if self.scopes.len() > 1 {
            self.scopes.pop()
        } else {
            None
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

    /// List of scopes in the stack.
    #[must_use]
    pub fn scopes(&self) -> &[Scope] { &self.scopes }

    /// List of scopes in the stack mutably.
    pub fn scopes_mut(&mut self) -> &mut [Scope] { &mut self.scopes }

    /// Searches for a named binding point in the stack.
    #[must_use]
    pub fn search(&self, name: &str) -> Option<&NameBinding<infer::Model>> {
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

    /// Checks if there's any scope in the stack that was declared with
    /// `unsafe` keyword.
    #[must_use]
    pub fn is_unsafe(&self) -> bool { self.scopes.iter().any(|x| x.is_unsafe) }
}
