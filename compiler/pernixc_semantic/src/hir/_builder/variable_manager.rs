use std::{
    borrow::Borrow,
    collections::HashMap,
    hash::Hash,
    ops::{Index, IndexMut},
};

use crate::hir::value::{IntermediateType, Variable, VariableID};

/// Is a data structure that emulates a stack of scopes in the form of a vector of [`HashMap`]s.
///
/// The scopes are represented as [`HashMap`]s and the vector is used to emulate a stack of scopes.
/// The search for a value in the [`ScopeMap`] is performed from the last scope to the first scope.
#[derive(Debug, Clone)]
struct ScopeMap<K, V> {
    scopes: Vec<HashMap<K, V>>,
}

impl<K, V> ScopeMap<K, V> {
    /// Creates a new [`ScopeMap`] with a single scope.
    #[must_use]
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    /// Starts a new scope.
    fn push(&mut self) { self.scopes.push(HashMap::new()); }

    /// Pops the last scope.
    fn pop(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Inserts a new value in the last scope. If the key already exists in the last scope, the
    /// previous value is returned (shadowing).
    fn insert(&mut self, key: K, value: V) -> Option<V>
    where
        K: Eq + Hash,
    {
        self.scopes.last_mut().unwrap().insert(key, value)
    }

    /// Searches for a value in the scopes. The search is performed from the last scope to the first
    /// scope. If the value is found, it is returned. Otherwise, `None` is returned.
    fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q> + Eq + Hash,
        Q: Eq + Hash + ?Sized,
    {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(key) {
                return Some(value);
            }
        }

        None
    }
}

impl<K, V> Default for ScopeMap<K, V> {
    fn default() -> Self { Self::new() }
}

/// Is a struct that manages the variables in the builder
#[derive(Debug, Clone)]
pub(super) struct VariableManager<'a> {
    variables: HashMap<VariableID, Variable<IntermediateType>>,
    variable_scope_map: ScopeMap<&'a str, VariableID>,
}

impl<'a> VariableManager<'a> {
    /// Creates a new variable scope.
    pub(super) fn new_scope(&mut self) { self.variable_scope_map.push(); }

    /// Pops the last variable scope.
    pub(super) fn pop_scope(&mut self) { self.variable_scope_map.pop(); }

    /// Creates a new variable manager with one scope.
    pub(super) fn new() -> Self {
        Self {
            variables: HashMap::new(),
            variable_scope_map: ScopeMap::new(),
        }
    }

    /// Adds a new variable to the container.
    pub(super) fn add_variable(
        &mut self,
        variable_id: VariableID,
        variable: Variable<IntermediateType>,
    ) {
        self.variables.insert(variable_id, variable);
    }

    /// Adds a new variable to the container and maps it to a name.
    pub(super) fn add_variable_with_name(
        &mut self,
        variable_id: VariableID,
        variable: Variable<IntermediateType>,
        name: &'a str,
    ) {
        self.add_variable(variable_id, variable);
        self.variable_scope_map.insert(name, variable_id);
    }

    /// Gets the [`VariableID`] of a variable with the given name.
    pub(super) fn map_name_to_id(&self, name: &str) -> Option<VariableID> {
        self.variable_scope_map.get(name).copied()
    }
}

impl Index<VariableID> for VariableManager<'_> {
    type Output = Variable<IntermediateType>;

    fn index(&self, index: VariableID) -> &Self::Output { &self.variables[&index] }
}

impl IndexMut<VariableID> for VariableManager<'_> {
    fn index_mut(&mut self, index: VariableID) -> &mut Self::Output {
        self.variables.get_mut(&index).unwrap()
    }
}
