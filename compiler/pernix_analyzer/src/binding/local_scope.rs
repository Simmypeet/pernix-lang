use std::collections::HashMap;

use crate::symbol::VariableSymbol;

pub(super) struct LocalScope<'table, 'ast> {
    variable_declaration: HashMap<&'ast str, VariableSymbol<'table, 'ast>>,
}

impl<'table, 'ast> LocalScope<'table, 'ast> {
    pub(super) fn new() -> Self {
        Self {
            variable_declaration: HashMap::new(),
        }
    }

    pub(super) fn declare_variable(
        &mut self,
        variable: VariableSymbol<'table, 'ast>,
    ) {
        self.variable_declaration.insert(variable.name, variable);
    }

    pub(super) fn lookup_variable(
        &self,
        name: &str,
    ) -> Option<&VariableSymbol<'table, 'ast>> {
        self.variable_declaration.get(name)
    }
}

pub(super) struct LockScopeStack<'table, 'ast> {
    stack: Vec<LocalScope<'table, 'ast>>,
}

impl<'table, 'ast> LockScopeStack<'table, 'ast> {
    pub(super) fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub(super) fn push(&mut self) {
        self.stack.push(LocalScope::new());
    }

    pub(super) fn pop(&mut self) {
        self.stack.pop();
    }

    pub(super) fn declare_variable(
        &mut self,
        variable: VariableSymbol<'table, 'ast>,
    ) {
        self.stack.last_mut().unwrap().declare_variable(variable);
    }

    pub(super) fn lookup_variable(
        &self,
        name: &str,
    ) -> Option<&VariableSymbol<'table, 'ast>> {
        for scope in self.stack.iter().rev() {
            if let Some(variable) = scope.lookup_variable(name) {
                return Some(variable);
            }
        }

        None
    }
}
