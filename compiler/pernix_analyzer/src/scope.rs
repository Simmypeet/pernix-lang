use std::collections::HashMap;

use pernix_parser::{
    abstract_syntax_tree::{
        declaration::{Declaration, UsingDirective},
        PositionWrapper,
    },
    File,
};

/// Is a struct that contains information about the current scope.
#[derive(Clone, Debug)]
pub struct ScopeInfo {
    pub current_namespace_scope: String,
    pub active_using_directives: Vec<String>,
}

/// Is a helper struct that is used to traverse through the namespace scopes
/// and using directives.
pub struct ScopeTraverser<'parser, 'ast> {
    ast: &'parser File<'ast>,
    using_directives_count_stack: Vec<usize>,
    namespace_scope_depth_stack: Vec<usize>,
    current_scope_info: ScopeInfo,
}

impl<'parser: 'ast, 'ast> ScopeTraverser<'parser, 'ast> {
    /// Create a new instance of the scope manager.
    pub fn new(ast: &'parser File<'ast>) -> Self {
        Self {
            ast,
            using_directives_count_stack: Vec::new(),
            namespace_scope_depth_stack: Vec::new(),
            current_scope_info: ScopeInfo {
                current_namespace_scope: String::new(),
                active_using_directives: Vec::new(),
            },
        }
    }

    /// Push a new list of using directives to the stack.
    fn push_using_directive(
        &mut self,
        using_directive: &'ast [PositionWrapper<UsingDirective>],
    ) {
        self.using_directives_count_stack
            .push(using_directive.len());
        self.current_scope_info.active_using_directives.extend(
            using_directive
                .iter()
                .map(|x| x.value.namespace_name.value.to_string()),
        );
    }

    /// Pop the last list of using directives from the stack.
    fn pop_using_directive(&mut self) {
        let count = self
            .using_directives_count_stack
            .pop()
            .expect("using directive stack is empty");
        self.current_scope_info.active_using_directives.truncate(
            self.current_scope_info.active_using_directives.len() - count,
        );
    }

    /// Push a new namespace to the stack.
    fn push_namespace(&mut self, namespace: &str) {
        // Find the namespace scope depth. For example,
        // A -> 1
        // A.B -> 2
        // A.B.C -> 3
        let namespace_scope_depth = namespace.split('.').count();
        self.namespace_scope_depth_stack.push(namespace_scope_depth);

        // add the namespace to the current namespace
        if self.current_scope_info.current_namespace_scope.is_empty() {
            self.current_scope_info.current_namespace_scope =
                namespace.to_string();
        } else {
            self.current_scope_info.current_namespace_scope = format!(
                "{}.{}",
                self.current_scope_info.current_namespace_scope, namespace
            );
        }
    }

    /// Pop the last namespace from the stack.
    fn pop_namespace(&mut self) {
        let namespace_scope_depth = self
            .namespace_scope_depth_stack
            .pop()
            .expect("namespace scope depth stack is empty");
        // remove the topmost namespace scope by the namespace scope depth
        let namespace_scopes: Vec<&str> = self
            .current_scope_info
            .current_namespace_scope
            .split('.')
            .collect();
        self.current_scope_info.current_namespace_scope = namespace_scopes
            .iter()
            .take(namespace_scopes.len() - namespace_scope_depth)
            .map(|x| *x)
            .collect::<Vec<&str>>()
            .join(".");
    }

    /// Get the current scope information.
    pub fn get_current_scope_info(&self) -> &ScopeInfo {
        &self.current_scope_info
    }

    /// Traverse through the namespace scopes defined in the file.
    /// Every time a new namespace scope is entered, the `func` will be called.
    /// The `func` will be called with the current namespace scope.
    pub fn traverse<'a>(
        &mut self,
        func: &mut impl FnMut(
            &ScopeInfo,
            &'parser PositionWrapper<Declaration<'ast>>,
        ),
    ) {
        self.traverse_helper(
            self.ast.using_directives(),
            self.ast.declarations(),
            "",
            func,
        );
    }

    fn traverse_helper<'a>(
        &mut self,
        using_directives: &'parser [PositionWrapper<UsingDirective<'ast>>],
        declarations: &'parser [PositionWrapper<Declaration<'ast>>],
        namespace: &'ast str,
        func: &mut impl FnMut(
            &ScopeInfo,
            &'parser PositionWrapper<Declaration<'ast>>,
        ),
    ) {
        self.push_using_directive(using_directives);
        self.push_namespace(namespace);

        for declaration in declarations {
            match &declaration.value {
                Declaration::NamespaceDeclaration(namespace) => {
                    self.traverse_helper(
                        &namespace.using_directives,
                        &namespace.declarations,
                        namespace.namespace_name.value,
                        func,
                    );
                }
                _ => {
                    func(self.get_current_scope_info(), declaration);
                }
            }
        }

        self.pop_namespace();
        self.pop_using_directive();
    }
}

struct LocalScope<'a, T> {
    variable_declaration: HashMap<&'a str, T>,
}

impl<'a, T> LocalScope<'a, T> {
    fn new() -> Self {
        Self {
            variable_declaration: HashMap::new(),
        }
    }

    fn declare_variable(&mut self, name: &'a str, variable: T) {
        self.variable_declaration.insert(name, variable);
    }

    fn lookup_variable(&self, name: &str) -> Option<&T> {
        self.variable_declaration.get(name)
    }

    fn values(&self) -> impl Iterator<Item = &T> {
        self.variable_declaration.values()
    }
}

/// Represent a stack structure for local scopes. Each scope is a map from
/// variable name to variable symbol.
pub struct LockScopeStack<'a, T> {
    stack: Vec<LocalScope<'a, T>>,
}

impl<'a, T> LockScopeStack<'a, T> {
    /// Create a new scope stack.
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    /// Push a new local scope to the stack.
    pub fn push(&mut self) {
        self.stack.push(LocalScope::new());
    }

    /// Pop the last local scope from the stack.
    pub fn pop(&mut self) {
        self.stack.pop();
    }

    /// Declare a variable in the current scope.
    pub fn declare_variable(&mut self, name: &'a str, variable: T) {
        self.stack
            .last_mut()
            .unwrap()
            .declare_variable(name, variable);
    }

    /// Get all the variables in the current scope.
    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.stack.iter().rev().flat_map(|scope| scope.values())
    }

    /// Lookup a variable in the current scope.
    pub fn lookup_variable(&self, name: &str) -> Option<&T> {
        for scope in self.stack.iter().rev() {
            if let Some(variable) = scope.lookup_variable(name) {
                return Some(variable);
            }
        }

        None
    }
}
