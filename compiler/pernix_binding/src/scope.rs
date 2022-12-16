use pernix_parser::{
    abstract_syntax_tree::{
        declaration::{Declaration, UsingDirective},
        PositionWrapper,
    },
    File,
};

/// Is a struct that contains information about the current scope.
#[derive(Clone, Copy, Debug)]
pub struct ScopeInfo<'a, 'b> {
    pub current_namespace_scope: &'a str,
    pub active_using_directives: &'a [&'b str],
}

/// A helper struct that is used to manage the scope of the file
pub struct ScopeTransverser<'parser, 'ast> {
    ast: &'parser File<'ast>,
    using_directives_count_stack: Vec<usize>,
    using_diretives: Vec<&'ast str>,
    current_namespace: String,
    namespace_scope_depth_stack: Vec<usize>,
}

impl<'parser: 'ast, 'ast> ScopeTransverser<'parser, 'ast> {
    /// Create a new instance of the scope manager.
    pub fn new(ast: &'parser File<'ast>) -> Self {
        Self {
            ast,
            using_directives_count_stack: Vec::new(),
            using_diretives: Vec::new(),
            current_namespace: String::new(),
            namespace_scope_depth_stack: Vec::new(),
        }
    }

    /// Push a new list of using directives to the stack.
    fn push_using_directive(
        &mut self,
        using_directive: &'ast [PositionWrapper<UsingDirective>],
    ) {
        self.using_directives_count_stack
            .push(using_directive.len());
        self.using_diretives.extend(
            using_directive.iter().map(|x| x.value.namespace_name.value),
        );
    }

    /// Pop the last list of using directives from the stack.
    fn pop_using_directive(&mut self) {
        let count = self
            .using_directives_count_stack
            .pop()
            .expect("using directive stack is empty");
        self.using_diretives
            .truncate(self.using_diretives.len() - count);
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
        if self.current_namespace.is_empty() {
            self.current_namespace = namespace.to_string();
        } else {
            self.current_namespace =
                format!("{}.{}", self.current_namespace, namespace);
        }
    }

    /// Pop the last namespace from the stack.
    fn pop_namespace(&mut self) {
        let namespace_scope_depth = self
            .namespace_scope_depth_stack
            .pop()
            .expect("namespace scope depth stack is empty");
        // remove the topmost namespace scope by the namespace scope depth
        let namespace_scopes: Vec<&str> =
            self.current_namespace.split('.').collect();
        self.current_namespace = namespace_scopes
            .iter()
            .take(namespace_scopes.len() - namespace_scope_depth)
            .map(|x| *x)
            .collect::<Vec<&str>>()
            .join(".");
    }

    /// Get the current namespace.
    pub fn current_namespace(&self) -> &str {
        &self.current_namespace
    }

    /// Get the current list of using directives.
    pub fn using_directives(&self) -> &[&'ast str] {
        &self.using_diretives
    }

    /// Get the current scope information.
    pub fn get_current_scope_info(&self) -> ScopeInfo<'_, 'ast> {
        ScopeInfo {
            current_namespace_scope: self.current_namespace(),
            active_using_directives: self.using_directives(),
        }
    }

    /// Transverse through the namespace scopes defined in the file.
    /// Every time a new namespace scope is entered, the `func` will be called.
    /// The `func` will be called with the current namespace scope.
    pub fn transverse<'a>(
        &mut self,
        func: &mut impl FnMut(
            ScopeInfo<'_, 'ast>,
            &'parser PositionWrapper<Declaration<'ast>>,
        ),
    ) {
        self.transverse_helper(
            self.ast.using_directives(),
            self.ast.declarations(),
            "",
            func,
        );
    }

    fn transverse_helper<'a>(
        &mut self,
        using_directives: &'parser [PositionWrapper<UsingDirective<'ast>>],
        declarations: &'parser [PositionWrapper<Declaration<'ast>>],
        namespace: &'ast str,
        func: &mut impl FnMut(
            ScopeInfo<'_, 'ast>,
            &'parser PositionWrapper<Declaration<'ast>>,
        ),
    ) {
        self.push_using_directive(using_directives);
        self.push_namespace(namespace);

        for declaration in declarations {
            match &declaration.value {
                Declaration::NamespaceDeclaration(namespace) => {
                    self.transverse_helper(
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
