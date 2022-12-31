use pernixc_common::source_file::SourceFile;
use pernixc_syntactic_analysis::abstract_syntax_tree::{
    declaration::{NamespaceLevelDeclarationAST, UsingDirectiveAST},
    FileAST, PositionWrapper,
};

/// Is a struct containing information about the scope of a particular symbol
/// declaration.
#[derive(Clone)]
pub struct ScopeInfo<'src> {
    pub scope_name: String,
    pub active_using_directives: Vec<String>,
    pub source_file: &'src SourceFile,
}

impl<'src> ScopeInfo<'src> {
    /// Traverse through the namespace scopes defined in the file. Every declaration
    /// that is found in the file will be passed to the `func` parameter with the
    /// scope information of the declaration.
    pub fn traverse<'ast>(
        ast: &'ast FileAST<'src>,
        func: &mut impl FnMut(
            &ScopeInfo<'src>,
            &'ast PositionWrapper<NamespaceLevelDeclarationAST<'src>>,
        ),
    ) where
        'ast: 'src,
    {
        let mut traverser = ScopeTraverser::new(ast);
        traverser.traverse(func);
    }
}

/// A helper struct that is used to manage the scope of the file
struct ScopeTraverser<'ast, 'src> {
    ast: &'ast FileAST<'src>,
    using_directives_count_stack: Vec<usize>,
    namespace_scope_depth_stack: Vec<usize>,
    current_scope_info: ScopeInfo<'src>,
}

impl<'ast: 'src, 'src> ScopeTraverser<'ast, 'src> {
    /// Create a new instance of the scope manager.
    fn new(ast: &'ast FileAST<'src>) -> Self {
        Self {
            ast,
            using_directives_count_stack: Vec::new(),
            namespace_scope_depth_stack: Vec::new(),
            current_scope_info: ScopeInfo {
                scope_name: String::new(),
                active_using_directives: {
                    ast.using_directives()
                        .iter()
                        .map(|x| x.value.qualified_name.value.to_string())
                        .collect()
                },
                source_file: ast.source_file(),
            },
        }
    }

    /// Push a new list of using directives to the stack.
    fn push_using_directive(
        &mut self,
        using_directive: &'ast [PositionWrapper<UsingDirectiveAST<'src>>],
    ) {
        self.using_directives_count_stack
            .push(using_directive.len());
        self.current_scope_info.active_using_directives.extend(
            using_directive
                .iter()
                .map(|x| x.value.qualified_name.value.to_string()),
        );
    }

    /// Pop the last list of using directives from the stack.
    fn pop_using_directive(&mut self) {
        let count = self
            .using_directives_count_stack
            .pop()
            .expect("using directive stack is empty");
        self.current_scope_info
            .active_using_directives
            .truncate(self.current_scope_info.active_using_directives.len() - count);
    }

    /// Push a new namespace to the stack.
    fn push_namespace(&mut self, namespace: &str) {
        // Find the namespace scope depth. For example,
        // A -> 1
        // A::B -> 2
        // A::B::C -> 3
        let namespace_scope_depth = namespace.split("::").count();
        self.namespace_scope_depth_stack.push(namespace_scope_depth);

        // add the namespace to the current namespace
        if self.current_scope_info.scope_name.is_empty() {
            self.current_scope_info.scope_name = namespace.to_string();
        } else {
            self.current_scope_info.scope_name =
                format!("{}::{}", self.current_scope_info.scope_name, namespace);
        }
    }

    /// Pop the last namespace from the stack.
    fn pop_namespace(&mut self) {
        let namespace_scope_depth = self
            .namespace_scope_depth_stack
            .pop()
            .expect("namespace scope depth stack is empty");
        // remove the topmost namespace scope by the namespace scope depth
        let namespace_scopes: Vec<&str> = self.current_scope_info.scope_name.split("::").collect();
        self.current_scope_info.scope_name = namespace_scopes
            .iter()
            .take(namespace_scopes.len() - namespace_scope_depth)
            .map(|x| *x)
            .collect::<Vec<&str>>()
            .join("::");
    }

    /// Get the current scope information.
    fn get_current_scope_info(&self) -> &ScopeInfo<'src> {
        &self.current_scope_info
    }

    /// Transverse through the namespace scopes defined in the file.
    /// Every time a new namespace scope is entered, the `func` will be called.
    /// The `func` will be called with the current namespace scope.
    fn traverse<'a>(
        &mut self,
        func: &mut impl FnMut(
            &ScopeInfo<'src>,
            &'ast PositionWrapper<NamespaceLevelDeclarationAST<'src>>,
        ),
    ) {
        for namespace in self.ast.declarations() {
            self.transverse_helper(
                &namespace.value.using_directives,
                &namespace.value.declarations,
                &namespace.value.qualified_name.value,
                func,
            );
        }
    }

    fn transverse_helper<'a>(
        &mut self,
        using_directives: &'ast [PositionWrapper<UsingDirectiveAST<'src>>],
        declarations: &'ast [PositionWrapper<NamespaceLevelDeclarationAST<'src>>],
        namespace: &'src str,
        func: &mut impl FnMut(
            &ScopeInfo<'src>,
            &'ast PositionWrapper<NamespaceLevelDeclarationAST<'src>>,
        ),
    ) {
        self.push_using_directive(using_directives);
        self.push_namespace(namespace);

        for declaration in declarations {
            match &declaration.value {
                NamespaceLevelDeclarationAST::NamespaceDeclaration(namespace) => {
                    func(self.get_current_scope_info(), declaration);

                    self.transverse_helper(
                        &namespace.using_directives,
                        &namespace.declarations,
                        namespace.qualified_name.value,
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
