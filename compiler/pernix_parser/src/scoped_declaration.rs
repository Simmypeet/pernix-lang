use pernix_project::source_code::SourceCode;

use crate::{
    abstract_syntax_tree::{
        declaration::{Declaration, NamespaceDeclaration},
        PositionWrapper,
    },
    File,
};

/// Is a wrapper around [`Declaration`] that contains additional information about
/// its scope.
pub struct ScopedDeclaration<'a> {
    declaration: PositionWrapper<Declaration<'a>>,
    namespace: String,
    active_using_directives: Vec<String>,
    source_code: &'a SourceCode,
}

impl<'a> ScopedDeclaration<'a> {
    /// Return a reference to the declaration of this [`ScopedDeclaration`].
    pub fn declaration(&self) -> &PositionWrapper<Declaration<'a>> {
        &self.declaration
    }

    /// Return a namespace where this [`ScopedDeclaration`] is declared.
    pub fn namespace(&self) -> &str {
        self.namespace.as_ref()
    }

    /// Return a slice of using directives that are active in this [`ScopedDeclaration`].
    pub fn active_using_directives(&self) -> &[String] {
        self.active_using_directives.as_ref()
    }

    /// Return a reference to the source code where this [`ScopedDeclaration`] is
    /// declared.
    pub fn source_code(&self) -> &SourceCode {
        self.source_code
    }

    /// Analyze a given [`File`] and return a vector of [`ScopedDeclaration`] found
    /// in it.
    pub fn analyze(file_ast: File<'a>) -> Vec<Self> {
        todo!()
    }
}

struct ScopedDeclarationGenerator<'a> {
    current_namespace: String,
    current_using_directives: Vec<String>,
    declarations: Vec<ScopedDeclaration<'a>>,
}

impl<'a> ScopedDeclarationGenerator<'a> {
    fn new() -> Self {
        Self {
            current_namespace: String::new(),
            current_using_directives: Vec::new(),
            declarations: Vec::new(),
        }
    }

    fn generate(&mut self, mut namespace_declaration: NamespaceDeclaration) {
        // append namespace name to the current namespace
        {
            if !self.current_namespace.is_empty() {
                self.current_namespace.push('.');
            }

            self.current_namespace
                .push_str(namespace_declaration.namespace_name.value);
        }

        let using_directive_count =
            namespace_declaration.using_directives.len();
        // append using directives to the current using directives
        {
            self.current_using_directives.extend(
                namespace_declaration
                    .using_directives
                    .iter()
                    .map(|x| x.value.namespace_name.value.to_string()),
            );
        }

        while let Some(declaration) = namespace_declaration.declarations.pop() {
            if let Declaration::NamespaceDeclaration(namespace_decl) =
                declaration.value
            {
            } else {
                self.declarations.push(ScopedDeclaration {
                    declaration,
                    namespace: self.current_namespace.clone(),
                    active_using_directives: self.current_using_directives.clone(),
                    source_code: todo!(),
                })
            }
        }
    }
}
