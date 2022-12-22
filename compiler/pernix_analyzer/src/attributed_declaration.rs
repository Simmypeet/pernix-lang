use pernix_parser::{
    abstract_syntax_tree::{
        declaration::{Declaration, NamespaceDeclaration},
        PositionWrapper,
    },
    File,
};
use pernix_project::source_code::SourceCode;
use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};

/// Is a struct that holds the attributes of a declaration such as its namespace
/// scope and active using directives.
#[derive(Debug, Clone)]
pub struct DeclarationAttributes<'a> {
    namespace_scope: String,
    active_using_directives: Vec<String>,
    source_code: &'a SourceCode,
}

impl PartialEq for DeclarationAttributes<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.active_using_directives.len()
            != other.active_using_directives.len()
        {
            return false;
        }

        for (i, using_directive) in
            self.active_using_directives.iter().enumerate()
        {
            if using_directive != &other.active_using_directives[i] {
                return false;
            }
        }

        self.namespace_scope == other.namespace_scope
            && std::ptr::eq(self.source_code, other.source_code)
    }
}

impl Eq for DeclarationAttributes<'_> {}

impl Hash for DeclarationAttributes<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.namespace_scope.hash(state);
        for using_directive in self.active_using_directives.iter() {
            using_directive.hash(state);
        }
        std::ptr::hash(self.source_code, state);
    }
}

impl<'a> DeclarationAttributes<'a> {
    /// Return the namespace scope where the declaration is declared.
    pub fn namespace_scope(&self) -> &str {
        self.namespace_scope.as_ref()
    }

    /// Return a slice of using directives that are actuve in the declaration.
    pub fn active_using_directives(&self) -> &[String] {
        self.active_using_directives.as_ref()
    }

    /// Return a reference to the source code where the declaration is declared.
    pub fn source_code(&self) -> &SourceCode {
        self.source_code
    }
}

/// Is a struct that groups a list of declarations with the same attributes together.
#[derive(Debug, Clone)]
pub struct AttributedDeclarationGroup<'a> {
    declarations: Vec<PositionWrapper<Declaration<'a>>>,
    attributes: DeclarationAttributes<'a>,
}

impl<'a> AttributedDeclarationGroup<'a> {
    /// Return a reference to the declarations of this [`AttributedDeclarationGroup`].
    pub fn declarations(&self) -> &[PositionWrapper<Declaration>] {
        self.declarations.as_ref()
    }

    /// Return a reference to the attributes of this [`AttributedDeclarationGroup`].
    pub fn attributes(&self) -> &DeclarationAttributes<'a> {
        &self.attributes
    }
}

/// Is a struct responsible for extracting the structured [`pernix_parser::File`] AST
/// into a list of [`AttributedDeclarationGroup`].
#[derive(Debug, Clone, Default)]
pub struct FileASTExtractor<'a> {
    attributed_group: HashMap<
        DeclarationAttributes<'a>,
        Vec<PositionWrapper<Declaration<'a>>>,
    >,
}

impl<'a> FileASTExtractor<'a> {
    /// Return a vector of [`AttributedDeclarationGroup`] that has been fed into this
    /// [`FileASTExtractor`].
    pub fn finalize(self) -> Vec<AttributedDeclarationGroup<'a>> {
        self.attributed_group
            .into_iter()
            .map(|(attributes, declarations)| AttributedDeclarationGroup {
                declarations,
                attributes,
            })
            .collect()
    }

    fn add_entry(
        &mut self,
        attributes: &DeclarationAttributes<'a>,
        declaration: PositionWrapper<Declaration<'a>>,
    ) {
        match self.attributed_group.get_mut(attributes) {
            Some(declarations) => declarations.push(declaration),
            None => {
                self.attributed_group
                    .insert(attributes.clone(), vec![declaration]);
            }
        }
    }

    /// Extract all the declarations defined in the given [`pernix_parser::File`] AST
    /// and feed them into this [`FileASTExtractor`].
    pub fn feed(&mut self, file_ast: File<'a>) {
        let mut attribute = DeclarationAttributes {
            namespace_scope: String::new(),
            active_using_directives: file_ast
                .using_directives
                .iter()
                .map(|x| x.value.namespace_name.value.to_string())
                .collect(),
            source_code: file_ast.source_code,
        };

        for declaration in file_ast.declarations {
            if let Declaration::NamespaceDeclaration(namespace) =
                declaration.value
            {
                self.extract_namespace_declaration(&mut attribute, namespace);
            } else {
                self.add_entry(&attribute, declaration);
            }
        }
    }

    //  extract all the declarations in the namespace
    fn extract_namespace_declaration(
        &mut self,
        attribute: &mut DeclarationAttributes<'a>,
        namespace_declaration: NamespaceDeclaration<'a>,
    ) {
        // count the depth of the namespace scope by finding the number of
        // '.' found in its name + 1
        let depth = namespace_declaration
            .namespace_name
            .value
            .matches('.')
            .count()
            + 1;

        // append the namespace name to the previous namespace scope
        if attribute.namespace_scope.is_empty() {
            attribute.namespace_scope =
                namespace_declaration.namespace_name.value.to_string();
        } else {
            attribute.namespace_scope.push('.');
            attribute
                .namespace_scope
                .push_str(namespace_declaration.namespace_name.value);
        }

        let using_directive_count =
            namespace_declaration.using_directives.len();

        // add the using directives of the namespace to the active using directives
        attribute.active_using_directives.extend(
            namespace_declaration
                .using_directives
                .iter()
                .map(|x| x.value.namespace_name.value.to_string()),
        );

        // loop through the declarations in the namespace
        for declaration in namespace_declaration.declarations {
            if let Declaration::NamespaceDeclaration(namespace) =
                declaration.value
            {
                self.extract_namespace_declaration(attribute, namespace);
            } else {
                self.add_entry(attribute, declaration);
            }
        }

        // pop the added namespace name from the namespace scope
        for _ in 0..depth {
            if let Some(index) = attribute.namespace_scope.rfind('.') {
                attribute.namespace_scope.truncate(index);
            } else {
                attribute.namespace_scope.clear();
            }
        }

        // pop the added using directives from the active using directives
        attribute.active_using_directives.truncate(
            attribute.active_using_directives.len() - using_directive_count,
        );
    }
}

#[cfg(test)]
mod test {
    use pernix_parser::{
        abstract_syntax_tree::declaration::Declaration, Parser,
    };
    use pernix_project::source_code::SourceCode;

    use crate::attributed_declaration::AttributedDeclarationGroup;

    use super::FileASTExtractor;

    #[test]
    fn test_extractor() {
        let source = "
        void foo() {}
        void bar() {}
        
        namespace fizz {
            using buzz;
            void foo() {}
        }

        namespace fizz {
            void bar() {}
        }
        ";
        let source_code =
            SourceCode::new(source.to_string(), "test.pnx".to_string());
        let mut parser = Parser::new(&source_code);
        let file_ast = parser.parse_file();

        // There should be 3 groups of declarations
        // 1. namespace: None, using_directives: None, declarations: foo, bar
        // 2. namespace: fizz, using_directives: buzz, declarations: foo
        // 3. namespace: fizz, using_directives: None, declarations: bar

        let mut extractor = FileASTExtractor::default();
        extractor.feed(file_ast);

        let groups = extractor.finalize();
        assert_eq!(groups.len(), 3);

        fn is_group_one(group: &AttributedDeclarationGroup) -> bool {
            if !group.attributes().namespace_scope.is_empty()
                || !group.attributes().active_using_directives.is_empty()
            {
                return false;
            }

            if group.declarations().len() != 2 {
                return false;
            }

            let mut foo_found = false;
            let mut bar_found = false;

            for declaration in group.declarations() {
                match &declaration.value {
                    Declaration::FunctionDeclaration(function) => {
                        if function.function_name.value == "foo" {
                            foo_found = true;
                        } else if function.function_name.value == "bar" {
                            bar_found = true;
                        }
                    }
                    _ => return false,
                }
            }

            foo_found && bar_found
        }

        fn is_group_two(group: &AttributedDeclarationGroup) -> bool {
            if group.attributes().namespace_scope != "fizz"
                || group.attributes().active_using_directives.len() != 1
                || group.attributes().active_using_directives[0] != "buzz"
            {
                return false;
            }

            if group.declarations().len() != 1 {
                return false;
            }

            let mut foo_found = false;

            for declaration in group.declarations() {
                match &declaration.value {
                    Declaration::FunctionDeclaration(function) => {
                        if function.function_name.value == "foo" {
                            foo_found = true;
                        }
                    }
                    _ => return false,
                }
            }

            foo_found
        }

        fn is_group_three(group: &AttributedDeclarationGroup) -> bool {
            if group.attributes().namespace_scope != "fizz"
                || !group.attributes().active_using_directives.is_empty()
            {
                return false;
            }

            if group.declarations().len() != 1 {
                return false;
            }

            let mut bar_found = false;

            for declaration in group.declarations() {
                match &declaration.value {
                    Declaration::FunctionDeclaration(function) => {
                        if function.function_name.value == "bar" {
                            bar_found = true;
                        }
                    }
                    _ => return false,
                }
            }

            bar_found
        }

        let mut group_one_found = false;
        let mut group_two_found = false;
        let mut group_three_found = false;

        for group in groups {
            if is_group_one(&group) {
                group_one_found = true;
            } else if is_group_two(&group) {
                group_two_found = true;
            } else if is_group_three(&group) {
                group_three_found = true;
            }
        }

        assert!(group_one_found);
        assert!(group_two_found);
        assert!(group_three_found);
    }
}
