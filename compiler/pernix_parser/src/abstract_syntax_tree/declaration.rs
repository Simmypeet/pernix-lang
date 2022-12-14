use super::{statement::BlockScopeStatement, PositionWrapper};

/// Represent an enumeration containing all possible type annotaion patterns
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAnnotation<'a> {
    QualifiedName(&'a str),
}

/// Represent a wrapper around a type that contains additional qualifiers
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QualifiedType<'a> {
    pub type_annotation: PositionWrapper<TypeAnnotation<'a>>,
    pub is_mutable: bool,
}

/// Represent a statement that brings a namespace into the current scope
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UsingDirective<'a> {
    pub namespace_name: PositionWrapper<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// Represent a namespace that introduces a new scope and contains declarations
pub struct NamespaceDeclaration<'a> {
    pub namespace_name: PositionWrapper<&'a str>,
    pub declarations: Vec<PositionWrapper<Declaration<'a>>>,
    pub using_directives: Vec<PositionWrapper<UsingDirective<'a>>>,
}

/// Represent a function declaration that takes a list of parameters, returns a
/// value and contains a list of statements as its body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDeclaration<'a> {
    pub function_name: PositionWrapper<&'a str>,
    pub parameters: Vec<PositionWrapper<(QualifiedType<'a>, &'a str)>>,
    pub return_type: PositionWrapper<TypeAnnotation<'a>>,
    pub body: PositionWrapper<BlockScopeStatement<'a>>,
}

impl<'a> FunctionDeclaration<'a> {
    /// Convert the function declaration into a declaration.
    pub fn to_declaration(self) -> Declaration<'a> {
        Declaration::FunctionDeclaration(self)
    }
}

/// Represent an enumeration containing all kinds of declarations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declaration<'a> {
    NamespaceDeclaration(NamespaceDeclaration<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
}
