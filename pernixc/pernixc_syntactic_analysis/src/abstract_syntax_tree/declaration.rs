use super::{statement::BlockScopeStatementAST, PositionWrapper, TypeUnitAST};

/// Represent an enumeration containing all access modifiers.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum AccessModifier {
    Public,
    Private,
}

/// Is an abstract syntax tree node that represents a class declaration
///
/// ANTLR4 grammar:
///
/// ``` txt
/// ClassDeclaration:
///    'class' Identifier '{' ClassMemberDeclaration* '}';
/// ```
#[derive(Clone)]
pub struct ClassDeclarationAST<'src> {
    pub identifier: PositionWrapper<&'src str>,
    pub members: Vec<PositionWrapper<ClassMemberDeclarationAST<'src>>>,
}

/// Is an enumeration containing all possible class member declarations.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// ClassMemberDeclaration:
///     ClassMethodDeclaration
///     | ClassFieldDeclaration;
/// ```
#[derive(Clone)]
pub enum ClassMemberDeclarationAST<'src> {
    ClassMethodDeclaration(ClassMethodDeclarationAST<'src>),
    ClassFieldDeclaration(ClassFieldDeclarationAST<'src>),
}

/// Is an abstract syntax tree node that represents a class field declaration.
///     
/// ANTLR4 grammar:
///
/// ``` txt
/// ClassFieldDeclaration:
///     AccessModifier TypeAnnotation Identifier ';';
/// ```
#[derive(Clone)]
pub struct ClassFieldDeclarationAST<'src> {
    pub access_modifier: PositionWrapper<AccessModifier>,
    pub type_annotation: PositionWrapper<TypeAnnotationAST<'src>>,
    pub identifier: PositionWrapper<&'src str>,
}

/// Is an abstract syntax tree node that represents a class method declaration.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// ClassMethodDeclaration:
///     AccessModifier TypeAnnotation Identifier '(' Parameter (',' Parameter)* ')'
///     BlockScopeStatement;
/// ```
#[derive(Clone)]
pub struct ClassMethodDeclarationAST<'src> {
    pub access_modifier: PositionWrapper<AccessModifier>,
    pub identifier: PositionWrapper<&'src str>,
    pub parameters: Vec<PositionWrapper<ParameterAST<'src>>>,
    pub return_type_annotation: PositionWrapper<TypeAnnotationAST<'src>>,
    pub body: PositionWrapper<BlockScopeStatementAST<'src>>,
}

/// Is an abstract syntax tree node that represents a parameter..
///
/// ANTLR4 grammar:
///
/// ``` txt
/// Parameter:
///     QualifiedTypeAnnotation Identifier;
/// ```
#[derive(Clone)]
pub struct ParameterAST<'src> {
    pub qualified_type_annotation: PositionWrapper<QualifiedTypeAnnotationAST<'src>>,
    pub identifier: PositionWrapper<&'src str>,
}

/// Is an abstract syntax tree node that represents a using directive.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// UsingDirective:
///     'using' QualifiedName ';';
/// ```
#[derive(Clone)]
pub struct UsingDirectiveAST<'src> {
    pub qualified_name: PositionWrapper<&'src str>,
}

/// Is an abstract syntax tree node that represents a namespace declaration.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// NamespaceDeclaration:
///     'namespace' QualifiedName '{' UsingDirective* Declaration* '}';
/// ```
#[derive(Clone)]
pub struct NamespaceDeclarationAST<'src> {
    pub qualified_name: PositionWrapper<&'src str>,
    pub using_directives: Vec<PositionWrapper<UsingDirectiveAST<'src>>>,
    pub declarations: Vec<PositionWrapper<NamespaceLevelDeclarationAST<'src>>>,
}

/// Represent an enumeration containing all type annotations.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// TypeAnnotation:
///     TypeUnit;
/// ```
#[derive(Clone)]
pub enum TypeAnnotationAST<'src> {
    TypeUnit(TypeUnitAST<'src>),
}

/// Is an abstract syntax tree node that represents a qualified type annotation.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// QualifiedTypeAnnotation:
///     'mutable'? TypeAnnotation;
/// ```
#[derive(Clone)]
pub struct QualifiedTypeAnnotationAST<'src> {
    pub type_annotation: PositionWrapper<TypeAnnotationAST<'src>>,
    pub is_mutable: bool,
}

/// Is an enumeration containing all possible declarations.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// Declaration:
///     ClassDeclaration
///     | NamespaceDeclaration;
#[derive(Clone)]
pub enum NamespaceLevelDeclarationAST<'src> {
    ClassDeclaration(ClassDeclarationAST<'src>),
    NamespaceDeclaration(NamespaceDeclarationAST<'src>),
}

impl<'src> NamespaceLevelDeclarationAST<'src> {
    /// Return the class declaration if the variant is a class declaration.
    pub fn get_class_declaration(&self) -> Option<&ClassDeclarationAST<'src>> {
        match self {
            NamespaceLevelDeclarationAST::ClassDeclaration(class_declaration) => {
                Some(class_declaration)
            }
            _ => None,
        }
    }

    /// Return the namespace declaration if the variant is a namespace declaration.
    pub fn get_namespace_declaration(&self) -> Option<&NamespaceDeclarationAST<'src>> {
        match self {
            NamespaceLevelDeclarationAST::NamespaceDeclaration(namespace_declaration) => {
                Some(namespace_declaration)
            }
            _ => None,
        }
    }

    /// Returb the class declaration if the variant is a class declaration.
    pub fn to_class_declaration(self) -> Option<ClassDeclarationAST<'src>> {
        match self {
            NamespaceLevelDeclarationAST::ClassDeclaration(class_declaration) => {
                Some(class_declaration)
            }
            _ => None,
        }
    }

    /// Return the namespace declaration if the variant is a namespace declaration.
    pub fn to_namespace_declaration(self) -> Option<NamespaceDeclarationAST<'src>> {
        match self {
            NamespaceLevelDeclarationAST::NamespaceDeclaration(namespace_declaration) => {
                Some(namespace_declaration)
            }
            _ => None,
        }
    }

    /// Get the name of the declaration
    pub fn get_name(&self) -> &PositionWrapper<&'src str> {
        match self {
            NamespaceLevelDeclarationAST::ClassDeclaration(class_declaration) => {
                &class_declaration.identifier
            }
            NamespaceLevelDeclarationAST::NamespaceDeclaration(namespace_declaration) => {
                &namespace_declaration.qualified_name
            }
        }
    }
}
