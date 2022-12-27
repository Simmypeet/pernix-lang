use super::{statement::BlockScopeStatement, PositionWrapper, TypeUnit};

/// Represent an enumeration containing all access modifiers.
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
pub struct ClassDeclaration<'src> {
    pub identifier: PositionWrapper<&'src str>,
    pub members: Vec<PositionWrapper<ClassMemberDeclaration<'src>>>,
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
pub enum ClassMemberDeclaration<'src> {
    ClassMethodDeclaration(ClassMethodDeclaration<'src>),
    ClassFieldDeclaration(ClassFieldDeclaration<'src>),
}

/// Is an abstract syntax tree node that represents a class field declaration.
///     
/// ANTLR4 grammar:
///
/// ``` txt
/// ClassFieldDeclaration:
///     AccessModifier TypeAnnotation Identifier ';';
/// ```
pub struct ClassFieldDeclaration<'src> {
    pub access_modifier: PositionWrapper<AccessModifier>,
    pub type_annotation: PositionWrapper<TypeAnnotaion<'src>>,
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
pub struct ClassMethodDeclaration<'src> {
    pub access_modifier: PositionWrapper<AccessModifier>,
    pub identifier: PositionWrapper<&'src str>,
    pub parameters: Vec<PositionWrapper<Parameter<'src>>>,
    pub return_type_annotation: PositionWrapper<TypeAnnotaion<'src>>,
    pub body: PositionWrapper<BlockScopeStatement<'src>>,
}

/// Is an abstract syntax tree node that represents a parameter..
///
/// ANTLR4 grammar:
///
/// ``` txt
/// Parameter:
///     QualifiedTypeAnnotation Identifier;
/// ```
pub struct Parameter<'src> {
    pub qualified_type_annotation:
        PositionWrapper<QualifiedTypeAnnotaion<'src>>,
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
pub struct UsingDirective<'src> {
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
pub struct NamespaceDeclaration<'src> {
    pub qualified_name: PositionWrapper<&'src str>,
    pub using_directives: Vec<PositionWrapper<UsingDirective<'src>>>,
    pub declarations: Vec<PositionWrapper<Declaration<'src>>>,
}

/// Represent an enumeration containing all type annotations.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// TypeAnnotation:
///     TypeUnit;
/// ```
#[derive(Debug, Clone)]
pub enum TypeAnnotaion<'src> {
    TypeUnit(TypeUnit<'src>),
}

/// Is an abstract syntax tree node that represents a qualified type annotation.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// QualifiedTypeAnnotation:
///     'mutable'? TypeAnnotation;
/// ```
pub struct QualifiedTypeAnnotaion<'src> {
    pub type_annotation: PositionWrapper<TypeAnnotaion<'src>>,
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
pub enum Declaration<'src> {
    ClassDeclaration(ClassDeclaration<'src>),
    NamespaceDeclaration(NamespaceDeclaration<'src>),
}
