use super::{statement::BlockScopeStatementAST, PositionWrapper};

/// Represent an enumeration containing all access modifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
///    'export'? 'class' Identifier '{' ClassMemberDeclaration* '}';
/// ```
#[derive(Debug, Clone)]
pub struct ClassDeclarationAST<'src> {
    pub export: bool,
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
#[derive(Debug, Clone)]
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
///     AccessModifier? Identifier ':' TypeAnnotation ';';
/// ```
#[derive(Debug, Clone)]
pub struct ClassFieldDeclarationAST<'src> {
    pub access_modifier: Option<PositionWrapper<AccessModifier>>,
    pub type_annotation: PositionWrapper<TypeAnnotationAST<'src>>,
    pub identifier: PositionWrapper<&'src str>,
}

/// Is an abstract syntax tree node that represents a class method declaration.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// ClassMethodDeclaration:
///     AccessModifier? 'function' Identifier '(' Parameter (',' Parameter)* ')' ':' TypeAnnotation
///     BlockScopeStatement;
/// ```
#[derive(Debug, Clone)]
pub struct ClassMethodDeclarationAST<'src> {
    pub access_modifier: Option<PositionWrapper<AccessModifier>>,
    pub identifier: PositionWrapper<&'src str>,
    pub parameters: Vec<PositionWrapper<ParameterAST<'src>>>,
    pub return_type_annotation: PositionWrapper<TypeAnnotationAST<'src>>,
    pub body: PositionWrapper<BlockScopeStatementAST<'src>>,
}

/// Is an abstract syntax tree node that represents a parameter.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// Parameter:
///     'mutable'? Identifier ':' TypeAnnotation ';';
/// ```
#[derive(Debug, Clone)]
pub struct ParameterAST<'src> {
    pub qualified_type_annotation: QualifiedTypeAnnotation<'src>,
    pub identifier: PositionWrapper<&'src str>,
}

/// Is an enumeration containing all possible declarations.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// Declaration:
///    ClassDeclaration;
/// ```
pub enum DeclarationAST<'src> {
    ClassDeclaration(ClassDeclarationAST<'src>),
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
pub enum TypeAnnotationAST<'src> {
    TypeUnit(TypeUnitAST<'src>),
}

/// Is an abstract syntax tree node that represents a qualified type annotation.
#[derive(Debug, Clone)]
pub struct QualifiedTypeAnnotation<'src> {
    pub type_annotation: PositionWrapper<TypeAnnotationAST<'src>>,
    pub is_mutable: bool,
}

/// Represent an enumeration containing all type units.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// TypeUnit:
///     PrimitiveTypeUnit
///     | QualifiedName;
/// ```
#[derive(Debug, Clone)]
pub enum TypeUnitAST<'src> {
    PrimitiveTypeUnit(PrimitiveTypeUnit),
    QualifiedName(&'src str),
}

/// Represent an enumeration containing all primitive type units.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// PrimitiveTypeUnit:
///     'void'
///     | 'bool'
///     | 'int8'
///     | 'int16'
///     | 'int32'
///     | 'int64'
///     | 'uint8'
///     | 'uint16'
///     | 'uint32'
///     | 'uint64'
///     | 'float32'
///     | 'float64'
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveTypeUnit {
    Void = 0,
    Bool = 1,
    Int8 = 2,
    Int16 = 3,
    Int32 = 4,
    Int64 = 5,
    Uint8 = 6,
    Uint16 = 7,
    Uint32 = 8,
    Uint64 = 9,
    Float32 = 10,
    Float64 = 11,
}
