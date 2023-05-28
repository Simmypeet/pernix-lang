use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::{Identifier, Keyword, Punctuation};
use pernixc_source::{SourceElement, Span, SpanError};

use super::{
    statement::Statement, ConnectedList, LifetimeArgument, QualifiedIdentifier, TypeAnnotation,
    TypeSpecifier,
};

/// Represents a syntax tree node for a lifetime parameter.
///
/// Syntax Synopsis:
/// ``` text
/// LifetimeParameter:
///     Label
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LifetimeParameter {
    pub apostrophe: Punctuation,
    pub identifier: Identifier,
}

impl SourceElement for LifetimeParameter {
    fn span(&self) -> Result<Span, SpanError> { self.apostrophe.span.join(&self.identifier.span) }
}

/// Represents a syntax tree node for a type parameter.
///
/// Syntax Synopsis:
/// ```text
/// TypeParameter:
///     Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParameter {
    pub identifier: Identifier,
}

impl SourceElement for TypeParameter {
    fn span(&self) -> Result<Span, SpanError> { Ok(self.identifier.span.clone()) }
}

/// Represents a syntax tree node for a generic parameter.
///
/// Syntax Synopsis:
/// ```text
/// GenericParameter:
///     Label
///     | identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum GenericParameter {
    LifetimeParameter(LifetimeParameter),
    TypeParameter(TypeParameter),
}

impl SourceElement for GenericParameter {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::LifetimeParameter(lifetime_parameter) => lifetime_parameter.span(),
            Self::TypeParameter(type_parameter) => type_parameter.span(),
        }
    }
}

/// Represents a syntax tree node for a list of generic parameters separated by commas.
///
/// Syntax Synopsis:
/// ``` text
///  GenericParameterList:
///     GenericParameter (',' GenericParameter)*
///     ;
/// ```
pub type GenericParameterList = ConnectedList<GenericParameter, Punctuation>;

/// Represents a syntax tree node for a generic parameters used in various item definitions.
///
/// Syntax Synopsis:
/// ```text
/// GenericParameters:
///     '<' GenericParameterList '>'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericParameters {
    pub left_angle_bracket: Punctuation,
    pub generic_parameter_list: GenericParameterList,
    pub right_angle_bracket: Punctuation,
}

impl SourceElement for GenericParameters {
    fn span(&self) -> Result<Span, SpanError> {
        self.left_angle_bracket
            .span
            .join(&self.right_angle_bracket.span)
    }
}

/// Represents a syntax tree node of a constraint used in a where clause.
///
/// Syntax Synopsis:
/// ``` text
/// Constraint:
///     TraitConstraint
///     | LifetimeArgument
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Constraint {
    TraitConstraint(TraitConstraint),
    LifetimeArgument(LifetimeArgument),
}

impl SourceElement for Constraint {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::TraitConstraint(s) => s.span(),
            Self::LifetimeArgument(s) => s.span(),
        }
    }
}

/// Represents a syntax tree node for a trait constraint used in a where clause.
///
/// Syntax Synopsis:
/// ``` text
/// TraitConstraint:
///     QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitConstraint {
    pub qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for TraitConstraint {
    fn span(&self) -> Result<Span, SpanError> { self.qualified_identifier.span() }
}

/// Represents a syntax tree node for a list of constraints separated by commas.
///
/// Syntax Synopsis:
/// ``` text
/// ConstraintList:
///     Constraint (',' Constraint)*
///     ;
/// ```
pub type ConstraintList = ConnectedList<Constraint, Punctuation>;

/// Represents a syntax tree node for a where clause.
///
/// Syntax Synopsis:
/// ``` text
/// WhereClause:
///     'where' ':' ConstraintList
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhereClause {
    pub where_keyword: Keyword,
    pub colon: Punctuation,
    pub constraint_list: ConstraintList,
}

impl SourceElement for WhereClause {
    fn span(&self) -> Result<Span, SpanError> {
        self.where_keyword.span.join(&self.constraint_list.span()?)
    }
}

/// Represents a syntax tree node for a trait signature.
///
/// Syntax Synopsis:
/// ``` text
/// TraitDeclaration:
///     'trait' Identifier GenericParameters WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitSignature {
    pub trait_keyword: Keyword,
    pub identifier: Identifier,
    pub generic_parameters: GenericParameters,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitBody {
    pub left_brace: Punctuation,
    pub trait_members: Vec<TraitMember>,
    pub right_brace: Punctuation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait {
    pub access_modifier: AccessModifier,
    pub trait_signature: TraitSignature,
    pub trait_body: TraitBody,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitFunction {
    pub function_signature: FunctionSignature,
    pub semicolon: Punctuation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum TraitMember {
    Function(TraitFunction),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parameter {
    pub mutable_keyword: Option<Keyword>,
    pub identifier: Identifier,
    pub type_annotation: TypeAnnotation,
}

pub type ParameterList = ConnectedList<Parameter, Punctuation>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parameters {
    pub left_paren: Punctuation,
    pub parameter_list: Option<ParameterList>,
    pub right_paren: Punctuation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReturnType {
    pub colon: Punctuation,
    pub type_specifier: TypeSpecifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
    pub parameters: Parameters,
    pub return_type: Option<ReturnType>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionBody {
    pub left_brace: Punctuation,
    pub statements: Vec<Statement>,
    pub right_brace: Punctuation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub access_modifier: AccessModifier,
    pub function_signature: FunctionSignature,
    pub function_body: FunctionBody,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeSignature {
    pub type_keyword: Keyword,
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDefinition {
    pub equals: Punctuation,
    pub type_specifier: TypeSpecifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub access_modifier: AccessModifier,
    pub type_signature: TypeSignature,
    pub type_definition: TypeDefinition,
    pub semicolon: Punctuation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructSignature {
    pub struct_keyword: Keyword,
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructBody {
    pub left_brace: Punctuation,
    pub struct_fields: Vec<StructField>,
    pub right_brace: Punctuation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub access_modifier: AccessModifier,
    pub struct_signature: StructSignature,
    pub struct_body: StructBody,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub access_modifier: AccessModifier,
    pub let_keyword: Keyword,
    pub identifier: Identifier,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    pub access_modifier: AccessModifier,
    pub type_signature: TypeSignature,
    pub type_definition: TypeDefinition,
    pub semicolon: Punctuation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum StructMember {
    Field(StructField),
    Type(StructType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplementsSignature {
    pub implements_keyword: Keyword,
    pub generic_parameters: Option<GenericParameters>,
    pub qualified_identifier: QualifiedIdentifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum ImplementsMember {
    Function(Function),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplementsBody {
    pub left_brace: Punctuation,
    pub implements_members: Vec<ImplementsMember>,
    pub right_brace: Punctuation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Implements {
    pub implements_signature: ImplementsSignature,
    pub implements_body: ImplementsBody,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum AccessModifier {
    Public(Keyword),
    Private(Keyword),
    Internal(Keyword),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Item {
    Trait(Trait),
    Function(Function),
    Type(Type),
    Struct(Struct),
    Implements(Implements),
}
