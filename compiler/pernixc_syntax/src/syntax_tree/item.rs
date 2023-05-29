use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};
use pernixc_source::{SourceElement, Span, SpanError};
use pernixc_system::diagnostic::Handler;

use super::{
    statement::Statement, ConnectedList, LifetimeArgument, QualifiedIdentifier, TypeAnnotation,
    TypeSpecifier,
};
use crate::{
    error::{
        AccessModifierExpected, Error, GenericArgumentParameterListCannotBeEmpty, ItemExpected,
    },
    parser::{Error as ParserError, Parser, Result as ParserResult},
};

pub mod strategy;

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
///     LifetimeParameter
///     | TypeParameter
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum GenericParameter {
    Lifetime(LifetimeParameter),
    Type(TypeParameter),
}

impl SourceElement for GenericParameter {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Lifetime(lifetime_parameter) => lifetime_parameter.span(),
            Self::Type(type_parameter) => type_parameter.span(),
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

impl SourceElement for TraitSignature {
    fn span(&self) -> Result<Span, SpanError> {
        let start = &self.trait_keyword.span;
        match &self.where_clause {
            Some(where_clause) => start.join(&where_clause.span()?),
            None => start.join(&self.generic_parameters.span()?),
        }
    }
}

/// Represents a syntax tree node for a trait body.
///
/// Syntax Synopsis:
/// ``` text
/// TraitBody:
///     '{' TraitMember* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitBody {
    pub left_brace: Punctuation,
    pub trait_members: Vec<TraitMember>,
    pub right_brace: Punctuation,
}

impl SourceElement for TraitBody {
    fn span(&self) -> Result<Span, SpanError> { self.left_brace.span.join(&self.right_brace.span) }
}

/// Represents a syntax tree node for a trait item declaration.
///
/// Syntax Synopsis:
/// ``` text
/// Trait:
///     AccessModifier TraitSignature TraitBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait {
    pub access_modifier: AccessModifier,
    pub trait_signature: TraitSignature,
    pub trait_body: TraitBody,
}

impl SourceElement for Trait {
    fn span(&self) -> Result<Span, SpanError> {
        self.access_modifier.span()?.join(&self.trait_body.span()?)
    }
}

/// Represents a syntax tree node for a trait function member.
///
/// Syntax Synopsis:
/// ``` text
/// TraitFunction:
///     FunctionSignature ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitFunction {
    pub function_signature: FunctionSignature,
    pub semicolon: Punctuation,
}

impl SourceElement for TraitFunction {
    fn span(&self) -> Result<Span, SpanError> {
        self.function_signature.span()?.join(&self.semicolon.span)
    }
}

/// Represents a syntax tree node for a trait member.
///
/// Syntax Synopsis:
/// ``` text
/// TraitMember:
///     TraitFunction
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum TraitMember {
    Function(TraitFunction),
}

impl SourceElement for TraitMember {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Function(f) => f.span(),
        }
    }
}

/// Represents a syntax tree node for function parameter.
///
/// Syntax Synopsis:
/// ``` text
/// Parameter:
///     'mutable'? Identifier TypeAnnotation
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parameter {
    pub mutable_keyword: Option<Keyword>,
    pub identifier: Identifier,
    pub type_annotation: TypeAnnotation,
}

impl SourceElement for Parameter {
    fn span(&self) -> Result<Span, SpanError> {
        self.identifier.span()?.join(&self.type_annotation.span()?)
    }
}

/// Represents a list of parameter separated by commas.
///
/// Syntax Synopsis:
/// ``` text
/// ParameterList:
///     Parameter (',' Parameter)*
///     ;
/// ```
pub type ParameterList = ConnectedList<Parameter, Punctuation>;

/// Represents a syntax tree node for a list of parameters enclosed in parentheses.
///
/// Syntax Synopsis:
/// ``` text
/// Parameters:
///     '(' ParameterList? ')'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parameters {
    pub left_paren: Punctuation,
    pub parameter_list: Option<ParameterList>,
    pub right_paren: Punctuation,
}

impl SourceElement for Parameters {
    fn span(&self) -> Result<Span, SpanError> { self.left_paren.span.join(&self.right_paren.span) }
}

/// Represents a syntax tree node for a return type in a function signature.
///
/// Syntax Synopsis:
/// ``` text
/// ReturnType:
///     ':' TypeSpecifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReturnType {
    pub colon: Punctuation,
    pub type_specifier: TypeSpecifier,
}

impl SourceElement for ReturnType {
    fn span(&self) -> Result<Span, SpanError> { self.colon.span.join(&self.type_specifier.span()?) }
}

/// Represents a syntax tree node for a function signature.
///
/// Syntax Synopsis:
/// ``` text
/// FunctionSignature:
///     Identifier GenericParameters? Parameters ReturnType? WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
    pub parameters: Parameters,
    pub return_type: Option<ReturnType>,
    pub where_clause: Option<WhereClause>,
}

impl SourceElement for FunctionSignature {
    fn span(&self) -> Result<Span, SpanError> {
        match &self.where_clause {
            Some(where_clause) => self.identifier.span.join(&where_clause.span()?),
            None => match &self.return_type {
                Some(return_type) => self.identifier.span.join(&return_type.span()?),
                None => self.identifier.span.join(&self.parameters.span()?),
            },
        }
    }
}

/// Represents a syntax tree node for a function body.
///
/// Syntax Synopsis:
/// ``` text
/// FunctionBody:
///     '{' Statement* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionBody {
    pub left_brace: Punctuation,
    pub statements: Vec<Statement>,
    pub right_brace: Punctuation,
}

impl SourceElement for FunctionBody {
    fn span(&self) -> Result<Span, SpanError> { self.left_brace.span.join(&self.right_brace.span) }
}

/// Represents a syntax tree node for a function item declaration.
///
/// Syntax Synopsis:
/// ``` text
/// Function:
///     AccessModifier FunctionSignature FunctionBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub access_modifier: AccessModifier,
    pub function_signature: FunctionSignature,
    pub function_body: FunctionBody,
}

impl SourceElement for Function {
    fn span(&self) -> Result<Span, SpanError> {
        self.access_modifier
            .span()?
            .join(&self.function_body.span()?)
    }
}

/// Represents a syntax tree node for a `type` alias signature.
///
/// Syntax Synopsis:
/// ``` text
/// TypeSignature:
///     'type' Identifier GenericParameters?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeSignature {
    pub type_keyword: Keyword,
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
}

impl SourceElement for TypeSignature {
    fn span(&self) -> Result<Span, SpanError> {
        if let Some(generic_parameters) = &self.generic_parameters {
            self.type_keyword.span.join(&generic_parameters.span()?)
        } else {
            self.type_keyword.span.join(&self.identifier.span()?)
        }
    }
}

/// Represents a syntax tree node for a `type` alias declaration.
///
/// Syntax Synopsis:
/// ``` text
/// TypeDeclaration:
///     '=' TypeSpecifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDefinition {
    pub equals: Punctuation,
    pub type_specifier: TypeSpecifier,
}

impl SourceElement for TypeDefinition {
    fn span(&self) -> Result<Span, SpanError> {
        self.equals.span.join(&self.type_specifier.span()?)
    }
}

/// Represents a syntax tree node for a `type` alias item declaration.
///
/// Syntax Synopsis:
/// ``` text
/// Type:
///     AccessModifier TypeSignature TypeDefinition ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub access_modifier: AccessModifier,
    pub type_signature: TypeSignature,
    pub type_definition: TypeDefinition,
    pub semicolon: Punctuation,
}

impl SourceElement for Type {
    fn span(&self) -> Result<Span, SpanError> {
        self.access_modifier.span()?.join(&self.semicolon.span)
    }
}

/// Represents a syntax tree node for a struct signature.
///
/// Syntax Synopsis:
/// ``` text
/// StructSignature:
///     'struct' Identifier GenericParameters? WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructSignature {
    pub struct_keyword: Keyword,
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
    pub where_clause: Option<WhereClause>,
}

impl SourceElement for StructSignature {
    fn span(&self) -> Result<Span, SpanError> {
        self.struct_keyword.span.join(&self.identifier.span)
    }
}

/// Represents a syntax tree node for a struct definition.
///
/// Syntax Synopsis:
/// ``` text
/// StructDefinition:
///     '{' StructField* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructBody {
    pub left_brace: Punctuation,
    pub struct_fields: Vec<StructField>,
    pub right_brace: Punctuation,
}

impl SourceElement for StructBody {
    fn span(&self) -> Result<Span, SpanError> { self.left_brace.span.join(&self.right_brace.span) }
}

/// Represents a syntax tree node for a struct item declaration.
///
/// Syntax Synopsis:
/// ``` text
/// Struct:
///     AccessModifier StructSignature StructBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub access_modifier: AccessModifier,
    pub struct_signature: StructSignature,
    pub struct_body: StructBody,
}

impl SourceElement for Struct {
    fn span(&self) -> Result<Span, SpanError> {
        self.access_modifier.span()?.join(&self.struct_body.span()?)
    }
}

/// Represents a syntax tree node for a struct field member.
///
/// Syntax Synopsis:
/// ``` text
/// StructField:
///     AccessModifier 'let' Identifier TypeAnnotation
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub access_modifier: AccessModifier,
    pub let_keyword: Keyword,
    pub identifier: Identifier,
    pub type_annotation: TypeAnnotation,
}

impl SourceElement for StructField {
    fn span(&self) -> Result<Span, SpanError> {
        self.access_modifier
            .span()?
            .join(&self.type_annotation.span()?)
    }
}

/// Represents a syntax tree node for a struct `type` aliias member.
///
/// Syntax Synopsis:
/// ``` text
/// StructType:
///     AccessModifier TypeSignature TypeDefinition ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    pub access_modifier: AccessModifier,
    pub type_signature: TypeSignature,
    pub type_definition: TypeDefinition,
    pub semicolon: Punctuation,
}

impl SourceElement for StructType {
    fn span(&self) -> Result<Span, SpanError> {
        self.access_modifier.span()?.join(&self.semicolon.span()?)
    }
}

/// Represents a syntax tree node for a struct member.
///
/// Syntax Synopsis:
/// ``` text
/// StructMember:
///     StructField
///     | StructType
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum StructMember {
    Field(StructField),
    Type(StructType),
}

impl SourceElement for StructMember {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Field(field) => field.span(),
            Self::Type(ty) => ty.span(),
        }
    }
}

/// Represents a syntax tree node for a implements block signature.
///
/// Syntax Synopsis:
/// ``` text
/// ImplementsSignature:
///     'implements' GenericParameters? QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplementsSignature {
    pub implements_keyword: Keyword,
    pub generic_parameters: Option<GenericParameters>,
    pub qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for ImplementsSignature {
    fn span(&self) -> Result<Span, SpanError> {
        self.implements_keyword
            .span
            .join(&self.qualified_identifier.span()?)
    }
}

/// Represents a syntax tree node for a member in an implements block.
///
/// Syntax Synopsis:
/// ``` text
/// ImplementsMember:
///     Function
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum ImplementsMember {
    Function(Function),
}

impl SourceElement for ImplementsMember {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Function(function) => function.span(),
        }
    }
}

/// Represents a syntax tree node for an implements block body.
///
/// Syntax Synopsis:
/// ``` text
/// ImplementsBody:
///     '{' ImplementsMember* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplementsBody {
    pub left_brace: Punctuation,
    pub implements_members: Vec<ImplementsMember>,
    pub right_brace: Punctuation,
}

impl SourceElement for ImplementsBody {
    fn span(&self) -> Result<Span, SpanError> { self.left_brace.span.join(&self.right_brace.span) }
}

/// Represents a syntax tree node for an implements block item.
///
/// Syntax Synopsis:
/// ``` text
/// Implements:
///     ImplementsSignature ImplementsBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Implements {
    pub implements_signature: ImplementsSignature,
    pub implements_body: ImplementsBody,
}

impl SourceElement for Implements {
    fn span(&self) -> Result<Span, SpanError> {
        self.implements_signature
            .span()?
            .join(&self.implements_body.span()?)
    }
}

/// Represents a syntax tree for an access modifier.
///
/// Syntax Synopsis:
/// ```text
/// AccessModifier:
///     'public'
///      | 'private'
///      | 'internal'
///      ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum AccessModifier {
    Public(Keyword),
    Private(Keyword),
    Internal(Keyword),
}

impl SourceElement for AccessModifier {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Public(k) | Self::Private(k) | Self::Internal(k) => Ok(k.span.clone()),
        }
    }
}

/// Represents a syntax tree for an enum signature.
///
/// Syntax Synopsis:
/// ```text
/// EnumSignature:
///     'enum' Identifier
///     ;
/// ``
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumSignature {
    pub enum_keyword: Keyword,
    pub identifier: Identifier,
}

impl SourceElement for EnumSignature {
    fn span(&self) -> Result<Span, SpanError> { self.enum_keyword.span.join(&self.identifier.span) }
}

/// Represents a syntax tree for a list of enum variant identifiers separated by commas.
///
/// Syntax Synopsis:
/// ``` text
/// EnumVariantList:
///     Identifier (',' Identifier)*
///     ;
/// ```
pub type EnumVariantList = ConnectedList<Identifier, Punctuation>;

/// Represents a syntax tree for an enum body.
///
/// Syntax Synopsis:
/// ```text
/// EnumBody:
///     '{' EnumVariantList? '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumBody {
    pub left_brace: Punctuation,
    pub enum_variant_list: Option<EnumVariantList>,
    pub right_brace: Punctuation,
}

impl SourceElement for EnumBody {
    fn span(&self) -> Result<Span, SpanError> { self.left_brace.span.join(&self.right_brace.span) }
}

/// Represents a syntax tree for an enum.
///
/// Syntax Synopsis:
/// ```text
/// Enum:
///     AccessModifier EnumSignature EnumBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enum {
    pub access_modifier: AccessModifier,
    pub enum_signature: EnumSignature,
    pub enum_body: EnumBody,
}

impl SourceElement for Enum {
    fn span(&self) -> Result<Span, SpanError> {
        self.access_modifier.span()?.join(&self.enum_body.span()?)
    }
}

/// Represents a syntax tree node for an item.
///
/// Syntax Synopsis:
/// ```text
/// Item:
///     Trait
///     | Function
///     | Type
///     | Struct
///     | Implements
///     | Enum
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(clippy::large_enum_variant)]
pub enum Item {
    Trait(Trait),
    Function(Function),
    Type(Type),
    Struct(Struct),
    Implements(Implements),
    Enum(Enum),
}

impl SourceElement for Item {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Trait(t) => t.span(),
            Self::Function(f) => f.span(),
            Self::Type(t) => t.span(),
            Self::Struct(s) => s.span(),
            Self::Implements(i) => i.span(),
            Self::Enum(e) => e.span(),
        }
    }
}

impl<'a> Parser<'a> {
    fn parse_access_modifier(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<AccessModifier> {
        match self.next_significant_token() {
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Public => {
                Ok(AccessModifier::Public(k))
            }
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Private => {
                Ok(AccessModifier::Private(k))
            }
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Internal => {
                Ok(AccessModifier::Internal(k))
            }
            found => {
                handler.recieve(Error::AccessModifierExpected(AccessModifierExpected {
                    found,
                }));
                Err(ParserError)
            }
        }
    }

    fn parse_generic_parameters(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<GenericParameters> {
        let left_angle_bracket = self.parse_punctuation('<', true, handler)?;
        let (generic_parameter_list, right_angle_bracket) = self.parse_enclosed_list_manual(
            '>',
            ',',
            |parser| match parser.next_significant_token() {
                Some(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                    Ok(GenericParameter::Lifetime(LifetimeParameter {
                        apostrophe,
                        identifier: parser.parse_identifier(handler)?,
                    }))
                }
                _ => Ok(GenericParameter::Type(TypeParameter {
                    identifier: parser.parse_identifier(handler)?,
                })),
            },
            handler,
        )?;

        let Some(generic_parameter_list) = generic_parameter_list else {
            handler.recieve(
                Error::GenericArgumentParameterListCannotBeEmpty(GenericArgumentParameterListCannotBeEmpty {
                    span: left_angle_bracket.span.join(&right_angle_bracket.span).expect("Span should be joint successfully"),
                }
            ));
            return Err(ParserError);
        };

        Ok(GenericParameters {
            left_angle_bracket,
            generic_parameter_list,
            right_angle_bracket,
        })
    }

    fn try_parse_generic_parameters(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Option<GenericParameters>> {
        if matches!(self.stop_at_significant(), Some(Token::Punctuation(p)) if p.punctuation == '<')
        {
            Ok(Some(self.parse_generic_parameters(handler)?))
        } else {
            Ok(None)
        }
    }

    fn parse_implements_body(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<ImplementsBody> {
        let left_brace = self.step_into(Delimiter::Brace, handler)?;

        let mut implements_members = Vec::new();
        while !self.is_exhausted() {
            todo!("implements members")
        }

        let right_brace = self
            .step_out(handler)
            .expect("All the tokens should be consumed");

        Ok(ImplementsBody {
            left_brace,
            implements_members,
            right_brace,
        })
    }

    fn parse_implements(&mut self, handler: &impl Handler<Error>) -> ParserResult<Implements> {
        let implements_keyword = self.parse_keyword(KeywordKind::Implements, handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;
        let qualified_identifier = self.parse_qualified_identifier(false, handler)?;
        let implements_body = self.parse_implements_body(handler)?;

        Ok(Implements {
            implements_signature: ImplementsSignature {
                implements_keyword,
                generic_parameters,
                qualified_identifier,
            },
            implements_body,
        })
    }

    /// Parses an [`Item`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_item(&mut self, handler: &impl Handler<Error>) -> ParserResult<Item> {
        match self.stop_at_significant() {
            // parses an item with an access modifier
            Some(Token::Keyword(k))
                if matches!(
                    k.keyword,
                    KeywordKind::Public | KeywordKind::Private | KeywordKind::Internal
                ) => todo!("parse item with access modifier"),

            // parses an implements
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Implements => self.parse_implements(handler).map(Item::Implements)

            found => {
                handler.recieve(Error::ItemExpected(ItemExpected { found }));
                Err(ParserError)
            }
        }
    }
}

