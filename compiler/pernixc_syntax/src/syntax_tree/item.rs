use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};
use pernixc_source::{SourceElement, Span, SpanError};
use pernixc_system::diagnostic::{Dummy, Handler};

use super::{
    statement::Statement, AccessModifier, ConnectedList, LifetimeArgument,
    LifetimeArgumentIdentifier, QualifiedIdentifier, TypeAnnotation, TypeSpecifier,
};
use crate::{
    error::{
        AccessModifierExpected, Error, GenericArgumentParameterListCannotBeEmpty, ItemExpected,
        StructMemberExpected,
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone, EnumAsInner, From)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone, EnumAsInner, From)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
///     'trait' Identifier GenericParameters? WhereClause?
///     ;
/// ```
#[derive(Debug, Clone)]
pub struct TraitSignature {
    pub trait_keyword: Keyword,
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
    pub where_clause: Option<WhereClause>,
}

impl SourceElement for TraitSignature {
    fn span(&self) -> Result<Span, SpanError> {
        let start = &self.trait_keyword.span;
        match &self.where_clause {
            Some(where_clause) => start.join(&where_clause.span()?),
            None => match &self.generic_parameters {
                Some(generic_parameters) => start.join(&generic_parameters.span()?),
                None => start.join(&self.identifier.span),
            },
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone, EnumAsInner, From)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
///     TypeAnnotation
///     ;
/// ```
#[derive(Debug, Clone)]
pub struct ReturnType {
    pub type_annotation: TypeAnnotation,
}

impl SourceElement for ReturnType {
    fn span(&self) -> Result<Span, SpanError> { self.type_annotation.span() }
}

/// Represents a syntax tree node for a function signature.
///
/// Syntax Synopsis:
/// ``` text
/// FunctionSignature:
///     Identifier GenericParameters? Parameters ReturnType? WhereClause?
///     ;
/// ```
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
///     '{' StructMember* '}'
///     ;
/// ```
#[derive(Debug, Clone)]
pub struct StructBody {
    pub left_brace: Punctuation,
    pub struct_members: Vec<StructMember>,
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
#[derive(Debug, Clone)]
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
///     AccessModifier 'let' Identifier TypeAnnotation ';'
///     ;
/// ```
#[derive(Debug, Clone)]
pub struct StructField {
    pub access_modifier: AccessModifier,
    pub let_keyword: Keyword,
    pub identifier: Identifier,
    pub type_annotation: TypeAnnotation,
    pub semicolon: Punctuation,
}

impl SourceElement for StructField {
    fn span(&self) -> Result<Span, SpanError> {
        self.access_modifier.span()?.join(&self.semicolon.span)
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone, EnumAsInner, From)]
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
#[derive(Debug, Clone)]
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

/// Represents a syntax tree node for an implements function member.
///
/// Syntax Synopsis:
/// ``` text
/// ImplementsFunction:
///     FunctionSignature FunctionBody
///     ;
/// ```
#[derive(Debug, Clone)]
pub struct ImplementsFunction {
    pub function_signature: FunctionSignature,
    pub function_body: FunctionBody,
}

impl SourceElement for ImplementsFunction {
    fn span(&self) -> Result<Span, SpanError> {
        self.function_signature
            .span()?
            .join(&self.function_body.span()?)
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
#[derive(Debug, Clone, EnumAsInner, From)]
pub enum ImplementsMember {
    Function(ImplementsFunction),
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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

/// Represents a syntax tree for an enum signature.
///
/// Syntax Synopsis:
/// ```text
/// EnumSignature:
///     'enum' Identifier
///     ;
/// ``
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone, EnumAsInner, From)]
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
    /// Parses an [`AccessModifier`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_access_modifier(
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
            |parser| match parser.stop_at_significant() {
                Some(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                    // eat apostrophe
                    parser.forward();

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

    fn parse_cosntraint(&mut self, handler: &impl Handler<Error>) -> ParserResult<Constraint> {
        match self.stop_at_significant() {
            Some(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                // eat apostrophe
                self.forward();

                let lifetime_argument_identifier = match self.stop_at_significant() {
                    Some(Token::Keyword(static_keyword))
                        if static_keyword.keyword == KeywordKind::Static =>
                    {
                        // eat static keyword
                        self.forward();
                        LifetimeArgumentIdentifier::StaticKeyword(static_keyword)
                    }
                    _ => {
                        let identifier = self.parse_identifier(handler)?;
                        LifetimeArgumentIdentifier::Identifier(identifier)
                    }
                };

                Ok(Constraint::LifetimeArgument(LifetimeArgument {
                    apostrophe,
                    lifetime_argument_identifier,
                }))
            }
            _ => Ok(Constraint::TraitConstraint(TraitConstraint {
                qualified_identifier: self.parse_qualified_identifier(false, handler)?,
            })),
        }
    }

    fn parse_where_clause(&mut self, handler: &impl Handler<Error>) -> ParserResult<WhereClause> {
        let where_keyword = self.parse_keyword(KeywordKind::Where, handler)?;
        let colon = self.parse_punctuation(':', true, handler)?;

        let first = self.parse_cosntraint(handler)?;
        let mut rest = Vec::new();

        while let Ok(comma) = self.try_parse(|parser| parser.parse_punctuation(',', true, &Dummy)) {
            let constraint = self.parse_cosntraint(handler)?;
            rest.push((comma, constraint));
        }

        Ok(WhereClause {
            where_keyword,
            colon,
            constraint_list: ConnectedList {
                first,
                rest,
                trailing_separator: None,
            },
        })
    }

    fn try_parse_where_clause(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Option<WhereClause>> {
        match self.stop_at_significant() {
            Some(Token::Keyword(where_keyword)) if where_keyword.keyword == KeywordKind::Where => {
                self.parse_where_clause(handler).map(Some)
            }
            _ => Ok(None),
        }
    }

    fn parse_function_body(&mut self, handler: &impl Handler<Error>) -> ParserResult<FunctionBody> {
        let left_brace = self.step_into(Delimiter::Brace, handler)?;

        let mut statements = Vec::new();

        while !self.is_exhausted() {
            // parse statements
            if let Ok(statement) = self.parse_statement(handler) {
                statements.push(statement);
                continue;
            }

            // try to stop at next statement
            self.stop_at(|token| matches!(token, Token::Punctuation(p) if p.punctuation == ';'));

            // go after the semicolon
            if matches!(self.stop_at_significant(), Some(Token::Punctuation(p)) if p.punctuation == ';')
            {
                self.forward();
            }
        }

        let right_brace = self.step_out(handler)?;

        Ok(FunctionBody {
            left_brace,
            statements,
            right_brace,
        })
    }

    fn parse_function_signature(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<FunctionSignature> {
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;

        let parameters = self.parse_enclosed_tree(
            Delimiter::Parenthesis,
            ',',
            |parser, handler| {
                // parse optional mutable keyword
                let mutable_keyword = match parser.stop_at_significant() {
                    Some(Token::Keyword(k)) if k.keyword == KeywordKind::Mutable => {
                        parser.forward();
                        Some(k)
                    }
                    _ => None,
                };

                let identifier = parser.parse_identifier(handler)?;
                let type_annotation = parser.parse_type_annotation(handler)?;

                Ok(Parameter {
                    mutable_keyword,
                    identifier,
                    type_annotation,
                })
            },
            handler,
        )?;

        let parameters = Parameters {
            left_paren: parameters.open,
            parameter_list: parameters.list,
            right_paren: parameters.close,
        };

        let return_type = match self.stop_at_significant() {
            Some(Token::Punctuation(p)) if p.punctuation == ':' => Some(ReturnType {
                type_annotation: self.parse_type_annotation(handler)?,
            }),
            _ => None,
        };

        let where_clause = self.try_parse_where_clause(handler)?;

        Ok(FunctionSignature {
            identifier,
            generic_parameters,
            parameters,
            return_type,
            where_clause,
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
            let result = (|| -> Result<(FunctionSignature, FunctionBody), ParserError> {
                let function_signature = self.parse_function_signature(handler)?;
                let function_body = self.parse_function_body(handler)?;

                Ok((function_signature, function_body))
            })();

            if let Ok((function_signature, function_body)) = result {
                implements_members.push(ImplementsMember::Function(ImplementsFunction {
                    function_signature,
                    function_body,
                }));
                continue;
            }

            // try to stop at next function signature
            self.stop_at(|token| matches!(token, Token::Punctuation(p) if p.punctuation == '{'));

            if matches!(self.stop_at_significant(), Some(Token::Punctuation(p)) if p.punctuation == '{')
            {
                self.forward();
            }
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

    fn parse_trait_signature(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<TraitSignature> {
        let trait_keyword = self.parse_keyword(KeywordKind::Trait, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;
        let where_clause = self.try_parse_where_clause(handler)?;

        Ok(TraitSignature {
            trait_keyword,
            identifier,
            generic_parameters,
            where_clause,
        })
    }

    fn parse_trait_body(&mut self, handler: &impl Handler<Error>) -> ParserResult<TraitBody> {
        let left_brace = self.step_into(Delimiter::Brace, handler)?;

        let mut trait_members = Vec::new();

        while !self.is_exhausted() {
            let trait_member: Result<TraitMember, ParserError> = (|| {
                let function_signature = self.parse_function_signature(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Ok(TraitMember::Function(TraitFunction {
                    function_signature,
                    semicolon,
                }))
            })();

            if let Ok(trait_member) = trait_member {
                trait_members.push(trait_member);
                continue;
            }

            // try to stop at the next semicolon
            self.stop_at(|token| matches!(token, Token::Punctuation(p) if p.punctuation == ';'));

            if matches!(self.stop_at_significant(), Some(Token::Punctuation(p)) if p.punctuation == ';')
            {
                // eat semicolon
                self.forward();
            }
        }

        let right_brace = self.step_out(handler)?;

        Ok(TraitBody {
            left_brace,
            trait_members,
            right_brace,
        })
    }

    fn parse_type_signature(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<TypeSignature> {
        let type_keyword = self.parse_keyword(KeywordKind::Type, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;

        Ok(TypeSignature {
            type_keyword,
            identifier,
            generic_parameters,
        })
    }

    fn parse_type_definition(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<TypeDefinition> {
        let equals = self.parse_punctuation('=', true, handler)?;
        let type_specifier = self.parse_type_specifier(handler)?;

        Ok(TypeDefinition {
            equals,
            type_specifier,
        })
    }

    fn parse_struct_body(&mut self, handler: &impl Handler<Error>) -> ParserResult<StructBody> {
        let left_brace = self.step_into(Delimiter::Brace, handler)?;

        let mut struct_members = Vec::new();

        while !self.is_exhausted() {
            let result: Result<StructMember, ParserError> = (|| {
                let access_modifier = self.parse_access_modifier(handler)?;

                match self.stop_at_significant() {
                    Some(Token::Keyword(let_keyword))
                        if let_keyword.keyword == KeywordKind::Let =>
                    {
                        // eat let keyword
                        self.forward();

                        let identifier = self.parse_identifier(handler)?;
                        let type_annotation = self.parse_type_annotation(handler)?;
                        let semicolon = self.parse_punctuation(';', true, handler)?;

                        Ok(StructMember::Field(StructField {
                            access_modifier,
                            let_keyword,
                            identifier,
                            type_annotation,
                            semicolon,
                        }))
                    }
                    Some(Token::Keyword(type_keyword))
                        if type_keyword.keyword == KeywordKind::Type =>
                    {
                        let type_signature = self.parse_type_signature(handler)?;
                        let type_definition = self.parse_type_definition(handler)?;
                        let semicolon = self.parse_punctuation(';', true, handler)?;

                        Ok(StructMember::Type(StructType {
                            access_modifier,
                            type_signature,
                            type_definition,
                            semicolon,
                        }))
                    }
                    found => {
                        self.forward();
                        handler
                            .recieve(Error::StructMemberExpected(StructMemberExpected { found }));

                        Err(ParserError)
                    }
                }
            })();

            // pushes a result
            if let Ok(struct_member) = result {
                struct_members.push(struct_member);
                continue;
            }

            // stop at next member
            self.stop_at(|token| {
                matches!(token, Token::Keyword(k)
                if matches!(k.keyword,
                    KeywordKind::Public
                    | KeywordKind::Private
                    | KeywordKind::Internal)
                )
            });
        }

        let right_brace = self.step_out(handler)?;

        Ok(StructBody {
            left_brace,
            struct_members,
            right_brace,
        })
    }

    fn parse_struct_signature(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<StructSignature> {
        let struct_keyword = self.parse_keyword(KeywordKind::Struct, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;
        let where_clause = self.try_parse_where_clause(handler)?;

        Ok(StructSignature {
            struct_keyword,
            identifier,
            generic_parameters,
            where_clause,
        })
    }

    fn parse_enum_signature(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<EnumSignature> {
        let enum_keyword = self.parse_keyword(KeywordKind::Enum, handler)?;
        let identifier = self.parse_identifier(handler)?;

        Ok(EnumSignature {
            enum_keyword,
            identifier,
        })
    }

    fn parse_enum_body(&mut self, handler: &impl Handler<Error>) -> ParserResult<EnumBody> {
        let body =
            self.parse_enclosed_tree(Delimiter::Brace, ',', Parser::parse_identifier, handler)?;

        Ok(EnumBody {
            left_brace: body.open,
            enum_variant_list: body.list,
            right_brace: body.close,
        })
    }

    fn parse_item_with_access_modifier(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Item> {
        let access_modifier = self.parse_access_modifier(handler)?;

        match self.stop_at_significant() {
            // parse function
            Some(Token::Identifier(..)) => {
                let function_signature = self.parse_function_signature(handler)?;
                let function_body = self.parse_function_body(handler)?;

                Ok(Item::Function(Function {
                    access_modifier,
                    function_signature,
                    function_body,
                }))
            }

            // parse trait
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Trait => {
                let trait_signature = self.parse_trait_signature(handler)?;
                let trait_body = self.parse_trait_body(handler)?;

                Ok(Item::Trait(Trait {
                    access_modifier,
                    trait_signature,
                    trait_body,
                }))
            }

            // parse struct
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Struct => {
                let struct_signature = self.parse_struct_signature(handler)?;
                let struct_body = self.parse_struct_body(handler)?;

                Ok(Item::Struct(Struct {
                    access_modifier,
                    struct_signature,
                    struct_body,
                }))
            }

            // parse type
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Type => {
                let type_signature = self.parse_type_signature(handler)?;
                let type_definition = self.parse_type_definition(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Ok(Item::Type(Type {
                    access_modifier,
                    type_signature,
                    type_definition,
                    semicolon,
                }))
            }

            // parse enum
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Enum => {
                let enum_signature = self.parse_enum_signature(handler)?;
                let enum_body = self.parse_enum_body(handler)?;

                Ok(Item::Enum(Enum {
                    access_modifier,
                    enum_signature,
                    enum_body,
                }))
            }

            found => {
                self.forward();
                handler.recieve(Error::ItemExpected(ItemExpected { found }));
                Err(ParserError)
            }
        }
    }

    /// Parses an [`Item`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_item(&mut self, handler: &impl Handler<Error>) -> ParserResult<Item> {
        match self.stop_at_significant() {
            // parses an item with an access modifier
            Some(Token::Keyword(access_modifier))
                if matches!(
                    access_modifier.keyword,
                    KeywordKind::Public | KeywordKind::Private | KeywordKind::Internal
                ) =>
            {
                self.parse_item_with_access_modifier(handler)
            }

            // parses an implements
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Implements => {
                self.parse_implements(handler).map(Item::Implements)
            }

            found => {
                self.forward();
                handler.recieve(Error::ItemExpected(ItemExpected { found }));
                Err(ParserError)
            }
        }
    }
}

#[cfg(test)]
mod tests;
