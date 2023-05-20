//! Contains syntax tree nodes for items.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_lexical::token::{Identifier, Keyword, KeywordKind, Punctuation, Token};
use pernixc_source::{SourceElement, Span};
use pernixc_system::error_handler::ErrorHandler;

use super::{
    expression::BlockWithoutLabel, ConnectedList, Label, QualifiedIdentifier, TypeAnnotation,
    TypeSpecifier,
};
use crate::{
    error::{
        AccessModifierExpected, Error, GenericArgumentParameterListCannotBeEmpty, ItemExpected,
        PunctuationExpected, StructMemberExpected, TraitMemberExpected,
    },
    parser::Parser,
};

/// Represents a syntax tree node for a lifetime parameter.
///
/// Syntax Synopsis:
/// ``` text
/// LifetimeParameter:
///     Label
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct LifetimeParameter {
    #[get = "pub"]
    label: Label,
}

impl SourceElement for LifetimeParameter {
    fn span(&self) -> Span { self.label.span() }
}

/// Represents a syntax tree node for a type parameter.
///
/// Syntax Synopsis:
/// ```text
/// TypeParameter:
///     Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct TypeParameter {
    #[get = "pub"]
    identifier: Identifier,
}

impl SourceElement for TypeParameter {
    fn span(&self) -> Span { self.identifier.span() }
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum GenericParameter {
    LifetimeParameter(LifetimeParameter),
    TypeParameter(TypeParameter),
}

impl SourceElement for GenericParameter {
    fn span(&self) -> Span {
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct GenericParameters {
    #[get = "pub"]
    left_angle_bracket: Punctuation,
    #[get = "pub"]
    generic_parameter_list: GenericParameterList,
    #[get = "pub"]
    right_angle_bracket: Punctuation,
}

impl SourceElement for GenericParameters {
    fn span(&self) -> Span {
        self.left_angle_bracket
            .span()
            .join(&self.right_angle_bracket.span())
            .unwrap()
    }
}

/// Represents a syntax tree node of a constraint used in a where clause.
///
/// Syntax Synopsis:
/// ``` text
/// Constraint:
///     QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Constraint {
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for Constraint {
    fn span(&self) -> Span { self.qualified_identifier.span() }
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct WhereClause {
    #[get = "pub"]
    where_keyword: Keyword,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    constraint_list: ConstraintList,
}

impl SourceElement for WhereClause {
    fn span(&self) -> Span {
        self.where_keyword
            .span()
            .join(&self.constraint_list.span())
            .unwrap()
    }
}

/// Represents a syntax tree node for a trait item declaration.
///
/// Syntax Synopsis:
/// ``` text
/// Trait:
///     AccessModifier 'trait' Identifier GenericParameters? '{' TraitItem* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Trait {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    trait_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_parameters: GenericParameters,
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    trait_members: Vec<TraitMember>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for Trait {
    fn span(&self) -> Span {
        self.access_modifier
            .span()
            .join(&self.right_brace.span())
            .unwrap()
    }
}

/// Represents a syntax tree node for a trait function declaration.
///
/// Syntax Synopsis:
/// ``` text
/// TraitFunctionMember:
///     FunctionSignature ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct TraitFunctionMember {
    #[get = "pub"]
    function_signature: FunctionSignature,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for TraitFunctionMember {
    fn span(&self) -> Span {
        self.function_signature
            .span()
            .join(&self.semicolon.span())
            .unwrap()
    }
}

/// Represents a syntax tree node for a trait member.
///
/// Syntax Synopsis:
/// ``` text
/// TraitMember:
///     TraitFunctionMember
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum TraitMember {
    Function(TraitFunctionMember),
}

/// Represents a syntax tree node for an access modifier.
///
/// Syntax Synopsis:
/// ```text
/// AccessModifier:
///     'public'
///     | 'private'
///     | 'internal'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum AccessModifier {
    Public(Keyword),
    Private(Keyword),
    Internal(Keyword),
}

impl SourceElement for AccessModifier {
    fn span(&self) -> Span {
        match self {
            Self::Public(keyword) | Self::Private(keyword) | Self::Internal(keyword) => {
                keyword.span.clone()
            }
        }
    }
}

/// Represents a syntax tree node for a field.
///
/// Syntax Synopsis:
/// ``` text
/// Field:
///     AccessModifier 'let' Identifier TypeAnnotation ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Field {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    let_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    type_annotation: TypeAnnotation,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Field {
    fn span(&self) -> Span { self.identifier.span().join(&self.semicolon.span).unwrap() }
}

/// Represents a syntax tree node for a member of a struct.
///
/// Syntax Synopsis:
/// ``` text
/// Member:
///     Field
///     | TypeAlias
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Member {
    Field(Field),
    TypeAlias(TypeAlias),
}

impl SourceElement for Member {
    fn span(&self) -> Span {
        match self {
            Self::Field(field) => field.span(),
            Self::TypeAlias(type_alias) => type_alias.span(),
        }
    }
}

/// Represents a syntax tree node for a struct signature.
///
/// Syntax Synopsis:
/// ``` text
/// StructSignature:
///     AccessModifier 'struct' Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct StructSignature {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    struct_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_parameters: Option<GenericParameters>,
}

impl SourceElement for StructSignature {
    fn span(&self) -> Span {
        self.access_modifier
            .span()
            .join(&self.identifier.span)
            .unwrap()
    }
}

/// Represents a syntax tree node for a struct body.
///
/// Syntax Synopsis:
/// ``` text
/// StructBody:
///     '{' Members* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct StructBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    members: Vec<Member>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl StructBody {
    /// Dissolves the struct body into its components.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Vec<Member>, Punctuation) {
        (self.left_brace, self.members, self.right_brace)
    }
}

impl SourceElement for StructBody {
    fn span(&self) -> Span { self.left_brace.span().join(&self.right_brace.span).unwrap() }
}

/// Represents a syntax tree node for a struct.
///
/// Syntax Synopsis:
/// ``` text
/// Struct:
///     AccessModifier 'struct' Identifier '{' Members* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Struct {
    #[get = "pub"]
    signature: StructSignature,
    #[get = "pub"]
    body: StructBody,
}

impl Struct {
    /// Dissolves the struct into its components.
    #[must_use]
    pub fn dissolve(self) -> (StructSignature, StructBody) { (self.signature, self.body) }
}

impl SourceElement for Struct {
    fn span(&self) -> Span { self.signature.span().join(&self.body.span()).unwrap() }
}

/// Represents a syntax tree node for a type alias.
///
/// Syntax Synopsis:
/// ``` text
/// Type:
///     AccessModifier 'type' Identifier '=' TypeSpecifier ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct TypeAlias {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    type_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    type_specifier: TypeSpecifier,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for TypeAlias {
    fn span(&self) -> Span {
        self.access_modifier
            .span()
            .join(&self.semicolon.span)
            .unwrap()
    }
}

/// Represents a syntax tree node for an enum signature.
///
/// Syntax Synopsis:
/// ``` text
/// EnumSignature:
///     AccessModifier 'enum' Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct EnumSignature {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    enum_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
}

/// Represents a list of identifiers separated by commas.
///
/// Syntax Synopsis:
/// ``` text
/// EnumVariantList:
///     Identifier (',' Identifier)*
///     ;
/// ```
pub type EnumVariantList = ConnectedList<Identifier, Punctuation>;

/// Represents a syntax tree node for an enum body.
///
/// Syntax Synopsis:
/// ``` text
/// EnumBody:
///     '{' EnumVariantList? '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct EnumBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    variants: Option<EnumVariantList>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl EnumBody {
    /// Dissolves the syntax tree node into its components.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Option<EnumVariantList>, Punctuation) {
        (self.left_brace, self.variants, self.right_brace)
    }
}

/// Represents a syntax tree node for an enum.
///
/// Syntax Synopsis:
/// ``` text
/// Enum:
///     EnumSignature EnumBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Enum {
    #[get = "pub"]
    signature: EnumSignature,
    #[get = "pub"]
    body: EnumBody,
}

impl Enum {
    /// Dissolves the syntax tree node into its components.
    #[must_use]
    pub fn dissolve(self) -> (EnumSignature, EnumBody) { (self.signature, self.body) }
}

impl SourceElement for Enum {
    fn span(&self) -> Span {
        self.signature
            .access_modifier
            .span()
            .join(&self.body.right_brace.span)
            .unwrap()
    }
}

/// Represents a syntax tree node for a function parameter.
///
/// Syntax Synopsis:
/// ``` text
/// Parameter:
///     'mutable"? Identifier ':' TypeSpecifier     
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Parameter {
    #[get = "pub"]
    mutable_keyword: Option<Keyword>,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    type_annotation: TypeAnnotation,
}

impl SourceElement for Parameter {
    fn span(&self) -> Span {
        self.mutable_keyword
            .as_ref()
            .map_or_else(|| &self.identifier.span, |keyword| &keyword.span)
            .join(&self.type_annotation.span())
            .unwrap()
    }
}

/// Represents a list of function parameters separated by commas.
///
/// Syntax Synopsis:
/// ``` text
/// ParameterList:
///     Parameter (',' Parameter)*
///     ;
/// ```
pub type ParameterList = ConnectedList<Parameter, Punctuation>;

/// Represents a syntax tree node for a function signature.
///
/// Syntax Synopsis:
/// ``` text
/// FunctionSignature:
///     Identifier GenericParameters? '(' ParameterList? ')' TypeAnnotation
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct FunctionSignature {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_parameters: Option<GenericParameters>,
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    parameters: Option<ParameterList>,
    #[get = "pub"]
    right_paren: Punctuation,
    #[get = "pub"]
    type_annotation: TypeAnnotation,
    #[get = "pub"]
    where_clause: Option<WhereClause>,
}

impl FunctionSignature {
    /// Dissolves this syntax tree node into its parts.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Identifier,
        Option<GenericParameters>,
        Punctuation,
        Option<ParameterList>,
        Punctuation,
        TypeAnnotation,
        Option<WhereClause>,
    ) {
        (
            self.identifier,
            self.generic_parameters,
            self.left_paren,
            self.parameters,
            self.right_paren,
            self.type_annotation,
            self.where_clause,
        )
    }
}

impl SourceElement for FunctionSignature {
    fn span(&self) -> Span {
        self.identifier
            .span
            .join(&self.type_annotation.span())
            .unwrap()
    }
}

/// Represents a syntax tree node for a function.
///
/// Syntax Synopsis:
/// ``` text
/// Function:
///     AccessModifier FunctionSignature BlockWithoutLabel
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Function {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: FunctionSignature,
    #[get = "pub"]
    block_without_label: BlockWithoutLabel,
}

impl Function {
    /// Dissolves the function into its signature and block.
    #[must_use]
    pub fn dissolve(self) -> (AccessModifier, FunctionSignature, BlockWithoutLabel) {
        (
            self.access_modifier,
            self.signature,
            self.block_without_label,
        )
    }
}

impl SourceElement for Function {
    fn span(&self) -> Span {
        self.access_modifier
            .span()
            .join(&self.block_without_label.span())
            .unwrap()
    }
}

/// Represents a syntax tree node for a module.
///
/// Syntax Synopsis:
/// ``` text
/// Module:
///     AccessModifier 'module' Identifier ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Module {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    module_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Module {
    fn span(&self) -> Span {
        self.access_modifier
            .span()
            .join(&self.semicolon.span)
            .unwrap()
    }
}

/// Is an enumeration of all kinds of items.
///
/// Syntax Synopsis:
/// ``` text
/// Item:
///     Struct
///     | Enum
///     | Function
///     | Module
///     | TypeAlias
///     | Trait
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Item {
    Struct(Struct),
    Enum(Enum),
    Function(Function),
    Module(Module),
    TypeAlias(TypeAlias),
    Trait(Trait),
}

impl SourceElement for Item {
    fn span(&self) -> Span {
        match self {
            Self::Struct(item) => item.span(),
            Self::Enum(item) => item.span(),
            Self::Function(item) => item.span(),
            Self::Module(item) => item.span(),
            Self::TypeAlias(item) => item.span(),
            Self::Trait(item) => item.span(),
        }
    }
}

impl<'a> Parser<'a> {
    /// Parses an [Item].
    #[allow(clippy::too_many_lines)]
    pub fn parse_item(&mut self, handler: &impl ErrorHandler<Error>) -> Option<Item> {
        // expect access modifier keyword token
        let access_modifier = self.parse_access_modifier(handler)?;

        match self.peek_significant_token() {
            // Handles struct item
            Some(Token::Keyword(struct_keyword))
                if struct_keyword.keyword == KeywordKind::Struct =>
            {
                // eat struct keyword
                self.next_token();
                self.handle_struct(access_modifier, struct_keyword, handler)
                    .map(Item::Struct)
            }

            // Handles trait
            Some(Token::Keyword(trait_keyword)) if trait_keyword.keyword == KeywordKind::Trait => {
                // eat trait keyword
                self.next_token();
                self.parse_trait(access_modifier, trait_keyword, handler)
                    .map(Item::Trait)
            }

            // Handles enum
            Some(Token::Keyword(enum_keyword)) if enum_keyword.keyword == KeywordKind::Enum => {
                // eat token
                self.next_token();

                let identifier = self.expect_identifier(handler)?;
                let left_brace = self.expect_punctuation('{', handler)?;
                let (variants, right_brace) = self.parse_enclosed_list(
                    '}',
                    ',',
                    |parse| parse.expect_identifier(handler),
                    handler,
                )?;

                Some(
                    Enum {
                        signature: EnumSignature {
                            access_modifier,
                            enum_keyword,
                            identifier,
                        },
                        body: EnumBody {
                            left_brace,
                            variants,
                            right_brace,
                        },
                    }
                    .into(),
                )
            }
            // Handles module
            Some(Token::Keyword(module_keyword))
                if module_keyword.keyword == KeywordKind::Module =>
            {
                // eat token
                self.next_token();

                let identifier = self.expect_identifier(handler)?;
                let semicolon = self.expect_punctuation(';', handler)?;

                Some(
                    Module {
                        access_modifier,
                        module_keyword,
                        identifier,
                        semicolon,
                    }
                    .into(),
                )
            }
            // Handles type alias
            Some(Token::Keyword(type_keyword)) if type_keyword.keyword == KeywordKind::Type => {
                // eat token
                self.next_token();

                Some(
                    self.handle_type_alias(access_modifier, type_keyword, handler)?
                        .into(),
                )
            }
            // Handles function
            Some(Token::Identifier(identifier)) => {
                // eat identifier token
                self.next_token();

                let signature = self.parse_function_signature(identifier, handler)?;
                let block_without_label = self.parse_block_without_label(handler)?;

                Some(
                    Function {
                        access_modifier,
                        signature,
                        block_without_label,
                    }
                    .into(),
                )
            }
            found => {
                // make progress
                self.next_token();

                handler.recieve(ItemExpected { found }.into());
                None
            }
        }
    }

    fn parse_trait(
        &mut self,
        access_modifier: AccessModifier,
        trait_keyword: Keyword,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<Trait> {
        let identifier = self.expect_identifier(handler)?;

        match self.peek_significant_token() {
            Some(Token::Punctuation(punc)) if punc.punctuation == '<' => (),
            found => handler.recieve(Error::PunctuationExpected(PunctuationExpected {
                expected: '<',
                found,
            })),
        }

        let generic_parameters = self.try_parse_generic_parameters(handler).ok()?;
        let left_brace = self.expect_punctuation('{', handler)?;

        let mut trait_members = Vec::new();

        let right_brace = loop {
            match self.next_significant_token() {
                Some(Token::Punctuation(right_brace)) if right_brace.punctuation == '}' => {
                    break right_brace;
                }
                found => {
                    let result = match found {
                        Some(Token::Identifier(identifier)) => (|| {
                            Some(TraitMember::Function(TraitFunctionMember {
                                function_signature: self
                                    .parse_function_signature(identifier, handler)?,
                                semicolon: self.expect_punctuation(';', handler)?,
                            }))
                        })(),
                        found => {
                            handler
                                .recieve(Error::TraitMemberExpected(TraitMemberExpected { found }));
                            None
                        }
                    };

                    if let Some(result) = result {
                        trait_members.push(result);
                    } else {
                        match self.next_token_until(|token| {
                            matches!(token, Token::Punctuation(p) if p.punctuation == '}' || p.punctuation == ';') }) {
                                Some(Token::Punctuation(right_brace)) if right_brace.punctuation == '}' => {
                                    break right_brace;
                                }
                                _ => {}
                            }
                    }
                }
            }
        };

        Some(Trait {
            access_modifier,
            trait_keyword,
            identifier,
            generic_parameters: generic_parameters.unwrap(),
            left_brace,
            trait_members,
            right_brace,
        })
    }

    fn parse_function_signature(
        &mut self,
        identifier: Identifier,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<FunctionSignature> {
        let generic_parameters = self.try_parse_generic_parameters(handler).ok()?;
        let left_paren = self.expect_punctuation('(', handler)?;

        let (parameters, right_paren) = self.parse_enclosed_list(
            ')',
            ',',
            |parse| {
                let mutable_keyword = match parse.peek_significant_token() {
                    Some(Token::Keyword(mutable_keyword))
                        if mutable_keyword.keyword == KeywordKind::Mutable =>
                    {
                        // eat mutable keyword
                        parse.next_token();

                        Some(mutable_keyword)
                    }
                    _ => None,
                };
                let identifier = parse.expect_identifier(handler)?;
                let type_annotation = parse.parse_type_annotation(handler)?;

                Some(Parameter {
                    mutable_keyword,
                    identifier,
                    type_annotation,
                })
            },
            handler,
        )?;

        let type_annotation = self.parse_type_annotation(handler)?;
        let where_clause = self.try_parse_where_clause(handler).ok()?;

        Some(FunctionSignature {
            identifier,
            generic_parameters,
            left_paren,
            parameters,
            right_paren,
            type_annotation,
            where_clause,
        })
    }

    fn try_parse_where_clause(
        &mut self,
        handler: &impl ErrorHandler<Error>,
    ) -> Result<Option<WhereClause>, ()> {
        match self.peek_significant_token() {
            Some(Token::Keyword(where_keyword)) if where_keyword.keyword == KeywordKind::Where => {
                // eat where keyword
                self.next_token();

                let colon = self.expect_punctuation(':', handler).ok_or(())?;
                let first = Constraint {
                    qualified_identifier: self
                        .parse_qualified_identifier(handler, false)
                        .ok_or(())?,
                };

                let mut rest = Vec::new();

                while let Some(comma) = match self.peek_significant_token() {
                    Some(Token::Punctuation(comma)) if comma.punctuation == ',' => {
                        // eat comma
                        self.next_token();
                        Some(comma)
                    }
                    _ => None,
                } {
                    let constraint = Constraint {
                        qualified_identifier: self
                            .parse_qualified_identifier(handler, false)
                            .ok_or(())?,
                    };

                    rest.push((comma, constraint));
                }

                Ok(Some(WhereClause {
                    where_keyword,
                    colon,
                    constraint_list: ConstraintList {
                        first,
                        rest,
                        trailing_separator: None,
                    },
                }))
            }
            _ => Ok(None),
        }
    }

    fn try_parse_generic_parameters(
        &mut self,
        handler: &impl ErrorHandler<Error>,
    ) -> Result<Option<GenericParameters>, ()> {
        match self.peek_significant_token() {
            Some(Token::Punctuation(left_angle_bracket))
                if left_angle_bracket.punctuation == '<' =>
            {
                // eat '<'
                self.next_token();

                let (generic_parameter_list, right_angle_bracket) = self
                    .parse_enclosed_list(
                        '>',
                        ',',
                        |this| match this.peek_significant_token() {
                            Some(Token::Punctuation(apostrophe))
                                if apostrophe.punctuation == '\'' =>
                            {
                                // eat apostrophe
                                this.next_token();

                                let identifier = this.expect_identifier(handler)?;

                                Some(GenericParameter::LifetimeParameter(LifetimeParameter {
                                    label: Label {
                                        apostrophe,
                                        identifier,
                                    },
                                }))
                            }
                            _ => {
                                let identifier = this.expect_identifier(handler)?;
                                Some(GenericParameter::TypeParameter(TypeParameter {
                                    identifier,
                                }))
                            }
                        },
                        handler,
                    )
                    .ok_or(())?;

                let Some(generic_parameter_list) = generic_parameter_list else {
                    handler.recieve(Error::GenericArgumentParameterListCannotBeEmpty(
                        GenericArgumentParameterListCannotBeEmpty {
                            span: left_angle_bracket.span.join(&right_angle_bracket.span).unwrap()
                        }
                    ));
                    return Err(());
                };

                Ok(Some(GenericParameters {
                    left_angle_bracket,
                    generic_parameter_list,
                    right_angle_bracket,
                }))
            }
            _ => Ok(None),
        }
    }

    fn handle_type_alias(
        &mut self,
        access_modifier: AccessModifier,
        type_keyword: Keyword,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<TypeAlias> {
        let identifier = self.expect_identifier(handler)?;
        let equals = self.expect_punctuation('=', handler)?;
        let type_specifier = self.parse_type_specifier(handler)?;
        let semicolon = self.expect_punctuation(';', handler)?;

        Some(TypeAlias {
            access_modifier,
            type_keyword,
            identifier,
            equals,
            type_specifier,
            semicolon,
        })
    }

    fn handle_struct_member(
        &mut self,
        access_modifier: AccessModifier,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<Member> {
        match self.next_significant_token() {
            Some(Token::Keyword(let_keyword)) if let_keyword.keyword == KeywordKind::Let => {
                let identifier = self.expect_identifier(handler)?;
                let type_annotation = self.parse_type_annotation(handler)?;
                let semicolon = self.expect_punctuation(';', handler)?;

                Some(
                    Field {
                        access_modifier,
                        let_keyword,
                        identifier,
                        type_annotation,
                        semicolon,
                    }
                    .into(),
                )
            }
            Some(Token::Keyword(type_keyword)) if type_keyword.keyword == KeywordKind::Type => self
                .handle_type_alias(access_modifier, type_keyword, handler)
                .map(std::convert::Into::into),
            found => {
                handler.recieve(StructMemberExpected { found }.into());
                None
            }
        }
    }

    fn handle_struct(
        &mut self,
        access_modifier: AccessModifier,
        struct_keyword: Keyword,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<Struct> {
        let identifier = self.expect_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler).ok()?;
        let left_brace = self.expect_punctuation('{', handler)?;
        let mut members = Vec::new();
        let member_recoverer = |token: &Token| {
            matches!(token, Token::Punctuation(punc) if punc.punctuation == '}')
                || matches!(token, Token::Keyword(kw) if kw.keyword == KeywordKind::Public
                                    || kw.keyword == KeywordKind::Private
                                    || kw.keyword == KeywordKind::Internal)
        };
        let right_brace = loop {
            match self.next_significant_token() {
                Some(Token::Punctuation(right_brace)) if right_brace.punctuation == '}' => {
                    break right_brace;
                }
                Some(Token::Keyword(access_modifier))
                    if access_modifier.keyword == KeywordKind::Public
                        || access_modifier.keyword == KeywordKind::Private
                        || access_modifier.keyword == KeywordKind::Internal =>
                {
                    // parse access modifier
                    let access_modifier = match access_modifier.keyword {
                        KeywordKind::Public => AccessModifier::Public(access_modifier.clone()),
                        KeywordKind::Private => AccessModifier::Private(access_modifier.clone()),
                        KeywordKind::Internal => AccessModifier::Internal(access_modifier.clone()),
                        _ => unreachable!(),
                    };

                    #[allow(clippy::option_if_let_else)]
                    match self.handle_struct_member(access_modifier, handler) {
                        Some(member) => members.push(member),
                        None => {
                            // skip to either access modifier or right brace
                            self.forward_until(member_recoverer);
                        }
                    }
                }
                found => {
                    handler.recieve(AccessModifierExpected { found }.into());
                    self.forward_until(member_recoverer);
                }
            }
        };

        Some(Struct {
            signature: StructSignature {
                access_modifier,
                struct_keyword,
                identifier,
                generic_parameters,
            },
            body: StructBody {
                left_brace,
                members,
                right_brace,
            },
        })
    }

    /// Parses an [`AccessModifier`]
    pub fn parse_access_modifier(
        &mut self,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<AccessModifier> {
        match self.next_significant_token() {
            Some(Token::Keyword(public_keyword))
                if public_keyword.keyword == KeywordKind::Public =>
            {
                Some(AccessModifier::Public(public_keyword))
            }
            Some(Token::Keyword(private_keyword))
                if private_keyword.keyword == KeywordKind::Private =>
            {
                Some(AccessModifier::Private(private_keyword))
            }
            Some(Token::Keyword(internal_keyword))
                if internal_keyword.keyword == KeywordKind::Internal =>
            {
                Some(AccessModifier::Internal(internal_keyword))
            }
            found => {
                handler.recieve(AccessModifierExpected { found }.into());
                None
            }
        }
    }
}

#[cfg(test)]
mod tests;
