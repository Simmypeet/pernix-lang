//! Contains syntax tree nodes for items.

use derive_getters::Dissolve;
use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_common::source_file::{SourceElement, Span};
use pernixc_lexical::token::{Identifier, Keyword, KeywordKind, Punctuation, Token};

use super::{expression::BlockWithoutLabel, ConnectedList, TypeAnnotation, TypeSpecifier};
use crate::{
    errors::{AccessModifierExpected, ItemExpected, MemberExpected},
    parser::Parser,
};

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
#[allow(missing_docs)]
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
#[allow(missing_docs)]
pub struct Field {
    #[get = "pub"]
    pub(super) access_modifier: AccessModifier,
    #[get = "pub"]
    pub(super) let_keyword: Keyword,
    #[get = "pub"]
    pub(super) identifier: Identifier,
    #[get = "pub"]
    pub(super) type_annotation: TypeAnnotation,
    #[get = "pub"]
    pub(super) semicolon: Punctuation,
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
    pub(super) access_modifier: AccessModifier,
    #[get = "pub"]
    pub(super) struct_keyword: Keyword,
    #[get = "pub"]
    pub(super) identifier: Identifier,
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, Dissolve)]
pub struct StructBody {
    #[get = "pub"]
    pub(super) left_brace: Punctuation,
    #[get = "pub"]
    pub(super) members: Vec<Member>,
    #[get = "pub"]
    pub(super) right_brace: Punctuation,
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, Dissolve)]
#[allow(missing_docs)]
pub struct Struct {
    #[get = "pub"]
    pub(super) signature: StructSignature,
    #[get = "pub"]
    pub(super) body: StructBody,
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
    pub(super) access_modifier: AccessModifier,
    #[get = "pub"]
    pub(super) type_keyword: Keyword,
    #[get = "pub"]
    pub(super) identifier: Identifier,
    #[get = "pub"]
    pub(super) equals: Punctuation,
    #[get = "pub"]
    pub(super) type_specifier: TypeSpecifier,
    #[get = "pub"]
    pub(super) semicolon: Punctuation,
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
#[allow(missing_docs)]
pub struct EnumSignature {
    #[get = "pub"]
    pub(super) access_modifier: AccessModifier,
    #[get = "pub"]
    pub(super) enum_keyword: Keyword,
    #[get = "pub"]
    pub(super) identifier: Identifier,
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, Dissolve)]
#[allow(missing_docs)]
pub struct EnumBody {
    #[get = "pub"]
    pub(super) left_brace: Punctuation,
    #[get = "pub"]
    pub(super) variants: Option<EnumVariantList>,
    #[get = "pub"]
    pub(super) right_brace: Punctuation,
}

/// Represents a syntax tree node for an enum.
///
/// Syntax Synopsis:
/// ``` text
/// Enum:
///     EnumSignature EnumBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, Dissolve)]
#[allow(missing_docs)]
pub struct Enum {
    #[get = "pub"]
    pub(super) signature: EnumSignature,
    #[get = "pub"]
    pub(super) body: EnumBody,
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
#[allow(missing_docs)]
pub struct Parameter {
    #[get = "pub"]
    pub(super) mutable_keyword: Option<Keyword>,
    #[get = "pub"]
    pub(super) identifier: Identifier,
    #[get = "pub"]
    pub(super) type_annotation: TypeAnnotation,
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
///     AccessModifier 'function' Identifier '(' ParameterList? ')' TypeAnnotation
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, Dissolve)]
pub struct FunctionSignature {
    #[get = "pub"]
    pub(super) access_modifier: AccessModifier,
    #[get = "pub"]
    pub(super) function_keyword: Keyword,
    #[get = "pub"]
    pub(super) identifier: Identifier,
    #[get = "pub"]
    pub(super) left_paren: Punctuation,
    #[get = "pub"]
    pub(super) parameters: Option<ParameterList>,
    #[get = "pub"]
    pub(super) right_paren: Punctuation,
    #[get = "pub"]
    pub(super) type_annotation: TypeAnnotation,
}

impl SourceElement for FunctionSignature {
    fn span(&self) -> Span {
        self.access_modifier
            .span()
            .join(&self.type_annotation.span())
            .unwrap()
    }
}

/// Represents a syntax tree node for a function.
///
/// Syntax Synopsis:
/// ``` text
/// Function:
///     FunctionSignature BlockWithoutLabel
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, Dissolve)]
#[allow(missing_docs)]
pub struct Function {
    #[get = "pub"]
    pub(super) signature: FunctionSignature,
    #[get = "pub"]
    pub(super) block_without_label: BlockWithoutLabel,
}

impl Function {}

impl SourceElement for Function {
    fn span(&self) -> Span {
        self.signature
            .access_modifier
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
#[allow(missing_docs)]
pub struct Module {
    #[get = "pub"]
    pub(super) access_modifier: AccessModifier,
    #[get = "pub"]
    pub(super) module_keyword: Keyword,
    #[get = "pub"]
    pub(super) identifier: Identifier,
    #[get = "pub"]
    pub(super) semicolon: Punctuation,
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
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Item {
    Struct(Struct),
    Enum(Enum),
    Function(Function),
    Module(Module),
    TypeAlias(TypeAlias),
}

impl SourceElement for Item {
    fn span(&self) -> Span {
        match self {
            Self::Struct(item) => item.span(),
            Self::Enum(item) => item.span(),
            Self::Function(item) => item.span(),
            Self::Module(item) => item.span(),
            Self::TypeAlias(item) => item.span(),
        }
    }
}

impl<'a> Parser<'a> {
    /// Parses an [Item].
    #[allow(clippy::too_many_lines)]
    pub fn parse_item(&mut self) -> Option<Item> {
        // expect access modifier keyword token
        let access_modifier = self.parse_access_modifier()?;

        match self.peek_significant_token() {
            // Handles struct item
            Some(Token::Keyword(struct_keyword))
                if struct_keyword.keyword == KeywordKind::Struct =>
            {
                // eat struct keyword
                self.next_token();
                self.handle_struct(access_modifier, struct_keyword.clone())
            }
            // Handles enum
            Some(Token::Keyword(enum_keyword)) if enum_keyword.keyword == KeywordKind::Enum => {
                // eat token
                self.next_token();

                let identifier = self.expect_identifier()?;
                let left_brace = self.expect_punctuation('{')?;
                let (variants, right_brace) =
                    self.parse_enclosed_list('}', ',', |parse| parse.expect_identifier().cloned())?;

                Some(
                    Enum {
                        signature: EnumSignature {
                            access_modifier,
                            enum_keyword: enum_keyword.clone(),
                            identifier: identifier.clone(),
                        },
                        body: EnumBody {
                            left_brace: left_brace.clone(),
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

                let identifier = self.expect_identifier()?;
                let semicolon = self.expect_punctuation(';')?;

                Some(
                    Module {
                        access_modifier,
                        module_keyword: module_keyword.clone(),
                        identifier: identifier.clone(),
                        semicolon: semicolon.clone(),
                    }
                    .into(),
                )
            }
            // Handles type alias
            Some(Token::Keyword(type_keyword)) if type_keyword.keyword == KeywordKind::Type => {
                // eat token
                self.next_token();

                Some(
                    self.parse_type_alias(access_modifier, type_keyword.clone())?
                        .into(),
                )
            }
            // Handles function
            Some(Token::Keyword(function_keyword))
                if function_keyword.keyword == KeywordKind::Function =>
            {
                // eat function keyword
                self.next_token();

                let identifier = self.expect_identifier()?.clone();
                let left_paren = self.expect_punctuation('(')?.clone();

                let (parameters, right_paren) = self.parse_enclosed_list(')', ',', |parse| {
                    let mutable_keyword = match parse.peek_significant_token() {
                        Some(Token::Keyword(mutable_keyword))
                            if mutable_keyword.keyword == KeywordKind::Mutable =>
                        {
                            // eat mutable keyword
                            parse.next_token();

                            Some(mutable_keyword.clone())
                        }
                        _ => None,
                    };
                    let identifier = parse.expect_identifier()?;
                    let type_annotation = parse.parse_type_annotation()?;

                    Some(Parameter {
                        mutable_keyword,
                        identifier: identifier.clone(),
                        type_annotation,
                    })
                })?;

                let type_annotation = self.parse_type_annotation()?;
                let block_without_label = self.parse_block_without_label()?;

                Some(
                    Function {
                        signature: FunctionSignature {
                            access_modifier,
                            function_keyword: function_keyword.clone(),
                            identifier,
                            left_paren,
                            parameters,
                            right_paren,
                            type_annotation,
                        },
                        block_without_label,
                    }
                    .into(),
                )
            }
            token => {
                // make progress
                self.next_token();

                self.report_error(
                    ItemExpected {
                        found: token.cloned(),
                    }
                    .into(),
                );
                None
            }
        }
    }

    fn parse_type_alias(
        &mut self,
        access_modifier: AccessModifier,
        type_keyword: Keyword,
    ) -> Option<TypeAlias> {
        let identifier = self.expect_identifier()?;
        let equals = self.expect_punctuation('=')?;
        let type_specifier = self.parse_type_specifier()?;
        let semicolon = self.expect_punctuation(';')?;

        Some(TypeAlias {
            access_modifier,
            type_keyword,
            identifier: identifier.clone(),
            equals: equals.clone(),
            type_specifier,
            semicolon: semicolon.clone(),
        })
    }

    fn handle_struct(
        &mut self,
        access_modifier: AccessModifier,
        struct_keyword: Keyword,
    ) -> Option<Item> {
        let identifier = self.expect_identifier()?;
        let left_brace = self.expect_punctuation('{')?;
        let mut members = Vec::new();
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

                    match self.next_significant_token() {
                        Some(Token::Keyword(let_keyword))
                            if let_keyword.keyword == KeywordKind::Let =>
                        {
                            let identifier = self.expect_identifier()?;
                            let type_annotation = self.parse_type_annotation()?;
                            let semicolon = self.expect_punctuation(';')?;

                            members.push(
                                Field {
                                    access_modifier,
                                    let_keyword: let_keyword.clone(),
                                    identifier: identifier.clone(),
                                    type_annotation,
                                    semicolon: semicolon.clone(),
                                }
                                .into(),
                            );
                        }
                        Some(Token::Keyword(type_keyword))
                            if type_keyword.keyword == KeywordKind::Type =>
                        {
                            members.push(
                                self.parse_type_alias(access_modifier, type_keyword.clone())?
                                    .into(),
                            );
                        }
                        found => {
                            self.report_error(
                                MemberExpected {
                                    found: found.cloned(),
                                }
                                .into(),
                            );
                            self.forward_until(|token| {
                                matches!(token, Token::Punctuation(punc) if punc.punctuation == '}')
                                || matches!(token, Token::Keyword(kw) if kw.keyword == KeywordKind::Public
                                    || kw.keyword == KeywordKind::Private
                                    || kw.keyword == KeywordKind::Internal)
                            });
                        }
                    }
                }
                found => {
                    self.report_error(
                        AccessModifierExpected {
                            found: found.cloned(),
                        }
                        .into(),
                    );
                    self.forward_until(|token| {
                        matches!(token, Token::Punctuation(punc) if punc.punctuation == '}')
                        || matches!(token, Token::Keyword(kw) if kw.keyword == KeywordKind::Public
                            || kw.keyword == KeywordKind::Private
                            || kw.keyword == KeywordKind::Internal)
                    });
                }
            }
        };

        Some(
            Struct {
                signature: StructSignature {
                    access_modifier,
                    struct_keyword,
                    identifier: identifier.clone(),
                },
                body: StructBody {
                    left_brace: left_brace.clone(),
                    members,
                    right_brace: right_brace.clone(),
                },
            }
            .into(),
        )
    }

    /// Parses an [`AccessModifier`]
    pub fn parse_access_modifier(&mut self) -> Option<AccessModifier> {
        match self.next_significant_token() {
            Some(Token::Keyword(public_keyword))
                if public_keyword.keyword == KeywordKind::Public =>
            {
                Some(AccessModifier::Public(public_keyword.clone()))
            }
            Some(Token::Keyword(private_keyword))
                if private_keyword.keyword == KeywordKind::Private =>
            {
                Some(AccessModifier::Private(private_keyword.clone()))
            }
            Some(Token::Keyword(internal_keyword))
                if internal_keyword.keyword == KeywordKind::Internal =>
            {
                Some(AccessModifier::Internal(internal_keyword.clone()))
            }
            found => {
                self.report_error(
                    AccessModifierExpected {
                        found: found.cloned(),
                    }
                    .into(),
                );
                None
            }
        }
    }
}

#[cfg(test)]
mod tests;
