//! Contains syntax tree nodes for items.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use pernixc_lexical::token::{Identifier, Keyword, KeywordKind, Punctuation, Token};

use super::{
    expression::BlockWithoutLabel, ConnectedList, SourceElement, TypeBindingSpecifier,
    TypeSpecifier,
};
use crate::{
    errors::{AccessModifierExpected, FieldGroupExpected, ItemExpected},
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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
                keyword.span
            }
        }
    }
}

/// Represents a syntax tree node for a field.
///
/// Syntax Synopsis:
/// ``` text
/// Field:
///     TypeSpecifier Identifier ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Field {
    pub type_specifier: TypeSpecifier,
    pub identifier: Identifier,
    pub semicolon: Punctuation,
}

impl SourceElement for Field {
    fn span(&self) -> Span {
        Span {
            start: self.type_specifier.span().start,
            end: self.semicolon.span.end,
        }
    }
}

/// Represents a syntax tree node for a field group.
///
/// Syntax Synopsis:
/// ``` text
/// FieldGroup:
///     AccessModifier ':' Field*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct FieldGroup {
    pub access_modifier: AccessModifier,
    pub colon: Punctuation,
    pub fields: Vec<Field>,
}

impl SourceElement for FieldGroup {
    fn span(&self) -> Span {
        Span {
            start: self.access_modifier.span().start,
            end: self.fields.last().unwrap().span().end,
        }
    }
}

/// Represents a syntax tree node for a struct.
///
/// Syntax Synopsis:
/// ``` text
/// Struct:
///     AccessModifier 'struct' Identifier '{' FieldGroup* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Struct {
    pub access_modifier: AccessModifier,
    pub struct_keyword: Keyword,
    pub identifier: Identifier,
    pub left_brace: Punctuation,
    pub field_groups: Vec<FieldGroup>,
    pub right_brace: Punctuation,
}

impl SourceElement for Struct {
    fn span(&self) -> Span {
        Span {
            start: self.access_modifier.span().start,
            end: self.right_brace.span.end,
        }
    }
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

/// Represents a syntax tree node for an enum.
///
/// Syntax Synopsis:
/// ``` text
/// Enum:
///     AccessModifier 'enum' Identifier '{' EnumVariantList? '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Enum {
    pub access_modifier: AccessModifier,
    pub enum_keyword: Keyword,
    pub identifier: Identifier,
    pub left_brace: Punctuation,
    pub variants: Option<EnumVariantList>,
    pub right_brace: Punctuation,
}

impl SourceElement for Enum {
    fn span(&self) -> Span {
        Span {
            start: self.access_modifier.span().start,
            end: self.right_brace.span.end,
        }
    }
}

/// Represents a syntax tree node for a function parameter.
///
/// Syntax Synopsis:
/// ``` text
/// FunctionParameter:
///     TypeBindingSpecifier Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct FunctionParameterSyntaxTree {
    pub type_binding_specifier: TypeBindingSpecifier,
    pub identifier: Identifier,
}

impl SourceElement for FunctionParameterSyntaxTree {
    fn span(&self) -> Span {
        Span {
            start: self.type_binding_specifier.span().start,
            end: self.identifier.span.end,
        }
    }
}

/// Represents a list of function parameters separated by commas.
///
/// Syntax Synopsis:
/// ``` text
/// FunctionParameterList:
///     FunctionParameter (',' FunctionParameter)*
///     ;
/// ```
pub type FunctionParameterListSyntaxTree = ConnectedList<FunctionParameterSyntaxTree, Punctuation>;

/// Represents a syntax tree node for a function.
///
/// Syntax Synopsis:
/// ``` text
/// Function:
///     AccessModifier TypeSpecifier Identifier '(' FunctionParameterList? ')' BlockWithoutLabel
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Function {
    pub access_modifier: AccessModifier,
    pub type_specifier: TypeSpecifier,
    pub identifier: Identifier,
    pub left_paren: Punctuation,
    pub parameters: Option<FunctionParameterListSyntaxTree>,
    pub right_paren: Punctuation,
    pub block_without_label: BlockWithoutLabel,
}

impl SourceElement for Function {
    fn span(&self) -> Span {
        Span {
            start: self.access_modifier.span().start,
            end: self.block_without_label.span().end,
        }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Module {
    pub access_modifier: AccessModifier,
    pub module_keyword: Keyword,
    pub identifier: Identifier,
    pub semicolon: Punctuation,
}

impl SourceElement for Module {
    fn span(&self) -> Span {
        Span {
            start: self.access_modifier.span().start,
            end: self.semicolon.span.end,
        }
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
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Item {
    Struct(Struct),
    Enum(Enum),
    Function(Function),
    Module(Module),
}

impl SourceElement for Item {
    fn span(&self) -> Span {
        match self {
            Self::Struct(item) => item.span(),
            Self::Enum(item) => item.span(),
            Self::Function(item) => item.span(),
            Self::Module(item) => item.span(),
        }
    }
}

impl<'a> Parser<'a> {
    /// Parses an [Item].
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
                self.handle_struct(access_modifier, *struct_keyword)
            }
            // Handles enum
            Some(Token::Keyword(enum_keyword)) if enum_keyword.keyword == KeywordKind::Enum => {
                // eat token
                self.next_token();

                let identifier = self.expect_identifier()?;

                let left_brace = self.expect_punctuation('{')?;

                let (variants, right_brace) =
                    self.parse_enclosed_list('}', ',', |parse| parse.expect_identifier().copied())?;

                Some(
                    Enum {
                        access_modifier,
                        enum_keyword: *enum_keyword,
                        identifier: *identifier,
                        left_brace: *left_brace,
                        variants,
                        right_brace,
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
                        module_keyword: *module_keyword,
                        identifier: *identifier,
                        semicolon: *semicolon,
                    }
                    .into(),
                )
            }
            _ => self.handle_function(access_modifier),
        }
    }

    fn handle_function(&mut self, access_modifier: AccessModifier) -> Option<Item> {
        let peeked_token = self.peek_significant_token().copied();
        if matches!(&peeked_token,
            Some(Token::Keyword(keyword)) if
                keyword.keyword == KeywordKind::Bool ||
                keyword.keyword == KeywordKind::Void ||
                keyword.keyword == KeywordKind::Float32 ||
                keyword.keyword == KeywordKind::Float64 ||
                keyword.keyword == KeywordKind::Int8 ||
                keyword.keyword == KeywordKind::Int16 ||
                keyword.keyword == KeywordKind::Int32 ||
                keyword.keyword == KeywordKind::Int64 ||
                keyword.keyword == KeywordKind::Uint8 ||
                keyword.keyword == KeywordKind::Uint16 ||
                keyword.keyword == KeywordKind::Uint32 ||
                keyword.keyword == KeywordKind::Uint64
        ) || matches!(&peeked_token, Some(Token::Identifier(..)))
        {
            // return type
            let type_specifier = self.parse_type_specifier()?;

            // function name
            let identifier = self.expect_identifier()?;

            // left parenthesis
            let left_paren = self.expect_punctuation('(')?;

            let (parameters, right_paren) = self.parse_enclosed_list(')', ',', |parser| {
                let mutable_keyword = match parser.peek_significant_token() {
                    Some(Token::Keyword(keyword)) if keyword.keyword == KeywordKind::Mutable => {
                        // eat mutable keyword
                        parser.next_token();

                        Some(*keyword)
                    }
                    _ => None,
                };
                let type_specifier = parser.parse_type_specifier()?;
                let identifier = parser.expect_identifier()?;

                Some(FunctionParameterSyntaxTree {
                    type_binding_specifier: TypeBindingSpecifier {
                        mutable_keyword,
                        type_specifier,
                    },
                    identifier: *identifier,
                })
            })?;

            let block = self.parse_block_without_label()?;

            Some(
                Function {
                    access_modifier,
                    type_specifier,
                    identifier: *identifier,
                    left_paren: *left_paren,
                    parameters,
                    right_paren,
                    block_without_label: block,
                }
                .into(),
            )
        } else {
            // make progress
            self.report_error(
                ItemExpected {
                    found: peeked_token,
                }
                .into(),
            );
            None
        }
    }

    fn handle_struct(
        &mut self,
        access_modifier: AccessModifier,
        struct_keyword: Keyword,
    ) -> Option<Item> {
        let identifier = self.expect_identifier()?;
        let left_brace = self.expect_punctuation('{')?;
        let mut field_groups = Vec::new();
        let right_brace = loop {
            match self.next_significant_token() {
                Some(Token::Punctuation(right_brace)) if right_brace.punctuation == '}' => {
                    // eat right brace
                    self.next_token();

                    break right_brace;
                }
                Some(Token::Keyword(access_modifier))
                    if access_modifier.keyword == KeywordKind::Public
                        || access_modifier.keyword == KeywordKind::Private
                        || access_modifier.keyword == KeywordKind::Internal =>
                {
                    // parse access modifier
                    let access_modifier = match access_modifier.keyword {
                        KeywordKind::Public => AccessModifier::Public(*access_modifier),
                        KeywordKind::Private => AccessModifier::Private(*access_modifier),
                        KeywordKind::Internal => AccessModifier::Internal(*access_modifier),
                        _ => unreachable!(),
                    };

                    // expect colon
                    let colon = self.expect_punctuation(':')?;

                    let mut fields = Vec::new();

                    loop {
                        match self.peek_significant_token() {
                            Some(Token::Punctuation(punctuation))
                                if punctuation.punctuation == '}' =>
                            {
                                break;
                            }
                            Some(Token::Keyword(access_mod_keyword))
                                if access_mod_keyword.keyword == KeywordKind::Public
                                    || access_mod_keyword.keyword == KeywordKind::Private
                                    || access_mod_keyword.keyword == KeywordKind::Internal =>
                            {
                                break;
                            }
                            _ => {
                                let type_specifier = self.parse_type_specifier()?;
                                let identifier = self.expect_identifier()?;
                                let semicolon = self.expect_punctuation(';')?;

                                fields.push(Field {
                                    type_specifier,
                                    identifier: *identifier,
                                    semicolon: *semicolon,
                                });
                            }
                        }
                    }

                    field_groups.push(FieldGroup {
                        access_modifier,
                        colon: *colon,
                        fields,
                    });
                }
                found => {
                    self.report_error(
                        FieldGroupExpected {
                            found: found.copied(),
                        }
                        .into(),
                    );
                }
            }
        };

        Some(
            Struct {
                access_modifier,
                struct_keyword,
                identifier: *identifier,
                left_brace: *left_brace,
                field_groups,
                right_brace: *right_brace,
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
                Some(AccessModifier::Public(*public_keyword))
            }
            Some(Token::Keyword(private_keyword))
                if private_keyword.keyword == KeywordKind::Private =>
            {
                Some(AccessModifier::Private(*private_keyword))
            }
            Some(Token::Keyword(internal_keyword))
                if internal_keyword.keyword == KeywordKind::Internal =>
            {
                Some(AccessModifier::Internal(*internal_keyword))
            }
            found => {
                self.report_error(
                    AccessModifierExpected {
                        found: found.copied(),
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
