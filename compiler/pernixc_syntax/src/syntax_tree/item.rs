use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use pernixc_lexical::token::{IdentifierToken, Keyword, KeywordToken, PunctuationToken, Token};

use super::{
    expression::ExpressionSyntaxTree, ConnectedList, SyntaxTree, TypeBindingSyntaxTree,
    TypeSpecifierSyntaxTree,
};
use crate::{errors::SyntacticError, parser::Parser};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum AccessModifierSyntaxTree {
    Public(KeywordToken),
    Private(KeywordToken),
    Internal(KeywordToken),
}

impl SyntaxTree for AccessModifierSyntaxTree {
    fn span(&self) -> Span {
        match self {
            AccessModifierSyntaxTree::Public(keyword) => keyword.span,
            AccessModifierSyntaxTree::Private(keyword) => keyword.span,
            AccessModifierSyntaxTree::Internal(keyword) => keyword.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldSyntaxTree {
    pub type_specifier: TypeSpecifierSyntaxTree,
    pub identifier:     IdentifierToken,
    pub semicolon:      PunctuationToken,
}

impl SyntaxTree for FieldSyntaxTree {
    fn span(&self) -> Span { Span::new(self.type_specifier.span().start, self.semicolon.span.end) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldGroupSyntaxTree {
    pub access_modifier: AccessModifierSyntaxTree,
    pub colon:           PunctuationToken,
    pub fields:          Vec<FieldSyntaxTree>,
}

impl SyntaxTree for FieldGroupSyntaxTree {
    fn span(&self) -> Span {
        Span::new(
            self.access_modifier.span().start,
            self.fields.last().unwrap().span().end,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructSyntaxTree {
    pub access_modifier: AccessModifierSyntaxTree,
    pub struct_keyword:  KeywordToken,
    pub identifier:      IdentifierToken,
    pub left_brace:      PunctuationToken,
    pub field_groups:    Vec<FieldGroupSyntaxTree>,
    pub right_brace:     PunctuationToken,
}

impl SyntaxTree for StructSyntaxTree {
    fn span(&self) -> Span {
        Span::new(self.access_modifier.span().start, self.right_brace.span.end)
    }
}

pub type EnumVariantListSyntaxTree = ConnectedList<IdentifierToken, PunctuationToken>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumSyntaxTree {
    pub access_modifier: AccessModifierSyntaxTree,
    pub enum_keyword:    KeywordToken,
    pub identifier:      IdentifierToken,
    pub left_brace:      PunctuationToken,
    pub variants:        Option<EnumVariantListSyntaxTree>,
    pub right_brace:     PunctuationToken,
}

impl SyntaxTree for EnumSyntaxTree {
    fn span(&self) -> Span {
        Span::new(self.access_modifier.span().start, self.right_brace.span.end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionParameterSyntaxTree {
    pub type_binding: TypeBindingSyntaxTree,
    pub identifier:   IdentifierToken,
}

impl SyntaxTree for FunctionParameterSyntaxTree {
    fn span(&self) -> Span { Span::new(self.type_binding.span().start, self.identifier.span.end) }
}

pub type FunctionParameterListSyntaxTree =
    ConnectedList<FunctionParameterSyntaxTree, PunctuationToken>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionSyntaxTree {
    pub access_modifier: AccessModifierSyntaxTree,
    pub type_specifier:  TypeSpecifierSyntaxTree,
    pub identifier:      IdentifierToken,
    pub left_paren:      PunctuationToken,
    pub parameters:      Option<FunctionParameterListSyntaxTree>,
    pub right_paren:     PunctuationToken,
    pub expression:      ExpressionSyntaxTree,
}

impl SyntaxTree for FunctionSyntaxTree {
    fn span(&self) -> Span {
        Span::new(self.type_specifier.span().start, self.expression.span().end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleSyntaxTree {
    pub access_modifier: AccessModifierSyntaxTree,
    pub module_keyword:  KeywordToken,
    pub identifier:      IdentifierToken,
    pub semicolon:       PunctuationToken,
}

impl SyntaxTree for ModuleSyntaxTree {
    fn span(&self) -> Span { Span::new(self.access_modifier.span().start, self.semicolon.span.end) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ItemSyntaxTree {
    Struct(StructSyntaxTree),
    Enum(EnumSyntaxTree),
    Function(FunctionSyntaxTree),
    Module(ModuleSyntaxTree),
}

impl SyntaxTree for ItemSyntaxTree {
    fn span(&self) -> Span {
        match self {
            ItemSyntaxTree::Struct(item) => item.span(),
            ItemSyntaxTree::Enum(item) => item.span(),
            ItemSyntaxTree::Function(item) => item.span(),
            ItemSyntaxTree::Module(item) => item.span(),
        }
    }
}

impl<'a> Parser<'a> {
    /// Parses an [ItemSyntaxTree].
    pub fn parse_item(&mut self) -> Option<ItemSyntaxTree> {
        // expect access modifier keyword token
        let access_modifier = self.parse_access_modifier()?;

        match self.peek_significant_token() {
            // Handles struct item
            Some(Token::Keyword(struct_keyword)) if struct_keyword.keyword == Keyword::Struct => {
                // eat struct keyword
                self.next_token();

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
                            if access_modifier.keyword == Keyword::Public
                                || access_modifier.keyword == Keyword::Private
                                || access_modifier.keyword == Keyword::Internal =>
                        {
                            // parse access modifier
                            let access_modifier = match access_modifier.keyword {
                                Keyword::Public => {
                                    AccessModifierSyntaxTree::Public(access_modifier.clone())
                                }
                                Keyword::Private => {
                                    AccessModifierSyntaxTree::Private(access_modifier.clone())
                                }
                                Keyword::Internal => {
                                    AccessModifierSyntaxTree::Internal(access_modifier.clone())
                                }
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
                                        if access_mod_keyword.keyword == Keyword::Public
                                            || access_mod_keyword.keyword == Keyword::Private
                                            || access_mod_keyword.keyword == Keyword::Internal =>
                                    {
                                        break;
                                    }
                                    _ => {
                                        let type_specifier = self.parse_type_specifier()?;
                                        let identifier = self.expect_identifier()?;
                                        let semicolon = self.expect_punctuation(';')?;

                                        fields.push(FieldSyntaxTree {
                                            type_specifier,
                                            identifier: identifier.clone(),
                                            semicolon: semicolon.clone(),
                                        });
                                    }
                                }
                            }

                            field_groups.push(FieldGroupSyntaxTree {
                                access_modifier,
                                colon: colon.clone(),
                                fields,
                            });
                        }
                        found => {
                            self.report_error(SyntacticError::FieldGroupExpected(found.cloned()));
                        }
                    }
                };

                Some(ItemSyntaxTree::Struct(StructSyntaxTree {
                    access_modifier,
                    struct_keyword: struct_keyword.clone(),
                    identifier: identifier.clone(),
                    left_brace: left_brace.clone(),
                    field_groups,
                    right_brace: right_brace.clone(),
                }))
            }
            // Handles enum
            Some(Token::Keyword(enum_keyword)) if enum_keyword.keyword == Keyword::Enum => {
                // eat token
                self.next_token();

                let identifier = self.expect_identifier()?;

                let left_brace = self.expect_punctuation('{')?;

                let (variants, right_brace) =
                    self.parse_enclosed_list('}', ',', |parse| parse.expect_identifier().cloned())?;

                Some(ItemSyntaxTree::Enum(EnumSyntaxTree {
                    access_modifier,
                    enum_keyword: enum_keyword.clone(),
                    identifier: identifier.clone(),
                    left_brace: left_brace.clone(),
                    variants,
                    right_brace,
                }))
            }
            // Handles module
            Some(Token::Keyword(module_keyword)) if module_keyword.keyword == Keyword::Module => {
                // eat token
                self.next_token();

                let identifier = self.expect_identifier()?;
                let semicolon = self.expect_punctuation(';')?;

                Some(ItemSyntaxTree::Module(ModuleSyntaxTree {
                    access_modifier,
                    module_keyword: module_keyword.clone(),
                    identifier: identifier.clone(),
                    semicolon: semicolon.clone(),
                }))
            }
            found => {
                let peeked_token = self.peek_significant_token().cloned();
                if matches!(&peeked_token,
                    Some(Token::Keyword(keyword)) if
                        keyword.keyword == Keyword::Bool ||
                        keyword.keyword == Keyword::Void ||
                        keyword.keyword == Keyword::Float32 ||
                        keyword.keyword == Keyword::Float64 ||
                        keyword.keyword == Keyword::Int8 ||
                        keyword.keyword == Keyword::Int16 ||
                        keyword.keyword == Keyword::Int32 ||
                        keyword.keyword == Keyword::Int64 ||
                        keyword.keyword == Keyword::Uint8 ||
                        keyword.keyword == Keyword::Uint16 ||
                        keyword.keyword == Keyword::Uint32 ||
                        keyword.keyword == Keyword::Uint64
                ) || matches!(&peeked_token, Some(Token::Identifier(_)))
                {
                    // return type
                    let type_specifier = self.parse_type_specifier()?;

                    // function name
                    let identifier = self.expect_identifier()?;

                    // left parenthesis
                    let left_paren = self.expect_punctuation('(')?;

                    let (parameters, right_paren) =
                        self.parse_enclosed_list(')', ',', |parser| {
                            let mutable_keyword = match parser.peek_significant_token() {
                                Some(Token::Keyword(mutable_keyword))
                                    if mutable_keyword.keyword == Keyword::Mutable =>
                                {
                                    // eat mutable keyword
                                    parser.next_token();

                                    Some(mutable_keyword.clone())
                                }
                                _ => None,
                            };

                            let type_specifier = parser.parse_type_specifier()?;

                            let identifier = parser.expect_identifier()?;

                            Some(FunctionParameterSyntaxTree {
                                type_binding: TypeBindingSyntaxTree {
                                    mutable_keyword,
                                    type_specifier,
                                },
                                identifier:   identifier.clone(),
                            })
                        })?;

                    let expression = self.parse_expression()?;

                    Some(ItemSyntaxTree::Function(FunctionSyntaxTree {
                        access_modifier,
                        type_specifier,
                        identifier: identifier.clone(),
                        left_paren: left_paren.clone(),
                        parameters,
                        right_paren,
                        expression,
                    }))
                } else {
                    // make progress
                    self.report_error(SyntacticError::ItemExpected(found.cloned()));
                    None
                }
            }
        }
    }

    /// Parses an [AccessModifierSyntaxTree]
    pub fn parse_access_modifier(&mut self) -> Option<AccessModifierSyntaxTree> {
        match self.next_significant_token() {
            Some(Token::Keyword(public_keyword)) if public_keyword.keyword == Keyword::Public => {
                Some(AccessModifierSyntaxTree::Public(public_keyword.clone()))
            }
            Some(Token::Keyword(private_keyword))
                if private_keyword.keyword == Keyword::Private =>
            {
                Some(AccessModifierSyntaxTree::Private(private_keyword.clone()))
            }
            Some(Token::Keyword(internal_keyword))
                if internal_keyword.keyword == Keyword::Internal =>
            {
                Some(AccessModifierSyntaxTree::Internal(internal_keyword.clone()))
            }
            found => {
                self.report_error(SyntacticError::AccessModifierExpected(found.cloned()));
                None
            }
        }
    }
}

#[cfg(test)]
mod tests;
