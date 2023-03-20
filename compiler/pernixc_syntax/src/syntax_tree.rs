use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use pernixc_lexical::token::{
    CharacterLiteralToken, IdentifierToken, Keyword, KeywordToken, NumericLiteralToken,
    PunctuationToken, StringLiteralToken, Token,
};

use crate::{errors::SyntacticError, parser::Parser};

pub mod expression;
pub mod item;
pub mod statement;

/// Is a trait that all syntax tree types must implement.
pub trait SyntaxTree {
    /// Returns a [`Span`] representing the location of the syntax tree node in the source code.
    fn span(&self) -> Span;
}

impl<T: SyntaxTree> SyntaxTree for Box<T> {
    fn span(&self) -> Span { self.as_ref().span() }
}

impl SyntaxTree for IdentifierToken {
    fn span(&self) -> Span { self.span }
}

impl SyntaxTree for KeywordToken {
    fn span(&self) -> Span { self.span }
}

impl SyntaxTree for PunctuationToken {
    fn span(&self) -> Span { self.span }
}

impl SyntaxTree for CharacterLiteralToken {
    fn span(&self) -> Span { self.span }
}

impl SyntaxTree for StringLiteralToken {
    fn span(&self) -> Span { self.span }
}

impl SyntaxTree for NumericLiteralToken {
    fn span(&self) -> Span { self.span }
}

/// Represents a syntax tree node with a pattern of syntax tree nodes separated by a separator.
///
/// This struct is useful for representing syntax tree nodes that are separated by a separator.
/// For example, a comma separated list of expressions such as `1, 2, 3` can be represented by a
/// [`ConnectedList`] with the separator being a comma token and the elements being the expressions.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConnectedList<Element, Separator> {
    /// The first element of the list.
    pub first: Element,

    /// The rest of the elements of the list.
    ///
    /// Each element of the list is a tuple containing the separator and the element. The separator
    /// is the token/syntax tree node that separates the current element from the prior one.
    pub rest: Vec<(Separator, Element)>,
}

impl<Element: SyntaxTree, Separator: SyntaxTree> SyntaxTree for ConnectedList<Element, Separator> {
    fn span(&self) -> Span {
        Span::new(
            self.first.span().start,
            self.rest
                .last()
                .map_or(self.first.span().end, |(_, element)| element.span().end),
        )
    }
}

impl<Element, Separator> ConnectedList<Element, Separator> {
    /// Returns an iterator over the elements of the list.
    pub fn element_iter(&self) -> impl Iterator<Item = &Element> {
        std::iter::once(&self.first).chain(self.rest.iter().map(|(_, element)| element))
    }
}

/// Represents a syntax tree node of two consecutive colon tokens.
///
/// This syntax tree is used to represent the scope separator `::` in the qualified identifier
/// syntax
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeSeparatorSyntaxTree(pub PunctuationToken, pub PunctuationToken);

impl SyntaxTree for ScopeSeparatorSyntaxTree {
    fn span(&self) -> Span { Span::new(self.0.span.start, self.1.span.end) }
}

/// Represents a syntax tree node of identifiers separated by scope separators.
///
/// Syntax Synopsis:
/// ``` txt
/// QualifiedIdentifierSyntaxTree:
///     IdentifierSyntaxTree ('::' IdentifierSyntaxTree)*
///     ;
/// ```
pub type QualifiedIdentifierSyntaxTree = ConnectedList<IdentifierToken, ScopeSeparatorSyntaxTree>;

/// Represents a syntax tree node of primitive type identifiers.
///
/// Syntax Synopsis:
/// ``` txt
/// PrimitiveTypeIdentifierSyntaxTree:
///     Bool
///     | Void
///     | Float32
///     | Float64
///     | Int8
///     | Int16
///     | Int32
///     | Int64
///     | Uint8
///     | Uint16
///     | Uint32
///     | Uint64
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum PrimitiveTypeSpecifierSyntaxTree {
    Bool(KeywordToken),
    Void(KeywordToken),
    Float32(KeywordToken),
    Float64(KeywordToken),
    Int8(KeywordToken),
    Int16(KeywordToken),
    Int32(KeywordToken),
    Int64(KeywordToken),
    Uint8(KeywordToken),
    Uint16(KeywordToken),
    Uint32(KeywordToken),
    Uint64(KeywordToken),
}

impl SyntaxTree for PrimitiveTypeSpecifierSyntaxTree {
    fn span(&self) -> Span {
        match self {
            Self::Bool(token) => token.span,
            Self::Void(token) => token.span,
            Self::Float32(token) => token.span,
            Self::Float64(token) => token.span,
            Self::Int8(token) => token.span,
            Self::Int16(token) => token.span,
            Self::Int32(token) => token.span,
            Self::Int64(token) => token.span,
            Self::Uint8(token) => token.span,
            Self::Uint16(token) => token.span,
            Self::Uint32(token) => token.span,
            Self::Uint64(token) => token.span,
        }
    }
}

/// Represents a syntax tree node of type specifier.
///
/// The type specifier is used to annotate the type of various symbols in the syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// TypeSpecifierSyntaxTree:
///     PrimitiveTypeIdentifierSyntaxTree
///     | QualifiedIdentifierSyntaxTree
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum TypeSpecifierSyntaxTree {
    Primitive(PrimitiveTypeSpecifierSyntaxTree),
    Qualified(QualifiedIdentifierSyntaxTree),
}

impl SyntaxTree for TypeSpecifierSyntaxTree {
    fn span(&self) -> Span {
        match self {
            Self::Primitive(primitive) => primitive.span(),
            Self::Qualified(qualified) => qualified.span(),
        }
    }
}

/// Is a syntax tree node that represents a label.
///
/// It's commonly used in labeling the loop statements.
///
/// Syntax Synopsis:
/// ``` txt
/// LabelSyntaxTree:
///     '\'' IdentifierSyntaxTree
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LabelSyntaxTree {
    pub single_quote: PunctuationToken,
    pub name:         IdentifierToken,
}

impl SyntaxTree for LabelSyntaxTree {
    fn span(&self) -> Span { Span::new(self.single_quote.span.start, self.name.span.end) }
}

/// Is a syntax tree node that represents a type binding.
///
/// It's commonly used to annotate the type of lvalues in the syntax tree.
///
/// Syntax Sypnosis:
/// ``` txt
/// TypeBindingSyntaxTree:
///     'mutable'? TypeSpecifierSyntaxTree
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeBindingSyntaxTree {
    pub mutable_keyword: Option<KeywordToken>,
    pub type_specifier:  TypeSpecifierSyntaxTree,
}

impl SyntaxTree for TypeBindingSyntaxTree {
    fn span(&self) -> Span {
        Span::new(
            self.mutable_keyword
                .as_ref()
                .map_or(self.type_specifier.span().start, |keyword| {
                    keyword.span.start
                }),
            self.type_specifier.span().end,
        )
    }
}

impl<'a> Parser<'a> {
    /// Parses a [QualifiedIdentifierSyntaxTree]
    pub fn parse_qualified_identifier(&mut self) -> Option<QualifiedIdentifierSyntaxTree> {
        // expect the first identifier
        let first_identifier = self.expect_identifier()?.clone();

        let mut rest = Vec::new();
        let mut cursor = self.cursor;

        // check if the next token is a scope separator '::'
        while let Some((first_colon, second_colon)) = {
            let first_colon = match self.next_significant_token() {
                Some(Token::Punctuation(first_colon)) => Some(first_colon),
                _ => None,
            };
            let second_colon = match self.next_token() {
                Some(Token::Punctuation(second_colon)) => Some(second_colon),
                _ => None,
            };

            // check if two consecutive colons are found
            if let Some(x) = first_colon.zip(second_colon) {
                Some(x)
            } else {
                self.cursor = cursor;
                None
            }
        } {
            let scope_separator =
                ScopeSeparatorSyntaxTree(first_colon.clone(), second_colon.clone());

            // must be followed by an identifier
            let identifier = self.expect_identifier()?;

            rest.push((scope_separator, identifier.clone()));

            cursor = self.cursor;
        }

        Some(QualifiedIdentifierSyntaxTree {
            first: first_identifier,
            rest,
        })
    }

    /// Parses a [TypeSpecifierSyntaxTree]
    pub fn parse_type_specifier(&mut self) -> Option<TypeSpecifierSyntaxTree> {
        match self.peek_significant_token() {
            Some(token) => match token {
                Token::Identifier(_) => Some(TypeSpecifierSyntaxTree::Qualified(
                    self.parse_qualified_identifier()?,
                )),
                Token::Keyword(keyword) => {
                    // eat the token right away
                    self.next_token();

                    let primitive_type = match keyword.keyword {
                        Keyword::Bool => PrimitiveTypeSpecifierSyntaxTree::Bool(keyword.clone()),
                        Keyword::Void => PrimitiveTypeSpecifierSyntaxTree::Void(keyword.clone()),
                        Keyword::Float32 => {
                            PrimitiveTypeSpecifierSyntaxTree::Float32(keyword.clone())
                        }
                        Keyword::Float64 => {
                            PrimitiveTypeSpecifierSyntaxTree::Float64(keyword.clone())
                        }
                        Keyword::Int8 => PrimitiveTypeSpecifierSyntaxTree::Int8(keyword.clone()),
                        Keyword::Int16 => PrimitiveTypeSpecifierSyntaxTree::Int16(keyword.clone()),
                        Keyword::Int32 => PrimitiveTypeSpecifierSyntaxTree::Int32(keyword.clone()),
                        Keyword::Int64 => PrimitiveTypeSpecifierSyntaxTree::Int64(keyword.clone()),
                        Keyword::Uint8 => PrimitiveTypeSpecifierSyntaxTree::Uint8(keyword.clone()),
                        Keyword::Uint16 => {
                            PrimitiveTypeSpecifierSyntaxTree::Uint16(keyword.clone())
                        }
                        Keyword::Uint32 => {
                            PrimitiveTypeSpecifierSyntaxTree::Uint32(keyword.clone())
                        }
                        Keyword::Uint64 => {
                            PrimitiveTypeSpecifierSyntaxTree::Uint64(keyword.clone())
                        }
                        _ => return None,
                    };

                    Some(TypeSpecifierSyntaxTree::Primitive(primitive_type))
                }
                token => {
                    // eat the token, make progress
                    self.next_token();

                    self.report_error(SyntacticError::TypeSpecifierExpected(Some(token.clone())));
                    None
                }
            },
            None => {
                // eat the token, make progress
                self.next_token();

                self.report_error(SyntacticError::TypeSpecifierExpected(None));
                None
            }
        }
    }
}

#[cfg(test)]
mod tests;
