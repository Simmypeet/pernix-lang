use std::fmt::Debug;

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};
use pernixc_source::{SourceElement, Span};
use pernixc_system::diagnostic::Handler;

use super::{
    expression::{self, BooleanLiteral, NumericLiteral},
    ConnectedList,
};
use crate::{
    error::{self, PatternExpected},
    parser::Parser,
};

/// Syntax Synopsis:
/// ``` txt
/// FieldAssociation:
///     Identifier ':' Pattern
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
pub struct FieldAssociation<Pattern> {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    pattern: Box<Pattern>,
}

impl<Pattern: SourceElement> SourceElement for FieldAssociation<Pattern> {
    fn span(&self) -> Span { self.identifier.span().join(&self.pattern().span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// Field:
///     FieldAssociation
///     | Named
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner)]
pub enum Field<Pattern> {
    Association(FieldAssociation<Pattern>),
    Named(Named),
}

impl<Pattern: SourceElement> SourceElement for Field<Pattern> {
    fn span(&self) -> Span {
        match self {
            Self::Association(field_with_association) => field_with_association.span(),
            Self::Named(field_without_association) => field_without_association.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Structural:
///     '{' (Field (',' Field)* ','?)? '}'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
pub struct Structural<Pattern> {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    fields: Option<ConnectedList<Field<Pattern>, Punctuation>>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl<Pattern> SourceElement for Structural<Pattern> {
    fn span(&self) -> Span { self.left_brace.span.join(&self.right_brace.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// Enum:
///     Identifier '(' Pattern ')'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
pub struct Enum<Pattern> {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    pattern: Box<Pattern>,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl<Pattern> SourceElement for Enum<Pattern> {
    fn span(&self) -> Span { self.identifier.span().join(&self.right_paren.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// Unpack:
///     '...' 'mutable'? Identifier
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
pub struct Unpack {
    #[get = "pub"]
    ellipsis: (Punctuation, Punctuation, Punctuation),
    #[get = "pub"]
    mutable_keyword: Option<Keyword>,
    #[get = "pub"]
    identifier: Identifier,
}

impl SourceElement for Unpack {
    fn span(&self) -> Span { self.ellipsis.0.span.join(&self.identifier.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// Unpackable:
///     Unpack
///     | Pattern
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner)]
pub enum Unpackable<Pattern> {
    Unpack(Unpack),
    Pattern(Box<Pattern>),
}

impl<Pattern: SourceElement> SourceElement for Unpackable<Pattern> {
    fn span(&self) -> Span {
        match self {
            Self::Unpack(unpack) => unpack.span(),
            Self::Pattern(pattern) => pattern.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Tuple:
///     '(' (Unpackable (',' Unpackable)* ','?)? ')'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
pub struct Tuple<Pattern> {
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    patterns: Option<ConnectedList<Unpackable<Pattern>, Punctuation>>,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl<Pattern> SourceElement for Tuple<Pattern> {
    fn span(&self) -> Span { self.left_paren.span.join(&self.right_paren.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// Named:
///     'mutable'? Identifier
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
pub struct Named {
    #[get = "pub"]
    mutable_keyword: Option<Keyword>,
    #[get = "pub"]
    identifier: Identifier,
}

impl SourceElement for Named {
    fn span(&self) -> Span {
        self.mutable_keyword.as_ref().map_or_else(
            || self.identifier.span(),
            |keyword| keyword.span().join(&self.identifier.span()).unwrap(),
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Refutable:
///     BooleanLiteral
///     | NumericLiteral
///     | Structural
///     | Enum
///     | Named
///     | Tuple
/// ```
#[derive(Debug, Clone, EnumAsInner, derive_more::From)]
pub enum Refutable {
    BooleanLiteral(BooleanLiteral),
    NumericLiteral(NumericLiteral),
    Structural(Structural<Self>),
    Enum(Enum<Self>),
    Named(Named),
    Tuple(Tuple<Self>),
}

impl SourceElement for Refutable {
    fn span(&self) -> Span {
        match self {
            Self::BooleanLiteral(boolean_literal) => boolean_literal.span(),
            Self::NumericLiteral(numeric_literal) => numeric_literal.span(),
            Self::Structural(structural) => structural.span(),
            Self::Enum(associated_enum) => associated_enum.span(),
            Self::Named(identifier) => identifier.span(),
            Self::Tuple(tuple_pattern) => tuple_pattern.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Irrefutable:
///     Structural
///     | Enum
///     | Named
///     | Tuple
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner, derive_more::From)]
pub enum Irrefutable {
    Structural(Structural<Self>),
    Named(Named),
    Tuple(Tuple<Self>),
}

impl SourceElement for Irrefutable {
    fn span(&self) -> Span {
        match self {
            Self::Structural(structural) => structural.span(),
            Self::Named(identifier) => identifier.span(),
            Self::Tuple(tuple_pattern) => tuple_pattern.span(),
        }
    }
}

trait Pattern {
    fn parse(parser: &mut Parser, handler: &impl Handler<error::Error>) -> Option<Self>
    where
        Self: Sized;
}

impl<'a> Parser<'a> {
    fn parse_structural_pattern<T: Pattern + Debug>(
        &mut self,
        handler: &impl Handler<error::Error>,
    ) -> Option<Structural<T>> {
        let enclosed_tree = self.parse_enclosed_list(
            Delimiter::Brace,
            ',',
            |parser, handler| {
                let mutable_keyword = match parser.stop_at_significant() {
                    Some(Token::Keyword(keyword)) if keyword.keyword == KeywordKind::Mutable => {
                        parser.forward();
                        Some(keyword)
                    }
                    _ => None,
                };

                let identifier = parser.parse_identifier(handler)?;

                match (mutable_keyword.is_none(), parser.stop_at_significant()) {
                    (true, Some(Token::Punctuation(colon))) if colon.punctuation == ':' => {
                        // eat colon
                        parser.forward();

                        let pattern = T::parse(parser, handler).map(Box::new)?;

                        Some(Field::Association(FieldAssociation {
                            identifier,
                            colon,
                            pattern,
                        }))
                    }
                    _ => Some(Field::Named(Named {
                        mutable_keyword,
                        identifier,
                    })),
                }
            },
            handler,
        )?;

        Some(Structural {
            left_brace: enclosed_tree.open,
            fields: enclosed_tree.list,
            right_brace: enclosed_tree.close,
        })
    }

    fn parse_tuple_pattern<T: Pattern + Debug>(
        &mut self,
        handler: &impl Handler<error::Error>,
    ) -> Option<Tuple<T>> {
        let enclosed_tree = self.parse_enclosed_list(
            Delimiter::Parenthesis,
            ',',
            |parser, handler| {
                parser.stop_at_significant();

                match (parser.peek(), parser.peek_offset(1), parser.peek_offset(2)) {
                    (
                        Some(Token::Punctuation(
                            p1 @ Punctuation {
                                punctuation: '.', ..
                            },
                        )),
                        Some(Token::Punctuation(
                            p2 @ Punctuation {
                                punctuation: '.', ..
                            },
                        )),
                        Some(Token::Punctuation(
                            p3 @ Punctuation {
                                punctuation: '.', ..
                            },
                        )),
                    ) => {
                        let ellipsis = (p1, p2, p3);

                        parser.forward();
                        parser.forward();
                        parser.forward();

                        let mutable_keyword =
                            parser.stop_at_significant().and_then(|token| match token {
                                Token::Keyword(keyword)
                                    if keyword.keyword == KeywordKind::Mutable =>
                                {
                                    parser.forward();
                                    Some(keyword)
                                }
                                _ => None,
                            });

                        let identifier = parser.parse_identifier(handler)?;

                        Some(Unpackable::Unpack(Unpack {
                            ellipsis,
                            mutable_keyword,
                            identifier,
                        }))
                    }

                    _ => T::parse(parser, handler)
                        .map(|pattern| Unpackable::Pattern(Box::new(pattern))),
                }
            },
            handler,
        )?;

        Some(Tuple {
            left_paren: enclosed_tree.open,
            patterns: enclosed_tree.list,
            right_paren: enclosed_tree.close,
        })
    }

    fn parse_identifier_pattern<T>(&mut self, handler: &impl Handler<error::Error>) -> Option<T>
    where
        T: Pattern + From<Named> + From<Enum<T>>,
    {
        let identifier = self.parse_identifier(handler)?;

        match self.stop_at_significant() {
            // parse enum pattern
            Some(Token::Punctuation(left_paren)) if left_paren.punctuation == '(' => {
                let left_paren = self.step_into(Delimiter::Parenthesis, handler)?;
                let pattern = T::parse(self, handler).map(Box::new);
                let right_paren = self.step_out(handler)?;

                Some(
                    Enum {
                        identifier,
                        left_paren,
                        pattern: pattern?,
                        right_paren,
                    }
                    .into(),
                )
            }
            // parse named pattern
            _ => Some(
                Named {
                    mutable_keyword: None,
                    identifier,
                }
                .into(),
            ),
        }
    }
}

impl Pattern for Irrefutable {
    fn parse(parser: &mut Parser, handler: &impl Handler<error::Error>) -> Option<Self> {
        match parser.stop_at_significant() {
            // parse named pattern
            Some(Token::Keyword(mutable_keyword))
                if mutable_keyword.keyword == KeywordKind::Mutable =>
            {
                // eat the mutable keyword
                parser.forward();

                let identifier = parser.parse_identifier(handler)?;

                Some(Self::Named(Named {
                    mutable_keyword: Some(mutable_keyword),
                    identifier,
                }))
            }

            // parse named pattern
            Some(Token::Identifier(identifier)) => {
                // eat identifier
                parser.forward();

                Some(Self::Named(Named {
                    mutable_keyword: None,
                    identifier,
                }))
            }

            // parse tuple pattern
            Some(Token::Punctuation(left_paren)) if left_paren.punctuation == '(' => {
                parser.parse_tuple_pattern(handler).map(Self::Tuple)
            }

            // parse structural pattern
            Some(Token::Punctuation(left_brace)) if left_brace.punctuation == '{' => parser
                .parse_structural_pattern(handler)
                .map(Self::Structural),

            found => {
                handler.receive(error::Error::PatternExpected(PatternExpected { found }));
                None
            }
        }
    }
}

impl Pattern for Refutable {
    fn parse(parser: &mut Parser, handler: &impl Handler<error::Error>) -> Option<Self> {
        match parser.stop_at_significant() {
            // parse named pattern
            Some(Token::Keyword(mutable_keyword))
                if mutable_keyword.keyword == KeywordKind::Mutable =>
            {
                // eat the mutable keyword
                parser.forward();

                let identifier = parser.parse_identifier(handler)?;

                Some(Self::Named(Named {
                    mutable_keyword: Some(mutable_keyword),
                    identifier,
                }))
            }

            // parse named pattern
            Some(Token::Identifier(..)) => parser.parse_identifier_pattern(handler),

            // parse tuple pattern
            Some(Token::Punctuation(left_paren)) if left_paren.punctuation == '(' => {
                parser.parse_tuple_pattern(handler).map(Self::Tuple)
            }

            // parse structural pattern
            Some(Token::Punctuation(left_brace)) if left_brace.punctuation == '{' => parser
                .parse_structural_pattern(handler)
                .map(Self::Structural),

            // parse numeric literal pattern
            Some(Token::NumericLiteral(numeric_literal_token)) => {
                parser.forward();
                Some(Self::NumericLiteral(expression::NumericLiteral {
                    numeric_literal_token,
                }))
            }

            // parse boolean literal pattern
            Some(Token::Keyword(boolean_keyword))
                if matches!(
                    boolean_keyword.keyword,
                    KeywordKind::True | KeywordKind::False
                ) =>
            {
                parser.forward();
                let constructor = match boolean_keyword.keyword {
                    KeywordKind::True => BooleanLiteral::True,
                    KeywordKind::False => BooleanLiteral::False,
                    _ => unreachable!(),
                };
                Some(Self::BooleanLiteral(constructor(boolean_keyword)))
            }

            found => {
                handler.receive(error::Error::PatternExpected(PatternExpected { found }));
                None
            }
        }
    }
}

impl<'a> Parser<'a> {
    /// Parses an [`Irrefutable`] pattern.
    pub fn parse_irrefutable_pattern(
        &mut self,
        handler: &impl Handler<error::Error>,
    ) -> Option<Irrefutable> {
        Irrefutable::parse(self, handler)
    }

    /// Parses a [`Refutable`] pattern.
    pub fn parse_refutable_pattern(
        &mut self,
        handler: &impl Handler<error::Error>,
    ) -> Option<Refutable> {
        Refutable::parse(self, handler)
    }
}

#[cfg(test)]
pub(super) mod tests;
