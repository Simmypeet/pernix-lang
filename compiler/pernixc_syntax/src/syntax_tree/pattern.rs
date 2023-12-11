use std::fmt::Debug;

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};

use super::{
    expression::{BooleanLiteral, NumericLiteral},
    ConnectedList,
};
use crate::{
    error::{self, Error, SyntaxKind},
    parser::{Parser, Reading},
};

/// Syntax Synopsis:
/// ``` txt
/// FieldAssociation:
///     Identifier ':' Pattern
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From)]
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
    fn parse(parser: &mut Parser, handler: &dyn Handler<error::Error>) -> Option<Self>
    where
        Self: Sized;
}

impl<'a> Parser<'a> {
    fn parse_structural_pattern<T: Pattern + Debug>(
        &mut self,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Structural<T>> {
        let enclosed_tree = self.parse_enclosed_list(
            Delimiter::Brace,
            ',',
            |parser| {
                let mutable_keyword = match parser.stop_at_significant() {
                    Reading::Unit(Token::Keyword(keyword))
                        if keyword.kind == KeywordKind::Mutable =>
                    {
                        parser.forward();
                        Some(keyword)
                    }
                    _ => None,
                };

                let identifier = parser.parse_identifier(handler)?;

                match (mutable_keyword.is_none(), parser.stop_at_significant()) {
                    (true, Reading::Unit(Token::Punctuation(colon)))
                        if colon.punctuation == ':' =>
                    {
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
        handler: &dyn Handler<error::Error>,
    ) -> Option<Tuple<T>> {
        let enclosed_tree = self.parse_enclosed_list(
            Delimiter::Parenthesis,
            ',',
            |parser| {
                parser.stop_at_significant();

                match (parser.peek(), parser.peek_offset(1), parser.peek_offset(2)) {
                    (
                        Reading::Unit(Token::Punctuation(
                            p1 @ Punctuation {
                                punctuation: '.', ..
                            },
                        )),
                        Some(Reading::Unit(Token::Punctuation(
                            p2 @ Punctuation {
                                punctuation: '.', ..
                            },
                        ))),
                        Some(Reading::Unit(Token::Punctuation(
                            p3 @ Punctuation {
                                punctuation: '.', ..
                            },
                        ))),
                    ) => {
                        let ellipsis = (p1, p2, p3);

                        parser.forward();
                        parser.forward();
                        parser.forward();

                        let mutable_keyword = parser
                            .stop_at_significant()
                            .into_unit()
                            .ok()
                            .and_then(|token| match token {
                                Token::Keyword(keyword) if keyword.kind == KeywordKind::Mutable => {
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

    fn parse_identifier_pattern<T>(&mut self, handler: &dyn Handler<error::Error>) -> Option<T>
    where
        T: Pattern + From<Named> + From<Enum<T>>,
    {
        let identifier = self.parse_identifier(handler)?;

        match self.stop_at_significant() {
            // parse enum pattern
            Reading::IntoDelimited(Delimiter::Parenthesis, _) => {
                let delimited_tree = self.step_into(
                    Delimiter::Parenthesis,
                    |parser| T::parse(parser, handler).map(Box::new),
                    handler,
                )?;

                Some(
                    Enum {
                        identifier,
                        left_paren: delimited_tree.open,
                        pattern: delimited_tree.tree?,
                        right_paren: delimited_tree.close,
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
    fn parse(parser: &mut Parser, handler: &dyn Handler<error::Error>) -> Option<Self> {
        match parser.stop_at_significant() {
            // parse named pattern
            Reading::Unit(Token::Keyword(mutable_keyword))
                if mutable_keyword.kind == KeywordKind::Mutable =>
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
            Reading::Unit(Token::Identifier(identifier)) => {
                // eat identifier
                parser.forward();

                Some(Self::Named(Named {
                    mutable_keyword: None,
                    identifier,
                }))
            }

            // parse tuple pattern
            Reading::IntoDelimited(Delimiter::Parenthesis, _) => {
                parser.parse_tuple_pattern(handler).map(Self::Tuple)
            }

            // parse structural pattern
            Reading::IntoDelimited(Delimiter::Brace, _) => parser
                .parse_structural_pattern(handler)
                .map(Self::Structural),

            found => {
                handler.receive(Error {
                    expected: SyntaxKind::IrrefutablePattern,
                    alternatives: Vec::new(),
                    found: found.into_token(),
                });

                // make progress
                parser.forward();

                None
            }
        }
    }
}

impl Pattern for Refutable {
    fn parse(parser: &mut Parser, handler: &dyn Handler<error::Error>) -> Option<Self> {
        match parser.stop_at_significant() {
            // parse named pattern
            Reading::Unit(Token::Keyword(mutable_keyword))
                if mutable_keyword.kind == KeywordKind::Mutable =>
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
            Reading::Unit(Token::Identifier(..)) => parser.parse_identifier_pattern(handler),

            // parse tuple pattern
            Reading::IntoDelimited(Delimiter::Parenthesis, _) => {
                parser.parse_tuple_pattern(handler).map(Self::Tuple)
            }

            // parse structural pattern
            Reading::IntoDelimited(Delimiter::Brace, _) => parser
                .parse_structural_pattern(handler)
                .map(Self::Structural),

            // parse numeric literal pattern
            Reading::Unit(Token::Numeric(_)) => {
                Some(Self::NumericLiteral(parser.parse_numeric_literal()?))
            }

            // parse boolean literal pattern
            Reading::Unit(Token::Keyword(boolean_keyword))
                if matches!(boolean_keyword.kind, KeywordKind::True | KeywordKind::False) =>
            {
                parser.forward();
                let constructor = match boolean_keyword.kind {
                    KeywordKind::True => BooleanLiteral::True,
                    KeywordKind::False => BooleanLiteral::False,
                    _ => unreachable!(),
                };
                Some(Self::BooleanLiteral(constructor(boolean_keyword)))
            }

            found => {
                handler.receive(Error {
                    expected: SyntaxKind::RefutablePattern,
                    alternatives: Vec::new(),
                    found: found.into_token(),
                });
                parser.forward();
                None
            }
        }
    }
}

impl<'a> Parser<'a> {
    /// Parses an [`Irrefutable`] pattern.
    pub fn parse_irrefutable_pattern(
        &mut self,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Irrefutable> {
        Irrefutable::parse(self, handler)
    }

    /// Parses a [`Refutable`] pattern.
    pub fn parse_refutable_pattern(
        &mut self,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Refutable> {
        Refutable::parse(self, handler)
    }
}

#[cfg(test)]
pub(super) mod tests;
