//! Contains all definition of pattern syntax trees.

use std::{fmt::Debug, option::Option};

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
    expression::{Boolean, Numeric},
    ConnectedList, Qualifier,
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
    fn span(&self) -> Span {
        self.identifier.span().join(&self.pattern().span()).unwrap()
    }
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
            Self::Association(field_with_association) => {
                field_with_association.span()
            }
            Self::Named(field_without_association) => {
                field_without_association.span()
            }
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
    fn span(&self) -> Span {
        self.left_brace.span.join(&self.right_brace.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// EnumAssociation:
///     '(' Pattern ')'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct EnumAssociation<Pattern> {
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    pattern: Box<Pattern>,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl<Pattern> SourceElement for EnumAssociation<Pattern> {
    fn span(&self) -> Span {
        self.left_paren.span.join(&self.right_paren.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Enum:
///     'case' Identifier EnumAssociation?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Enum<Pattern> {
    #[get = "pub"]
    case_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    association: Option<EnumAssociation<Pattern>>,
}

impl<Pattern> SourceElement for Enum<Pattern> {
    fn span(&self) -> Span {
        self.association.as_ref().map_or_else(
            || self.case_keyword.span().join(&self.identifier.span()).unwrap(),
            |association| {
                self.case_keyword.span().join(&association.span()).unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Wildcard:
///     '?'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Wildcard {
    #[get = "pub"]
    question_mark: Punctuation,
}

impl SourceElement for Wildcard {
    fn span(&self) -> Span { self.question_mark.span() }
}

/// Syntax Synopsis:
/// ``` txt
/// Unpacked:
///     '...' Pattern
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Unpacked<Pattern> {
    #[get = "pub"]
    ellipsis: (Punctuation, Punctuation, Punctuation),
    #[get = "pub"]
    pattern: Box<Pattern>,
}

impl<Pattern: SourceElement> SourceElement for Unpacked<Pattern> {
    fn span(&self) -> Span {
        self.ellipsis.0.span.join(&self.pattern.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TupleElement:
///     Unpacked
///     | Pattern
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum TupleElement<Pattern> {
    Unpacked(Unpacked<Pattern>),
    Regular(Box<Pattern>),
}

impl<Pattern: SourceElement> SourceElement for TupleElement<Pattern> {
    fn span(&self) -> Span {
        match self {
            Self::Unpacked(unpack) => unpack.span(),
            Self::Regular(pattern) => pattern.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Tuple:
///     '(' (TupleElement (',' TupleElement)* ','?)? ')'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Tuple<Pattern> {
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    patterns: Option<ConnectedList<TupleElement<Pattern>, Punctuation>>,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl<Pattern> SourceElement for Tuple<Pattern> {
    fn span(&self) -> Span {
        self.left_paren.span.join(&self.right_paren.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Ref:
///     `ref` Qualifier?
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Ref {
    #[get = "pub"]
    ref_keyword: Keyword,
    #[get = "pub"]
    qualifier: Option<Qualifier>,
}

impl SourceElement for Ref {
    fn span(&self) -> Span {
        self.qualifier().as_ref().map_or_else(
            || self.ref_keyword.span(),
            |end| self.ref_keyword.span().join(&end.span()).unwrap(),
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// NamedKind:
///     Ref
///     | 'mutable'?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum NamedKind {
    Ref(Ref),
    Value { mutable_keyword: Option<Keyword> },
}

/// Syntax Synopsis:
/// ``` txt
/// Named:
///     'ref'? 'mutable'? Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Named {
    #[get = "pub"]
    kind: NamedKind,
    #[get = "pub"]
    identifier: Identifier,
}

impl SourceElement for Named {
    fn span(&self) -> Span {
        match &self.kind {
            NamedKind::Ref(r) => {
                r.span().join(&self.identifier.span()).unwrap()
            }
            NamedKind::Value { mutable_keyword } => {
                mutable_keyword.as_ref().map_or_else(
                    || self.identifier.span(),
                    |keyword| {
                        keyword.span().join(&self.identifier.span()).unwrap()
                    },
                )
            }
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Refutable:
///     Boolean
///     | Numeric
///     | Structural
///     | Enum
///     | Named
///     | Tuple
/// ```
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum Refutable {
    Boolean(Boolean),
    Numeric(Numeric),
    Structural(Structural<Self>),
    Enum(Enum<Self>),
    Named(Named),
    Tuple(Tuple<Self>),
    Wildcard(Wildcard),
}

impl SourceElement for Refutable {
    fn span(&self) -> Span {
        match self {
            Self::Boolean(boolean_literal) => boolean_literal.span(),
            Self::Numeric(numeric_literal) => numeric_literal.span(),
            Self::Structural(structural) => structural.span(),
            Self::Enum(associated_enum) => associated_enum.span(),
            Self::Named(identifier) => identifier.span(),
            Self::Tuple(tuple_pattern) => tuple_pattern.span(),
            Self::Wildcard(wildcard) => wildcard.span(),
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
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum Irrefutable {
    Structural(Structural<Self>),
    Named(Named),
    Tuple(Tuple<Self>),
    Wildcard(Wildcard),
}

impl SourceElement for Irrefutable {
    fn span(&self) -> Span {
        match self {
            Self::Structural(structural) => structural.span(),
            Self::Named(identifier) => identifier.span(),
            Self::Tuple(tuple_pattern) => tuple_pattern.span(),
            Self::Wildcard(wildcard) => wildcard.span(),
        }
    }
}

trait Pattern {
    fn parse(
        parser: &mut Parser,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Self>
    where
        Self: Sized;
}

impl<'a> Parser<'a> {
    fn parse_named_pattern(
        &mut self,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Named> {
        match self.stop_at_significant() {
            Reading::Unit(Token::Keyword(mutable_keyword))
                if mutable_keyword.kind == KeywordKind::Mutable =>
            {
                // eat the mutable keyword
                self.forward();

                let identifier = self.parse_identifier(handler)?;

                Some(Named {
                    kind: NamedKind::Value {
                        mutable_keyword: Some(mutable_keyword),
                    },
                    identifier,
                })
            }

            Reading::Unit(Token::Keyword(ref_keyword))
                if ref_keyword.kind == KeywordKind::Ref =>
            {
                // eat ref keyword
                self.forward();

                // parse qualifier
                let qualifier = match self.stop_at_significant() {
                    Reading::Unit(Token::Keyword(mutable_keyword))
                        if mutable_keyword.kind == KeywordKind::Mutable =>
                    {
                        // eat mutable keyword
                        self.forward();

                        Some(Qualifier::Mutable(mutable_keyword))
                    }

                    Reading::Unit(Token::Keyword(unique_keyword))
                        if unique_keyword.kind == KeywordKind::Unique =>
                    {
                        // eat unique keyword
                        self.forward();

                        Some(Qualifier::Unique(unique_keyword))
                    }

                    _ => None,
                };

                let identifier = self.parse_identifier(handler)?;

                Some(Named {
                    kind: NamedKind::Ref(Ref { ref_keyword, qualifier }),
                    identifier,
                })
            }

            Reading::Unit(Token::Identifier(identifier)) => {
                self.forward();
                Some(Named {
                    kind: NamedKind::Value { mutable_keyword: None },
                    identifier,
                })
            }

            found => {
                handler.receive(Error {
                    expected: SyntaxKind::Identifier,
                    alternatives: vec![
                        SyntaxKind::Keyword(KeywordKind::Mutable),
                        SyntaxKind::Keyword(KeywordKind::Ref),
                    ],
                    found: found.into_token(),
                });
                None
            }
        }
    }

    fn parse_structural_pattern<T: Pattern + Debug>(
        &mut self,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Structural<T>> {
        let enclosed_tree = self.parse_enclosed_list(
            Delimiter::Brace,
            ',',
            |parser| {
                let named_pattern = parser.parse_named_pattern(handler)?;

                // can accept a nested pattern
                if named_pattern.kind.as_value().map_or(false, Option::is_none)
                    && matches!(
                        parser.stop_at_significant(),
                        Reading::Unit(Token::Punctuation(punc))
                        if punc.punctuation == ':'
                    )
                {
                    let colon = parser.parse_punctuation(':', true, handler)?;
                    let pattern = T::parse(parser, handler)?;

                    Some(Field::Association(FieldAssociation {
                        identifier: named_pattern.identifier,
                        colon,
                        pattern: Box::new(pattern),
                    }))
                } else {
                    Some(Field::Named(named_pattern))
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

                match (
                    parser.peek(),
                    parser.peek_offset(1),
                    parser.peek_offset(2),
                ) {
                    (
                        Reading::Unit(Token::Punctuation(
                            p1 @ Punctuation { punctuation: '.', .. },
                        )),
                        Some(Reading::Unit(Token::Punctuation(
                            p2 @ Punctuation { punctuation: '.', .. },
                        ))),
                        Some(Reading::Unit(Token::Punctuation(
                            p3 @ Punctuation { punctuation: '.', .. },
                        ))),
                    ) => {
                        let ellipsis = (p1, p2, p3);

                        parser.forward();
                        parser.forward();
                        parser.forward();

                        let pattern = Box::new(T::parse(parser, handler)?);

                        Some(TupleElement::Unpacked(Unpacked {
                            ellipsis,
                            pattern,
                        }))
                    }

                    _ => T::parse(parser, handler).map(|pattern| {
                        TupleElement::Regular(Box::new(pattern))
                    }),
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
}

impl Pattern for Irrefutable {
    fn parse(
        parser: &mut Parser,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Self> {
        match parser.stop_at_significant() {
            // parse wildcard pattern
            Reading::Unit(Token::Punctuation(punc))
                if punc.punctuation == '?' =>
            {
                parser.forward();
                Some(Self::Wildcard(Wildcard { question_mark: punc }))
            }

            // parse named pattern
            Reading::Unit(
                Token::Keyword(Keyword {
                    kind: KeywordKind::Mutable | KeywordKind::Ref,
                    ..
                })
                | Token::Identifier(_),
            ) => parser.parse_named_pattern(handler).map(Self::Named),

            // parse tuple pattern
            Reading::IntoDelimited(Delimiter::Parenthesis, _) => {
                parser.parse_tuple_pattern(handler).map(Self::Tuple)
            }

            // parse structural pattern
            Reading::IntoDelimited(Delimiter::Brace, _) => {
                parser.parse_structural_pattern(handler).map(Self::Structural)
            }

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
    fn parse(
        parser: &mut Parser,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Self> {
        match parser.stop_at_significant() {
            // parse wildcard pattern
            Reading::Unit(Token::Punctuation(punc))
                if punc.punctuation == '?' =>
            {
                parser.forward();
                Some(Self::Wildcard(Wildcard { question_mark: punc }))
            }

            // parse named pattern
            Reading::Unit(
                Token::Keyword(Keyword {
                    kind: KeywordKind::Mutable | KeywordKind::Ref,
                    ..
                })
                | Token::Identifier(_),
            ) => parser.parse_named_pattern(handler).map(Self::Named),

            Reading::Unit(Token::Keyword(case_keyword))
                if case_keyword.kind == KeywordKind::Case =>
            {
                // eat the case keyword
                parser.forward();

                let identifier = parser.parse_identifier(handler)?;

                let association = if matches!(
                    parser.stop_at_significant(),
                    Reading::IntoDelimited(Delimiter::Parenthesis, _)
                ) {
                    let tree = parser.step_into(
                        Delimiter::Parenthesis,
                        |parser| parser.parse_refutable_pattern(handler),
                        handler,
                    )?;

                    Some(EnumAssociation {
                        left_paren: tree.open,
                        pattern: Box::new(tree.tree?),
                        right_paren: tree.close,
                    })
                } else {
                    None
                };

                Some(Self::Enum(Enum { case_keyword, identifier, association }))
            }

            // parse tuple pattern
            Reading::IntoDelimited(Delimiter::Parenthesis, _) => {
                parser.parse_tuple_pattern(handler).map(Self::Tuple)
            }

            // parse structural pattern
            Reading::IntoDelimited(Delimiter::Brace, _) => {
                parser.parse_structural_pattern(handler).map(Self::Structural)
            }

            // parse numeric literal pattern
            Reading::Unit(Token::Numeric(_)) => {
                Some(Self::Numeric(parser.parse_numeric_literal()?))
            }

            // parse boolean literal pattern
            Reading::Unit(Token::Keyword(boolean_keyword))
                if matches!(
                    boolean_keyword.kind,
                    KeywordKind::True | KeywordKind::False
                ) =>
            {
                parser.forward();
                let constructor = match boolean_keyword.kind {
                    KeywordKind::True => Boolean::True,
                    KeywordKind::False => Boolean::False,
                    _ => unreachable!(),
                };
                Some(Self::Boolean(constructor(boolean_keyword)))
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
