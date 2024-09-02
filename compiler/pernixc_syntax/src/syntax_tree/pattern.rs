//! Contains all definition of pattern syntax trees.

use std::{fmt::Debug, option::Option};

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_lexical::{
    token::{self, Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};

use super::{expression::Boolean, ConnectedList, Qualifier};
use crate::{
    error::{self, Error, SyntaxKind},
    parser::{Parser, Reading},
};

pub mod strategy;

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
///     '{' (Field (',' Field)* ','?)? '..'? '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Structural<Pattern> {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    fields: Option<ConnectedList<Field<Pattern>, Punctuation>>,
    #[get = "pub"]
    wildcard: Option<Wildcard>,
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
///     '..'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Wildcard(Punctuation, Punctuation);

impl SourceElement for Wildcard {
    fn span(&self) -> Span { self.0.span.join(&self.1.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// TupleElement:
///     '...'? Pattern
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TupleElement<Pattern> {
    #[get = "pub"]
    ellipsis: Option<(Punctuation, Punctuation, Punctuation)>,
    #[get = "pub"]
    pattern: Box<Pattern>,
}

impl<Pattern: SourceElement> SourceElement for TupleElement<Pattern> {
    fn span(&self) -> Span {
        self.ellipsis.as_ref().map_or_else(
            || self.pattern.span(),
            |(dots, _, _)| dots.span().join(&self.pattern.span()).unwrap(),
        )
    }
}

impl<Pattern> TupleElement<Pattern> {
    /// Dissolves the tuple element into its components.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (Option<(Punctuation, Punctuation, Punctuation)>, Box<Pattern>) {
        (self.ellipsis, self.pattern)
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
/// Binding:
///     Ref
///     | 'mutable'?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Binding {
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
    binding: Binding,
    #[get = "pub"]
    identifier: Identifier,
}

impl SourceElement for Named {
    fn span(&self) -> Span {
        match &self.binding {
            Binding::Ref(r) => r.span().join(&self.identifier.span()).unwrap(),
            Binding::Value { mutable_keyword } => {
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
/// Integer:
///     '-'?
///     NumericToken
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Integer {
    #[get = "pub"]
    minus: Option<Punctuation>,
    #[get = "pub"]
    numeric: token::Numeric,
}

impl SourceElement for Integer {
    fn span(&self) -> Span {
        self.minus.as_ref().map_or_else(
            || self.numeric.span(),
            |minus| minus.span().join(&self.numeric.span()).unwrap(),
        )
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
    Integer(Integer),
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
            Self::Integer(numeric_literal) => numeric_literal.span(),
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

impl Irrefutable {
    /// Returns `true` if the pattern contains a named pattern.
    pub fn contains_named(&self) -> bool {
        match self {
            Self::Structural(structural) => {
                structural.fields.iter().flat_map(ConnectedList::elements).any(
                    |x| match x {
                        Field::Association(pattern) => {
                            pattern.pattern.contains_named()
                        }
                        Field::Named(_) => true,
                    },
                )
            }
            Self::Named(_) => true,
            Self::Tuple(tuple) => tuple
                .patterns
                .iter()
                .flat_map(ConnectedList::elements)
                .any(|x| x.pattern.contains_named()),
            Self::Wildcard(_) => false,
        }
    }
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
                    binding: Binding::Value {
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
                    binding: Binding::Ref(Ref { ref_keyword, qualifier }),
                    identifier,
                })
            }

            Reading::Unit(Token::Identifier(identifier)) => {
                self.forward();
                Some(Named {
                    binding: Binding::Value { mutable_keyword: None },
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
                    found: self.reading_to_found(found),
                });
                None
            }
        }
    }

    fn parse_field<T: Pattern + Debug>(
        &mut self,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Field<T>> {
        let named_pattern = self.parse_named_pattern(handler)?;

        // can accept a nested pattern
        if named_pattern.binding.as_value().map_or(false, Option::is_none)
            && matches!(
                self.stop_at_significant(),
                Reading::Unit(Token::Punctuation(punc))
                if punc.punctuation == ':'
            )
        {
            let colon = self.parse_punctuation(':', true, handler)?;
            let pattern = T::parse(self, handler)?;

            Some(Field::Association(FieldAssociation {
                identifier: named_pattern.identifier,
                colon,
                pattern: Box::new(pattern),
            }))
        } else {
            Some(Field::Named(named_pattern))
        }
    }

    fn parse_structural_pattern<T: Pattern + Debug>(
        &mut self,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Structural<T>> {
        fn skip_to_next_separator(
            this: &mut Parser,
            separator: char,
        ) -> Option<Punctuation> {
            if let Reading::Unit(Token::Punctuation(pun)) =
                this.stop_at(|token| {
                    matches!(
                        token, Reading::Unit(Token::Punctuation(pun))
                        if pun.punctuation == separator
                    )
                })
            {
                this.forward();
                Some(pun)
            } else {
                None
            }
        }
        let tree = self.step_into(
            Delimiter::Brace,
            |parser| {
                let mut first = None;
                let mut rest = Vec::new();
                let mut trailing_separator: Option<Punctuation> = None;
                let mut wildcard = None;

                while !parser.is_exhausted() && wildcard.is_none() {
                    if let (
                        Reading::Unit(Token::Punctuation(
                            first_dot @ Punctuation { punctuation: '.', .. },
                        )),
                        Some(Reading::Unit(Token::Punctuation(
                            second_dot @ Punctuation {
                                punctuation: '.', ..
                            },
                        ))),
                    ) = (parser.stop_at_significant(), parser.peek_offset(1))
                    {
                        // eat the first dot
                        parser.forward();
                        parser.forward();

                        wildcard = Some(Wildcard(first_dot, second_dot));
                        break;
                    }

                    let Some(element) = parser.parse_field(handler) else {
                        skip_to_next_separator(parser, ',');
                        continue;
                    };

                    // adds new element
                    match (&first, &trailing_separator) {
                        (None, None) => {
                            first = Some(element);
                        }
                        (Some(_), Some(separator)) => {
                            rest.push((separator.clone(), element));
                            trailing_separator = None;
                        }
                        (first, trailing_separator) => {
                            unreachable!("{first:?} {trailing_separator:?}")
                        }
                    }

                    // expect separator if not exhausted
                    if !parser.is_exhausted() {
                        if let (
                            Reading::Unit(Token::Punctuation(
                                first_dot @ Punctuation {
                                    punctuation: '.', ..
                                },
                            )),
                            Some(Reading::Unit(Token::Punctuation(
                                second_dot @ Punctuation {
                                    punctuation: '.',
                                    ..
                                },
                            ))),
                        ) = (
                            parser.stop_at_significant(),
                            parser.peek_offset(1),
                        ) {
                            // eat the first dot
                            parser.forward();
                            parser.forward();

                            wildcard = Some(Wildcard(first_dot, second_dot));
                            break;
                        }

                        let Some(separator) =
                            parser.parse_punctuation(',', true, handler)
                        else {
                            if let Some(punctuation) =
                                skip_to_next_separator(parser, ',')
                            {
                                trailing_separator = Some(punctuation);
                            }

                            continue;
                        };

                        trailing_separator = Some(separator);
                    }
                }

                Some((
                    first.map(|first| ConnectedList {
                        first,
                        rest,
                        trailing_separator,
                    }),
                    wildcard,
                ))
            },
            handler,
        )?;

        let inner_tree = tree.tree?;
        Some(Structural {
            left_brace: tree.open,
            fields: inner_tree.0,
            wildcard: inner_tree.1,
            right_brace: tree.close,
        })
    }

    fn parse_tuple_pattern<T: Pattern + Debug>(
        &mut self,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Tuple<T>> {
        let enclosed_tree = self.parse_delimited_list(
            Delimiter::Parenthesis,
            ',',
            |parser| {
                let ellipsis = if parser
                    .stop_at_significant()
                    .into_token()
                    .and_then(|x| x.into_punctuation().ok())
                    .map_or(false, |x| x.punctuation == '.')
                    && parser
                        .peek_offset(1)
                        .and_then(Reading::into_token)
                        .and_then(|x| x.into_punctuation().ok())
                        .map_or(false, |x| x.punctuation == '.')
                    && parser
                        .peek_offset(2)
                        .and_then(Reading::into_token)
                        .and_then(|x| x.into_punctuation().ok())
                        .map_or(false, |x| x.punctuation == '.')
                {
                    let first_punc =
                        parser.parse_punctuation('.', false, handler)?;
                    let second_punc =
                        parser.parse_punctuation('.', false, handler)?;
                    let third_punc =
                        parser.parse_punctuation('.', false, handler)?;

                    Some((first_punc, second_punc, third_punc))
                } else {
                    None
                };

                let pattern = Box::new(T::parse(parser, handler)?);

                Some(TupleElement { ellipsis, pattern })
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
                if punc.punctuation == '.' =>
            {
                parser.forward();

                let second_punc =
                    parser.parse_punctuation('.', false, handler)?;

                Some(Self::Wildcard(Wildcard(punc, second_punc)))
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
                    found: parser.reading_to_found(found),
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
                if punc.punctuation == '.' =>
            {
                parser.forward();

                let second_punc =
                    parser.parse_punctuation('.', false, handler)?;

                Some(Self::Wildcard(Wildcard(punc, second_punc)))
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

            // parse integer literal pattern
            Reading::Unit(Token::Numeric(numeric)) => {
                parser.forward();

                Some(Self::Integer(Integer { minus: None, numeric }))
            }

            // parse integer literal pattern with minus sign
            Reading::Unit(Token::Punctuation(
                punc @ Punctuation { punctuation: '-', .. },
            )) => {
                parser.forward();

                let numeric = parser.parse_numeric(handler)?;

                Some(Self::Integer(Integer { minus: Some(punc), numeric }))
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
                    found: parser.reading_to_found(found),
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
mod test;
