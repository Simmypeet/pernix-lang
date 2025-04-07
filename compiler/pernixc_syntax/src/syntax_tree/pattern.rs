//! Contains all definition of pattern syntax trees.

use std::option::Option;

use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{self, Identifier, Keyword, KeywordKind, Punctuation},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, Span};

use super::{
    expression::unit::Boolean, ConnectedList, EnclosedConnectedList,
    EnclosedTree, Parse, ParseExt, ReferenceOf, SyntaxTree,
};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Branch, ExpectExt},
        StateMachine,
    },
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldAssociation<Pattern> {
    pub identifier: Identifier,
    pub colon: Punctuation,
    pub pattern: Box<Pattern>,
}

impl<Pattern: SyntaxTree + 'static> SyntaxTree for FieldAssociation<Pattern> {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            expect::Identifier.to_owned(),
            ':'.to_owned(),
            Pattern::parse.map(Box::new),
        )
            .map(|(identifier, colon, pattern)| Self {
                identifier,
                colon,
                pattern,
            })
            .parse(state_machine, handler)
    }
}

impl<Pattern: SourceElement> SourceElement for FieldAssociation<Pattern> {
    fn span(&self) -> Span { self.identifier.span().join(&self.pattern.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Field<Pattern> {
    Association(FieldAssociation<Pattern>),
    Named(Named),
}

impl<Pattern: SyntaxTree + 'static> SyntaxTree for Field<Pattern> {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            FieldAssociation::parse.map(Self::Association),
            Named::parse.map(Self::Named),
        )
            .branch()
            .parse(state_machine, handler)
    }
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Structural<Pattern> {
    pub left_brace: Punctuation,
    pub fields: Option<ConnectedList<Field<Pattern>, Punctuation>>,
    pub wildcard: Option<Wildcard>,
    pub right_brace: Punctuation,
}

impl<Pattern: SyntaxTree + 'static> SyntaxTree for Structural<Pattern> {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Field::parse.connected_list(','.to_owned()).or_none(),
            Wildcard::parse.or_none(),
        )
            .step_into_delimited(DelimiterKind::Brace)
            .map(|(open, (fields, wildcard), close)| Self {
                left_brace: open.clone(),
                fields,
                wildcard,
                right_brace: close.clone(),
            })
            .parse(state_machine, handler)
    }
}

impl<Pattern> SourceElement for Structural<Pattern> {
    fn span(&self) -> Span { self.left_brace.span.join(&self.right_brace.span) }
}

pub type EnumAssociation = EnclosedTree<Box<Refutable>>;

impl SyntaxTree for EnumAssociation {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Refutable::parse
            .map(Box::new)
            .enclosed_tree(DelimiterKind::Parenthesis)
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    pub case_keyword: Keyword,
    pub identifier: Identifier,
    pub association: Option<EnumAssociation>,
}

impl SyntaxTree for Enum {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Case.to_owned(),
            expect::Identifier.to_owned(),
            EnumAssociation::parse.or_none(),
        )
            .map(|(case_keyword, identifier, association)| Self {
                case_keyword,
                identifier,
                association,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Enum {
    fn span(&self) -> Span {
        self.association.as_ref().map_or_else(
            || self.case_keyword.span().join(&self.identifier.span()),
            |association| self.case_keyword.span().join(&association.span()),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Wildcard(Punctuation, Punctuation);

impl SyntaxTree for Wildcard {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('.'.to_owned(), '.'.no_skip().to_owned())
            .map(|(f, s)| Self(f, s))
            .parse(state_machine, handler)
    }
}

impl SourceElement for Wildcard {
    fn span(&self) -> Span { self.0.span.join(&self.1.span) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleElement<Pattern> {
    pub ellipsis: Option<(Punctuation, Punctuation, Punctuation)>,
    pub pattern: Box<Pattern>,
}

impl<Pattern: SyntaxTree + 'static> SyntaxTree for TupleElement<Pattern> {
    fn parse(
        parser: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            (
                '.'.to_owned(),
                '.'.no_skip().to_owned(),
                '.'.no_skip().to_owned(),
            )
                .commit_in(3)
                .or_none(),
            Pattern::parse.map(Box::new),
        )
            .map(|(ellipsis, pattern)| Self { ellipsis, pattern })
            .parse(parser, handler)
    }
}

impl<Pattern: SourceElement> SourceElement for TupleElement<Pattern> {
    fn span(&self) -> Span {
        self.ellipsis.as_ref().map_or_else(
            || self.pattern.span(),
            |(dots, _, _)| dots.span().join(&self.pattern.span()),
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

pub type Tuple<Pattern> =
    EnclosedConnectedList<TupleElement<Pattern>, Punctuation>;

impl<Pattern: SyntaxTree + 'static> SyntaxTree for Tuple<Pattern> {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        TupleElement::parse
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Parenthesis)
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Named {
    pub mutable_keyword: Option<Keyword>,
    pub reference_of: Option<ReferenceOf>,
    pub identifier: Identifier,
}

impl SyntaxTree for Named {
    fn parse(
        parser: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Mut.to_owned().or_none(),
            ReferenceOf::parse.or_none(),
            expect::Identifier.to_owned(),
        )
            .map(|(mutable_keyword, reference_of, identifier)| Self {
                mutable_keyword,
                reference_of,
                identifier,
            })
            .parse(parser, handler)
    }
}

impl SourceElement for Named {
    fn span(&self) -> Span {
        self.mutable_keyword.as_ref().map_or_else(
            || {
                self.reference_of.as_ref().map_or_else(
                    || self.identifier.span(),
                    |qualifier| qualifier.span().join(&self.identifier.span()),
                )
            },
            |mutable_keyword| {
                mutable_keyword.span().join(&self.identifier.span())
            },
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Integer {
    pub minus: Option<Punctuation>,
    pub numeric: token::Numeric,
}

impl SyntaxTree for Integer {
    fn parse(
        parser: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('-'.to_owned().or_none(), expect::Numeric.to_owned())
            .map(|(minus, numeric)| Self { minus, numeric })
            .parse(parser, handler)
    }
}

impl SourceElement for Integer {
    fn span(&self) -> Span {
        self.minus.as_ref().map_or_else(
            || self.numeric.span(),
            |minus| minus.span().join(&self.numeric.span()),
        )
    }
}

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
    Enum(Enum),
    Named(Named),
    Tuple(Tuple<Self>),
    Wildcard(Wildcard),
}

impl SyntaxTree for Refutable {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Boolean::parse.map(Self::Boolean),
            Integer::parse.map(Self::Integer),
            Structural::parse.map(Self::Structural),
            Enum::parse.map(Self::Enum),
            Named::parse.map(Self::Named),
            Tuple::parse.map(Self::Tuple),
            Wildcard::parse.map(Self::Wildcard),
        )
            .branch()
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for Irrefutable {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Structural::parse.map(Self::Structural),
            Named::parse.map(Self::Named),
            Tuple::parse.map(Self::Tuple),
            Wildcard::parse.map(Self::Wildcard),
        )
            .branch()
            .parse(state_machine, handler)
    }
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
                .connected_list
                .as_ref()
                .into_iter()
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
